---
title: "DTensor erasure"
date: 2026-02-01T23:20:12-05:00
slug: "dtensor-erasure"
categories: [PyTorch]
---

DTensor has famously terrible eager mode performance; for example, [this paper](https://arxiv.org/abs/2509.07003v1) measured a 35-60% slowdown in end-to-end training performance with and without DTensor (with DTensor operations taking at least 7x longer than actually running the computation for real).  While it is possible to alleviate some of this slowdown via optimizations (in the paper, veScale shows fast bypass of sharding propagation, improved cache lookups and C++ code can take dispatch overhead to 30us), this is still too high for some settings.

* You could eliminate this overhead by rewriting your code *without* DTensors at all, but this gives up the benefits of expressing your code in [global SPMD form]({{<relref "global-vs-local-spmd.md">}}).
* You could eliminate the overhead by using a compiler or CUDA graphs, but this requires your code to be traceable.

Is there a way to have our cake (global SPMD) while eating it too (eager mode with no DTensor)?  veScale proposed [Static Eager mode](https://arxiv.org/html/2509.07003v1#S6.SS2) as a way of eliminating DTensor at runtime, by observing that DTensor placements are largely static in a program, which means that you can just drop DTensor at runtime as long as you manually insert any communication that would have occurred if you had run with DTensor (veScale does extra work under the hood to add hooks for these communications).  However, it is quite difficult for researchers to understand how to insert redistributes / gradient redistributes.  In this blog post, I want to reimagine their system under the following constraint: what if you could just erase the DTensors, without having to add any hooks at all?  Spoiler alert: [JAX-style sharding in types]({{<relref "jax-sharding-type-system.md">}}) without implicit conversions is enough to get the job done.

First, let's describe the problem more carefully.  Typically, a desirable property for type systems is that the types can be erased before execution without changing the runtime behavior of a program.  In the case of DTensor, we want to erase all of the placements in a program, with the hope that we can still run everything we need to without them.  Actually, most of the time in DTensor this will *work*, because the ideal situation for DTensor is that you just run the operation as-is on the local tensors.  The problem is, of course, `redistribute`, which needs to know what the original placement of a DTensor is to issue the collectives to get it into the new desired placement.  Even worse, to detect if an implicit redistribution would occur, you need to compute that input placements are illegal and how to insert redistributes to make it legal.

The explicit redistribute problem is easy enough to fix: a user could specify a specific collective (which, with DTensors, would be checked for consistency against the actual input/output placements), or we could ask the user to specify the input placements so that we can still compute the sequence of collectives to get to the output placement.  To avoid implicit redistributions in forwards, one can simply ensure you insert explicit redistributes anywhere they are needed; to avoid implicit redistributions in backwards, you need a type system that guarantees that the backwards collectives correspond precisely to forward collectives.  You need two ideas from the [JAX sharding type system]({{<relref "jax-sharding-type-system.md">}}): first, the backward gradient placement should always be computable from the forward primal placement, so you can always reason locally about if comms need (as you always know the placement of `grad_output`.)  Second, you need enough vocabulary (e.g., reduced/unreduced) to ensure that these forced backward gradient placements don't lead to communication inefficiencies.

Once your user program has a DTensor erasure property, you now have code that can be run with *either* plain Tensor or DTensor.  Running with DTensor is akin to running a (dynamic) type checker on the program: in explicit mode it will error if implicit redistributes occur, and it will also error if you messed up and claimed that something was in some placement when it was not.  Running with Tensor, you just elide all of the shard checking and trust that the user program was written correctly.  This works if the user program doesn't branch; if you are worried about inexhaustive testing, you could run under both DTensor and `torch.compile`, where guards and Dynamo can help you identify whether or not you have actually exercised all potential inputs or not.

The resulting code you write is very similar to a Megatron-style training framework, but with the twist that you can check that you inserted all of your collectives by running with DTensors rather than Tensors.  More generally, this is an interesting pattern for **gradual compilation**; your program can be entirely *run* in eager mode, and the compiler is relegated to a sideband static analysis tool (akin to an optional type checker), which still can be an essential part of the workflow for catching and debugging problems.

# Appendix

**Sum should not cast to Partial.** In classic PyTorch DTensor, a sum on a sharded dimension performs no communications and produces a Partial placement.  This behavior cannot be supported under DTensor erasure.  Let us reason through it:

```
input: Shard(0)

sum = input.sum(0)
sum: Partial()

grad_sum = torch.ones_like(sum)
grad_sum: Replicate()  # if we suppose Partial() <-> Replicate()

grad_input = grad_sum.expand(input.shape)
grad_input: Replicate()
```

We see that the backwards formula for sum is a plain eager operation that expands `grad_sum`.  This operation will produce a Replicate tensor.  But the primal-cotangent sharding mapping specifies that `grad_input` should be a `Shard(0)` tensor; a (free) redistribute from `Replicate` to `Shard(0)` is required.  In ordinary DTensor, we discover this redistribution happens later when there is a shard-replicate interaction; however, with DTensor erasure, there is no way to discover this, and we must ban this `sum`.

To resolve the problem, we simply need to distinguish between two distinct APIs for sum.  Standard `torch.sum` should only ever do a local summation and error out if the reduction is across a sharded dimension.  Cast to partial (aka `lax.pcast(to='unreduced')` in JAX) is a separate function that only works on sharded tensors.  The distinct function can now be associated with a custom autograd function that triggers the re-sharding in backwards.

Update (Feb 8, 2026): The analysis here is incomplete.  In particular, it assumes that differentiation is done *on the global tensor*.  If instead we desugar global SPMD into local SPMD operations and run a local expand operation, I don't expand "too much" and consequently have to slice it again.  So it is possible sum can still cast to partial in global SPMD.

**Broadcasts over shards require conversion to 'reduced'.**  Above, I claimed that JAX's explicit mode for global SPMD only issues collectives when explicitly asked to do so.  But this isn't completely true.  When I do an operation between a sharded and replicated tensor (under JAX semantics), this can result in an implicit all-reduce in backwards:

```
import jax
import jax.numpy as jnp

jax.config.update('jax_num_cpu_devices', 4)
jax.set_mesh(jax.make_mesh((4,), ('dp',)))

batch = 8
hidden = 16
out_dim = 4

x = jax.device_put(
    jnp.ones((batch, hidden), dtype=jnp.float32),
    jax.P('dp', None)
)
w = jax.device_put(
    jnp.ones((hidden, out_dim), dtype=jnp.float32),
    jax.P(None, None)
)
grad_out = jax.device_put(
    jnp.ones((batch, out_dim), dtype=jnp.float32),
    jax.P('dp', None)
)

def forward(x, w):
    return jnp.einsum("bh,ho->bo", x, w)

def backward(x, w, grad_out):
    _, vjp_fn = jax.vjp(forward, x, w)
    return vjp_fn(grad_out)[1]  # gradient w.r.t. w

compiled = jax.jit(backward).lower(x, w, grad_out).compile()
print(compiled.as_text())
```

This prints:

```
%add.clone (x.3: f32[], y.1: f32[]) -> f32[] {
  %x.3 = f32[] parameter(0)
  %y.1 = f32[] parameter(1)
  ROOT %add.1 = f32[] add(%x.3, %y.1)
}

%fused_computation (param_0.1: f32[4,16]) -> f32[16,4] {
  %param_0.1 = f32[4,16]{1,0} parameter(0)
  %transpose.7 = f32[16,4]{0,1} transpose(%param_0.1), dimensions={1,0}, metadata={op_name="jit(backward)/transpose(jvp(bh,ho->bo))/transpose" stack_frame_id=3}
  ROOT %copy.1 = f32[16,4]{1,0} copy(%transpose.7), metadata={op_name="jit(backward)/transpose(jvp(bh,ho->bo))/transpose" stack_frame_id=3}
}

ENTRY %main.0_spmd (param.1: f32[2,16], param: f32[2,4]) -> f32[16,4] {
  %param = f32[2,4]{1,0} parameter(1), sharding={devices=[4,1]<=[4]}, metadata={op_name="grad_out"}
  %param.1 = f32[2,16]{1,0} parameter(0), sharding={devices=[4,1]<=[4]}, metadata={op_name="x"}
  %dot = f32[4,16]{1,0} dot(%param, %param.1), lhs_contracting_dims={0}, rhs_contracting_dims={0}, metadata={op_name="jit(backward)/transpose(jvp(bh,ho->bo))/dot_general" stack_frame_id=3}
  %all-reduce = f32[4,16]{1,0} all-reduce(%dot), channel_id=1, replica_groups=[1,4]<=[4], use_global_device_ids=true, to_apply=%add.clone, metadata={op_name="jit(backward)/transpose(jvp(bh,ho->bo))/dot_general" stack_frame_id=3}
  ROOT %transpose_copy_fusion = f32[16,4]{1,0} fusion(%all-reduce), kind=kLoop, calls=%fused_computation, metadata={op_name="jit(backward)/transpose(jvp(bh,ho->bo))/transpose" stack_frame_id=3}
}
```

We can see the original program has no collectives, but the HLO has an `all-reduce`.  Actually, this is just the classic all-reduce you have to do to the gradients of all parameters when doing DP, so this isn't exactly surprising.  This is bad for DTensor erasure, though, because the way JAX knows to insert the collective here is by noticing that the backwards of the linear produces an unreduced result, but sharding-in-types demands that the gradient be replicated.

Now, we could fix the *particular* case of DP by arguing that DP sharding is special and it's the responsibility of the DP framework to know that a reduction is necessary (this is how torchtitan on DTensor classically operates: we don't represent DP directly in the DTensor, and FSDP is responsible for actually doing the all-reduces and grad scaling.)  A more theoretically sound solution, however, is to simply say that einsum should forbid broadcasting a replicated tensor with a sharded tensor (even though in forwards this can be done without any communication); instead, in this case, you must have a reduced tensor on the sharded mesh axis, so that the gradient is an unreduced tensor on that mesh axis.  A conversion from replicate to reduced will trigger the all-reduce in backwards.

We can do an analysis on einsum to see that the broadcast situation is the only situation where these extra reductions can occur.  Recall from [Computing sharding with einsum]({{<relref "computing-sharding-with-einsum.md">}}) that when we compute the gradient for an input, we interchange the indices for that input with the indices for the output in the einsum formula.  We can do a case-by-case analysis for every valid input sharding to see what communication happens in the backwards:

* `Shard("batch"), Shard("batch") -> Shard("batch")`: interchanging an input with output still results in a batch pass-through, no comms.
* `Shard("contract"), Shard("contract") -> Partial()`: interchanging an input with output results in `Shard("contract"), Replicate() -> Shard("contract")` (recall that the cotangent type for reduced, is unreduced, aka replicate!), no comms
* `Shard("broadcast"), Replicate() -> Shard("broadcast")`: there are two cases here:
  * `Shard("broadcast"), Replicate() -> Shard("broadcast")` (grad of first input): this is the same as forwards, no comms.
  * `Shard("broadcast"), Shard("broadcast") -> Partial()` (grad of second input): contraction over sharded dimension produces partial, **yes comms**.

So this really is the only situation in einsum that has this problem.  (Pointwise ops that broadcast would also have this problem.)
