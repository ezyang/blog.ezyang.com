---
title: "Replicate Forwards, Partial Backwards"
date: 2026-02-03T10:10:25-05:00
slug: "replicate-forwards-partial-backwards"
categories: [JAX]
---

A central thesis of [sharding in types]({{<relref "jax-sharding-type-system.md">}}) is that the backward sharding can be directly computed from the forward sharding.  This is not true for DTensor today, e.g., as seen in [sum to Partial]({{<relref "dtensor-erasure.md#appendix">}}), and it leads to confusion where users cannot easily predict what the sharding of tensors are in their program.  The question now arises: given a forward sharding, what should its backward sharding be?  There are some easy cases to fill in:

* If forwards is sharded, backwards is sharded.
* If forwards is partial, backwards should be replicate (the easiest example to see this is local loss backwards: your local loss is partial over DP, but when you generate a ones tensor as `grad_loss`, this should clearly be a replicated tensor).

But if your forwards is replicate, what should your backwards be?  One answer to this question, [adopted by JAX](https://github.com/jax-ml/jax/pull/29381), is that the backwards should also be replicate.  From a theoretical standpoint, this semantics is easily motivated by the following degenerate example: if my forwards computation is replicated across all my nodes (e.g., everyone is performing the same compute on exactly the same tensor), then the gradient should clearly be replicated across all nodes (and this is the only choice that allows us to avoid communications entirely).  However, this does lead to an irritating problem where if you have a forwards that is replicated, and want your backwards to be partial, you need to introduce an entirely new forwards sharding (JAX calls it "reduced") to indicate this.  JAX chose to preserve replicate-to-replicate for backwards compatibility reasons.

The purpose of this post is to argue that **Replicate Forwards, Partial Backwards** is a better default (concretely, in JAX, swap the default so that all axes are "reduced" by default, and you have to explicitly make some "not" reduced--not to be confused with unreduced!)  To see this, let's look carefully at the sharding of a gated MLP with DP and TP (without the w1-w3 concatenation) in JAX.  Here is the end-to-end example, [done with einsums for ease of differentiation]({{<relref "computing-sharding-with-einsum.md">}}), written *as explicitly* as possible (the reshards can be elided in JAX, but they play an important role for DTensor erasure):

```
import jax
import jax.numpy as jnp
from jax import lax

jax.config.update('jax_num_cpu_devices', 4)
jax.set_mesh(jax.make_mesh((2, 2), ('dp', 'tp')))

def mlp(x, w1, w3, w2):
    print(f"{jax.typeof(x)=}, {jax.typeof(w1)=}, {jax.typeof(w3)=}, {jax.typeof(w2)=}")

    # !!! ATTENTION !!!
    rx = jax.reshard(x, jax.P(None, 'dp', None, reduced={'tp'}))
    rw1 = jax.reshard(w1, jax.P(None, 'tp', reduced={'dp'}))
    rw3 = jax.reshard(w3, jax.P(None, 'tp', reduced={'dp'}))
    rw2 = jax.reshard(w2, jax.P('tp', None, reduced={'dp'}))
    print(f"{jax.typeof(rx)=}, {jax.typeof(rw1)=}, {jax.typeof(rw3)=}, {jax.typeof(rw2)=}")

    h1 = jnp.einsum("sbh,hi->sbi", rx, rw1)
    print(f"{jax.typeof(h1)=}")

    h3 = jnp.einsum("sbh,hi->sbi", rx, rw3)
    print(f"{jax.typeof(h3)=}")

    h = jnp.einsum("sbi,sbi->sbi", jax.nn.silu(h1), h3)
    print(f"{jax.typeof(h)=}")

    out = jnp.einsum("sbi,ih->sbh", h, rw2, out_sharding=jax.P(None, 'dp', None, unreduced={'tp'}))
    print(f"{jax.typeof(out)=}")

    return out

seq = 4
batch = 8
hidden = 16
intermediate = 32

x = jax.device_put(
    jnp.ones((seq, batch, hidden), dtype=jnp.float32),
    jax.P(None, 'dp', None)
)
w1 = jax.device_put(
    jnp.ones((hidden, intermediate), dtype=jnp.float32),
    jax.P(None, 'tp')
)
w3 = jax.device_put(
    jnp.ones((hidden, intermediate), dtype=jnp.float32),
    jax.P(None, 'tp')
)
w2 = jax.device_put(
    jnp.ones((intermediate, hidden), dtype=jnp.float32),
    jax.P('tp', None)
)
mlp(x, w1, w3, w2)
```

If you take the prints and annotated them inline in the program, it looks like this:

```
x:  f32[seq, batch@dp, hidden]
w1: f32[hidden, intermediate@tp]
w3: f32[hidden, intermediate@tp]
w2: f32[intermediate@tp, hidden]

rx: f32[seq, batch@dp, hidden]{R:tp} = jax.reshard(x, jax.P(None, 'dp', None, reduced={'tp'}))
rw1: f32[hidden, intermediate@tp]{R:dp} = jax.reshard(w1, jax.P(None, 'tp', reduced={'dp'}))
rw3: f32[hidden, intermediate@tp]{R:dp} = jax.reshard(w3, jax.P(None, 'tp', reduced={'dp'}))
rw2: f32[intermediate@tp, hidden]{R:dp} = jax.reshard(w2, jax.P('tp', None, reduced={'dp'}))

h1: f32[seq, batch@dp, intermediate@tp] = jnp.einsum("sbh,hi->sbi", x, rw1)
h3: f32[seq, batch@dp, intermediate@tp] = jnp.einsum("sbh,hi->sbi", x, rw3)
h: f32[seq, batch@dp, intermediate@tp] = jnp.einsum("sbi,sbi->sbi", jax.nn.silu(h1), h3)

out: f32[seq, batch@dp, hidden]{U:tp} = jnp.einsum("sbi,ih->sbh", h, rw2, out_sharding=jax.P(None, 'dp', None, unreduced={'tp'}))
```

Each reshard corresponds to a situation where we have a no-op in forwards, but an all-reduce in backwards.  Why does JAX's primal-cotangent rule for sharding in types imply this?  I have three arguments.

**The intuitive argument.** In a DP+TP gated MLP, you expect to need to do a TP all-reduce on `grad_input` (because as you leave the sharded TP region you need to aggregate gradients from the TP shards), and you need to do the traditional DP all-reduces on all the parameters.  In PyTorch, the DP all-reduces are typically handled by the DDP/FSDP wrapper outside of this code, but when we accept JAX semantics, `grad_w1: f32[hidden, intermediate@tp]` (it's replicated!) so we are obligated to ensure the all-reduce occurs before we exit this region.

**The peephole argument.** Let's just look at one specific backwards and work it out by hand.

```
# Recall:
rw1: f32[hidden, intermediate@tp]{R:dp}
h1: f32[seq, batch@dp, intermediate@tp]
# Therefore: (reduced->unreduced)
grad_rw1: f32[hidden, intermediate@tp]{U:dp}
grad_h1: f32[seq, batch@dp, intermediate@tp]

# Recall:
h1: f32[seq, batch@dp, intermediate@tp] = jnp.einsum("sbh,hi->sbi", x, rw1)
# Einsum backwards says:
grad_rw1: f32[hidden, intermediate@tp]{U:dp} = jnp.einsum("sbh,sbi->hi", x, grad_h1)
# Contraction is on replicated 's' and sharded 'b', so the result is unreduced on dp axis
```

The conversion to 'reduced' in forwards turns into an all-reduce to compute `grad_w1: f32[hidden, intermediate@tp]`.  If we want to be extremely explicit about our code, we are obligated to convert `w1` to "reduced", so that its backward is "unreduced" as is implied by einsum backwards.  By the way, inside of a `shard_map` region, a very similar thing occurs; as `w1` is invariant in DP, but `x` is varying in DP, we must pcast `w1` from invariant to varying to get correct gradients.

**The exhaustive argument.** We can write out the full backwards (for brevity, I use `g_` instead of `grad_` for the gradients):

```
g_out: f32[seq, batch@dp, hidden]{R:tp}

# out: f32[seq, batch@dp, hidden]{U:tp} = jnp.einsum("sbi,ih->sbh", h, rw2, out_sharding=jax.P(None, 'dp', None, unreduced={'tp'}))
g_h: f32[seq, batch@dp, intermediate@tp] = einsum("sbh,ih->sbi", g_out, rw2)
g_rw2: f32[intermediate@tp, hidden]{U:dp} = einsum("sbi,sbh->ih", h, g_out, out_sharding=jax.P('tp', None, unreduced={'dp'}))

# h: f32[seq, batch@dp, intermediate@tp] = jnp.einsum("sbi,sbi->sbi", jax.nn.silu(h1), h3)
g_silu_h1: f32[seq, batch@dp, intermediate@tp] = einsum("sbi,sbi->sbi", g_h, h3)
g_h3: f32[seq, batch@dp, intermediate@tp] = einsum("sbi,sbi->sbi", g_h, silu(h1))
g_h1: f32[seq, batch@dp, intermediate@tp] = silu_backward(g_silu_h1, h1)

# h3: f32[seq, batch@dp, intermediate@tp] = jnp.einsum("sbh,hi->sbi", x, rw3)
g_rx_from_h3: f32[seq, batch@dp, hidden]{U:tp} = einsum("sbi,hi->sbh", g_h3, rw3, out_sharding=jax.P(None, 'dp', None, unreduced={'tp'}))
g_rw3: f32[hidden, intermediate@tp]{U:dp} = einsum("sbh,sbi->hi", rx, g_h3, out_sharding=jax.P(None, 'tp', unreduced={'dp'}))

# h1: f32[seq, batch@dp, intermediate@tp] = jnp.einsum("sbh,hi->sbi", x, rw1)
g_rx_from_h1: f32[seq, batch@dp, hidden]{U:tp} = einsum("sbi,hi->sbh", g_h1, rw1, out_sharding=jax.P(None, 'dp', None, unreduced={'tp'}))
g_rw1: f32[hidden, intermediate@tp]{U:dp} = einsum("sbh,sbi->hi", rx, g_h1, out_sharding=jax.P(None, 'tp', unreduced={'dp'}))

g_rx: f32[seq, batch@dp, hidden]{U:tp} = g_rx_from_h1 + g_rx_from_h3

g_w2: f32[intermediate@tp, hidden] = reshard(g_rw2, P('tp', None))
g_w1: f32[hidden, intermediate@tp] = reshard(g_rw1, P(None, 'tp'))
g_w3: f32[hidden, intermediate@tp] = reshard(g_rw3, P(None, 'tp'))
g_x: f32[seq, batch@dp, hidden] = reshard(g_rx, P(None, 'dp', None))
```

You can individually verify that each unreduced gradient is implied by the einsum in question.

**The upshot.** The real point of this example is to see that the "intuitive" sharding type for the arguments on `mlp`, actually forces a lot of communications in backwards, because we must make the gradients replicated, and that implies all-reduces.  This can actually result in a suboptimal communication pattern: when both TP and SP are being used, the unreduced `grad_input` can be delayed all the way to the forwards all-gather between the SP and TP region.  In backwards, we can [directly do a reduce-scatter](https://huggingface.co/spaces/weege007/ultrascale-playbook?section=sequence_parallelism) than doing an all-reduce and then later throwing out most of the result in the all-gather backwards.  (Arguably, this isn't a huge deal if you have a compiler like XLA, since you would expect it to know how to optimize the comms here, but the whole point of sharding-in-types is to give more control over when comms occur.)

A better type signature for this function is `mlp(rx, rw1, rw3, rw2)`, where all of these arguments are reduced (`rx` on tp, and `rw{1,3,2}` on dp).  Now the reshards can be controlled by the user; you can do exactly the same communication pattern as our original implementation, or you can delay them until later.  And the best way to encourage people to write their code this way is to have replicate forwards imply partial backwards.  (P.S. It is still useful to have another variant of replicate which really does have a replicate backwards.  I don't have a good name for it, but it could occasionally be used to do an all-reduce early before fan-out would imply you have to do multiple all-reduces.)

# Appendix

Patrick Toulme requested the HLO and Shardy MLIR for the JAX program.  Here they are, generated from [this script](https://gist.github.com/ezyang/f0e541a7f426f0ab3b7399bec7a041e2).  The [raw output is here](https://gist.github.com/ezyang/53a2151fce613cf4cd6b04581146aebe).  Below, I have posted annotated versions, courtesy of Claude.

## Pre-partition HLO

[Full pre-partition HLO](https://gist.github.com/ezyang/bf6e13ad90636566d0c338956c446335). The most interesting thing is you can see the sharding constraints added which trigger reductions:

```
    %16 = sdy.sharding_constraint %15 <@mesh, [{"tp"}, {}], unreduced={"dp"}> : tensor<32x16xf32>
    %27 = sdy.sharding_constraint %26 <@mesh, [{}, {"tp"}], unreduced={"dp"}> : tensor<16x32xf32>
    %29 = sdy.sharding_constraint %28 <@mesh, [{}, {"dp"}, {}], unreduced={"tp"}> : tensor<4x8x16xf32>
    %33 = sdy.sharding_constraint %32 <@mesh, [{}, {"tp"}], unreduced={"dp"}> : tensor<16x32xf32>
    %35 = sdy.sharding_constraint %34 <@mesh, [{}, {"dp"}, {}], unreduced={"tp"}> : tensor<4x8x16xf32>
    %36 = stablehlo.add %29, %35 : tensor<4x8x16xf32>

    ...

    # ========== REDUCE WEIGHT GRADIENTS (dp reduction) ==========
    # Reduce g_w2 across dp dimension
    %37 = sdy.sharding_constraint %16 <@mesh, [{"tp"}, {}]> : tensor<32x16xf32>
    # Reduce g_w3 across dp dimension
    %38 = sdy.sharding_constraint %27 <@mesh, [{}, {"tp"}]> : tensor<16x32xf32>
    # Reduce g_w1 across dp dimension
    %39 = sdy.sharding_constraint %33 <@mesh, [{}, {"tp"}]> : tensor<16x32xf32>

    # ========== REDUCE g_x (tp reduction) ==========
    # Reduce g_x across tp dimension
    %40 = sdy.sharding_constraint %36 <@mesh, [{}, {"dp"}, {}]> : tensor<4x8x16xf32>

    # ========== RETURN ==========
    # Returns: (out, grad_x, grad_w1, grad_w3, grad_w2)
    return %12, %40, %39, %38, %37 : tensor<4x8x16xf32>, tensor<4x8x16xf32>, tensor<16x32xf32>, tensor<16x32xf32>, tensor<32x16xf32>
```

## Post-partition HLO

[Full post-partition HLO](https://gist.github.com/ezyang/20715de362d7851598018500b23d1ed0). The most notable thing to observe here is that the DP all-reduces have been bucketed together into a single all-reduce, which is what you'd expect any self-respecting DP implementation to do.  Also interestingly, the TP reduction is actually done first before summing the gradients together on both paths; you could probably also sum the gradients together first before all-reducing.

```
# ============================================================================
# MAIN ENTRY POINT
# Returns: (out, grad_x, grad_w1, grad_w3, grad_w2)
# ============================================================================
ENTRY %main.11_spmd (param.5: f32[4,4,16], param.6: f32[16,16], param.7: f32[16,16], param.8: f32[16,16], param.9: f32[4,4,16]) -> (f32[4,4,16], f32[4,4,16], f32[16,16], f32[16,16], f32[16,16]) {

  # ========== BACKWARD: All-reduce weight gradients across TP ==========
  # Sum weight gradients across tensor parallel devices (replica_groups={{0,2},{1,3}})
  # Returns: (g_w1, g_w3, g_w2)
  %all-reduce.1 = (f32[16,16]{1,0}, f32[16,16]{1,0}, f32[16,16]{1,0}) all-reduce(%dot.27, %dot.28, %dot.29), channel_id=3, replica_groups={{0,2},{1,3}}, use_global_device_ids=true, to_apply=%region_2.5, metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/transpose/jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/transpose" stack_frame_id=5}

  # line 16 backward: reshape g_x_h3
  %bitcast.8 = f32[4,4,16]{2,1,0} bitcast(%dot.18), metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/dot_general" stack_frame_id=8}

  # Extract weight gradients from all-reduce tuple
  # line 15 backward: g_w1 (reduced)
  %get-tuple-element.4 = f32[16,16]{1,0} get-tuple-element(%all-reduce.1), index=0, metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/transpose/jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/transpose" stack_frame_id=5}
  # line 16 backward: g_w3 (reduced)
  %get-tuple-element.6 = f32[16,16]{1,0} get-tuple-element(%all-reduce.1), index=1, metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/transpose/jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/transpose" stack_frame_id=8}
  # line 18 backward: g_w2 (reduced)
  %get-tuple-element.8 = f32[16,16]{1,0} get-tuple-element(%all-reduce.1), index=2, metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbi,ih->sbh))/transpose/jit(forward_and_backward)/transpose(jvp(sbi,ih->sbh))/transpose" stack_frame_id=11}

  # ========== BACKWARD: All-reduce input gradients across TP ==========
  # Sum input gradients across tensor parallel devices (replica_groups={{0,1},{2,3}})
  # Returns: (g_x_h3, g_x_h1)
  %all-reduce = (f32[4,4,16]{2,1,0}, f32[4,4,16]{2,1,0}) all-reduce(%bitcast.8, %bitcast.11), channel_id=1, replica_groups={{0,1},{2,3}}, use_global_device_ids=true, to_apply=%region_0.1, metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/dot_general/jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/dot_general" stack_frame_id=8}

  # Extract input gradients from all-reduce tuple
  # line 16 backward: g_x_h3 (reduced across TP)
  %get-tuple-element = f32[4,4,16]{2,1,0} get-tuple-element(%all-reduce), index=0, metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/dot_general/jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/dot_general" stack_frame_id=8}
  # line 15 backward: g_x_h1 (reduced across TP)
  %get-tuple-element.2 = f32[4,4,16]{2,1,0} get-tuple-element(%all-reduce), index=1, metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/dot_general/jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/dot_general" stack_frame_id=5}

  # ========== BACKWARD: Sum input gradients from both paths ==========
  # line 15+16 backward: g_x = g_x_h1 + g_x_h3
  %wrapped_add = f32[4,4,16]{2,1,0} fusion(%get-tuple-element, %get-tuple-element.2), kind=kLoop, calls=%wrapped_add_computation, metadata={op_name="jit(forward_and_backward)/transpose(jvp(sbh,hi->sbi))/add_any" stack_frame_id=5}

  # ========== FINAL OUTPUT ==========
  # Returns: (out, grad_x, grad_w1, grad_w3, grad_w2)
  ROOT %tuple.6 = (f32[4,4,16]{2,1,0}, f32[4,4,16]{2,1,0}, f32[16,16]{1,0}, f32[16,16]{1,0}, f32[16,16]{1,0}) tuple(%bitcast.4, %wrapped_add, %get-tuple-element.4, %get-tuple-element.6, %get-tuple-element.8)
}
```
