---
title: "Megatron via shard_map"
date: 2026-01-26T08:34:34-05:00
slug: "megatron-via-shard-map"
categories: [PyTorch, JAX]
---

In [Computing sharding with einsum]({{<relref "computing-sharding-with-einsum.md">}}), we worked an example of Megatron style tensor parallelism where we discover that the ordinary backwards formula for linear results in a pending reduction on `grad_input`, even though the `input` was replicated and no communications happened in forwards.  In Megatron, which is implemented with plain Tensors and manual collectives, you just have to *know* that this reduction is necessary and manually insert it with a custom autograd function.

If we wanted to write a similar explicit-collective-style Megatron implementation in JAX, we would use [shard_map](https://docs.jax.dev/en/latest/notebooks/shard_map.html).  Like in Megatron, you have to call a function which is a no-op in forwards and an all-reduce in backwards.  However, JAX has built this function into its core library, and has an interesting name for it: `jax.lax.pcast(..., to='varying')` (previously known as `pvary`, although apparently this is deprecated now.)  Why is this a "cast"?  The answer is that JAX's `shard_map` actually comes with an optional type system (enabled with `check_vma=True`) which reject your program if you forget to insert an all-reduce in your backwards!

Let's see how this works.  As a reminder, our shapes are:

```
input: [sequence, batch, in_features]
weight: [in_features, out_features]
output: [sequence, batch, out_features]
```

We can describe the input and output sharding of these in JAX style (tensor-dim oriented), which is what we would feed into the `in_specs` and `out_specs` of `shard_map`:

```
input: P(None, None, None)
weight: P(None, "tp")
output: P(None, None, "tp")
```

Although on the boundaries of `shard_map` we can say exactly what the sharding of the input/output tensors are (e.g., how to reassemble them back into full tensors), on the *inside* of a `shard_map` this is not a well-defined question: you only have the local tensors and can do whatever you want with them before you reassemble them back into global tensors with shardings.

However, when `check_vma=True`, JAX will keep still track of something weaker than sharding: whether or not the tensors are varying (i.e., different) across a mesh dimension.  This is conventionally notated as `dtype[local_shape]@{varying axes}`, e.g., `f32[3]{i}` means that this tensor varies across the mesh axis `i` (the braces are omitted when nothing varies).  Let's write down the local shapes of our input/output tensors with variance information (note that the sharded tensor dimensions have shapes that are divided by the mesh they are sharded over):

```
input: f32[sequence, batch, in_features]  # nothing varying
weight: f32[in_features, out_features/tp]@{tp}  # varies over tp dim
output: f32[sequence, batch, out_features/tp]@{tp}  # varies over tp dim
```

The point of a type system is that you have typing rules that say whether or not an operation is legal between two types, rejecting programs that are ill-typed.  In particular, in jaxpr it's illegal to have an operation like matrix multiply between two tensors with differing VMA: you have to insert a cast to make the VMAs match before you can do the operation.  Actually, JAX will typically insert these casts implicitly for you, but for clarity we're going to insert the cast explicitly here:

```
import jax
import jax.numpy as jnp

jax.config.update('jax_num_cpu_devices', 2)

jax.set_mesh(jax.make_mesh((2,), ('tp',)))

sequence, batch, in_features, out_features = 4, 2, 8, 16

input = jnp.ones((sequence, batch, in_features))
# NB: the full weight, shard_map will automatically shard weight for us given the
# input spec
weight = jnp.ones((in_features, out_features))

@jax.shard_map(in_specs=(jax.P(None, None, None), jax.P(None, "tp")), out_specs=jax.P(None, None, "tp"))
def colwise_linear(input, weight):
    print('input', jax.typeof(input))
    print('weight', jax.typeof(weight))
    input = jax.lax.pcast(input, "tp", to="varying")
    print('pcast_input', jax.typeof(input))
    output = jnp.einsum("sbi,io->sbo", input, weight)
    print('output', jax.typeof(output))
    return output

output = colwise_linear(input, weight)
```

This prints:

```
input float32[4,2,8]
weight float32[8,8]{V:tp}
pcast_input float32[4,2,8]{V:tp}
output float32[4,2,8]{V:tp}
```

It's a little difficult to see why the program "doesn't typecheck" when you omit the pcast, because even if you don't pcast JAX will implicitly insert it for you, but you can verify that the cast happens anyway by inspecting the HLO with and without the explicit pcast (this is left as an exercise to the reader to verify; an LLM can one-shot this program transformation).  The type system here is also not that invasive: local operations simply propagate variance (varying to varying, invariant to invariant), and you only need a small menagerie of collective operations to help you manage collectives which can take you between varying and invariant.

Although JAX's type system here is a bit difficult to explain from first principles, it seems quite beneficial as it makes it impossible to forget backwards reductions that are required when you do operations between sharded and replicated tensors.  We're hoping to bring a similar capability to PyTorch DTensor in the near future, introducing a new "LTensor" subclass that is able to track metadata along these lines.
