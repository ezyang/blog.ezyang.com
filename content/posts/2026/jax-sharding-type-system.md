---
title: "The JAX sharding type system"
date: 2026-01-28T09:00:50-05:00
slug: "jax-sharding-type-system"
categories: [JAX]
---

Conventionally, a type system is something that classifies values into data types like `float32` or `int64`.  However, fancy type systems go beyond data types, allowing us to talk about potentially arbitrary invariants on data; for example, if we were to talk about the "type" of a array, it would cover not only its data type, but also its shape, e.g., `f32[40, 20]`. JAX's type system of abstract values (avals) goes further than just data types and shapes and is equipped to reason about sharding related invariants.  However, this type system is poorly documented, especially recent additions like reduced/unreduced axes (circa June 2025).  In this blog post, I want to give a consolidated description of the sharding related aspects of JAX's typing in explicit sharding mode, as of 2026. Disclaimer: I am not a JAX developer, and there may potentially be mistakes in this presentation; please let me know about errors in Twitter.  I will assume that you have some knowledge about how to work with JAX sharding in the frontend; please refer to [Distributed arrays and automatic parallelization](https://docs.jax.dev/en/latest/notebooks/Distributed_arrays_and_automatic_parallelization.html), [Explicit sharding](https://docs.jax.dev/en/latest/notebooks/explicit-sharding.html) and [Manual parallelism with shard_map](https://docs.jax.dev/en/latest/notebooks/shard_map.html) for a refresher on these topics.

**Warning:**  All of this discussion is about [Explicit sharding](https://docs.jax.dev/en/latest/notebooks/explicit-sharding.html).  The avals do not store accurate sharding information in the (default) auto mode.  For example:

```
import jax
import jax.numpy as jnp
from jax.sharding import AxisType

jax.config.update('jax_num_cpu_devices', 2)

mesh1 = jax.make_mesh((2,), ('i',), axis_types=(AxisType.Auto,))
jax.set_mesh(mesh1)
array1 = jax.device_put(jnp.ones((8,)), jax.P("i"))
print(array1.aval.sharding)

mesh2 = jax.make_mesh((2,), ('i',), axis_types=(AxisType.Explicit,))
jax.set_mesh(mesh2)
array2 = jax.device_put(jnp.ones((8,)), jax.P("i"))
print(array2.aval.sharding)
```

will print:

```
NamedSharding(mesh=AbstractMesh('i': 2, axis_types=(Auto,), device_kind=cpu, num_cores=None), spec=PartitionSpec(None,))
NamedSharding(mesh=AbstractMesh('i': 2, axis_types=(Explicit,), device_kind=cpu, num_cores=None), spec=PartitionSpec('i',))
```

The new `jax.make_mesh` API defaults to Explicit, but the direct `jax.sharding.Mesh` constructor defaults to Auto.  Beware!

# Definition of a type

The name of a type in JAX is `AbstractValue`.  The important concrete subclass of this type is [ShapedArray](https://github.com/jax-ml/jax/blob/b12727238016decfbf83de60fe6c1fd01226cca3/jax/_src/core.py#L2203), whose type I have simplified below:

```
class ShapedArray(AbstractValue):
  shape: tuple[int, ...]
  dtype: dtype
  weak_type: bool  # when True, don't force type promotion
  sharding: NamedSharding
  vma: frozenset[MeshAxisName]
  memory_space: MemorySpace  # see https://github.com/jax-ml/jax/pull/30556
```

You can see ordinary things like `shape` and `dtype`, as well as some weirder things.  In this post, we are going to focus on `sharding` and `vma`.  Here are simplified definitions for [NamedSharding](https://github.com/jax-ml/jax/blob/b12727238016decfbf83de60fe6c1fd01226cca3/jax/_src/named_sharding.py#L87) and [PartitionSpec](https://github.com/jax-ml/jax/blob/b12727238016decfbf83de60fe6c1fd01226cca3/jax/_src/partition_spec.py#L40):

```
MeshAxisName = str

class NamedSharding:
  mesh: Mesh
  spec: PartitionSpec

class PartitionSpec(tuple[None | MeshAxisName | tuple[MeshAxisName, ...], ...]):
  unreduced: frozenset[MeshAxisName]
  reduced: frozenset[MeshAxisName]
```

Let us now describe these types in more detail.  For each, we will ask:

* What does it mean?
* Is the type applicable for global SPMD or local SPMD (or both?)
* How does the type propagate across operations?
* How is the type transformed by autograd (what is the mapping from primal to cotangent type?)

# PartitionSpec

PartitionSpec is the most user visible concept.  In this section, we'll ignore unreduced/reduced for now. Without those fields, it is simply a tuple with one entry per dimension of the array (in PyTorch I often refer to this as tensor-oriented sharding, as opposed to mesh-oriented sharding).  For each array dimension, you specify which mesh axes shard it.  There can be zero (`None`), one (`"i"`) or many (`("i", "j")`) mesh axes sharding a dimension; sharding is applied from left to right.  You recover the global view (i.e., the one whose shape is described in ShapedArray) by stacking the arrays distributed across those mesh axes.  PartitionSpec is commonly abbreviated as just P in JAX code.  When you print the type of an array in explicit mode, JAX will inline the partition spec into the shape: e.g., `float32[8,16@tp]` implies a PartitionSpec of `P(None, "tp")`.  There's a much longer description with pretty pictures at [Distributed arrays and automatic parallelization](https://docs.jax.dev/en/latest/notebooks/Distributed_arrays_and_automatic_parallelization.html).

PartitionSpec propagate according to shard propagation rules, which must be defined on a per-operator basis.  If you want to perform an operation without performing communication, it must be the case that running that operation locally on the sharded tensors and then stacking it (per the output sharding), would be the same as stacking the arrays first (per the input sharding) and then running the operation globally.  The output sharding is not always the same as the input sharding, and the shard propagation rule is also responsible for computing this output sharding.

How does PartitionSpec interact with autograd?  The PartitionSpec of primals and cotangents matches: a value that is replicated in forwards, will also be replicated in backwards; similarly, if it is sharded in forwards, it will be sharded in backwards.  (Reduced/unreduced will be an exception to this rule, discussed below.)

I want to take a moment to discuss a subtlety of PartitionSpec with respect to `shard_map`.  As I discussed in [Global vs Local SPMD]({{<relref "global-vs-local-spmd.md">}}), inside of a `shard_map` region, it's not really well defined what the global shape of a array is: you only have to specify a PartitionSpec on the inputs and outputs.  By default, inside of a `shard_map`, the `shape` is the local shape of a array, the mesh says your axis is `Manual`, and the PartitionSpec says there is no sharding on the array anymore (perhaps confusingly, since None here in the global SPMD view would imply it's replicated, but that's not at all the meaning here).

As a small example:

```
import jax
import jax.numpy as jnp

jax.config.update('jax_num_cpu_devices', 2)
jax.set_mesh(jax.make_mesh((2,), ('i',)))

x = jax.device_put(jnp.ones((8,)), jax.P("i"))

@jax.shard_map(out_specs=jax.P("i"))
def f(x_local):
    print(jax.typeof(x_local))
    print(jax.typeof(x_local).sharding)
    return x_local

f(x)
```

prints:

```
float32[4]{V:i}
NamedSharding(mesh=AbstractMesh('i': 2, axis_types=(Manual,), device_kind=cpu, num_cores=None), spec=PartitionSpec(None,))
```

The PartitionSpec is along for the ride, but it no longer contains sharding information for the manual axes (as you would expect.)  (Wondering about the `{V:i}`?  See the next section.)  Another subtlety is that JAX allows for mixing Manual mode with not-Manual mode, via `axis_names`.  So you can actually potentially see nontrivial PartitionSpec inside of a `shard_map` region!  The footnote has a worked example.[^1]

# VMA

VMA is short for "varying manual axes".  The motivation for this feature is described in [Efficient transposition of replication-inducing collectives](https://docs.jax.dev/en/latest/jep/17111-shmap-transpose.html) and the feature has been around long enough that the `shard_map` docs have [a section about it](https://docs.jax.dev/en/latest/notebooks/shard_map.html#tracking-how-values-vary-over-manual-mesh-axes-and-check-vma-true).

Unlike PartitionSpec, VMA is a `shard_map` only concept; as its name suggests, it only applies to Manual axes.  VMA tracks whether or not values are varying or invariant across a given mesh dimension.  Actually, we can think of VMA as an approximation of PartitionSpec.  PartitionSpec tells us exactly how to stack the local arrays to form a global array--they vary across that mesh dimension: if PartitionSpec for a particular array dim is None, it instead claims says this array is invariant across this mesh dimension (no stacking needed, everything is the same!)  With VMA, we don't know how to stack the arrays (because the whole point of local SPMD is that the global view is undefined), but we do know if they are the same or different across a mesh dimension.  You don't need to track VMA for non-Manual axes, because PartitionSpec subsumes it.  In the print of `jax.typeof`, the `{V:i}` indicates all of the varying mesh axes (V is for varying).  When all axes are invariant, we just elide this from the print.

Because VMA is an approximation of PartitionSpec, its propagation rules are simpler as well.  For non-communication ops, it is simply required that the VMA of all inputs match exactly.  If one input is invariant while another is varying, JAX will insert an implicit conversion from invariant to varying to make the VMA match. In [Megatron via shard_map]({{<relref "megatron-via-shard-map.md">}}), I have a worked example of this, where VMA is how JAX triggers an all-reduce on gradients from the TP region into a replicated `grad_input`.

How does VMA interact with autograd?  The VMA of primals and cotangents matches:  a value that is varying in forwards, will also be varying in backwards; similarly, if it is invariant in forwards, it will be invariant in backwards.  If you are skeptical that forcing this constraint is an efficient thing to do, you would be right! (See reduced/unreduced below.)

Collective operations are more complex regarding VMA, because we must reason about what happens to the variance of the local tensors before and after the collective.  In [the bottom of this section](https://docs.jax.dev/en/latest/notebooks/shard_map.html#tracking-how-values-vary-over-manual-mesh-axes-and-check-vma-true) there is a table of how all the collectives affect variance.  Beyond the standard collectives, VMA also introduces the necessity to represent a conversion from invariant to varying (which is a no-op in forwards and an all-reduce in backwards.)  As a type system nerd, I think this is a very cool use of type systems to rule out illegal programs (that forget to do required all-reduces in backwards!)

One side note: VMA is not actually required for correctness, and you can actually disable this type system with `check_vma=False`.  It was actually introduced as a way to make it possible to write more efficient programs that were impossible to write without it.  Without VMA, JAX can conservatively assumes that all axes are potentially varying, and it will insert extra collectives to ensure you get the correct result.  By actually modeling VMA, we can tell when something is invariant and potentially skip this collective.

# Unreduced

The unreduced and reduced fields in PartitionSpec are quite new and are not documented in the public JAX documentation.  However, we think of them as quite important when doing work with explicit sharding (for example, Wanchao, one of the original authors of DTensor, has told me the need to represent Partial placements is one of the big reasons why DTensor has mesh-oriented placements rather than tensor-oriented placements.)  Unlike PartitionSpec/VMA, these fields apply for *both* Explicit and Manual axes.

It is easiest to first describe what unreduced means.  Unreduced means there is a pending reduction (summation) on a device mesh axis that is necessary to get the global view of the array.  Outside of `shard_map`, the most common way to generate an unreduced array in JAX is to use `jnp.einsum` with `out_sharding` that specifies unreduced.  For example:

```
import jax
import jax.numpy as jnp

jax.config.update('jax_num_cpu_devices', 2)

jax.set_mesh(jax.make_mesh((2,), ('i',)))

x = jax.device_put(jnp.ones((4, 8,)), jax.P(None, "i"))
y = jax.device_put(jnp.ones((4, 8,)), jax.P(None, "i"))
u = jnp.einsum('bx,bx->b', x, y, out_sharding=jax.P(unreduced={'i'}))
print("u", jax.typeof(u))
```

prints:

```
float32[4]{U:i}
```

As I described in [Computing sharding with einsum]({{<relref "computing-sharding-with-einsum.md">}}), when you do a contraction on two dimensions that are sharded on the same mesh axis, this can be done locally as long as you remember that there is a pending reduction (aka, partial/unreduced) that you need to do across that mesh axis to get the final result.  Prior to the existence of `unreduced` in JAX, it wasn't possible to express that you wanted no communication: the output sharding could only express replicated/sharded states, so you were going to end up doing an all-reduce or reduce-scatter.

You can also have unreduced axes in `shard_map`.  For example, you can cast a varying array into an unreduced array, and then trigger the reduction. (Warning: unreduced doesn't work with `shard_map` without `jax.jit`, see [issue #34684](https://github.com/jax-ml/jax/issues/34684))

```
import jax
import jax.numpy as jnp
from jax import lax

jax.config.update('jax_num_cpu_devices', 2)
jax.set_mesh(jax.make_mesh((2,), ('i',)))

x = jax.device_put(jnp.ones((8,)), jax.P("i"))

@jax.jit
@jax.shard_map(out_specs=jax.P(None))
def f(x_local):
    u = lax.pcast(x_local, to='unreduced', axis_name='i')
    print(jax.typeof(u))
    return lax.psum(u, axis_name='i')

f(x)
```

prints:

```
float32[4]{U:i}
```

How does unreduced propagate?  Alas, this once again is something you have to define on a per operator basis, like in sharding propagation rules.  One rule of thumb is that *linear* functions can always propagate unreduced, since linearity means `f(x + y) == f(x) + f(y)`, but at time of writing JAX just [writes all the unreduced rules out by hand](https://github.com/jax-ml/jax/blob/3ebe54ed98c8ecbea1b5bdfa6ae535c3cc1f4afb/jax/_src/lax/lax.py#L4657-L4677).

How does unreduced interact with VMA and PartitionSpec?  It is mutually exclusive from varying/sharded.  If an array is unreduced on a mesh axis, it cannot also be [varying](https://github.com/jax-ml/jax/blob/3ebe54ed98c8ecbea1b5bdfa6ae535c3cc1f4afb/jax/_src/core.py#L2186-L2193) or [sharded](https://github.com/jax-ml/jax/blob/3ebe54ed98c8ecbea1b5bdfa6ae535c3cc1f4afb/jaxlib/partition_spec.cc#L88-L96) on that mesh axis.  (Technically, one might argue that if something is unreduced on a mesh axis, it is obviously varying on that axis, but varying and unreduced need to be treated differently for AD purposes, so it's best to keep these distinct.)

Inside of `shard_map`, there are a few functions for working with unreduced:

* `lax.pcast` will let you directly convert a varying axis into an unreduced axis, declaring your intent to do a reduction later.  However, you can't do the "no-op" pcast from unreduced to varying, because this is not well-defined from a global semantics perspective: it's in general not defined to define a function as "take its input, decompose it into x + y, and then run an arbitrary function on x and y individually.
* The existing reduction collectives like [lax.psum](https://docs.jax.dev/en/latest/_autosummary/jax.lax.psum.html#jax.lax.psum) and [lax.psum_scatter](https://docs.jax.dev/en/latest/_autosummary/jax.lax.psum_scatter.html#jax.lax.psum_scatter) will accept both axes that are varying as well as unreduced, and do the obvious thing.

# Reduced

So what is reduced?  As described in the original PR to [make unreduced + AD work](https://github.com/jax-ml/jax/pull/29381), reduced is like replicate, but it causes the cotangent (gradient) to be unreduced (and vice versa).  Remember that in sharding with types, the cotangent sharding is always a function of the primal sharding.  When you have a replicated primal, it is ambiguous whether or not you want the cotangent to be replicated or unreduced, so JAX introduces a new type (reduced) to let you distinguish them solely even if you are only looking at the primal type.  The rule in JAX is replicate goes to replicate, and reduced goes to unreduced (and vice versa).  Like unreduced, reduced is tracked both in Explicit and Manual mode.

How can you interact with reduced shardings?  Unlike unreduced, you can directly `device_put` some data as reduced (since reduced is the same as replicated), or `jax.reshard` a tensor to a reduced placement.  A transition from invariant/replicated to reduced is a no-op in forwards but triggers an all-reduce in backwards; similarly, you can pcast reduced to varying, which is a no-op for both forwards and backwards (invariant to varying forces an all-reduce immediately, since invariant's cotangent is invariant, but reduced can delay the all-reduce since its cotangent type is unreduced).  Separately, inside of a `shard_map`, [lax.all_gather](https://docs.jax.dev/en/latest/_autosummary/jax.lax.all_gather.html) can be instructed to directly go to reduced (which will result in a reduce-scatter in backwards.)

How does reduced propagate?  The propagation rule is simple: reduced is a statement about a set of mesh axes (not array dims), so we simply [pass through the set of reduced axes whenever we do an operation](http://github.com/jax-ml/jax/blob/3ebe54ed98c8ecbea1b5bdfa6ae535c3cc1f4afb/jax/_src/lax/lax.py#L3936-L3937).  If an operation is N-ary, it's [required that all inputs are reduced on the same mesh axes](https://github.com/jax-ml/jax/blob/3ebe54ed98c8ecbea1b5bdfa6ae535c3cc1f4afb/jax/_src/lax/lax.py#L4036-L4040).  Unlike replicate, inputs cannot be replicated or sharded on reduced mesh axis, and JAX will force you to add conversions to make it typecheck.  (It's actually not clear to me that there isn't a good default choice for implicit conversions here, but it's certainly a lot safer to not allow it to start!)

[^1]: Here is an example of mixed Manual and Explicit mode:

    ```
    import jax
    import jax.numpy as jnp

    jax.config.update('jax_num_cpu_devices', 4)

    jax.set_mesh(jax.make_mesh((2, 2), ('i', 'j')))

    x = jax.device_put(jnp.ones((4, 4)), jax.P("i", "j"))

    @jax.shard_map(out_specs=jax.P("i", None), axis_names={"i"})
    def f(x_local):
        print(jax.typeof(x_local))
        print(jax.typeof(x_local).sharding)
        return x_local

    out = f(x)
    print(jax.typeof(out))
    ```

    prints:

    ```
    float32[2,4@j]{V:i}
    NamedSharding(mesh=AbstractMesh('i': 2, 'j': 2, axis_types=(Manual, Explicit), device_kind=cpu, num_cores=None), spec=PartitionSpec(None, 'j'))
    float32[4@i,4@j]
    ```
