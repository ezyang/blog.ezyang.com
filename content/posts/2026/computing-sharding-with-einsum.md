---
title: "Computing sharding with einsum"
date: 2026-01-25T00:05:03-05:00
slug: "computing-sharding-with-einsum"
categories: [PyTorch]
---

Mental arithmetic in grade school (e.g., memorizing your times tables) is typically justified on the grounds that facility in basic calculations makes it easier to focus on higher-level problems that require being able to do these manipulations.  When working on DTensor, I have also found it important to be able to quickly calculate what shardings you get when you do matrix multiplies on sharded tensors.  Without being able to do this quickly and accurately, working through examples becomes a slog.  I've also found that while diagrammatic approaches (e.g., drawing a matrix and slicing it into shards) are intuitive, they are slow and unwieldy to do calculations with.

Recently, I've found that working on sharding with einsum is nice and efficient, and I hope to persuade you to do it this way when you need to reason about sharding!  This post somewhat overlaps with [Sharded Matrices and How to Multiply Them](https://jax-ml.github.io/scaling-book/sharding/), but with some different emphasis and some different notation.

# Einsum primer

[Einstein summation](https://docs.pytorch.org/docs/stable/generated/torch.einsum.html) is a compact way of representing many multi-dimensional linear algebra operations, including matrix multiplies.  It is nice because you don't have to puzzle through the abstruse differences of matrix multiply operations like `@`, `torch.matmul`, `torch.bmm`, `torch.mm`: for any "matrix multiply", as long as you know the input and output shapes of your tensor, you can directly write out an einsum equation.  For example, classic matrix multiply as you see it in math has a signature like `mm(x: f32[A, B], y: f32[B, C]) -> f32[A, C]`.  In einsum notation, you would simply write `torch.einsum("ij,jk->ik", x, y)`: each of the indices lines up exactly with the input sizes.  As another example, in `nn.Linear`, your weight has shape `(in_features, out_features)`.  You don't have to remember how to setup the transposition, just write `torch.einsum("bi,oi->bo", input, weight)`.

A useful piece of terminology that pops up for einsum is a **contraction dimension**.  This is any index that appears in the input tensors but not the output tensors.  The ones that show up in both inputs and outputs are **free dimensions**: if the free dimension is in all inputs it's a **batch** dimension, and if it's missing from some inputs we will **broadcast** those tensors.

# Einsum backwards

Do you always forget how exactly you should transpose your tensors in the backward formula for matrix multiply?  As long as you aren't doing weird things in your einsum (e.g., no repeated indices, every input index is paired with another index), there is a very simple way to compute backwards: keep every input constant except the one you want to compute the gradient for, and swap its index set with the output index set.

For example, linear is `"bi,oi->bo"` for `(input, weight -> output)`.  Then we have:

```
grad_input  = torch.einsum("bo,oi->bi", grad_output, weight)
grad_weight = torch.einsum("bi,bo->oi", input, grad_output)
```

Intuitively, the reason this works is because reverse-mode AD actually just [transposing the linear function](https://arxiv.org/pdf/2204.10923) defined by our einsum, and transposed matrix multiplies can be implemented by just reading off its shapes.

# Einsum sharding

Now that we're thinking in terms of einsum formulas, all we need is the *sharding rule* for einsum.  The sharding rule tells us under what situations we can perform a matrix multiply by simply doing matrix multiplies on the local shards, producing the output matrix under some output placement.

There are not too many rules.  Take a running example `"abi,aoi->abo"`, we can write down these valid placements for a particular mesh dimension (I've replaced numeric dim indices with the einsum character index for readability):

1. If everything is replicated, the output is replicated: `Replicate(), Replicate() -> Replicate()`
2. If a batch dimension is sharded, the output batch dimension is also sharded: `Shard("a"), Shard("a") -> Shard("a")`
3. If a free dimension is sharded, the output free dimension is sharded, but any broadcasted input must be replicated: `Shard("b"), Replicate() -> Shard("b")`
4. If a contraction dimension is sharded, we will have a pending reduction: `Shard("i"), Shard("i") -> Partial()`

You can look at [Computation With Sharded Arrays](https://jax-ml.github.io/scaling-book/sharding/#computation-with-sharded-arrays) for a more detailed explanation for each of these cases.

# Worked example: Tensor parallelism

In 2019, Xiaolin Li [asked this question about CopyToModelParallelRegion in Megatron](https://github.com/NVIDIA/Megatron-LM/issues/19):

> Why the backward function of `_CopyToModelParallelRegion` calls reduce fuction? Can somebody share the mathematical proof?

Let's answer Xiaolin's question.  In Megatron, ColumnParallelLinear is defined as:

```
input: [sequence, batch, in_features]
weight: [in_features, out_features]
output: [sequence, batch, out_features]
```

In einsum notation, this is `torch.einsum("sbi,io->sbo", input, weight)`.

On the TP mesh dimension, we have this sharding:

```
input: Replicate()
weight: Shard("out_features")
output: Shard("out_features")
```

Let us assume that `grad_output: Shard("out_features")`.  Let's compute the placements of `grad_weight` and `grad_input`.  First the derivative formulas:

```
grad_input = torch.einsum("sbo,io->sbi", grad_output, weight)
grad_weight = torch.einsum("sbi,sbo->io", input, grad_output)
```

So we see:

```
grad_input: Partial()  # o is sharded and a contraction dim
grad_weight: Shard("out_features")  # o is sharded and a free dim
```

We see that `grad_input` has a pending reduction, and if downstream backwards is expecting to receive replicated tensors, we must trigger an all-reduce (e.g., in Megatron this all-reduce is manually triggered by `_CopyToModelParallelRegion`; if you use DTensor, it will just propagate the Partial() until a redistribution to Replicate() is required.)

# Worked example: Sequence parallel with a replicated scaling factor

In sequence parallel, we will shard the sequence dimension of an input, but not the weight.  Let's say we have a learnable scaling factor:

```
input: [sequence, batch, hidden]
weight: [hidden]
output: [sequence, batch, hidden]
```

In einsum notation, this is `torch.einsum("sbh,h->sbh", input, weight)`.

On the SP mesh dimension, we have this sharding:

```
input: Shard("sequence")
weight: Replicate()
output: Shard("sequence")
```

Then we have:

```
grad_input = torch.einsum("sbh,h->sbh", grad_output, weight)
grad_weight = torch.einsum("sbh,sbh->h", input, grad_output)
```

So we see:

```
grad_input: Shard("sequence")  # s is sharded and a free dim
grad_weight: Partial()  # s is sharded and a contraction dim
```

Here, we must do an all-reduce over `grad_weight` to get the true replicated gradient.

Notice that this example is very similar to the tensor parallelism one, but the roles of `input` and `weight` have been swapped!
