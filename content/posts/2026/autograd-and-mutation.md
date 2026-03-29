---
title: "Autograd and Mutation"
date: 2026-03-28T23:23:32-04:00
slug: "autograd-and-mutation"
categories: ["PyTorch"]
---

How does PyTorch autograd deal with mutation?  In particular, what happens when a mutation occurs on a view, which aliases with some other tensor?  In 2017, Sam Gross implemented support for [in-place operations on views](https://github.com/pytorch/pytorch/pull/3384), but the details of which have never been described in plain English... until now.

# Autograd, the simple case

We all know that autograd operates by constructing a backwards graph as you execute the forwards of your model.  Every time you execute an operation `Op`, we generate an autograd node `OpBackward` and record it on the `grad_fn` of the resulting tensor.  A diagram here is helpful.  Consider this program:

```
x = torch.randn(8, requires_grad=True)
y = x ** 2
z = y * 2
```

Then we expect to have constructed this autograd graph:

```
x -- grad_fn --> AccumulateGrad
                      ^
                      |
y -- grad_fn --> PowBackward0
                      ^
                      |
z -- grad_fn --> MulBackward0
```

PyTorch lets you walk over the autograd graph from Python, and you can use a package like [torchviz](https://github.com/szagoruyko/pytorchviz) to programatically generate diagrams like this.  However, I will be doing these diagrams by hand because there are some important technical details that I would like to convey accurately.

A details about the graph above:

* If you actually try to print `x.grad_fn`, it prints `None`.  This is because PyTorch hides the internal implementation detail that there is an `AccumulateGrad` node associated with every leaf variable (recall, a leaf variable is a variable that `requires_grad` but wasn't computed from some other variable).  Under the hood, this node exists, and you can actually access it by writing something like `y.grad_fn.next_functions[0][0]`.  Intuitively, what an `AccumulateGrad` node does is take the in-flowing gradient and accumulates it into the `grad` field of the leaf tensor in question. This makes the autograd engine runtime modestly simpler since we don't need a special case for leaves with this design.

* The arrows here indicate strong owning pointers.  Under the hood, autograd is implemented using reference count pointers (just like Python), and avoidance of memory cycles and global state is going to play an important role in our implementation strategy.  Notably, these arrows are the *opposite* of what torchviz prints, but they fortunately do flow in the same direction that gradients flow in backwards.

* Each of these backward names (MulBackward0, PowBackward0, etc.) correspond to a generated C++ class that represents the autograd node.  The integer suffix distinguishes a specific overload of the function in question.  You can view the generated C++ files in a PyTorch build at `torch/csrc/autograd/generated/Functions.h` or `torch/csrc/autograd/generated/VariableTypeEverything.cpp`.

Now, in the process of making a backwards graph, we are writing the derivative for an *implicit* forwards graph, that corresponds to the trace of pure (non-mutating) operations that occurred in forwards.  This forwards graph is never materialized (unless you were using some sort of tracer), but it's helpful to think about this graph because, in some sense, all autograd is doing is taking this forward graph, reversing the direction of all the arrows and replacing the forward nodes with backward nodes.  Yes, this is kind of handwave-y, but if you just follow the recipe, it will help you work out most autograd examples on pencil and paper, even complicated ones involving double backwards.

```
 Forward           Backward
===================================
    x               x.grad
    |                  ^
    V                  |
 [ Pow ]         [ PowBackward ]
    |                  ^
    |                  |
    y               y.grad
    |                  |
    V                  |
 [ Mul ]         [ MulBackward ]
    |                  ^
    V                  |
    z               z.grad
```

(Note that `y.grad` and `z.grad` wouldn't actually get populated unless you ran autograd with `retain_grad=True`.)


# Handling mutation

It bears emphasizing that this implicit forward graph is *only ever* thought of as pure graph.  There is no MulInplaceBackward node.  To figure out what happens when mutation occurs, imagine the version of the program that *didn't* have mutation: that's the one that the autograd graph is the backwards of.  Consider:

```
x = torch.randn(8, requires_grad=True)
y = x ** 2
y.mul_(2)
```

This is semantically equivalent to:

```
x = torch.randn(8, requires_grad=True)
y = x ** 2
y2 = y * 2
# Imagine all uses of y now replaced with y2, assuming
# for now no aliases of y
```

And so that's the program which we will see encoded in the backwards graph.  How exactly does this happen?  Well, immediately before the mutation, we have a graph like this:

```
x -- grad_fn --> AccumulateGrad
                      ^
                      |
y -- grad_fn --> PowBackward0
```

When we do the multiplication, we go ahead and generate the new node MulBackward0:

```
x -- grad_fn --> AccumulateGrad
                      ^
                      |
y -- grad_fn --> PowBackward0
                      ^
                      |
                 MulBackward0
```

But because we mutated y inplace, we must also modify the `grad_fn` pointer inplace to point to this new node:

```
x -- grad_fn --> AccumulateGrad
                      ^
                      |
y                PowBackward0
|                     ^
|                     |
---- grad_fn --> MulBackward0
```

If y didn't have any aliases, once we do the mutation, there is no Tensor whose contents contains the value immediately after the pow; and correspondingly, there is no tensor whose `grad_fn` points to `PowBackward0`.

To recap, when mutation occurs, we have to do two things:

1. Generate a new backwards node corresponding to the computation that occurred on the tensor(s).

2. Modify the `grad_fn` of the tensor(s) whose value was affected by the mutation, to point to this node.

# Handling aliasing with a base tensor

When y doesn't alias anything, only one tensor is affected and life is easy.  But if there are aliases, multiple tensors might be affected.  Let's take a very simple example, where mutation is used to modify only a single row of a tensor:

```
x = torch.randn((8, 8), requires_grad=True)
y = x ** 2
v = y[0]
v.mul_(2)
```

Prior to the mutation, we have a graph that looks like this:

```
x -- grad_fn --> AccumulateGrad
                      ^
                      |
y -- grad_fn --> PowBackward0
                      ^
                      |
v -- grad_fn --> SelectBackward0
```

To perform this mutation, we have to update both `v.grad_fn` and `y.grad_fn`.  There are a few problems we have to solve here.  First, what exactly is the backward node we're going to point `y` to?  It's certainly not a `MulBackward0`, since that would be the backwards for the case if ALL of y had been multiplied; but in this case, only a single row of y was multiplied.  PyTorch has a neat composite backward node for cases like this: `CopySlices`.  Fortunately there's a nice comment about it in PyTorch's codebase that explains what it does:

```
// What is CopySlices?
// ~~~~~~~~~~~~~~~~~~~
//
// We support autograd with inplace mutation; e.g., if you write x.mul_(2)
// the autograd will work as if you now had multiple Tensors under the hood and
// you did
//   x = t.clone()
//   x0 = x
//   x1 = x0 * 2
//   x = x1
// As you can see here, after this operation, x.grad_fn now points to x1.grad_fn
// (the MulBackward node) and this node points to x's original grad_fn (which is
// also x0.grad_fn). It is important to keep in mind that after the inplace,
// there is no Tensor object that represents the x0 state anymore. But the graph
// for it is still around in autograd (in case x was used before being modified
// inplace). See Example 1 in
// https://docs.google.com/drawings/d/1-T5DyYfChMX1ONQkY-zU-hj_ayQ2zmA5CBOKDWqvEhE
// We call this rebasing the history of the Tensor.
//
// Now, a difficult situation is what happens if x is a differentiable view
// of a base b.
//   b = t.clone()
//   x = b.select(0, 0)
//   x *= 2
// With the same approach as above, this will become
//   b = t.clone()
//   x = b.select(0, 0)
//   b0 = b
//   x0 = x
//   x1 = x0 * 2
//   b1 = b0.select_scatter(x1, 0, 0)
//   x2 = b1.select(0, 0)
//   x = x2
//   b = b1
// As you can see here, not only we need to modify x's grad_fn, we also need to
// modify the one from b. We also need to ensure that the new grad_fn on x is
// linked to b's new grad_fn. The chain the select_scatter, multiplication and
// select is what CopySlices does, all wrapped into a single Node.
//
// See Example 1 in
// https://docs.google.com/drawings/d/1-T5DyYfChMX1ONQkY-zU-hj_ayQ2zmA5CBOKDWqvEhE
```

If you're a PL nerd, you might just say that views are actually just [lenses](https://www.cis.upenn.edu/~bcpierce/papers/lenses-etapsslides.pdf) and there's always a "putback" function that lets you scatter the modified result back into the bigger tensor the view was taken from.  And this purely functional interpretation of the mutation on a view, can have a backwards, and that backwards node is called CopySlices.

OK, sure, so we have ended up with this graph state:

```
x -- grad_fn --> AccumulateGrad
                      ^
                      |
y                PowBackward0
|                     ^
|                     |
+--- grad_fn --> CopySlices(MulBackward0)
```

What happens to v?  To avoid redundant backwards computation, what we want to do is "rebase" v's `grad_fn` on top of the new CopySlices backward node its base (y) is pointing to.  (You could have also just directly hung a MulBackward0 on top of the old v backwards, but if you do that, you will compute MulBackward0 twice!)  The final graph then looks like this:


```
x -- grad_fn --> AccumulateGrad
                      ^
                      |
y                PowBackward0  <--- SelectBackward0  (now dead!)
|                     ^
|                     |
+--- grad_fn --> CopySlices(MulBackward0)
                      ^
                      |
v -------------> SelectBackward0
```

Notice that the old `SelectBackward0 --> PowBackward0` node is now dead!  If the contents of view `v` hadn't been used in some other differentiable computation before the mutation, we would now GC this node as it doesn't contribute to the derivative.  However, if `v` had been used by some other compute prior to differentiation, the `grad_fn` of those values would still point to the old `SelectBackward0`!

(Another technical detail: if you actually print `v.grad_fn` in this example, it prints as `AsStridedBackward0`.  This is because, by default, when view operators are rebased, we desugar them into an `as_strided` call, as every view operation in PyTorch can be represented via `as_strided`.  This has caused no end of headache to alternate backend implementors that don't support `as_strided`, so there's an optional, backend specific mode that tries to induce PyTorch to try harder to preserve the original autograd function.  The one good thing about `as_strided` is that if you have multiple views, they will collapse into a single `AsStridedBackward0` node.)

To recap, when we mutate an alias of a tensor:

1. We update the base tensor (making a `CopySlices` backward node).

2. We rebase the alias tensor (reapply its backward node on top of the new `CopySlices` node).

Because you need access to the base tensor to do this, all differentiable views in PyTorch keep track of their base tensor.  You can actually access it from userland with `v._base`.  Some operations break differentiable views (most notably `detach()`), allowing you to modify a tensor without propagating derivatives.  However, even if you have a non-differentiable, version counters will still be shared (and so you will be able to detect if a value saved for backwards was invalidated by mutation.)

# Handling multiple aliases

In the example above, it was easy to rebase the view, since it was the one we were calling the mutation on.  But what if there are multiple views?

```
x = torch.randn((8, 8), requires_grad=True)
y = x ** 2
v1 = y[0,:]
v2 = y[:,0]
v1.mul_(2)
```

Now we are in trouble: how do we know to rebase `v2`?  We can't: `v1` only maintains a pointer to `y`, not to `v2`.  One obvious thing to do in this situation is to somehow have `y` keep track of all views into it.  In fact, when Sam was originally planning how to implement this, we had a design discussion about how to handle this case, and this was the "obvious" thing to do.  However, there are downsides.  There can be potentially many views of `y`, and so if y is keeping track of all of the views, it needs a dynamically allocated buffer to keep track of all the views.  The pointers to views cannot be strong references, since the views themselves hold a pointer to the base and so this would cause a cycle.  Finally, a design like this is not friendly to multithreading, since multiple threads may be manipulating views of a shared base, and they would all contend over these tracking pointers.

Here's a better idea: no one needs `v2` until `v2` is actually used in some computation.  So let's lazily rebase: we'll record a version for the parent on the `grad_fn` for `v2`, and when `v2.grad_fn` is accessed, we check if it is still up-to-date with the base.  If it isn't, we on-the-fly generate a rebased autograd node before doing compute.  Nice and easy!

# Conclusion

To summarize:

1. Within every mutating computation, there is implicitly a pure computation inside, which is what gets reflected in the autograd backwards graph.

2. When tensors are mutated, their `grad_fn` are mutated to point to new backward nodes.

3. Mutating views modifies the `grad_fn` of the base (CopySlices), before rebasing the view on top of the new base backward node.

4. When there are multiple views, the other views get lazily rebased on first access later.

I've heard people be surprised that it is possible to handle all of this.  Once you know the trick, it's actually quite easy!
