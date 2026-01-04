---
title: "You could have invented CuTe hierarchical layout (but maybe not the rest of it?)"
date: 2025-08-22 02:48:48
slug: you-could-have-invented-cute-hierarchical-layout-but-maybe-not-the-rest-of-it
categories: [PyTorch]
---

CuTe is a C++ library that aims to make dealing with complicated indexing easier. A key part of how it does this is by defining a [Layout](https://docs.nvidia.com/cutlass/media/docs/cpp/cute/01_layout.html) type, which specifies how to map from logical coordinates to physical locations (CuTe likes to say layouts are "functions from integers to integers.") In fact, CuTe layouts are a generalization of PyTorch strides, which say you always do this mapping by multiplying each coordinate with its respective stride and summing them together, e.g., `i0 * s0 + i1 * s1 + ...`. Although NVIDIA's docs don't spell it out, the CuTe's generalization here is actually very natural, and in this blog post I'd like to explain how you could have invented it (on a good day).

First, a brief recap about strides. PyTorch views allow us to reinterpret the physical layout of a tensor in different ways, changing how we map logical coordinates into physical locations. For example, consider this 2-D tensor:

    >>> torch.arange(4).view(2, 2)
    tensor([[0, 1],
            [2, 3]])
    >>> torch.arange(4).view(2, 2).stride()
    (2, 1)

The physical memory reads `0, 1, 2, 3`, and if I want to know what the value at coordinate (0, 1) is (row 0, col 1), I compute `0 * 2 + 1 * 1`, which tells me I should read out the value at index 1 in physical memory. If I change the strides, I can change the order I read out the physical locations. For example, if I transpose I have:

    >>> torch.arange(4).view(2, 2).T
    tensor([[0, 2],
            [1, 3]])
    >>> torch.arange(4).view(2, 2).T.stride()
    (1, 2)

The physical memory hasn't changed, but now when we read out coordinate (0, 1), we compute `0 * 1 + 1 * 2`, which tells me I should read the value at index 2 (which is indeed what I see at this coordinate!)

PyTorch also allows us to "flatten" dimensions of a tensor, treating them as a 1D tensor. Intuitively, a 2-D tensor flattened into a 1-D one involves just concatenating all the rows together into one line:

    >>> torch.arange(4).view(2, 2).view(-1)
    tensor([0, 1, 2, 3])

We should be able to do this for the transpose too, getting `tensor([0, 2, 1, 3])`, but instead, this is what you get:

    >>> torch.arange(4).view(2, 2).T.view(-1)
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
    RuntimeError: view size is not compatible with input tensor's size and stride (at least one dimension spans across two contiguous subspaces). Use .reshape(...) instead.

The dreaded "use reshape instead" error! The error is unavoidable under PyTorch striding: there is *no* stride we can select that will cause us to read the elements in this order (0, 2, 1, 3); after all, `i0 * s0` is a pretty simple equation, we can't simultaneously have `1 * s0 == 2` and `2 * s0 == 1`.

Upon learning this, an understandable reaction is to just shrug, assume that this is impossible to fix, and move on with your life. But today, you are especially annoyed by this problem, because you were only trying to flatten N batch dimensions into a single batch dimension so that you could pass it through a function that only works with one batch dimension, with the plan of unflattening it when you're done. It doesn't matter that this particular layout is inexpressible with strides; you aren't going to rely on the layout in any nontrivial way, you just care that you can flatten and then unflatten back to the original layout.

Imagine we're dealing with a tensor of size `(2, 2, 2)` where the strides for dim 0 and dim 1 were transposed as `(2, 4, 1)`. It should be OK to flatten this into a tensor `(4, 2)` and then unflatten it back to `(2, 2, 2)`. Intuitively, I'd like to "remember" what the original sizes and strides are, so that I can go back to them. Here's an idea: let's just store the original size/stride as a *nested* entry in our size tuple. So instead of the size `(4, 2)`, we have `((2, 2), 2)`; and now analogously the stride can simply be `((2, 4), 1)`. When I write `(2, 2)` as the "size" of a dimension, I really just mean the product 4, but there is some *internal structure* that affects how I should index its inside, namely, the strides `(2, 4)`. If I ask for the row at index 2, I first have to translate this 1D coordinate into a 2D coordinate (1, 0), and then apply the strides to it like before.

Well, it turns out, this is exactly how CuTe layouts work! In CuTe, sizes/strides are hierarchical: a size is actually a tree of ints, where the hierarchy denotes internal structure of a dimension that you can address linearly (in fact, everything by default can be addressed in a 1-D linear way, even if its an N-D object.) The [documentation of Layout](https://docs.nvidia.com/cutlass/media/docs/cpp/cute/01_layout.html) does say this... but I actually suffered a lot extracting out the high level intuition of this blog post, because CuTe uses co-lexicographic ordering when linearizing (it iterates over coordinates (0,0), (1,0), (2,0), etc. rather than in the more normal lexicographic order (0,0), (0,1), (0,2)). This leads to some truly deranged example code where they print a 2D matrix in conventional lexicographic ordering, and then turn around and say, "But wait, if I have the layout take care of translating the 1D coordinate into an ND coordinate, it is colexicographic!!":

    > print2D(s2xh4)
      0    2    1    3
      4    6    5    7
    # sure, why not?

    > print1D(s2xh4)
      0    4    2    6    1    5    3    7
    # wtf???

In any case, if you want to engage with the documentation, `s2xh4` is the important example to pay attention to for understanding the nested semantics. However, note the example is smeared across like five sections and also you need to know about the co-lexicographic thing to understand why the examples print the way they do.
