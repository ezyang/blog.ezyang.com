---
title: "Polymorphic accumulators"
date: 2012-07-28 22:56:51
slug: polymorphic-accumulators
draft: true
categories: [Programming Languages]
---

A classic exercise for beginning functional programmers is this: how do you reverse a list using a (right) fold in linear time? The trick is to use an accumulator which is a function, not a bare value:

    Prelude> foldr (\x k -> k . (x:)) id [1,2,3] []
    [3,2,1]

This fold builds up a function `[Int] -> [Int]`, which we pass the empty list `[]` to get our final list. (Another way of looking at this solution is that we are using difference lists which support efficient snoc).

However, this technique is not limited only to functions that take values as arguments: they can also work with functions that take *types* as arguments. We call these **polymorphic accumulators**, since polymorphic values are really just functions from a concrete type to a value. Alas, Haskell’s type-level computation facilities are a little unwieldy, so our example here will be written in Ur/Web, which has more direct support for type-level computation.

Suppose that we have a type-level record `ts`, e.g. `[ A = int, B = bool ]`. The usual
