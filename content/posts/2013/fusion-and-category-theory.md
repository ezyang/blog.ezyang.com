---
title: "Fusion and category theory"
date: 2013-04-25 21:30:26
slug: fusion-and-category-theory
draft: true
categories: [Haskell]
---

*Folklore post today; bear with me if you’ve seen this all before.* A well known loop fusion rule is the following one on maps (where `g :: a -> b` and `h :: b -> c`) :

    map h . map g == map (h . g)

The rule is so obvious it seems barely worth mentioning; in *A Short Cut to Deforestation*, Gill, Launchbury and Jones note that there are many such algebraic transformations, and that any approach to deforesting list functions needs a way to standardize across these transformations, so that the compiler author doesn’t have to list them all.

Of course, there is something rather special about this rule, which is clearer when the rule is rewritten slightly:

    fmap h . fmap g == fmap (h . g)

It is one of the functor laws! So, unlike many of the other transformations in the tradition of stream fusion, which only apply to lists, this transformation applies to all functors.

As it turns out, category theory is dripping in “fusion laws”. A particularly important instance (since we are speaking of lists) is the fusion law for initial algebras, aka folds. When specialized for lists (`k :: r1 -> r2`, `z1 :: r1`, `z2 :: r2`, `h1 :: a -> r1 -> r1`, `h2 :: a -> r2 -> r2`), the fusion law looks like this:

    if  k z1 = z2
    and  k (h1 a as) = h2 a (k as)
    then  f . foldr z1 h1 = foldr z2 h2

What this law says that if you perform a fold, and then some other operation on the result of the fold, if that operation obeys two invariants (the *if* statement), then you can replace the fold with a single fold.
