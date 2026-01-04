---
title: "vmap in Haskell"
date: 2020-01-29 14:14:34
slug: vmap-in-haskell
categories: [Haskell, PyTorch]
comments:
    - id: 23449
      author: "New top story on Hacker News: Vmap in Haskell &#8211; Latest news"
      date: "2020-01-31 04:08:40"
      content: "[&#8230;] Vmap in Haskell 4 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23450
      author: "New top story on Hacker News: Vmap in Haskell &#8211; Ultimate News"
      date: "2020-01-31 04:10:26"
      content: "[&#8230;] Vmap in Haskell 4 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23451
      author: "New top story on Hacker News: Vmap in Haskell &#8211; Hckr News"
      date: "2020-01-31 04:14:30"
      content: "[&#8230;] Vmap in Haskell 4 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23452
      author: "Top story HACKER NEWS: Vmap in Haskell - Nate&#039;s Blog"
      date: "2020-01-31 04:20:37"
      content: "[&#8230;] Vmap in Haskell 4 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23453
      author: "New top story on Hacker News: Vmap in Haskell &#8211; protipsss"
      date: "2020-01-31 04:23:24"
      content: "[&#8230;] Vmap in Haskell 4 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23454
      author: "New top story on Hacker News: Vmap in Haskell &#8211; Outside The Know"
      date: "2020-01-31 04:36:50"
      content: "[&#8230;] Vmap in Haskell 5 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23455
      author: "New top story on Hacker News: Vmap in Haskell &#8211; Outside The Know"
      date: "2020-01-31 04:38:51"
      content: "[&#8230;] Vmap in Haskell 8 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23456
      author: "New top story on Hacker News: Vmap in Haskell &#8211; News about world"
      date: "2020-01-31 04:44:28"
      content: "[&#8230;] Vmap in Haskell 8 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23457
      author: "Top story HACKER NEWS: Vmap in Haskell - Nate&#039;s Blog"
      date: "2020-01-31 04:50:45"
      content: "[&#8230;] Vmap in Haskell 11 by gbrown_ | 1 comments on Hacker News. [&#8230;]"
    - id: 23458
      author: Anonymous
      date: "2020-01-31 06:13:17"
      content: |
        "[...] trivial implementation of vmap would have the signature ([a] -&gt; [b]) -&gt; [a] -&gt; [b] (aka the identity function) [...]"
        
        Are you sure? An identity `a -&gt; a` over a map would give the type `[a] -&gt; [a]` surely.
    - id: 23462
      author: Edward Z. Yang
      date: "2020-01-31 12:02:22"
      content: "Yes, but if actually take a pointwise function as argument to map, then we have to implement map by looping, defeating the purpose of vmap."
    - id: 23464
      author: a1369209993
      date: "2020-01-31 17:28:57"
      content: |
        Found a error:
        
        &gt; in example1, we broadcast along each dimension separately, so that we end up with a cartesian product in the end; in example2, we broadcast the dimensions together and get the zippy behavior
        
        It's actually the other way around (as stated later); example1 is zippy and example2 is cartesian.
    - id: 23468
      author: "Resumen de lecturas compartidas del 25 al 31 de enero de 2020 | Vestigium"
      date: "2020-02-01 06:16:27"
      content: "[&#8230;] vmap in Haskell. ~ Edward Z. Yang (@ezyang). #Haskell #FunctionalProgramming [&#8230;]"
    - id: 23473
      author: Edward Z. Yang
      date: "2020-02-03 10:28:17"
      content: "Thanks, that's my fault for swapping the examples and not fixing the text. Should be good now!"
    - id: 24052
      author: "Resumen de lecturas compartidas durante enero de 2020 | Vestigium"
      date: "2020-08-01 12:50:49"
      content: "[&#8230;] vmap in Haskell. ~ Edward Z. Yang (@ezyang). #Haskell #FunctionalProgramming [&#8230;]"
    - id: 25731
      author: rouanth
      date: "2021-03-20 12:48:28"
      content: |
        &gt; vmapG ::  (forall s. Vec s a -&gt; f (Vec s b)) -&gt; [a] -&gt; f [b] and have f unify with, well, the identity type lambda /\a. a when we need it to have the type of vmap0?
        
        You may want to look at the identity functor: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Functor-Identity.html. Sure, using it would still require some boilerplate, so having `vmap0` as a convenient wrapper around `vmapG` could still be useful, but, in any case, the implementation of the core logic would be in one function.
    - id: 26490
      author: Anonymous
      date: "2021-07-20 02:52:39"
      content: "Thank for your publication!"
---

[vmap](https://github.com/google/jax#auto-vectorization-with-vmap) is an interface popularized by JAX which offers you a vectorizing map. Semantically, a vmap is exactly equivalent to a map in Haskell; the key difference is that operations run under a vmap are vectorized. If you map a convolution and a matrix multiply, you will have one big loop which repeatedly calls convolution and matrix multiply for each entry in your batch. If you *vmap* a convolution and matrix multiply, you'll call the batched versions of convolution and matrix multiply once. Unless you have a fuser, on most modern deep learning frameworks, calling the batched implementations of these operations will be much faster.

JAX implements vmap in a somewhat complicated fashion; they have a "batched interpreter" which translates operations on primitives into their batched versions, and have to track metadata about what tensors are batched and in what way so that they can insert appropriate broadcasts and unsqueezes. I mentioned this to Simon Peyton Jones, and he immediately asked, couldn't Haskell's typechecker work this out automatically? The answer is, yes! All of the book-keeping JAX has to do is effectively doing runtime type inference; if you have a compiler that can do it for you at compile time, there is nearly nothing to implement.

To give away the punchline, we are going to implement a family of functions <span class="title-ref">vmap</span> that will run these two examples:

    example1 :: [Float] -> [Float] -> [Float]
    example1 a0 b0 =
      vmap0_2 (\a b -> add a b) a0 b0

    example2 :: [Float] -> [Float] -> [[Float]]
    example2 a0 b0 =
      vmap0 (\a -> vmap1 (\b -> add a b) b0) a0

When run in an interpreter, we will see:

    *Test> example1 [1,2,3] [4,6,8]
    [5.0,8.0,11.0]
    *Test> example2 [1,2,3] [4,6,8]
    [[5.0,7.0,9.0],[6.0,8.0,10.0],[7.0,9.0,11.0]]

These results are equivalent to what you would have gotten using a plain old `map`; however, there will be **no** loop in the implementation of vmap. (The fact that we can't write a single vmap that works universally is due to a limitation in Haskell; we'll discuss this more later.)

------------------------------------------------------------------------

We're going to need a few language extensions, so let's get this out of the way first:

    {-# LANGUAGE RankNTypes, GADTs, MultiParamTypeClasses,
                 KindSignatures, TypeApplications, FunctionalDependencies,
                 FlexibleContexts, FlexibleInstances, UndecidableInstances,
                 IncoherentInstances #-}

Our plan of attack is that we want to write the definitions of vmap so that we infer a type for `add` which makes the necessary broadcasting clear. A trivial implementation of vmap would have the signature `([a] -> [b]) -> [a] -> [b]` (aka the identity function), but the standard list type doesn't let us distinguish between dimensions we should broadcast together, and dimensions we shouldn't (this is the reason `example1` and `example2` give different results: in `example2`, we broadcast along each dimension separately, so that we end up with a cartesian product in the end; in `example1`, we broadcast the dimensions together and get the zippy behavior). Each distinct invocation of vmap should give us a new dimension, which ought not to be mixed up with other invocations of vmap. When you hear this in Haskell, your first instinct should be, "I know, let's use a rank 2 type!" vmap moves us from the non-type-branded world of vanilla lists `[Float]` to a type-branded world of size-indexed vectors `Vec s Float`, where the `s` variables are all skolem variables bound by our rank 2 type:

    data Vec s a = Vec { unVec :: [a] }
    instance Functor (Vec s) where
      fmap f (Vec xs) = Vec (map f xs)

    vmap0 :: (forall s. Vec s a -> Vec s b) -> [a] -> [b]
    vmap0 f = unVec . f . Vec

The implementation of `vmap0` doesn't do anything: we just wrap the lists into their type-branded equivalent vectors. We can also provide a 2-ary version of vmap0, which takes two lists and assigns them the same type branding all at once:

    vmap0_2 :: (forall s. Vec s a -> Vec s b -> Vec s c) -> [a] -> [b] -> [c]
    vmap0_2 f a b = unVec (f (Vec a) (Vec b))

(In principle, some sort of applicative-y thing should make it possible to write just a vap (analogous to `ap`) and then get all of the n-ary versions for free, but in my brief investigation I didn't see a good way of doing this.)

When we nest vmap, it may be the case that the function doesn't directly return a `Vec s b`, but a functor containing `Vec s b`. `vmap1` handles this case (we'll discuss this more shortly):

    vmap1 :: Functor f => (forall s. Vec s a -> f (Vec s b)) -> [a] -> f [b]
    vmap1 f = fmap unVec . f . Vec

With our implementations of vmap in hand, we can take a look at our examples and ask Haskell what the type of `add` ought to be, if we didn't have an implementation of it:

    example1 :: [Float] -> [Float] -> [Float]
    example1 a0 b0 =
      vmap0_2 (\a b -> _add a b) a0 b0

Gives:

    • Found hole: _add :: Vec s Float -> Vec s Float -> Vec s Float
      Where: ‘s’ is a rigid type variable bound by
               a type expected by the context:
                 forall s. Vec s Float -> Vec s Float -> Vec s Float

However:

    example2 :: [Float] -> [Float] -> [[Float]]
    example2 a0 b0 =
      vmap0 (\a -> vmap1 (\b -> _add a b) b0) a0

Gives:

    • Found hole:
        _add :: Vec s Float -> Vec s1 Float -> Vec s (Vec s1 Float)
      Where: ‘s1’ is a rigid type variable bound by
               a type expected by the context:
                 forall s1. Vec s1 Float -> Vec s (Vec s1 Float)
               at test.hs:41:20-44
             ‘s’ is a rigid type variable bound by
               a type expected by the context:
                 forall s. Vec s Float -> Vec s [Float]
               at test.hs:41:7-48

Notice that the inferred types of `_add` are different in these two cases: in the first example, we infer that we have two tensors batched in the same way, and we want to "zip" them together. In the second example, we see that each tensor has a distinct batch dimension, and we end up with a 2-D result!

At this point, the job of vmap is done: our holes have types which we can use to determine what the necessary behavior is. You could use these types to select an appropriate kernel to perform vectorized addition. But I promised runnable code, so let's implement a simple version of add using old fashioned map.

The good old fashioned way to do type level computation in Haskell is with a type class, of course! Let's define a multi-parameter type class for the function `add`; unlike the definition of `(+)` in `Num`, we'll let the inputs and output all have different types:

    class Add a b c | a b -> c where
      add :: a -> b -> c

We can easily implement addition on plain floating point:

    instance Add Float Float Float where
      add = (+)

If I pass add two arguments whose outer-most vector agree in their type brand (aka, they came from the same vmap), I should zip them together, as I did in `example1`. I can write another instance to express this logic:

    instance Add a b r  => Add (Vec s a) (Vec s b) (Vec s r) where
      add (Vec a) (Vec b) = Vec (zipWith add a b)

Otherwise, I should broadcast one of the dimensions and then do an addition on the inside. This choice can't easily be made locally, so I have to define these two incoherent instances:

    instance Add a b r => Add (Vec s a) b (Vec s r) where
      add (Vec a) b = Vec (map (\x -> add x b) a)

    instance Add a b r => Add a (Vec s b) (Vec s r) where
      add a (Vec b) = Vec (map (\x -> add a x) b)

(GHC's type class resolution engine doesn't backtrack, so I'm not actually sure how it manages to pick the correct instance to use, but in my testing, I got the right instance no matter what order I specified the arguments to add.)

That's it! Running the two examples:

    example1 :: [Float] -> [Float] -> [Float]
    example1 a0 b0 =
      vmap0_2 (\a b -> add a b) a0 b0

    example2 :: [Float] -> [Float] -> [[Float]]
    example2 a0 b0 =
      vmap0 (\a -> vmap1 (\b -> add a b) b0) a0

I get:

    *Test> example1 [1,2,3] [4,6,8]
    [5.0,8.0,11.0]
    *Test> example2 [1,2,3] [4,6,8]
    [[5.0,7.0,9.0],[6.0,8.0,10.0],[7.0,9.0,11.0]]

------------------------------------------------------------------------

So there you have it! vmap in less than a dozen lines of Haskell. One unsatisfactory thing about this implementation is the necessity to define `vmap0`, `vmap1`, etc. Can't we just define a generic `vmapG ::  (forall s. Vec s a -> f (Vec s b)) -> [a] -> f [b]` and have `f` unify with, well, the identity type lambda `/\a. a` when we need it to have the type of `vmap0`? Regretfully, type inference with type lambdas is undecidable (the so-called higher-order unification problem), so it seem we have to help GHC out here, even though in our particular case the unification we can do here is very restricted.
