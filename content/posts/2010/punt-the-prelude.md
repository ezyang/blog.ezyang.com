---
title: "Punt the Prelude"
date: 2010-05-31 09:00:03
slug: punt-the-prelude
categories: [Haskell]
comments:
    - id: 467
      author: Eric Mertens
      date: "2010-05-31 15:10:10"
      content: "We need to nip this reverse generalization of (.) to fmap. It is no better than renaming (=&lt;&lt;) to concatMap!"
    - id: 468
      author: Imam Tashdid ul Alam
      date: "2010-05-31 15:22:59"
      content: |
        I think you mean the type "(-&gt;) a" is the functor.
        
        Also, sometimes people need more specific types [citation needed?]. For example, I thought renaming fmap to map would be a good idea but then "map (1+) (return 2)" for example has type "(Num a, Monad f, Functor f) =&gt; f a" rather than the more obvious "[Int]". Amazingly when I tried this in ghci the result was "3". Not "[3]". Admittedly, the confusion arises because when writing "map" I am still thinking "(a -&gt; b) -&gt; [a] -&gt; [b]", and wondering which Functor is ghci defaulting to. Interesting. Possibly potentially confusing. But to be honest I am fine with that.
        
        Nice post but we need to take this seriously. If only I could find the time (and the motivation) to push on the other prelude.
    - id: 469
      author: wren ng thornton
      date: "2010-05-31 16:31:03"
      content: |
        Indeed, the idea of using (.) for fmap needs to be nipped. The (.) operator is far more appropriate as a method of Category for composing morphisms. We already have (&lt;$&gt;) as a symbolic name for fmap, which is a far better name anyways since fmap is just generalizing ($).
        
        For generalized zipping, there are some other approaches too. There's <a href="http://community.haskell.org/~wren/wren-extras/src/Data/List/ZipWithN.hs" rel="nofollow">my version</a> based on Chung-chieh Shan's work with polyvariadic non-regular types. And there's <a href="http://paczesiowa.blogspot.com/2010/03/generalized-zipwithn.html" rel="nofollow">paczesiowa's version</a> which is vastly more complex but lets you avoid a few type signatures.
    - id: 470
      author: C. McCann
      date: "2010-05-31 17:14:29"
      content: "For what it's worth--GHCi will default ambiguous Functor/Applicative/Monad values to IO, which shouldn't be too surprising upon reflection. So \"fmap (1+) (return 2)\" at the prompt evaluates as the type \"IO Integer\", which GHCi then binds and prints, as it does with any type \"Show a =&gt; IO a\"."
    - id: 471
      author: Imam Tashdid ul Alam
      date: "2010-05-31 18:10:26"
      content: "Aha! Thanks C. McCann."
    - id: 472
      author: Edward Z. Yang
      date: "2010-05-31 21:26:16"
      content: "Imam, the \"reader monad\" in Control.Monad.Reader is isomorphic to (->) a. I've updated it to be the less ambiguous notion though."
    - id: 473
      author: jberryman
      date: "2010-05-31 21:35:23"
      content: |
        That was quite an interesting roundup. Thanks! I stumbled upon the Numeric Prelude when I was writing a blog post about lazy arithmetic: 
        
        http://coder.bsimmons.name/blog/2010/03/lazy-arithmetic-in-haskell/
        
        ...but I wasn't familiar with most of these other ideas and projects.
---

*Conservation attention notice.* What definitions from the Haskell 98 Prelude tend to get hidden? I informally take a go over the Prelude and mention some candidates.

`(.)` in the Prelude is function composition, that is, `(b -> c) -> (a -> b) -> a -> c`. But the denizens of \#haskell know it can be much more than that: the function `a -> b` is really just the functor, so a more general type is `Functor f => (b -> c) -> f b -> f c`, i.e. fmap. Even more generally, `(.)` can indicate morphism composition, as it does in [Control.Category](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Category.html).

`all`, `and`, `any`, `concat`, `concatMap`, `elem`, `foldl`, `foldl1`, `foldr`, `foldr1`, `mapM_`, `maximum`, `minimum`, `or`, `product`, `sequence_`. These are all functions that operate on lists, that easily generalize to the `Foldable` type class; just replace `[a]` with `Foldable t => t a`. They can be found in [Data.Foldable](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Foldable.html).

`mapM`, `sequence`. These functions generalize to the `Traversable` type class. They can be found in [Data.Traversable](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Traversable.html).

*Any numeric function or type class.* Thurston, Thielemann and Johansson wrote [numeric-prelude](http://hackage.haskell.org/package/numeric-prelude-0.1.3.4), which dramatically reorganized the hierarchy of numeric classes and generally moved things much closer to their mathematical roots. While dubbed experimental, it's seen airplay in more mathematics oriented Haskell modules such as Yorgey's [species](http://hackage.haskell.org/package/species) package.

*Any list function.* Many data structures look and smell like lists, and support some set of the operations analogous to those on lists. Most modules rely on naming convention, and as a result, list-like constructs like vectors, streams, bytestrings and others ask you to import themselves qualified. However, there is [Data.ListLike](http://hackage.haskell.org/packages/archive/ListLike/latest/doc/html/Data-ListLike.html) which attempts to encode similarities between these. [Prelude.Listless](http://hackage.haskell.org/packages/archive/list-extras/0.3.0/doc/html/Prelude-Listless.html) offers a version of the Prelude minus list functions.

`Monad`, `Functor`. It is widely believed that Monad should probably be an instance of `Applicative` (and the category theorists might also have you insert `Pointed` functors in the hierarchy too.) [The Other Prelude](http://www.haskell.org/haskellwiki/The_Other_Prelude) contains this other organization, although it is cumbersome to use in practice since the new class means most existing monad libraries are not usable.

`repeat`, `until`. There is an admittedly oddball generalization for these two functions in [Control.Monad.HT](http://hackage.haskell.org/packages/archive/utility-ht/latest/doc/html/Control-Monad-HT.html). In particular, `repeat` generalizes the identity monad (explicit (un)wrapping necessary), and `until` generalizes the `(->) a` monad.

`map`. It's `fmap` for lists.

`zip`, `zipWith`, `zipWith3`, `unzip`. Conal's [Data.Zip](http://hackage.haskell.org/packages/archive/TypeCompose/latest/doc/html/Data-Zip.html) generalize zipping into the `Zip` type class.

*IO.* By far you'll see the most variation here, with a multitude of modules working on many different levels to give extra functionality. (Unfortunately, they're not really composable...)

- [System.IO.Encoding](http://hackage.haskell.org/packages/archive/encoding/latest/doc/html/System-IO-Encoding.html) makes the IO functions encoding aware, and uses implicit parameters to allow for a "default encoding." Relatedly, [System.UTF8IO](http://hackage.haskell.org/packages/archive/utf8-prelude/latest/doc/html/System-UTF8IO.html) exports functions just for UTF-8.
- [System.IO.Jail](http://hackage.haskell.org/packages/archive/jail/latest/doc/html/System-IO-Jail.html) lets you force input-output to only take place on whitelisted directories and/or handles.
- [System.IO.Strict](http://hackage.haskell.org/packages/archive/strict-io/latest/doc/html/System-IO-Strict.html) gives strict versions of IO functions, so you don't have to worry about running out of file handles.
- [System.Path.IO](http://hackage.haskell.org/packages/archive/pathtype/latest/doc/html/System-Path-IO.html), while not quite IO per se, provides typesafe filename manipulation and IO functions to use those types accordingly.
- [System.IO.SaferFileHandles](http://hackage.haskell.org/packages/archive/safer-file-handles/latest/doc/html/System-IO-SaferFileHandles.html) allows handles to be used with monadic regions, and parametrizes handles on the IO mode they were opened with. [System.IO.ExplicitIOModes](http://hackage.haskell.org/packages/archive/explicit-iomodes/latest/doc/html/System-IO-ExplicitIOModes.html) just handles IOMode.
