---
title: "Backpack in base"
date: 2016-08-08 02:13:52
slug: backpack-in-base
draft: true
categories: [Miscellaneous]
---

When I first joined the Backpack project, I thought its implementation would be just a summer internship. Two years later, it has become my PhD thesis, and we have only just hit the closing stretch of its implementation. Both the [GHC](https://github.com/ezyang/ghc/tree/ghc-backpack) and [Cabal](https://github.com/ezyang/cabal/tree/backpack) branches have gotten to the point where they are ready for code review for merging. I have also been working on a [specification](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst) which will serve as a definitive technical reference for Backpack and help people who are trying to understand the architecture and code implementing Backpack.

In short, there is a very good chance that in GHC 8.2, you will get the ability to write a package with some of its modules left unimplemented (you write an `hsig` signature instead), and specify, through Cabal, how you want those signatures to be implemented later.

That’s all very well and good, but what is an average user supposed to do with this new feature? One could certainly play around with it in a small personal project, or perhaps a larger one that can commit to being GHC 8.2 only, but it will take some time for the ecosystem to decide that it is a good idea (think several GHC releases) and start using it.

So how can Backpack start getting some boots on the ground, to find out what works, and what doesn't? Here's my suggestion: let's split and Backpack'ify base. There are two major reasons why I think this would be a good idea:

1.  It is [no hidden](https://ghc.haskell.org/trac/ghc/wiki/SplitBase) secret that there are many functions in `base` which really should return `Text` or `ByteString`, not `String`.
2.  base is already intimately tied to a particular GHC version (to the extent where you can't (easily) reinstall it), and so there is no problem deploying it while it is using bleeding edge features like Backpack.
