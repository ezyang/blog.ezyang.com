---
title: "Separate compilation for type classes is difficult"
date: 2015-10-28 05:26:25
slug: 
draft: true
categories: [GHC]
---

Separate compilation is the ability to, given an interface, build code which uses the interface and code which implements the interface separately, and then link them together later. The "interface" in question is sometimes a bit vague: what is the interface between a module `A` and a module `B` that imports it? For GHC, that interface is the `A.hi` file, which may vary anywhere from extremely concrete (the output of `ghc -O -c A.hs`) to extremely abstract (the output of `ghc -c A.hs-boot`). The more concrete the interface, the more opportunity for optimization; the more abstract, the more we can avoid recompilation.

Type classes—or more specifically, type class instances—pose difficulties for separate compilation, because they are *implicit*. When I write an `hs-boot` file:

    module A where
    instance Eq (a -> b) where show _ = "function"

and then compile an
