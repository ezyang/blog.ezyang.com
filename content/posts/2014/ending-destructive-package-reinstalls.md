---
title: "The beginning of the end of destructive package reinstalls in Haskell"
date: 2014-08-12 10:01:56
slug: ending-destructive-package-reinstalls
draft: true
categories: [Backpack, GHC, Haskell]
---

<div class="container center">

This post is part of [a series about Backpack](http://blog.ezyang.com/2014/08/whats-a-module-system-good-for-anyway/), a new module system I have been implementing in GHC.

</div>

THE FACTS

- Package key, based off of hash of dependency tree (and instantiated modules)

THE THINGS TO DO

- Change installed package ID from ABI hash to *guaranteed unique* identifier based on hash of source code, command line options, etc. Goal is that equal IPID = equal ABI hash.
- IHG package environments
- Constraint solver adjustments

CONCEPTUALLY, we are giving users the TOOLS to manage THEIR OWN STACKAGE in an INCREMENTAL WAY

Every Cabal user is familiar with the message: "The following packages are likely to be broken by the reinstalls; use `--force-reinstalls` if you want to install anyway." In [my first post of this series](http://blog.ezyang.com/2014/08/whats-a-module-system-good-for-anyway/), I stated that Cabal often needs to reinstall packages because of an invariant that GHC's package database (the list of packages which is visible to GHC during compilation) there can only be one instance of a package for any given package name and version (e.g. containers-0.9).

While I would like to report that this restriction has gone away in GHC HEAD, this is not quite true yet. In fact, by default, Cabal behaves exactly the same as it did before. So what's different? First, we can experimentally enable non-destructive reinstalls by passing the flags `--ghc-pkg-options="--enable-multi-instance" --force-reinstalls` to Cabal. (The flags are terrible because they are intended to be temporary.) Second, thanks to changes to how GHC internally thinks about *module identity*, it's now possible to link together two instances of the same package and version in the same program, even if the package was compiled against different dependencies.

# Why can't we just enable multi-instances by default?

# A comparison with Nix

# A comparison with node.js and NPM
