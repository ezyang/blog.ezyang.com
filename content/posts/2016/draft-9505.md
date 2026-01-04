---
title: "Announcing cabal new-build"
date: 2016-04-12 17:26:29
slug: 
draft: true
categories: [Miscellaneous]
---

**tl;dr** With the soon-to-be released cabal-install 1.24, try `cabal new-build` instead of sandboxes or Stack; you'll be pleasantly surprised.

`cabal new-build`, also known as “Nix-style local builds”, is a new command that will be tech-previewed with cabal-install 1.24. Nix-style local builds combine the best of non-sandboxed and sandboxed Cabal:

1.  Like sandboxed Cabal today, we build sets of independent local packages deterministically and independent of any global state. new-build will never tell you that it can't build your package because it would result in a “dangerous reinstall.” Given a particular state of the Hackage index, your build is completely reproducible.
2.  Like non-sandboxed Cabal today, builds of external packages are cached globally, so that a package can be built once, and then reused anywhere else it is also used. No need to continually rebuild dependencies whenever you make a new sandbox: dependencies which can be shared, are shared.

I’ve been using Nix-style local builds exclusively to do my Haskell development, and it's hard to understate my enthusiasm for this new feature. It achieves much of the same user experience as Stack, but without positing the existence of a distribution of blessed, version-pegged packages to build against (e.g., Stackage). There is still lots of polish needed (it's missing commands and no user interface for freezing dependency solver results), but it is working well enough to be useful for early adopters.

# Quick start

1.  Download and install the [cabal-install 1.24 prerelease](https://github.com/haskell/cabal/tree/1.24):

        git clone https://github.com/haskell/cabal.git --branch="1.24" cabal-nix-local-build
        cd cabal-nix-local-build
        (cd Cabal; cabal install)
        (cd cabal-install; cabal install)

2.  To build a single Cabal package, instead of running `cabal configure; cabal build`, you can use Nix-style builds by prefixing these commands with `new-`; e.g., `cabal new-configure; cabal new-build`.

3.  To build multiple Cabal packages, you need to first create `cabal.project` file in some root directory, as so:

        packages: Cabal
                  cabal-install

In the `packages` field, list the Cabal file of every package that you would like to build locally. This field supports abbreviations: you can glob using `*`, and the use of braces to denote alternatives is also supported.

By default, `cabal new-build` builds every component in the package that is your current working directory. If there is no package in your current working directory (e.g., you're in the root of the folder containing your packages), `cabal new-build` builds nothing. You can instead specify what packages you would like to be built as arguments. For example, `cabal new-build Cabal:package-tests exe:cabal --enable-tests` requests two components be built: the component `package-tests` from the package `Cabal`, and the executable named `cabal` (which happens to be from the package `cabal-install`).

# cabal.project files

# New commands

# Known issues

As a tech preview, the code is still [a little rough around the edges](https://github.com/haskell/cabal/labels/nix-local-build). Here are some more major issues you might run into:

- If you get the error “Encountered missing dependencies”, you have problem run into issue [\#3199](https://github.com/haskell/cabal/issues/3199), which we plan on fixing prior to the release of 1.24. Packages known to be affected by this issue include `cabal-install` and `gtk3`. The referenced ticket mentions some workarounds.
- Although dependency resolution is deterministic, if you update your Hackage index with `cabal update`, [dependency resolution will change too](https://github.com/haskell/cabal/issues/2996). `cabal freeze` does not currently work, so you'll have to manually construct the set of desired constraints.
- A new feature of new-build is that it avoids rebuilding packages when there have been no changes to them, by tracking the hashes of their contents. However, this dependency tracking is not 100% accurate (specifically, it relies on your Cabal file accurately reporting all file dependencies ala `sdist`, and it doesn't know about search paths). There's currently no UI for forcing a package to be recompiled; however you can induce a recompilation fairly easily by removing an appropriate cache file: specifically, for the package named `p-1.0`, delete the file `dist-newstyle/build/p-1.0/cache/build`.
