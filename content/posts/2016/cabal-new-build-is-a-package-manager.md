---
title: "cabal new-build is a package manager"
date: 2016-08-29 17:32:36
slug: cabal-new-build-is-a-package-manager
categories: [Haskell]
comments:
    - id: 21161
      author: Wow
      date: "2016-08-29 18:02:45"
      content: |
        Wow!
        Does it stop a current flame war with stack?
    - id: 21162
      author: Edward Z. Yang
      date: "2016-08-29 18:07:44"
      content: |
        Only YOU can stop forest fires!
        
        <img src="https://upload.wikimedia.org/wikipedia/commons/a/ae/Smokey3.jpg">
    - id: 21164
      author: Ruben
      date: "2016-08-29 20:48:00"
      content: "You &amp; the cabal devs are awesome. Thanks for the interesting features!"
    - id: 21169
      author: gasche
      date: "2016-08-31 04:26:45"
      content: |
        Regarding pkg-config depends: In the OCaml community we have been incrementally building a mapping from "external dependencies" (some of them being queryable through pkg-config, some not) to package names on package distribution systems for major UNIX-style distributions out there. See for example our metadata for the conf-libev package which lists package names for debian, ubuntu, mageia, centos, fedora, {free,open}bsd, and homebrew (an OSX package manager).
        
          https://github.com/ocaml/opam-repository/blob/master/packages/conf-cairo/conf-cairo.1/opam
        
        (This is a good case, in general you have debian, ubuntu for most packages, and the rest is built over time as users of various distributions report their package name.)
        
        It could be possible to export this information in a format that cabal could also use to give more specific advice than "we need pkg-config X available", and even share information. Now I wonder if this same information is already available somewhere else.
    - id: 21177
      author: Steven Shaw
      date: "2016-09-01 08:44:42"
      content: "@gasche, that looks really nice. I hadn't realised that opam tries to install system dependencies."
    - id: 21442
      author: Mitsutoshi Aoe
      date: "2016-10-18 20:07:16"
      content: |
        Is there a way to instruct cabal new-build to install a build tool that is not hard-coded in Cabal?
        
        For example, pandoc +embed_data_files requires hsb2hs to be installed but cabal new-build (from HEAD) doesn't seem to understand how to install it.
    - id: 21443
      author: Edward Z. Yang
      date: "2016-10-18 20:17:04"
      content: "Mitsutoshi: Unfortunately not yet. The tracking bug for this is https://github.com/haskell/cabal/issues/3708"
    - id: 21444
      author: Mitsutoshi Aoe
      date: "2016-10-18 20:22:26"
      content: "Edward: Thanks for the link! I worked around this by installing hsb2hs separately for now."
---

An old article I occasionally see cited today is [Repeat after me: "Cabal is not a Package Manager"](https://ivanmiljenovic.wordpress.com/2010/03/15/repeat-after-me-cabal-is-not-a-package-manager/). Many of the complaints don't apply to cabal-install 1.24's new [Nix-style local builds](http://blog.ezyang.com/2016/05/announcing-cabal-new-build-nix-style-local-builds/). Let's set the record straight.

# Fact: cabal new-build doesn't handle non-Haskell dependencies

OK, so this is one thing that hasn't changed since Ivan's article. Unlike Stack, `cabal new-build` will not handle downloading and installing GHC for you, and like Stack, it won't download and install system libraries or compiler toolchains: you have to do that yourself. This is definitely a case where you should lean on your system package manager to bootstrap a working installation of Cabal and GHC.

# Fact: The Cabal file format can record non-Haskell pkg-config dependencies

Since 2007, the Cabal file format has a `pkgconfig-depends` field which can be used to specify dependencies on libraries understood by the [pkg-config](https://en.wikipedia.org/wiki/Pkg-config) tool. It won't install the non-Haskell dependency for you, but it can let you know early on if a library is not available.

In fact, cabal-install's dependency solver knows about the `pkgconfig-depends` field, and will pick versions and set flags so that we don't end up with a package with an unsatisfiable <span class="title-ref">pkg-config</span> dependency.

# Fact: cabal new-build 2.0 handles build-tools dependencies

As of writing, this feature is unreleased (if you are impatient, get a copy of HEAD from the [GitHub repository](https://github.com/haskell/cabal) or install `cabal-install-head` from [hvr's PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc)). However, in cabal-install 2.0, `build-tools` dependencies will be transparently built and added to your `PATH`. Thus, if you want to install a package which has `build-tools: happy`, `cabal new-build` will automatically install `happy` and add it to the `PATH` when building this package. These executables are tracked by `new-build` and we will avoid rebuilding the executable if it is already present.

Since build-tools identify executable names, not packages, there is a set of hardcoded build-tools which are treated in this way, coinciding with the set of build-tools that simple Setup scripts know how to use natively. They are hscolour, haddock, happy, alex, hsc2hs, c2hs, cpphs and greencard.

# Fact: cabal new-build can upgrade packages without breaking your database

Suppose you are working on some project which depends on a few dependencies. You decide to upgrade one of your dependencies by relaxing a version constraint in your project configuration. After making this change, all it takes is a `cabal new-build` to rebuild the relevant dependency and start using it. That's it! Even better, if you had an old project using the old dependency, well, it still is working, just as you would hope.

What is actually going on is that `cabal new-build` doesn't do anything like a traditional upgrade. Packages installed to `cabal new-build`'s global store are uniquely identified by a Nix-style identifier which captures *all* of the information that may have affected the build, including the specific versions that were built against. Thus, a package "upgrade" actually is just the installation of a package under a different unique identifier which can coexist with the old one. You will never end up with a broken package database because you typed `new-build`.

There is not presently a mechanism for *removing* packages besides deleting your store (`.cabal/store`), but it is worth noting that deleting your store is a completely safe operation: `cabal new-build` won't decide that it wants to build your package differently if the store doesn't exist; the store is purely a cache and does *not* influence the dependency solving process.

# Fact: Hackage trustees, in addition to package authors, can edit Cabal files for published packages to fix bugs

If a package is uploaded with bad version bounds and a subsequent new release breaks them, a [Hackage Trustee](https://www.haskell.org/wiki/Hackage_trustees) can intervene, making a modification to the Cabal file to update the version bounds in light of the new information. This is a more limited form of intervention than the patches of Linux distributions, but it is similar in nature.

# Fact: If you can, use your system package manager

`cabal new-build` is great, but it's not for everyone. If you just need a working `pandoc` binary on your system and you don't care about having the latest and greatest, you should download and install it via your operating system's package manager. Distro packages are great for binaries; they're less good for libraries, which are often too old for developers (though it is often the easiest way to get a working install of OpenGL). `cabal new-build` is oriented at developers of Haskell packages, who need to build and depend on packages which are not distributed by the operating system.

I hope this post clears up some misconceptions!
