---
title: "ghc-shake: Reimplementing ghc -&#8203;-make"
date: 2016-01-07 12:59:32
slug: ghc-shake-reimplementing-ghc-make
categories: [GHC, Haskell, Toolbox]
comments:
    - id: 20354
      author: Mikhail Glushenkov
      date: "2016-01-08 10:04:00"
      content: |
        Could the Shake integration make it easier to build multi-language projects (for example, a Haskell program that also includes a substantial amount of C++ code)? This is an area that Cabal is pretty bad at.
        
        Also, you haven't written anything about how the parallel performance of 'ghc-shake' compares to 'ghc --make -j'.
    - id: 20360
      author: Edward Z. Yang
      date: "2016-01-08 17:04:14"
      content: |
        It should, assuming the rest of your build system is written in Shake. But I haven't actually lib-ified our rules yet, so you won't be able to do this with ghc-shake just yet. If someone has a project they want to convert this way, I'd definitely be interesting in working with them to make this work.
        
        In principle, the parallel performance of ghc-shake should be the same as ghc --make -j (Shake's parallel performance is known to be reasonable), but I haven't done any serious testing on this front. We'll run into all of the same scaling problems as plain ghc, of course.
    - id: 20366
      author: Mikhail Glushenkov
      date: "2016-01-09 10:49:46"
      content: "IIRC Evan Laforge had a mixed Haskell/C++ project with a complicated dependency structure (.cpp files depending on .hs files and vice versa, plus liberal use of hsc2hs)."
    - id: 20536
      author: Anonymous
      date: "2016-03-06 14:28:20"
      content: |
        &gt; Unfortunately, Shake is a bit too big of a dependency to actually have GHC depend on
        
        This makes me sad. It's a sad truth that every major self-hosting compiler I know of predates the creation of a good package manager for the language in question, and thus is extremely conservative about taking on dependencies, even when the rest of the language ecosystem aggressively promotes code reuse.
    - id: 20964
      author: Paolo G. Giarrusso
      date: "2016-07-17 07:08:24"
      content: |
        &gt; It’s a sad truth that every major self-hosting compiler I know of predates the creation of a good package manager for the language in question, and thus is extremely conservative about taking on dependencies, even when the rest of the language ecosystem aggressively promotes code reuse.
        
        Interesting point, but I guess there are other reasons, like ease and speed of bootstrapping. Not sure whether they're valid reason though, if most people install binary distributions of the compiler.
        
        The big issue with GHC is that the compiler dependencies are exposed in the global package database, and having other versions of those with cabal-install is dangerous. That's a fragile truce.
        
        Also, let me offer as a counterexample *both* Scala compilers, that is Scalac and dotc from dotty. Nowadays they're both built using SBT, hence adding any library from any Maven repo takes one or two lines of code. But even there, getting to this point was an uphill battle—and historically Scalac was built not with Maven (which can download dependencies as easily) but with Ant (and there you can only ship binary libraries with your source distribution or have a script pulling them).
    - id: 20966
      author: Edward Z. Yang
      date: "2016-07-17 13:55:00"
      content: |
        > The big issue with GHC is that the compiler dependencies are exposed in the global package database, and having other versions of those with cabal-install is dangerous. That’s a fragile truce.
        
        That's not quite the issue here. The issue is more that any of GHC's dependencies are fixed (unless you are hvr and trying to make it sure Cabal can rebuild GHC), so anyone who depends on GHC as a library must also use those same versions.
        
        I see that you can use Scala as a compiler library. So I wonder what happens if Scala depends on a library, but the user wants to use a different version. https://stackoverflow.com/questions/34411819/how-to-use-multiple-versions-of-a-library-in-scala suggests that SBT has some sort of conflict manager, but I don't really know how it works.
    - id: 21062
      author: Paolo G. Giarrusso
      date: "2016-08-02 14:52:43"
      content: |
        Just seen your answer (can I get notifications?). Thanks for the correction!
        
        &gt; I see that you can use Scala as a compiler library. So I wonder what happens if Scala depends on a library, but the user wants to use a different version.
        
        Good question, and we do have a similar potential problem I expect. Kind of.
        There are two kinds of answers:
        
        - in principle, this scenario can be supported if the library does not infect the compiler API: the JVM allows loading two identically-named classes in different classloaders and keeping the types incompatible. This is true even in practice if you use OSGi, but nobody does. Info e.g. here should you want: http://stackoverflow.com/q/6105124/53974.
        That's a bit like linking pkg-1 and pkg-2 in Haskell and keeping them separate... If I understand your earlier post (http://blog.ezyang.com/2015/09/is-no-reinstall-cabal-coming-to-ghc-8/), that's supported since GHC 7.10 at least in the REPL. And I'd argue Backpack should support it as well if the dependency is thinned away? Don't know the current status.
        
        - in practice using OSGI seems hard and nobody does it (Java 9 with Project Jigsaw might help); I don't know if the Scala compiler does but I assume not (though it is mentioned in the build script). I've often seeing SBT just pick the latest version, as discussed in your link, and giving a warning, but never investigated. If the picked library isn't binary compatible with the original one this will produce failures at runtime (JVM linking isn't eager). I suppose I've never developed a project big enough that this ever happened to my memory. And really big ones (like Eclipse) do use OSGi.
---

`ghc --make` is a useful mode in GHC which automatically determines what modules need to be compiled and compiles them for you. Not only is it a convenient way of building Haskell projects, its single-threaded performance is good too, by reusing the work of reading and deserializing external interface files. However, the are a number of downsides to `ghc --make`:

1.  Projects with large module graphs have a hefty latency before recompilation begins. This is because `ghc --make` (re)computes the full module graph, parsing each source file's header, before actually doing any work. If you have a preprocessor, [it's even worse](https://ghc.haskell.org/trac/ghc/ticket/1290).
2.  It's a monolithic build system, which makes it hard to integrate with other build systems if you need something more fancy than what GHC knows how to do. (For example, GHC's painstakingly crafted build system knows how to build in parallel across package boundaries, which Cabal has no idea how to do.)
3.  It doesn't give you any insight into the performance of your build, e.g. what modules take a long time to build or what the big "blocker" modules are.

[ghc-shake](https://github.com/ezyang/ghc-shake) is a reimplementation of `ghc --make` using the [Shake build system](http://shakebuild.com/). It is a drop-in replacement for `ghc`. ghc-shake sports the following features:

1.  Greatly reduced latency to recompile. This is because Shake does not recompute the module graph by parsing the header of every file; it reuses cached information and only re-parses source files which have changed.
2.  If a file is rebuilt (and its timestamp updated) but the build output has not changed, we don't bother recompiling anything that depended on it. This is in contrast to `ghc --make`, which has to run the recompilation check on every downstream module before concluding there is nothing to do. In fact, ghc-shake never runs the recompilation test, because we reimplemented this dependency structure natively in Shake.
3.  Using `-ffrontend-opt=--profile`, you can get nice profiling information about your build, including how long it took to build each module, and how expensive it is to change one of the modules.
4.  It's as fast as `ghc --make` on single-threaded builds. Compare this to [ghc-make](https://github.com/ndmitchell/ghc-make), another build system which uses Shake to build Haskell. ghc-make does not use the GHC API and must use the (slow) `ghc -M` to get initial dependency information about your project.
5.  It's accurate. It handles many edge-cases (like `-dynamic-too`) correctly, and because it is written using the GHC API, it can in principle be feature-for-feature compatible with `ghc --make`. (It's not currently, but only because I haven't implemented them yet.)

There are some downsides:

1.  Shake build systems require a `.shake` directory to actual store metadata about the build. This is in contrast to `ghc --make`, which operates entirely off of the timestamps of build products in your directory.
2.  Because it is directly implemented with the GHC API, it only works with a specific version of GHC (the upcoming GHC 8.0 release).
3.  It needs a patched version of the Shake library, because we have custom rule for building modules based off of Shake's (not exported) file representation. I've [reported it here](https://github.com/ndmitchell/shake/issues/388).
4.  There are still some missing features and bugs. The ones I've run into are that (1) we [forget to relink](https://ghc.haskell.org/trac/ghc/ticket/10161) in some cases, and (2) it doesn't work for [building profiled code](https://ghc.haskell.org/trac/ghc/ticket/11293).

If you want to use `ghc-shake` today (not for the faint of heart), try `git clone https://github.com/ezyang/ghc-shake` and follow the instructions in the `README`. But even if you're not interested in using it, I think the code of `ghc-shake` has some good lessons for anyone who wants to write a build system involving Haskell code. One of the most important architectural decisions was to make the rules in `ghc-shake` not be organized around output files (e.g. `dist/build/Data/Foo.hi`, as in `make`) but around Haskell modules (e.g. `Data.Foo`). Semantic build systems work a lot better than forcing everything into a "file abstraction" (although Shake doesn't quite support this mode of use as well as I would like.) There were some other interesting lessons... but that should be the subject for another blog post!

Where is this project headed? There are a few things I'm considering doing in the not-so-immediate future:

1.  To support multiple GHC versions, we should factor out the GHC specific code into a separate executable and communicate over IPC (hat tip Duncan Coutts). This would also allow us to support separate-process parallel GHC builds which still get to reuse read interface files. In any case, `ghc-shake` could serve as the blueprint for what information GHC needs to make more easily accessible to build systems.
2.  We could consider moving this code back to GHC. Unfortunately, Shake is a bit too big of a dependency to actually have GHC depend on, but it may be possible to design some abstract interface (hello Backpack!) which represents a Shake-style build system, and then have GHC ship with a simple implementation for `--make` (but let users swap it out for Shake if they like.)
3.  We can extend this code beyond `ghc --make` to understand how to build entire Cabal projects (or bigger), ala [ToolCabal](https://github.com/TiborIntelSoft/ToolCabal), a reimplementation of Cabal using Shake. This would let us capture patterns like GHC's build system, which can build modules from all the boot packages in parallel (without waiting for the package to completely finish building first.

P.S. ghc-shake is not to be confused with [shaking-up-ghc](https://github.com/snowleopard/shaking-up-ghc), which is a project to replace GHC's Makefile-based build system with a Shake based build system.
