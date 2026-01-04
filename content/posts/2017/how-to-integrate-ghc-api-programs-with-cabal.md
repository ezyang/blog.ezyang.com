---
title: "How to integrate GHC API programs with Cabal"
date: 2017-02-08 19:45:00
slug: how-to-integrate-ghc-api-programs-with-cabal
categories: [Haskell]
comments:
    - id: 21817
      author: metaleap
      date: "2017-02-16 12:06:08"
      content: |
        What might the `args` (of type `[(String, Maybe Phase)]`) passed to a frontend function ever contain? Even when I call my `--frontend` with all the typical command-line arguments that probably `stack build`, *certainly* `stack repl` also passes, this is still empty. The flags contain of course all the passed `-ffrontend-opt` (in reverse order ;)
        
        So for the args.. sure I can easily grab command-line arguments from `System.Environment` but what might ever be in `args`? Of course a frontend aka new major-mode takes over the pipeline *very* early in the process so it could well be that outside of `--make` this doesn't ever get populated, but then do you know why it's even there?
    - id: 21818
      author: metaleap
      date: "2017-02-16 14:45:55"
      content: |
        I'm giving up on this approach, emulating it 1-for-1 somehow I end up running into:
        
        "Bad interface file: D:\dev\hs\haxpile\.stack-work\dist\ca59d0ab\build\Hxp\Via\Frontend.hi
            Something is amiss; requested module  main@main:Hxp.Via.Frontend differs from name found in the interface file haxpile-0.1.0.0@haxpile-0.1.0.0-AeCMZYDjMvPKHwk6tWTlKG:Hxp.Via.Frontend
        
        Could be due to the specific combination of GHC 8.0.2, Stack 1.3.2, and Windows 8.1 64bit or who knows what.. also the plugin-package is a stack.yaml "local package" dependency of the test-package so to speak, who knows how that messes things up compared to using only cabal.
        
        What I CAN do is just run my --frontend without having a "faux repl" invoke it. BUT my frontend func only gets the "flags" (1st arg aka -ffrontend-opt), not the "args" (2nd arg). Ie the crucial `[(String, Maybe Phase)]` list that tells me GHC has a sufficient idea of the current project!  :(  even when I pass everything EXACTLY as "stack repl --system-ghc --with-ghc my-frontend-launcher" would (and does) pass its command-line arguments: that crucial list remains empty.
        
        So back to the drawing board..
    - id: 21819
      author: metaleap
      date: "2017-02-16 15:02:16"
      content: "Okay I GOT it.. to populate args, unlike standard --make or --interactive (which absent specified target files are both equivalent to \"ghc figure out this whole project structure yourself with these dirs\") I need to *ensure* that some target module names are given on the command line, only then does GHC fill it out. In case anyone else reading this ever bumps into .. \"issues\" (or rather, their own naivety ;)"
    - id: 21822
      author: metaleap
      date: "2017-02-17 11:15:16"
      content: |
        Only two more little clarification questions!  =)
        
        "-user-package-db" --- any particular reason for this? I mean as per GHC Users Guide it would be on-by-default, and `stack repl` doesn't pass anything to disable it AFAICT.. did `cabal repl` at your end?
        
        From your last paragraph:
        
        "cabal-install and stack differ slightly in how they go about passing home modules to the invocation of GHCi: cabal-install will call GHC with an argument for every module in the home package"
        
        By cabal-install do you mean `cabal repl` or `cabal new-repl` or both? And what does "call GHC for every module" mean, you mean during `cabal repl` (new-repl?), as this invokes your ghc-replacer it sends along 1-argument-per-module? Asking as I'm working with Stack but I could/would easily support this for Cabal users if this is what you meant..
        
        Cheers!
    - id: 21823
      author: Edward Z. Yang
      date: "2017-02-17 15:11:06"
      content: |
        Hey metaleap,
        
        Re "-use-package-db", I believe new-repl explicitly disables the user package database so I have to turn it back on so we can find the plugin you installed to the user package database.
        
        Re "module arguments", yes, that's exactly what I mean.
    - id: 21824
      author: metaleap
      date: "2017-02-17 16:42:47"
      content: "Thanks  =)"
---

GHC is not just a compiler: it is also a library, which provides a variety of functionality that anyone interested in doing any sort of analysis on Haskell source code. Haddock, hint and ghc-mod are all packages which use the GHC API.

One of the challenges for any program that wants to use the GHC API is integration with Cabal (and, transitively, cabal-install and Stack). The most obvious problem that, when building against packages installed by Cabal, GHC needs to be passed appropriate flags telling it which package databases and actual packages should be used. At this point, people tend to adopt [some hacky strategy](https://groups.google.com/forum/#!topic/haskell-cafe/3ZgLB2khhcI) to get these flags, and hope for the best. For commonly used packages, this strategy will get the job done, but for the rare package that needs something extra--preprocessing, extra GHC flags, building C sources--it is unlikely that it will be handled correctly.

A more reliable way to integrate a GHC API program with Cabal is *inversion of control*: have Cabal call your GHC API program, not the other way around! How are we going to get Cabal/Stack to call our GHC API program? What we will do is replace the GHC executable which passes through all commands to an ordinary GHC, except for `ghc --interactive`, which we will then pass to the GHC API program. Then, we will call `cabal repl`/`stack repl` with our overloaded GHC, and where we would have opened a GHCi prompt, instead our API program gets run.

With this, all of the flags which would have been passed to the invocation of `ghc --interactive` are passed to our GHC API program. How should we go about parsing the flags? The most convenient way to do this is by creating a [frontend plugin](https://downloads.haskell.org/~ghc/master/users-guide/extending_ghc.html#frontend-plugins), which lets you create a new major mode for GHC. By the time your code is called, all flags have already been processed (no need to muck about with `DynFlags`!).

Enough talk, time for some code. First, let's take a look at a simple frontend plugin:

    module Hello (frontendPlugin) where

    import GhcPlugins
    import DriverPhases
    import GhcMonad

    frontendPlugin :: FrontendPlugin
    frontendPlugin = defaultFrontendPlugin {
      frontend = hello
      }

    hello :: [String] -> [(String, Maybe Phase)] -> Ghc ()
    hello flags args = do
        liftIO $ print flags
        liftIO $ print args

This frontend plugin is taken straight from the GHC documentation (but with enough imports to make it compile ;-). It prints out the arguments passed to it.

Next, we need a wrapper program around GHC which will invoke our plugin instead of regular GHC when we are called with the `--interactive` flag. Here is a simple script which works on Unix-like systems:

    import GHC.Paths
    import System.Posix.Process
    import System.Environment

    main = do
      args <- getArgs
      let interactive = "--interactive" `elem` args
          args' = do
            arg <- args
            case arg of
              "--interactive" ->
                ["--frontend", "Hello",
                 "-plugin-package", "hello-plugin"]
              _ -> return arg
      executeFile ghc False (args' ++ if interactive then ["-user-package-db"] else []) Nothing

Give this a Cabal file, and then install it to the user package database with `cabal install` (see the second bullet point below if you want to use a non-standard GHC via the `-w` flag):

    name:                hello-plugin
    version:             0.1.0.0
    license:             BSD3
    author:              Edward Z. Yang
    maintainer:          ezyang@cs.stanford.edu
    build-type:          Simple
    cabal-version:       >=1.10

    library
      exposed-modules:     Hello
      build-depends:       base, ghc >= 8.0
      default-language:    Haskell2010

    executable hello-plugin
      main-is:             HelloWrapper.hs
      build-depends:       base, ghc-paths, unix
      default-language:    Haskell2010

Now, to run your plugin, you can do any of the following:

- `cabal repl -w hello-plugin`
- `cabal new-repl -w hello-plugin`
- `stack repl --system-ghc --with-ghc hello-plugin`

To run the plugin on a specific package, pass the appropriate flags to the `repl` command.

The full code for this example can be retrieved at [ezyang/hello-plugin](https://github.com/ezyang/hello-plugin) on GitHub.

Here are a few miscellaneous tips and tricks:

- To pass extra flags to the plugin, add `--ghc-options=-ffrontend-opt=arg` as necessary (if you like, make another wrapper script around this!)
- If you installed `hello-plugin` with a GHC that is not the one from your PATH, you will need to put the correct `ghc`/`ghc-pkg`/etc executables first in the PATH; Cabal's autodetection will get confused if you just use `-w`. If you are running `cabal`, another way to solve this problem is to pass `--with-ghc-pkg=PATH` to specify where `ghc-pkg` lives (Stack does not support this.)
- You don't have to install the plugin to your user package database, but then the wrapper program needs to be adjusted to be able to find wherever the package does end up being installed. I don't know of a way to get this information without writing a Custom setup script with Cabal; hopefully installation to the user package database is not too onerous for casual users.
- `cabal-install` and `stack` differ slightly in how they go about passing home modules to the invocation of GHCi: `cabal-install` will call GHC with an argument for every module in the home package; Stack will pass a GHCi script of things to load. I'm not sure which is more convenient, but it probably doesn't matter too much if you know already know which module you want to look at (perhaps you got it from a frontend option.)
