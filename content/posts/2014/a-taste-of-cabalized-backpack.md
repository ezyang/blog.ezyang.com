---
title: "A taste of Cabalized Backpack"
date: 2014-08-26 18:01:48
slug: a-taste-of-cabalized-backpack
categories: [Backpack, Haskell]
comments:
    - id: 7633
      author: Joachim Breitner
      date: "2014-08-26 20:20:09"
      content: |
        “Unlike normal dependencies, signature dependencies should be exact: after all, while you might want an upgraded implementation, you don't want the signature to change on you!”
        
        This means that bytestrings-sigs-1.0 will stay around indefinitely, and new versions of bytestring, which are likely to add new code, will then start to accumulate a list of supported signatures (implements: bytestring-sig == 1.0, bytestring-sig == 1.1, bytestring-sig == 1.2 ...), right?
    - id: 7634
      author: Edward Z. Yang
      date: "2014-08-26 20:24:08"
      content: "Right. Furthermore, a backwards incompatible change would cause you to remove unsupported signatures from the list."
    - id: 7638
      author: Tikitu de Jager
      date: "2014-08-27 02:39:41"
      content: "It might be blatantly obvious to more experienced Haskell devs, but I'm wondering what testing would look like for an indefinite library (utilities, say). I would have to instantiate the signature to test, and I might like to test with several different instantiations: do I make a separate named package for each such instantiation? (I'm generally somewhat unclear on best practises for structuring tests; as I say, it might be this is 100% clear to folks who have that a bit more figured out.)"
    - id: 7641
      author: Edward Z. Yang
      date: "2014-08-27 04:57:15"
      content: "Tikitu: Good question. Something that I haven't mentioned is that you can instantiate an indefinite package multiple times in the same package (using syntax which I haven't shown you in this post.) So you can do it all in one package; you can even have your test harness be an indefinite package that you are instantiating multiple times."
    - id: 7644
      author: Tikitu de Jager
      date: "2014-08-27 05:57:56"
      content: "Ah, that sounds exactly like what I would expect to be doing (instantiating the same tests multiple times with different implementations). Good to hear there's (already) a tidy way to do it. Cheers."
    - id: 7651
      author: Jan Stolarek
      date: "2014-08-27 11:27:35"
      content: |
        Why can't we simply deduce "indefinite: True" based on presence of "required-signatures" field or dependency on an indefinite package? Why is this information made explicit when it could be implict? This seems like a redundancy.
        
        &gt; If you are using a signature, there's not much point in also specifying an explicit import list when you import it
        
        What if we want to import a subset of a signature?
    - id: 7652
      author: Edward Z. Yang
      date: "2014-08-27 11:33:01"
      content: |
        > Why can’t we simply deduce “indefinite: True” based on presence of “required-signatures” field or dependency on an indefinite package? Why is this information made explicit when it could be implict? This seems like a redundancy.
        
        We could, and we could remove it. The reason why we have it in the file for now is that a package may be indefinite based on the transitive closure of its dependencies (i.e. it's not a local property). So if you depend on foo which depends on bar which depends on baz which depends on bling, and *that* package is the one with a hole, all of them are indefinite. The indefinite marker is just a nice documentary redundancy. 
        
        > What if we want to import a subset of a signature?
        
        Sure, you could do that. Or, you could write down a subsetted signature...
    - id: 7686
      author: Robert Harper
      date: "2014-08-29 13:54:57"
      content: "I haven't studied all of the details here, but I have to ask at what point are you just going to reinvent the ML modules system, which was invented almost 30 years ago, to solve the same problems you are describing?"
    - id: 7726
      author: Scott Kilpatrick
      date: "2014-09-01 11:40:21"
      content: |
        Hi Bob (and anyone else with the same very good question),
        
        Obviously, yes, functors in ML capture a lot of the new functionality of Backpack in Haskell that Edward describes above. For example, in ML, the "utilities" package above would be a functor parameterized by a signature for the ByteString module. So why did Haskell designers never deem such functionality important enough to add to the language? I have no idea. But now there's some will to finally do so, albeit at the level of "packages" rather than "modules." That's where Backpack comes in.
        
        We had to come up with a modular design that focuses on the particulars of package-scale modularity and that remains fairly backwards compatible with Haskell today. Both of these constraints led us to Backpack's mixin design rather than ML's functor design. There's some discussion of this in the paper [1], but for posterity, three main reasons come to mind:
        
        1) Packages are typically defined as deeply nested hierarchies of dependency. Expressed as functors, everything would require explicit parameterization -- not only the packages themselves but also the signatures corresponding to their (direct) dependencies. (If these signatures were not themselves parameterized, they would then require sharing constraints to ensure coherence of those signatures' own dependencies.) Mixins obviate all the explicit parameterization and sharing constraints, instead relying on (module) names to make everything link up the way you'd expect. And it does this in a way that looks exactly like Haskell modules already do today.
        
        2) Packages often involve modules with cyclic imports, i.e., recursive modules. Some ML languages do handle recursive modules and solve the dreaded double vision problem in doing so. But no ML language supports separate development/typechecking of recursive modules; in the package setting this means recursive modules across packages. Although I don't have a particularly good use case of inter-package recursion, the mixin design supports both intra-package and inter-package recursion in a unified, straightforward manner. (In fact, it's just the generalization of the way recursive modules are already defined in GHC Haskell.)
        
        3) The ML module system is already insufficient for structuring separate "compilation units" (think "packages") in ML languages. That's why these languages typically have additional systems for expressing compilation units. For OCaml there's the ocamlc tool that essentially treats the file system as a mixin layer for structuring modules with holes. For Standard ML there's the SMLSC system [2] that defines a layer of compilation units on top of modules, which itself has a mixin-like behavior for easily composing units that have the same dependencies on other units (holes). So if we used the ML module system for Haskell -- whatever that would actually mean! -- then we'd still need that additional layer on top. Backpack kills two birds with one stone by providing some of the ML module system functionality (e.g. code depending on interfaces, instantiation) and some of the separate compilation units functionality (e.g. holes, easy linking/composition).
        
        The first point claims that Backpack expresses package-level modularity more naturally than ML would (and in accordance to all the existing Haskell code), while the latter two points claim that Backpack does things that the ML module system simply doesn't do (without an additional system on top).
        
        It's not all roses though. I'd never argue that Backpack *subsumes* the ML module system. It's simply not trying to do everything the ML module system does: it focuses on the structuring of large, separately-developed modular programs rather than the program-level enforcement of abstraction boundaries with abstract types. In my personal opinion, these things are still very important and I would never say that Haskell doesn't need them.
        
        Backpack is, regardless, a step in the right direction, for Haskell and for package management in general -- toward typed interfaces of modular boundaries and toward structural matching of implementations against those interfaces. And, thanks to Edward, Simon and perhaps eventually others, it's magically (to me, anyway) appearing in a widely-used existing system rather than just a formal model in a research paper. Anyone who glances at my Twitter feed will see that I'm all for radical transformations of the way things are done ;-) but, after recognizing the way things *should* be done, we still need a pathway to get us there.
        
        sk
        
        [1] Kilpatrick, Dreyer, Peyton Jones, and Marlow. Backpack: retrofitting Haskell with interfaces. POPL 2014. http://plv.mpi-sws.org/backpack/backpack-paper.pdf
        [2] Swasey, Murphy, Crary, and Harper. A separate compilation extension to Standard ML. ML 2006. http://www.cs.cmu.edu/~tom7/papers/smlsc2-ml06.pdf
    - id: 7839
      author: Stefan Wehr
      date: "2014-09-06 15:16:38"
      content: |
        Is there a reason why you match package arguments implicitly by name instead of doing so explicitly?
        
        So instead of writing
        
        <pre>name: lazy-utilities
        build-depends:
          utilities,
          bytestring (Data.ByteString.Lazy as Data.ByteString)
        reexported-modules: Utils as Utils.Lazy</pre>
        
        why not write
        
        <pre>name: lazy-utilities
        build-depends:
          utilities(Data.ByteSring ~ bytestring::Data.ByteString.Lazy)
        reexported-modules: Utils as Utils.Lazy</pre>
        
        which means: "fill the hole Data.ByteString of the utilities package with the module Data.ByteString.Lazy from the bytestring package".
        
        A related question:
        
        What happens if you have two packages P1 and P2 in your build-depends, which both have a hole H, and now you want to fill the hole H of P1 with module M1 from package P3 and the hole H of P2 with module M2 from package P4?
    - id: 7840
      author: Edward Z. Yang
      date: "2014-09-06 15:34:05"
      content: |
        Stefan: One of the primary reasons for the "mix-in" design, as opposed to ML style functor application, is to avoid the propagation of coherence constraints stating when modules are to be considered the same. Also, from a theoretical perspective, mix-ins support recursive modules much better than explicit instantiation, although recursive packages are not high on our priority list yet! In any case, the "explicit application" syntax is quite doable, desugaring into the creation of a fresh module name at which the linkage occurs.
        
        Here's the solution for your second problem:
        
        <pre>
        build-depends:
          P1 (H as M1),
          P2 (H as M2),
          P3 (M1),
          P4 (M2)
        </pre>
    - id: 7841
      author: Stefan Wehr
      date: "2014-09-06 15:41:38"
      content: |
        Edward, thanks for clarification.
        
        I was actually only reasoning about syntax, not about mixins / functors in general. To me, the functor-style application syntax looks a bit clearer, because it's more explicit what's filled with what.
        
        But in the end it's only syntax...
    - id: 7842
      author: Edward Z. Yang
      date: "2014-09-06 15:49:30"
      content: "It's possible that we want to adopt some nicer surface syntax. Reuse of the renaming syntax is nice because of parsimony, but if parsimony were the only thing we looked for in programming languages we'd still be programming the lambda calculus..."
    - id: 21210
      author: Janus
      date: "2016-09-05 09:46:10"
      content: "Is all of this still current?"
    - id: 21213
      author: Edward Z. Yang
      date: "2016-09-05 18:11:22"
      content: "Janus: I just went through the article to update it to be up-to-date. :)"
---

**Update.** Want to know more about Backpack? Read the [specification](https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst)

So perhaps you've [bought into modules and modularity](http://blog.ezyang.com/2014/08/whats-a-module-system-good-for-anyway/) and want to get to using Backpack straightaway. How can you do it? In this blog post, I want to give a tutorial-style taste of how to program Cabal in the Backpack style. These examples are executable, but you'll have to build custom versions of [GHC](https://github.com/ezyang/ghc/tree/ghc-backpack) and [Cabal](https://github.com/ezyang/cabal/tree/backpack) to build them. Comments and suggestions would be much appreciated; while the design here is theoretically well-founded, for obvious reasons, we don't have much on-the-ground programmer feedback yet.

------------------------------------------------------------------------

# A simple package in today's Cabal

To start, let's briefly review how Haskell modules and Cabal packages work today. Our running example will be the `bytestring` package, although I'll inline, simplify and omit definitions to enhance clarity.

Let's suppose that you are writing a library, and you want to use efficient, packed strings for some binary processing you are doing. Fortunately for you, the venerable Don Stewart has already written a `bytestring` package which implements this functionality for you. This package consists of a few modules: an implementation of strict `ByteStrings`...

    module Data.ByteString(ByteString, empty, singleton, ...) where
      data ByteString = PS !(ForeignPtr Word8) !Int !Int
      empty :: ByteString
      empty = PS nullForeignPtr 0 0
      -- ...

...and an implementation of lazy `ByteStrings`:

    module Data.ByteString.Lazy(ByteString, empty, singleton, ...) where
      data ByteString = Empty | Chunk !S.ByteString ByteString
      empty :: ByteString
      empty = Empty
      -- ...

These modules are packaged up into a package which is specified using a Cabal file:

    name: bytestring
    version: 0.10.4.0
    library
      build-depends: base >= 4.2 && < 5, ghc-prim, deepseq
      exposed-modules: Data.ByteString, Data.ByteString.Lazy, ...
      other-modules: ...

We can then make a simple module and package which depends on the `bytestring` package:

    module Utils where
      import Data.ByteString.Lazy as B
      blank :: IO ()
      blank = B.putStr B.empty

    name: utilities
    version: 0.1
    library
      build-depends: base, bytestring >= 0.10
      exposed-modules: Utils

It's worth noting a few things about this completely standard module setup:

1.  It's not possible to switch `Utils` from using lazy `ByteStrings` to strict `ByteStrings` without literally editing the `Utils` module. And even if you do that, you can't have `Utils` depending on strict `ByteString`, and `Utils` depending on lazy `ByteString`, in the same program, without copying the entire module text. (This is not too surprising, since the code *really is* different.)
2.  Nevertheless, there is some amount of indirection here: while `Utils` includes a specific `ByteString` module, it is unspecified *which* version of `ByteString` it will be. If (hypothetically) the `bytestring` library released a new version where lazy byte-strings were actually strict, the functionality of `Utils` would change accordingly when the user re-ran dependency resolution.
3.  I used a qualified import to refer to identifiers in `Data.ByteString.Lazy`. This is a pretty common pattern when developing Haskell code: we think of `B` as an *alias* to the actual model. Textually, this is also helpful, because it means I only have to edit the import statement to change which `ByteString` I refer to.

------------------------------------------------------------------------

# Generalizing Utils with a signature

To generalize `Utils` with some Backpack magic, we need to create a *signature* for `ByteString`, which specifies what the interface of the module providing `ByteStrings` is. Here one such signature, which is placed in the file `Data/ByteString.hsig` inside the *utilities* package:

    signature Data.ByteString where
      import Data.Word
      data ByteString
      instance Eq ByteString
      empty :: ByteString
      singleton :: Word8 -> ByteString
      putStr :: ByteString -> IO ()

The format of a signature is essentially the same of that of an `hs-boot` file: we have normal Haskell declarations, but omitting the actual implementations of values.

The `utilities` package now needs a new field to record signatures:

    name: utilities
    library
      build-depends: base
      exposed-modules: Utils
      signatures: Data.ByteString

Notice that there have been three changes: (1) We've removed the direct dependency on the `bytestring` package, and (2) we have a new field **signatures** which simply lists the names of the signature files (also known as **holes**) that we need filled in.

How do we actually use the utilities package, then? Let's suppose our goal is to produce a new module, `Utils.Strict`, which is `Utils` but using strict `ByteStrings` (which is exported by the bytestring package under the module name `Data.ByteString`). To do this, we'll need to create a new package:

    name: strict-utilities
    library
      build-depends: utilities, bytestring
      reexported-modules: Utils as Utils.Strict

That's it! `strict-utilities` exports a single module `Utils.Strict` which is `utilities` using `Data.ByteString` from `bytestring` (which is the strict implementation). This is called a *mix-in*: in the same dependency list, we simply mix together:

- `utilities`, which *requires* a module named `Data.ByteString`, and
- `bytestring`, which *supplies* a module named `Data.ByteString`.

Cabal automatically figures out that how to instantiate the utilities package by matching together *module names*. Specifically, the two packages above are connected through the module name `Data.ByteString`. This makes for a very convenient (and as it turns out, expressive) mode of package instantiation. By the way, **reexported-modules** is a new (orthogonal) feature which lets us reexport a module from the current package or a dependency to the outside world under a different name. The modules that are exported by the package are the exposed-modules and the reexported-modules. The reason we distinguish them is to make clear which modules have source code in the package (exposed-modules).

Unusually, `strict-utilities` is a package that contains no code! Its sole purpose is to mix existing packages.

Now, you might be wondering: how do we instantiate utilities with the lazy `ByteString` implementation? That implementation was put in `Data.ByteString.Lazy`, so the names don't match up. In this case, we can use another new feature, module thinning and renaming:

    name: lazy-utilities
    library
      build-depends: utilities, bytestring
      backpack-includes:
        bytestring (Data.ByteString.Lazy as Data.ByteString)
      reexported-modules: Utils as Utils.Lazy

The new `backpack-includes` field says that only the `Data.ByteString.Lazy` module should brought into scope, under the name `Data.ByteString`. This is sufficient to mix in link `utilities` with the lazy implementation of `ByteString`.

An interesting duality is that you can do the renaming the other way:

    name: lazy-utilities
    library
      build-depends:
        utilities (Utils, Data.ByteString as Data.ByteString.Lazy),
        bytestring

Instead of renaming the implementation, I renamed the hole! It's equivalent: the thing that matters it that the signature and implementation need to be mixed under the *same* name in order for linking (the instantiation of the signature with the implementation) to occur.

There are a few things to note about signature usage:

1.  If you are using a signature, there's not much point in also specifying an explicit import list when you import it: you are guaranteed to *only* see types and definitions that are in the signature (modulo type classes... a topic for another day). Signature files act like a type-safe import list which you can share across modules.

2.  A signature can, and indeed often must, import other modules. In the type signature for `singleton` in `Data/ByteString.hsig`, we needed to refer to a type `Word8`, so we must bring it into scope by importing `Data.Word`.

    Now, when we compile the signature in the `utilities` package, we need to know where `Data.Word` came from. It could have come from another signature, but in this case, it's provided by the *definite* package base: it's a proper concrete module with an implementation! Signatures can depend on implementations: since we can only refer to types from those modules, we are saying, in effect: any implementation of the `singleton` function and any representation of the `ByteString` type is acceptable, but regarding `Word8` you must use the *specific* type from `Data.Word` in `prelude`.

3.  What happens if, independently of my packages `strict-utilities`, someone else also instantiatiates `utilities` with `Data.ByteString`? Backpack is clever enough to reuse the instantiation of `utilities`: this property is called **applicativity** of the module system. The specific rule that we use to decide if the instantiation is the same is to look at how all of the holes needed by a *package* are instantiated, and if they are instantiated with precisely the same modules, the instantiated packages are considered type equal. So there is no need to actually create `strict-utilities` or `lazy-utilities`: you can just instantiate `utilities` on the fly.

**Mini-quiz:** What does this package do? :

    name: quiz-utilities
    library
      build-depends:
        utilities (Utils, Data.ByteString as B),
        bytestring (Data.ByteString.Lazy as B)

------------------------------------------------------------------------

# Sharing signatures

It's all very nice to be able to explicitly write a signature for `Data.ByteString` in my package, but this could get old if I have to do this for every single package I depend on. It would be much nicer if I could just put all my signatures in a package and include that when I want to share it. I want all of the Hackage mechanisms to apply to my signatures as well as my normal packages (e.g. versioning). Well, you can!

The author of `bytestring` can write a `bytestring-sig` package which contains only signatures:

    name: bytestring-sig
    version: 1.0
    library
      build-depends: base
      signatures: Data.ByteString

Now, `utilities` can include this package to indicate its dependence on the signature:

    name: utilities
    library
      build-depends: base, bytestring-sig-1.0
      exposed-modules: Utils

Unlike normal dependencies, signature dependencies should be *exact*: after all, while you might want an upgraded implementation, you don't want the signature to change on you!

We can summarize all of the fields as follows:

1.  **exposed-modules** says that there is a public module defined *in this package*
2.  **other-modules** says that there is a private module defined in this package
3.  **signatures** says that there is a public signature defined in this package (there are no private signatures; they are always public, because a signature *always* must be implemented)
4.  **reexported-modules** says that there is a public module or signature defined in a dependency.

In this list, public means that it is available to clients. Notice the first four fields list all of the source code in this package. Here is a simple example of a client:

    name: utilities-extras
    library
      build-depends: utilities
      exposed-modules: Utils.Extra

------------------------------------------------------------------------

# Summary

We've covered a lot of ground, but when it comes down to it, Backpack really comes together because of set of orthogonal features which interact in a good way:

1.  **Module signatures**: the *heart* of a module system, giving us the ability to write *indefinite packages* and mix together implementations,
2.  **Module reexports**: the ability to take locally available modules and reexport them under a different name, and
3.  **Module thinning and renaming** : the ability to selectively make available modules from a dependency.

To compile a Backpack package, we first run the traditional version dependency solving, getting exact versions for all packages involved, and then we calculate how to link the packages together. That's it! In a future blog post, I plan to more comprehensively describe the semantics of these new features, especially module signatures, which can be subtle at times.
