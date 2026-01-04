---
title: "Backpack and type classes"
date: 2014-06-18 09:05:02
slug: backpack-and-type-classes
draft: true
categories: [Miscellaneous]
---

[Backpack](http://plv.mpi-sws.org/backpack/) is a new module system for Haskell which was recently presented at POPL'14. The overall goal is to make it possible to specify *interfaces* for Haskell modules, so that it's possible to develop modules to interfaces rather than to implementations (as one is forced to do in today's Haskell). This method of development is referred to by the Backpack authors as *separate modular development* (SMD), since the presence of module interfaces means implementations which depend on one another can be type-checked separately and then linked together later.

At the moment, the formal system for Backpack is worked out in detail, but without support for type classes. The Backpack paper briefly sketches how such an extension might work, but [word on the street](http://www.reddit.com/r/haskell/comments/1id0p7/backpack_retrofitting_haskell_with_interfaces/) is that the need to explicitly name instances in import statements could be a show-stopper as far as actually deploying Backpack is concerned. In this post, I want to summarize *why* this is a problem and *what* some of the proposed solutions are.

# What's the problem?

tl;dr The problem is in two parts: the ability to restrict imports of instances is required for modularity, and the ability to define orphan instances allows users to break abstraction.

First, it's important to know enough about Backpack to understand why one needs the ability to explicitly import instances. So first, I want to give a whirlwind tour of Backpack's features. The most important feature of Backpack, from a user perspective, is the ability to have *module holes*, which are modules we eventually want to depend on, but haven't committed to yet. To take a [classic example from SICP](http://mitpress.mit.edu/sicp/full-text/sicp/book/node42.html):

    package abstract-package where
      Complex :: [
        data Complex
        mkPolar :: Float -> Float -> Complex
        mkRect :: Float -> Float -> Complex
      ]
      Main = [
        import Complex
        main = ... mkPolar 0 1 ...
      ]

Hopefully, the syntax for defining multiple modules at once is self-explanatory. Here, we have written a program (`Main`) which uses complex numbers, but rather than import a particular implementation of these numbers (e.g. [Data.Complex](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Complex.html)), we have instead specified an *signature* for the module. A user who wants to run this program can pick any module which typechecks against this signature and link them together. In Backpack, this is achieved by combining packages together in a “mixin” style (using the `include` operator, which takes the contents of the package and dumps them in the new package):

    package concrete-package where
      include abstract-package
      Complex = [ ... your implementation of complex numbers ... ]

Now, this linking process is a bit more sophisticated than just dumping all of the modules in a directory, as can be evidenced by this package, which type-checks fine under Backpack:

    package shadowing where
      A :: [ a :: Int ]
      Main = [
        import A
        main = print (a * 2)
      ]
      A = [
        a = 2
        main = print a
      ]

If we simply dumped these module definitions into a directory and tried to compile them, GHC would complain that `main` is ambiguous: it might refer to the instance in `Main` or in `A`. But there is nothing wrong with the definition of `Main` or the definition of `A` individually, and since the *sine qua non* of separate modular development is the fact that if the modules type check individually, they should type check together, this is a highly undesirable situation. The solution, of course, is to explicitly import only entities that were named in the signature depended upon:

    module Main where
    import A (a)
    main = print (a * 2)

This requirement is mentioned in the Backpack paper under the *Explicitness of imports and exports*.

------------------------------------------------------------------------

I'm sure you can see the writing on the wall, but let’s be explicit. The need for type class imports Take the example from above, but replace the identifiers with instances.

> package shadowing where  
> A :: \[  
> data A = A
>
> \] Main = \[ import A instance Show A where ... main = print A \] A = \[ data A = A instance Show A where ... \]

Once again, if we simply dumped these modules into files and attempted to compile them, we would get an error: in this case, GHC would complain that there were multiple instances of `Show` present when attempting to solve `print A`. However, by analogy to the previous solution, the instance defined by module A should be hidden, because the signature it was checked against never mentioned anything about defining instances.

------------------------------------------------------------------------

Now, you might be thinking, “Wait a second, these two examples are nothing alike!” For a moment, let’s explore some alternative responses to the conundrum presented above.

**Type classes shouldn’t be part of the signature of a module.** This is a bit of a strawman proposal, but I'm including it because it is essentially how package imports work today. The reason why it is critical to include type classes in the signature, is that, otherwise, there is no way to tell if this module will type-check. Take this example:

    package printer-buggy where
      A :: [ data A = A ]
      Main = [
        import A
        main = print A
      ]

Whether or not this will type-check depends on whether or not A ends up defining a `Show` type-class for its datatype, which is just terrible: we should just say, “We depend on A exporting an instance for Show.”

> package printer where  
> A :: \[  
> data A = A instance Show A
>
> \] Main = \[ import A main = print A \]

(There is a subtler rejoinder here, which is that typeclass resolution should be deferred to the end of linking. I’ll talk about this more later.)

**If I declare an orphan instance, I should be obligated to show that no one else could have declared that instance.** Rather than fix the problem at the use-site (as shadowing does), this proposal seeks to solve the problem at the declaration site: a module which attempts to link against a signature while implementing a type class

make this real is to assume that signature definitions retain their meaning, but now any instance which is omitted is a promise that this instance was *not* implemented.

This proposal is a bit awkward:  
if you ever define a type class in a module, this module can now no longer link against *any* hole whose signature does not mention this type class. I suppose this could be alleviated to some degree by treating orphan instances as a *separate* language feature from ordinary instances: some type classes allow orphans, and some do not, but this runs counter to the “unexpected pluggability” that orphan instances are so useful for.

**The use of type-classes here is incoherent!** This is a common heuristic argument, wherein “local type classes” implies “type class resolution incoherence.” The source of this argument comes from an Oleg and Ken paper [Functional Pearl: Implicit Configurations](http://www.cs.rutgers.edu/~ccshan/prepose/prepose.pdf)

------------------------------------------------------------------------

The shaping and type checking pass specified by a module system subsumes the type-checking

It is hard to understate this fact: the gold standard of separate modular development is that *well typed packages can't link wrong!* (Channeling the similar catch-phrase for type safety, "well typed programs can't go wrong"). Formally, this is stated as a soundness theorem for Backpack:

> If a package definition D has a type `∀ ̅α.(Φ;ℒ)` and elaborates into a parametrized directory expression `λ ̅α.dexp`, then every module in `dexp` is well-typed in the IL, and the identities and types in `Φ` directly match those of `dexp`.

In particular, a bad module system would require us to elaborate to ordinary Haskell modules and then type-check the result of the elaboration.

Type classes are thus a delicate addition to the module system, because there are a number of situations where they can cause otherwise well-typed packages to fail to link:

1.  The type class instances defined by a module should be considered part of its signature. Failure case: a module depends on a type class instance being defined, but none of the modules it links against provide it.
2.  It must be possible to explicitly name instances on import statements. Failure case: a module links against two modules, both of which provide a type class instance for the same type.
3.  It must be possible

------------------------------------------------------------------------

The heuristic argument that I've seen levied by Edward Kmett is, "Well, that looks like local type class instances, and Scala has those, and it worked out terribly."
