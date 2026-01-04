---
title: "Type classes: confluence, coherence and global uniqueness"
date: 2014-07-11 12:07:53
slug: type-classes-confluence-coherence-global-uniqueness
categories: [Haskell]
comments:
    - id: 7116
      author: Mikhail Glushenkov
      date: "2014-07-12 23:47:08"
      content: |
        Thanks, a very informative post. But note that a variant of this problem crops up even when global uniqueness of instances holds: 
        
        https://gist.github.com/rwbarton/dd8e51dce2a262d17a80
        
        Perhaps we should be talking about "global instance coherence"?
    - id: 7119
      author: Aaron Levin
      date: "2014-07-13 06:34:49"
      content: |
        I've even wondering about this for a while, coming to Haskell from Scala. Thanks!
        
        Can expand on what you meant by this: "Furthermore, the types of ins and ins' discharge type class resolution"?
        
        I am very surprised GHC didn't throw an error in that example!
        
        Thanks!
    - id: 7121
      author: Edward Z. Yang
      date: "2014-07-13 07:35:03"
      content: "Aaron: Because the type of ins does not have a type class constraint in it, it means that any use of type classes was resolved at the definition site, and that the user of the library did not need to do any type class resolution."
    - id: 7120
      author: Aaron Levin
      date: "2014-07-13 06:35:27"
      content: "(Sorry about typos, on mobile)"
    - id: 7154
      author: Dominique Devriese
      date: "2014-07-15 04:21:12"
      content: "@Mikhail Glushenkov: I don't understand why you say that global uniqueness holds for your example.  It has two different instances for constraint Ord (T U MB MC), no?"
    - id: 7176
      author: Mikhail Glushenkov
      date: "2014-07-17 16:48:47"
      content: |
        @Dominique Devriese 
        
        Hmm, now after looking again at Edward's post, I think you're right. The point of that example is that implementing a global instance uniqueness check in GHC is not as easy as just emitting a symbol for each instance declaration in the program and then relying on linker to produce an error.
    - id: 12417
      author: Steven Shaw
      date: "2015-01-28 09:16:20"
      content: |
        When I compile and run your second example, it doesn't (seem to) terminate! 
        GHC 7.8.3 on Mac 10.10.1
        
        Is this the last word on global uniqueness of instances in GHC? I had perhaps misunderstood that Haskell guaranteed it.
    - id: 12420
      author: Edward Z. Yang
      date: "2015-01-28 17:09:01"
      content: |
        Steven: So it goes! Perhaps the set implementation changed so that when the invariant is violated it loops. In any case it's just another way things break when you have inconsistent instances. 
        
        I wouldn't call this the last word on global uniqueness; if we could have GHC check the instance easily we would, but we don't because this situation is fairly rare and fixing this properly would penalize normal compilation times.
    - id: 14475
      author: Anonymous
      date: "2015-06-01 19:17:50"
      content: |
        I agree that this kind of global instances is nonmodular and has other problem.
        
        In the example given, my opinion is that Haskell ought to allow to define a datatype (or newtype) to depend on a constant, therefore (Set x) depends on (Ord x), and specifying the type (Set U) in D.hs is an error because it is ambiguous which type you mean; (Set U) is going to be one type in B.hs and another type in C.hs. Even if you do specify, the ins and ins' functions then aren't composable because it is a type mismatch.
        
        Another problem with these global instances is that some packages define instances that are wrong (of: Alternative IO, MonadPlus IO, MonadTrans Free, etc) and then you cannot use the correct one!!!
    - id: 22085
      author: AntC
      date: "2017-08-18 23:11:53"
      content: |
        Thanks Edward, sorry to be late to the party but somebody's just linked to this piece. I was indeed aware that Haskell allows you to declare overlapping instances (even without the `Overlapping/Incoherent` flags or pragmas), but doesn't warn you unless you try to use a function at the overlap. (Contrast that dear old Hugs used to validate instances 'eagerly' at the instance decl. And Type Family instances also validate eagerly, as a matter of type safety.)
        
        There's a sentence that's ambiguous (last of the second section).
        
        &gt; Languages with local type class instances such as Scala generally do not have this property, and this assumption is a very convenient one when building abstractions like sets.
        
        Does "this assumption" mean the assumption of local instances? I think not: I think you mean the assumption of global uniqueness of instances is very convenient.
        
        Not just convenient, but essential for the laws to work: `Eq` is a superclass of `Ord`. So having `Ord a =&gt; Set a` is supposed to deliver uniqueness of elements within the set. What we don't want by using both `ins'` and `ins` to construct a set is a duplicate `X`. That would be a bag, not a set.
        
        I presume your demonstration was to show a repeated `X` is a Bad Thing/a _reductio ad absurdum_. (There's somebody seems to think it was the desired result.)
    - id: 22086
      author: Edward Z. Yang
      date: "2017-08-19 22:12:26"
      content: "I think I am in agreement with you, but I have reworded the sentence in question to remove the ambiguity (as I see it!)"
---

Today, I'd like to talk about some of the core design principles behind type classes, a wildly successful feature in Haskell. The discussion here is closely motivated by the work we are doing at MSRC to support type classes in Backpack. While I was doing background reading, I was flummoxed to discover widespread misuse of the terms "confluence" and "coherence" with respect to type classes. So in this blog post, I want to settle the distinction, and propose a new term, "global uniqueness of instances" for the property which people have been colloquially referred to as confluence and coherence.

------------------------------------------------------------------------

Let's start with the definitions of the two terms. Confluence is a property that comes from term-rewriting: a set of instances is **confluent** if, no matter what order constraint solving is performed, GHC will terminate with a canonical set of constraints that must be satisfied for any given use of a type class. In other words, confluence says that we won't conclude that a program doesn't type check just because we swapped in a different constraint solving algorithm.

Confluence's closely related twin is **coherence** (defined in the paper "Type classes: exploring the design space"). This property states that every different valid typing derivation of a program leads to a resulting program that has the same dynamic semantics. Why could differing typing derivations result in different dynamic semantics? The answer is that context reduction, which picks out type class instances, elaborates into concrete choices of dictionaries in the generated code. Confluence is a prerequisite for coherence, since one can hardly talk about the dynamic semantics of a program that doesn't type check.

So, what is it that people often refer to when they compare Scala type classes to Haskell type classes? I am going to refer to this as **global uniqueness of instances**, defining to say: in a fully compiled program, for any type, there is at most one instance resolution for a given type class. Languages with local type class instances such as Scala generally do not have this property, but in Haskell, we find this property is a very convenient one when building abstractions like sets.

------------------------------------------------------------------------

So, what properties does GHC enforce, in practice? In the absence of any type system extensions, GHC's employs a set of rules to ensure that type class resolution is confluent and coherent. Intuitively, it achieves this by having a very simple constraint solving algorithm (generate wanted constraints and solve wanted constraints) and then requiring the set of instances to be *nonoverlapping*, ensuring there is only ever one way to solve a wanted constraint. Overlap is a more stringent restriction than either confluence or coherence, and via the `OverlappingInstances` and `IncoherentInstances`, GHC allows a user to relax this restriction "if they know what they're doing."

Surprisingly, however, GHC does *not* enforce global uniqueness of instances. Imported instances are not checked for overlap until we attempt to use them for instance resolution. Consider the following program:

    -- T.hs
    data T = T
    -- A.hs
    import T
    instance Eq T where
    -- B.hs
    import T
    instance Eq T where
    -- C.hs
    import A
    import B

When compiled with one-shot compilation, `C` will not report overlapping instances unless we actually attempt to use the `Eq` instance in C. This is [by design](https://ghc.haskell.org/trac/ghc/ticket/2356): ensuring that there are no overlapping instances eagerly requires eagerly reading all the interface files a module may depend on.

------------------------------------------------------------------------

We might summarize these three properties in the following manner. Culturally, the Haskell community expects *global uniqueness of instances* to hold: the implicit global database of instances should be confluent and coherent. GHC, however, does not enforce uniqueness of instances: instead, it merely guarantees that the *subset* of the instance database it uses when it compiles any given module is confluent and coherent. GHC does do some tests when an instance is declared to see if it would result in overlap with visible instances, but the check is [by no means perfect](https://ghc.haskell.org/trac/ghc/ticket/9288); truly, *type-class constraint resolution* has the final word. One mitigating factor is that in the absence of *orphan instances*, GHC is guaranteed to eagerly notice when the instance database has overlap (assuming that the instance declaration checks actually worked...)

Clearly, the fact that GHC's lazy behavior is surprising to most Haskellers means that the lazy check is mostly good enough: a user is likely to discover overlapping instances one way or another. However, it is relatively simple to construct example programs which violate global uniqueness of instances in an observable way:

    -- A.hs
    module A where
    data U = X | Y deriving (Eq, Show)

    -- B.hs
    module B where
    import Data.Set
    import A

    instance Ord U where
    compare X X = EQ
    compare X Y = LT
    compare Y X = GT
    compare Y Y = EQ

    ins :: U -> Set U -> Set U
    ins = insert

    -- C.hs
    module C where
    import Data.Set
    import A

    instance Ord U where
    compare X X = EQ
    compare X Y = GT
    compare Y X = LT
    compare Y Y = EQ

    ins' :: U -> Set U -> Set U
    ins' = insert

    -- D.hs
    module Main where
    import Data.Set
    import A
    import B
    import C

    test :: Set U
    test = ins' X $ ins X $ ins Y $ empty

    main :: IO ()
    main = print test

    -- OUTPUT
    $ ghc -Wall -XSafe -fforce-recomp --make D.hs
    [1 of 4] Compiling A ( A.hs, A.o )
    [2 of 4] Compiling B ( B.hs, B.o )

    B.hs:5:10: Warning: Orphan instance: instance [safe] Ord U
    [3 of 4] Compiling C ( C.hs, C.o )

    C.hs:5:10: Warning: Orphan instance: instance [safe] Ord U
    [4 of 4] Compiling Main ( D.hs, D.o )
    Linking D ...
    $ ./D
    fromList [X,Y,X]

Locally, all type class resolution was coherent: in the subset of instances each module had visible, type class resolution could be done unambiguously. Furthermore, the types of `ins` and `ins'` discharge type class resolution, so that in `D` when the database is now overlapping, no resolution occurs, so the error is never found.

It is easy to dismiss this example as an implementation wart in GHC, and continue pretending that global uniqueness of instances holds. However, the problem with global uniqueness of instances is that they are inherently nonmodular: you might find yourself unable to compose two components because they accidentally defined the same type class instance, even though these instances are plumbed deep in the implementation details of the components. This is a big problem for Backpack, or really any module system, whose mantra of separate modular development seeks to guarantee that linking will succeed if the library writer and the application writer develop to a common signature.
