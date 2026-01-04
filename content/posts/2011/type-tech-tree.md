---
title: "Type Technology Tree"
date: 2011-03-07 09:00:26
slug: type-tech-tree
categories: [GHC, Haskell]
comments:
    - id: 1874
      author: Erik
      date: "2011-03-07 11:20:33"
      content: "Note that GADTs don't imply kind signatures, so your GADT example doesn't compile, since it also needs KindSignatures enabled."
    - id: 1876
      author: illissius
      date: "2011-03-07 12:31:46"
      content: |
        Also: TypeFamilies + ExistentialQuantification lets you do all the same things as GADTs, even without GADTSyntax. (I don't believe there's a separate extension which only enables equality constraints without type/data families? That would also be sufficient.)
        
        data Foo a where Foo :: forall b. Show b =&gt; b -&gt; Foo Int
        
        data Foo a where Foo :: forall b. (Show b, a ~ Int) =&gt; b -&gt; Foo a
        
        data Foo a = forall b. (Show b, a ~ Int) =&gt; Foo b
        
        Also: I was under the impression that MultiParamTypeClasses + FunctionalDependencies corresponds to MultiParamTypeClasses + TypeFamilies -- not just TypeFamilies alone. Though it may be possible to encode MultiParamTypeClasses with just TypeFamilies, but it's probably hella awkward. (Supposedly, per Oleg, it's still possible to encode the whole typeclass system even if you're restricted to a single typeclass with a single method -- but it's presumably not too convenient.)
        Anyway, the differences are:
        - While TFs are equivalent in power to FDs in theory, in practice GHC (still) doesn't support equality constraints for classes, which means TFs can't currently encode the equivalent to FDs with cycles in them. (Hopefully support is coming in 7.2?)
        - FDs don't have any direct equivalent to equality constraints or data families, though you can probably encode either one. (I know there exist TypeCast/TypeEq classes (don't know if Oleg came up with these or just made use of them), one of which is the same as an equality constraint though I'm not sure which, and you can simulate data families with a bidirectional FD and a standalone datatype.)
        - TFs prohibit OverlappingInstances, FDs allow them -- though I have the impression that they're mostly oblivious to each other so it doesn't necessariy work as you might like.
        
        Anyone have further clarifications? :)
    - id: 1877
      author: illissius
      date: "2011-03-07 12:33:19"
      content: "(Sorry, the last case was also supposed to be 'Foo a' at the end, not 'Foo b'.)"
    - id: 1878
      author: Edward Z. Yang
      date: "2011-03-07 13:46:14"
      content: |
        Erik, good catch. I've amended the example.
        
        illisius:
        
        Equality constraints + ExistentialQuantification is an interesting combo that I hadn't thought about before. No doubt all of these equivalences would make for an interesting article. I avoided expounding on them because I need to study the correspondence more ;-) and it would have gotten in the way of the trees.
        
        it is true that you do need MultiParamTypeClasses with type families for a strict equivalence. I guess I was thinking of the case where you have a type class on a single type, and then fundeps was used to specify all of the "auxiliary types" that are now type families. I might adjust that diagram shortly.
    - id: 1889
      author: Mathnerd314
      date: "2011-03-07 17:14:32"
      content: "I was hoping for a larger tree, containing perhaps the lambda cube and the calculus of constructions. But such a tree would be even easier to get wrong than this one. :-/"
    - id: 1898
      author: Edward Z. Yang
      date: "2011-03-07 17:59:54"
      content: "Yeah, I was originally imagining a gigantic tree, but it turned out lots of the features were orthogonal. Which ended up being a good thing. :-)"
---

They say that one doesn’t discover advanced type system extensions: rather, the type system extensions discover you! Nevertheless, it’s worthwhile to know what the tech tree for GHC’s type extensions are, so you can decide how much power (and the correspondingly headache inducing error messages) you need. I’ve organized the relations in the following diagram with the following criterion in mind:

1.  Some extensions automatically enable other extensions (implies);
2.  Some extensions offer all the features another extension offers (subsumes);
3.  Some extensions work really nicely with other extensions (synergy);
4.  Some extensions offer equivalent (but differently formulated) functionality to another extension (equiv).

It’s also worth noting that the GHC manual divides these extensions into “Extensions to data types and type synonyms”, “Class and instances declarations”, “Type families” and “Other type system extensions”. I have them organized here a little differently.

# Rank and data

Our first tech tree brings together two extensions: arbitrary-rank polymorphism and generalized algebraic data types.

![image](/img/type-tech-tree/rank-and-data.png)

Briefly:

- GADTSyntax permits ordinary data types to be written GADT-style (with explicit constructor signatures): `data C where C :: Int -> C`
- [ExplicitForall](http://hackage.haskell.org/trac/haskell-prime/wiki/ExplicitForall) allows you to explicitly state the quantifiers in polymorphic types: `forall a. a -> a`
- [ExistentialQuantification](http://hackage.haskell.org/trac/haskell-prime/wiki/ExistentialQuantification) allows types to be hidden inside a data constructor: `data C = forall e. C e`
- [GADTs](http://hackage.haskell.org/trac/haskell-prime/wiki/GADTs) permits explicit constructor signatures: `data C where C :: C a -> C b -> C (a, b)`. Subsumes ExistentialQuantification because existentially quantified data types are simply polymorphic constructors for which the type variable isn’t in the result.
- [PolymorphicComponents](http://hackage.haskell.org/trac/haskell-prime/wiki/PolymorphicComponents) allows you to write `forall` inside data type fields: `data C = C (forall a. a)`
- [Rank2Types](http://hackage.haskell.org/trac/haskell-prime/wiki/Rank2Types) allows polymorphic arguments: `f :: (forall a. a -> a) -> Int -> Int`. This with GADTs subsumes PolymorphicComponents because data type fields with `forall` within them correspond to data constructors with rank-2 types.
- [RankNTypes](http://hackage.haskell.org/trac/haskell-prime/wiki/RankNTypes): `f :: Int -> (forall a. a -> a)`
- ImpredicativeTypes allows polymorphic functions and data structures to be parametrized over polymorphic types: `Maybe (forall a. a -> a)`

# Instances

Our next tech tree deals with type class instances.

![image](/img/type-tech-tree/instances.png)

Briefly:

- [TypeSynonymInstances](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeSynonymInstances) permits macro-like usage of type synonyms in instance declarations: `instance X String`
- [FlexibleInstances](http://hackage.haskell.org/trac/haskell-prime/wiki/FlexibleInstances) allows more instances for more interesting type expressions, with restrictions to preserve decidability: `instance MArray (STArray s) e (ST s)` (frequently seen with multi-parameter type classes, which are not in the diagram)
- [UndecidableInstances](http://hackage.haskell.org/trac/haskell-prime/wiki/UndecidableInstances) allows instances for more interesting type expression with no restrictions, at the cost of decidability. See [Oleg](http://okmij.org/ftp/Haskell/types.html#undecidable-inst-defense) for a legitimate example.
- [FlexibleContexts](http://hackage.haskell.org/trac/haskell-prime/wiki/FlexibleContexts) allows more type expressions in constraints of functions and instance declarations: `g :: (C [a], D (a -> b)) => [a] -> b`
- [OverlappingInstances](http://hackage.haskell.org/trac/haskell-prime/wiki/OverlappingInstances) allows instances to overlap if there is a most specific one: `instance C a; instance C Int`
- [IncoherentInstances](http://hackage.haskell.org/trac/haskell-prime/wiki/IncoherentInstances) allows instances to overlap arbitrarily.

Perhaps conspicuously missing from this diagram is `MultiParamTypeClasses` which is below.

# Type families and functional dependencies

Our final tech tree addresses programming with types:

![image](/img/type-tech-tree/families-and-fundeps.png)

Briefly:

- KindSignatures permits stating the kind of a type variable: `m :: * -> *`
- [MultiParamTypeClasses](http://hackage.haskell.org/trac/haskell-prime/wiki/MultiParamTypeClasses) allow type classes to range over multiple type variables: `class C a b`
- [FunDeps](http://hackage.haskell.org/trac/haskell-prime/wiki/FunctionalDependencies) allow restricting instances of multi-parameter type classes, helping resolve ambiguity: `class C a b | a -> b`
- [TypeFamilies](http://www.haskell.org/ghc/docs/7.0.1/html/users_guide/type-families.html) allow “functions” on types: `data family Array e`

The correspondence between functional dependencies and type families is well known, though not perfect (type families can be more wordy and can’t express certain equalities, but play more nicely with GADTs).
