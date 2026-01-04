---
title: "Haskell for Coq programmers"
date: 2014-03-17 02:06:47
slug: haskell-for-coq-programmers
categories: [Coq, Haskell]
comments:
    - id: 6554
      author: Jason Dagit
      date: "2014-03-17 13:30:44"
      content: |
        Thanks for the interesting article. I'm coming from the Haskell side and trying to understand what you mean when you say that Haskell types are not types.
        
        Specifically, these two quotes:
        
        &gt; Terms of type * are called types (an unfortunate overloading of the term)
        
        &gt; a language for handling kind-level terms (in the Haskell vernacular, people will often call these types and kinds, even if they're not).
        
        Please elaborate :)
    - id: 6555
      author: Edward Z. Yang
      date: "2014-03-17 14:41:17"
      content: "I think what I was trying to say is that Haskell types are types, but in dependently typed languages there is a more expansive view of what a \"type\" is (i.e. one may talk about the type of a type). Your confusion is not helped by the fact that the terminology in this article is a little confused itself, as pointed out by Bob Harper, I am going to try to fix it up."
    - id: 6561
      author: Robert Harper
      date: "2014-03-20 23:21:17"
      content: "I regret using Twitter for discussion.  It's tempting to be quick but horrible to be confined to snippets like that.  My apologies.  I can sense future blog posts coming to explain myself more fully.  Meanwhile thanks for responding!"
    - id: 6562
      author: Edward Z. Yang
      date: "2014-03-20 23:22:16"
      content: "I eagerly anticipate your posts. :)"
    - id: 6563
      author: "Philip Johnson-Freyd"
      date: "2014-03-20 23:43:18"
      content: |
        I'm confused by the polarity of products discussion: Haskell's products are positive.  But, this is most certainly not what you want.  You see this in a missing eta law in Haskell:  `(fst m, snd m) = m`.  The proper categorical product is a negative type, and this is (practically) not definable in Haskell.
        
        The Haskell product is properly seen as the call-by-name version of "tensor".  The categorical product would be the call-by-name version of "with".
        
        The existential is not a sigma.  A sigma is a negative type.  A Haskell existential is a positive type.
        
        The connection between polarity and evaluation strategy is not as close as is sometimes suggested: ML has negative types (such as function) while haskell has positive types (anything declared with "data").  The point is that these are less canonical, not that they don't exist.
    - id: 6564
      author: Edward Z. Yang
      date: "2014-03-21 00:11:50"
      content: "Philip, thank you for your comments. You are right that I have not discussed the fact that all data types in Haskell additionally admit bottom as an element. But if you assume that m is not bottom, then the eta law you have defined holds. I think I would side with Bob here, and say that Haskell products are more negative than they are positive, but the whole issue is very confused, and it would be very nice if there was a proper writeup of this somewhere. Ditto with existentials: I literally do not know how to evaluate the claim made here, in the context of Haskell."
    - id: 6565
      author: "Philip Johnson-Freyd"
      date: "2014-03-21 02:22:25"
      content: |
        I'm pretty sure the paper by Downen and Ariola that is coming out in this years ESOP has a short discussion of the Haskell datatypes from the point of view of polarity. (Full disclosure: Paul Downen and I share an office and Zena Ariola is my advisor.)
        
        We almost always end up using bottom to evaluate polarity when dealing with simple types.  As you remarked: logically the tensor and the with connectives coincide in intuitionistic/classical logic.  The difference is in the eta laws.  Now, Haskell is secretly a strict language because of sec, so even functions don't have the proper eta laws.  But, for pairs we don't have that issue.  Pairs in Haskell-without-seq would still lack this eta law.  And, I think this makes it clear they are positive.  This though is a language decision unique to Haskell (an "unforced error" if you will).  Other call-by-name languages need not do this.  If the point in the language design space you took was "call by name smalltalk with types" you would almost certainly design a language that handled negative types correctly (this would be the dual to ML).  In fact, in Haskell-without-seq you could get simulate a true product by hiding the constructor in a module and thus preventing pattern matching (I actually have an ugly encoding of true products in Haskell-with-seq but it uses unamb).
        
        Pairs in Haskell are defined by the constructor (,).  fst and snd are syntactic sugar for pattern matching.  True products would be defined by the co-constructors fst and snd (eliminator forms) and would have a introduction form given by co-case analysis.
        
        For sigma/existential the difference is clearer as it is logical.  Bob actually pointed it out in one of his OPLSS lectures last summer.  Sigma is defined by the destructors fst and snd, while the elimination form for existential is case analysis.  The type of `snd : Π (p : Σ x : A.B).B[fst p/x]` is not derivable if you give the rules in terms of the existential.
        
        The negative Sigma is strictly stronger than the positive existential.  I have preliminary work suggesting that true sigma can not exist in a call-by-name language with effects (such as Haskell).  Sigma, bizarrely for negative types, seems to force a call-by-value perspective in the presence of effects.  Interestingly enough, the fact that Sigma is weaker than existential has a dual: the positive version of the "dependent function" type (actually a dependent co-product) is strictly more powerful than the Pi-type.  
        
        Haskell though is even further from having sigma.  The Haskell existential does not have a witness of its argument, and is thus related to the (more primitive) infinite-union type.  The difference between the more artificial/compound first order existential and the infinite-union is discussed to some extent in Locus Solum, and perhaps also Hugo Herbelin's Habilitation (but I'm not very good at French and so am really not sure what all Hugo worked out in that).
    - id: 6566
      author: Edward Z. Yang
      date: "2014-03-21 04:31:02"
      content: |
        Alas, there appears to be no preprint available of "The Duality of Construction." You should ask your officemate to post one.
        
        Let me try to restate your argument, to see if I have understood it. The crux of polarity is how a type is defined; either by the introduction rules or the elimination. While "is it lazy" or "is it strict" are plausible heuristics for determining polarity, they can actually be quite misleading, because it can be difficult to distinguish a negative type from a positive one with suspensions. Since in Haskell the primitive mechanism for destructing over a data type is pattern matching, *definitionally* we can only ever define positive data-types.
        
        Why does this seem wrong to me? Well, aren't Haskell data types lazy? Yes and no. We can make this precise by taking a PCF± style language, as presented in PFPL, and translating a Haskell type into it. Let's suppose we have PTyp τ+ ::= dn(τ-) | pair+(τ+, τ+) and NTyp τ- ::= up(τ+) | pair-(τ-, τ-) (where dn represents suspension, while up represents inclusion); that is to say, PCF± supports both positive and negative pairs. The translation of the type (a, b) is dn(up(pair+(a, b))) under your description. Since all pairs in Haskell are inhabited by bottom, we have to use dn to indicate we have a suspension. Furthermore, we must be using the positive pairs (due to pattern-matching), so we also need to apply the inclusion up. Now, crucially, a and b, in principle, don't have to be suspensions; but it just so happens that all types in Haskell are suspensions.
        
        What is the comment about Smalltalk about? Well, Smalltalk is a language with the tradition of only interacting with other objects by sending messages. This is what a proper negative type should look like (methods, after all, are functions).
        
        Now, here is what I don't understand: (1) Why is the lack of an eta-law a strong indicator that a type is positive? If I was in ML, where the pairs are still positive, the eta law *would* hold. (2) Why isn't it sufficient, in Haskell without seq, to hide the constructor—i.e. why does your proper version of negative types in Haskell require unamb?
        
        I agree with the comments on sigma: existential types in Haskell are quite clearly positive. However, by a similar token, why wouldn't existential types in Coq be considered positive? After all, at the end of the day fst/snd are implemented using dependent pattern matching—it just so happens that the negative eliminators are definable.
        
        I am not sure if Haskell's "existential types" are merely infinite-unions: I've never heard that term before, except maybe for intersection/union types, which I suspect are not what you are referring to.
        
        (P.S. The linked blog post discussing classical/intuitionistic is Bob's, not mine ;-)
    - id: 6567
      author: "Philip Johnson-Freyd"
      date: "2014-03-21 07:57:35"
      content: |
        By and large I think you summarized my argument well.  The point about how a type is defined isn't just a syntactic one though: it is about what laws we expect to hold for that type.  The definition in terms of (co-)constructors tell us some computation rules and some eta laws that should hold.
        
        Products in there full negative glory have two computation laws `fst (a,b) = a` and `snd (a,b) = b` as well as an eta law `(fst m,snd m) = m`.  These laws can be derived using a uniform method for all types.  I think this is much cleaner with the sequent calculus presentation of Curien and Herbelin:
        
         = 
         = 
        case[fst(a).;snd(b).] = m
        
        those rules are exactly the dual of what you get for co-products.
        
        Anyway, if you do the same thing for tensor instead of with you get
         = &lt;a | mutilde x.&lt;b>&gt;
        case[pair(x,y).] = a
        
        where that second rule is a the eta law and is a law about *contexts* instead of terms. 
        
        Note that the ML pair can't be the first of these:
        
         = 
        
        is the same as `fst (a,b) = a` which is not true in ML.  So, the ML pair must be positive.  Sure enough, we can see that it satisfies the first law, and that
        
        case BOX of (a,b) =&gt; E[(a,b)
        
        is the same as `E`
        
        So to answer question 1: 
        The basic story then is this: if you are CBV you should have both the beta and the eta law for a positive type.  Dually, if you are CBN you should have both the beta and the eta law for a negative type.
        
        Also, in the presence of effects the eta law for with does not hold in ML.  
        
        Question 2:
        In Haskell without seq the eta law does not hold since
        
        case undefined of
            (a,b) -&gt; c
        
        is not equal to 
        
        case (undefined,undefined) of
            (a,b) -&gt; c
        
        hiding the definition of tuples in a module that exposed the type, a "smart constructor", and the fst and snd projections would resolve this.  The problem is that sec gives you a way to differentiate them again.  
        
        The unamb encoding is just a clever way of defining an interface such that you can never end up with (_|_,_|_)...but there is no free lunch: doing this breaks the computation laws.  And so my claim that it encodes "true products" was false--I knew this but apparently forgot it.
        
        Regardless, the idea of a CBN language could have true products should not be controversial.  Haskell isn't CBN because of the sec abomination.
        
        I'm really not sure about the polarity of connectives in Coq.  All the eta laws hold.  Dependent pattern matching is deeply weird to me, and not something I really understand.   This could just be negative types in disguise though.
        
        BTW: I don't think haskell's existentials are "merely" infinite-unions: just that they don't include a witness.
    - id: 6568
      author: "Philip Johnson-Freyd"
      date: "2014-03-21 08:01:15"
      content: "and your blogging software ate my sequent calculus :)  I can't fix that now, but perhaps we can resume this discussion at a later date.  I'll try to get you a copy of \"Duality of Construction\""
    - id: 6569
      author: Edward Z. Yang
      date: "2014-03-21 15:06:30"
      content: "Philip, if you pastebin the correct version, I can go and reformat your comment so that it shows up correctly. Oh, but silly me, I guess that would require retyping the comment, wouldn't it. >:("
    - id: 16744
      author: Aaron Stump
      date: "2015-09-09 21:35:39"
      content: |
        I just stumbled upon this post while searching for "open types".  One small correction (I think -- I am not much of a Haskeller): * in Haskell is predicative by default.  This actually surprised me a lot.  Apparently one has to use -XImpredicativeTypes to make it impredicative.
        Cheers,
        Aaron
---

So you may have heard about this popular new programming language called Haskell. What's Haskell? Haskell is a non-dependently typed programming language, sporting general recursion, type inference and built-in side-effects. It is true that dependent types are considered an essential component of modern, expressive type systems. However, giving up dependence can result in certain benefits for other aspects of software engineering, and in this article, we'd like to talk about the omissions that Haskell makes to support these changes.

# Syntax

There are a number of syntactic differences between Coq and Haskell, which we will point out as we proceed in this article. To start with, we note that in Coq, typing is denoted using a single colon (`false : Bool`); in Haskell, a double colon is used (`False :: Bool`). Additionally, Haskell has a syntactic restriction, where constructors must be capitalized, while variables must be lower-case.

Similar to my [OCaml for Haskellers](http://blog.ezyang.com/2010/10/ocaml-for-haskellers/) post, code snippets will have the form:

    (* Coq *)

    {- Haskell -}

# Universes/kinding

A universe is a type whose elements are types. They were originally introduced to constructive type theory by Per Martin-Löf. Coq sports an infinite hierarchy of universes (e.g. `Type (* 0 *) : Type (* 1 *)`, `Type (* 1 *) : Type (* 2 *)`, and so forth).

Given this, it is tempting to draw an analogy between universes and Haskell’s kind of types `*` (pronounced “star”), which classifies types in the same way `Type (* 0 *)` classifies primitive types in Coq. Furthermore, the sort *box* classifies kinds (`* : BOX`, although this sort is strictly internal and cannot be written in the source language). However, the resemblance here is only superficial: it is misleading to think of Haskell as a language with only two universes. The differences can be summarized as follows:

1.  In Coq, universes are used purely as a sizing mechanism, to prevent the creation of types which are too big. In Haskell, types and kinds do double duty to enforce the *phase distinction*: if `a` has kind `*`, then `x :: a` is guaranteed to be a runtime value; likewise, if `k` has sort box, then `a :: k` is guaranteed to be a compile-time value. This structuring is a common pattern in traditional programming languages, although knowledgeable folks like [Conor McBride](https://twitter.com/pigworker/status/446784239754022912) think that ultimately this is a design error, since one doesn’t [really need a kinding system to have type erasure.](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.105.788&rep=rep1&type=pdf)
2.  In Coq, universes are cumulative: a term which has type `Type (* 0 *)` also has type `Type (* 1 *)`. In Haskell, there is no cumulativity between between types and kinds: if `Nat` is a type (i.e. has the type `*`), it is not automatically a kind. However, in some cases, partial cumulativity can be achieved using [datatype promotion](http://www.haskell.org/ghc/docs/latest/html/users_guide/promotion.html), which constructs a separate kind-level replica of a type, where the data constructors are now type-level constructors. Promotion is also capable of promoting type constructors to kind constructors.
3.  In Coq, a common term language is used at all levels of universes. In Haskell, there are three distinct languages: a language for handling base terms (the runtime values), a language for handling type-level terms (e.g. types and type constructors) and a language for handling kind-level terms. In some cases this syntax is overloaded, but in later sections, we will often need to say how a construct is formulated separately at each level of the kinding system.

One further remark: `Type` in Coq is predicative; in Haskell, `*` is *impredicative*, following the tradition of System F and other languages in the lambda cube, where kinding systems of this style are easy to model.

# Function types

In Coq, given two types `A` and `B`, we can construct the type `A -> B` denoting functions from A to B (for A and B of any universe). Like Coq, functions with multiple arguments are natively supported using currying. Haskell supports function types for both types (`Int -> Int`) and kinds (`* -> *`, often called *type constructors*) and application by juxtaposition (e.g. `f x`). (Function types are subsumed by pi types, however, we defer this discussion for later.) However, Haskell has some restrictions on how one may construct functions, and utilizes different syntax when handling types and kinds:

For *expressions* (with type `a -> b` where `a, b :: *`), both direct definitions and lambdas are supported. A direct definition is written in an equational style:

    Definition f x := x + x.

    f x = x + x

while a lambda is represented using a backslash:

    fun x => x + x

    \x -> x + x

For *type families* (with type `k1 -> k2` where `k1` and `k2` are kinds), the lambda syntax is not supported. In fact, no higher-order behavior is permitted at the type-level; while we can directly define appropriately kinded type functions, at the end of the day, these functions must be fully applied or they will be rejected by the type-checker. From an implementation perspective, the omission of type lambdas makes type inference and checking much easier.

1.  *Type synonyms*:

        Definition Endo A := A -> A.

        type Endo a = a -> a

    Type synonyms are judgmentally equal to their expansions. As mentioned in the introduction, they cannot be partially applied. They were originally intended as a limited syntactic mechanism for making type signatures more readable.

2.  *Closed type (synonym) families*:

        Inductive fcode :=
          | intcode : fcode
          | anycode : fcode.
        Definition interp (c : fcode) : Type := match c with
          | intcode -> bool
          | anycode -> char
        end.

        type family F a where
          F Int = Bool
          F a   = Char

    While closed type families look like the addition of typecase (and would violate parametricity in that case), this is not the case, as closed type families can only return types. In fact, closed type families correspond to a well-known design pattern in Coq, where one writes inductive data type representing *codes* of types, and then having an interpretation function which interprets the codes as actual types. As we have stated earlier, Haskell has no direct mechanism for defining functions on types, so this useful pattern had to be supported directly in the type families functionality. Once again, closed type families cannot be partially applied.

    In fact, the closed type family functionality is a bit more expressive than an inductive code. In particular, closed type families support *non-linear pattern matches* (`F a a = Int`) and can sometimes reduce a term when no iota reductions are available, because some of the inputs are not known. The reason for this is because closed type families are “evaluated” using unification and constraint-solving, rather than ordinary term reduction as would be the case with codes in Coq. Indeed, nearly all of the “type level computation” one may perform in Haskell, is really just constraint solving. Closed type families are not available in a released version of GHC (yet), but there is a [Haskell wiki page describing closed type families in more detail](http://www.haskell.org/haskellwiki/GHC/Type_families#Closed_family_simplification).

3.  *Open type (synonym) families*:

        (* Not directly supported in Coq *)

        type family F a
        type instance F Int = Char
        type instance F Char = Int

    Unlike closed type families, open type families operate under an open universe, and have no analogue in Coq. Open type families do not support nonlinear matching, and must completely unify to reduce. Additionally, there are number of restrictions on the left-hand side and right-hand side of such families in order maintain decidable type inference. The section of the GHC manual [Type instance declarations](http://www.haskell.org/ghc/docs/latest/html/users_guide/type-families.html#type-instance-declarations) expands on these limitations.

Both closed and type-level families can be used to implement computation at the type-level of data constructors which were lifted to the type-level via promotion. Unfortunately, any such algorithm must be implemented twice: once at the expression level, and once at the type level. Use of metaprogramming can alleviate some of the boilerplate necessary; see, for example, the [singletons](https://hackage.haskell.org/package/singletons) library.

# Dependent function types (Π-types)

A Π-type is a function type whose codomain type can vary depending on the element of the domain to which the function is applied. Haskell does not have Π-types in any meaningful sense. However, if you only want to use a Π-type solely for polymorphism, Haskell does have support. For polymorphism over types (e.g. with type `forall a : k, a -> a`, where `k` is a kind), Haskell has a twist:

    Definition id : forall (A : Type), A -> A := fun A => fun x => x.

    id :: a -> a
    id = \x -> x

In particular, the standard notation in Haskell is to omit both the type-lambda (at the expression level) and the quantification (at the type level). The quantification at the type level can be recovered using the explicit universal quantification extension:

    id :: forall a. a -> a

However, there is no way to directly explicitly state the type-lambda. When the quantification is not at the top-level, Haskell requires an explicit type signature with the quantification put in the right place. This requires the rank-2 (or rank-n, depending on the nesting) polymorphism extension:

    Definition f : (forall A, A -> A) -> bool := fun g => g bool true.

    f :: (forall a. a -> a) -> Bool
    f g = g True

Polymorphism is also supported at the kind-level using the [kind polymorphism extension](http://www.haskell.org/ghc/docs/latest/html/users_guide/kind-polymorphism.html). However, there is no explicit forall for kind variables; you must simply mention a kind variable in a kind signature.

Proper dependent types cannot be supported directly, but they can be simulated by first promoting data types from the expression level to the type-level. A runtime data-structure called a *singleton* is then used to refine the result of a runtime pattern-match into type information. This pattern of programming in Haskell is not standard, though there are recent academic papers describing how to employ it. One particularly good one is [Hasochism: The Pleasure and Pain of Dependently Typed Haskell Program](https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf), by Sam Lindley and Conor McBride.

# Product types

Coq supports cartesian product over types, as well as a nullary product type called unit. Very similar constructs are also implemented in the Haskell standard library:

    (true, false) : bool * bool
    (True, False) :: (Bool, Bool)

    tt : unit
    () :: ()

Pairs can be destructed using pattern-matching:

    match p with
      | (x, y) => ...
    end

    case p of
      (x, y) -> ...

Red-blooded type theorists may take issue with this identification: in particular, Haskell’s default pair type is what is considered a *negative* type, as it is lazy in its values. (See more on [polarity](http://existentialtype.wordpress.com/2012/08/25/polarity-in-type-theory/).) As Coq’s pair is defined inductively, i.e. positively, a more accurate identification would be with a strict pair, defined as `data SPair a b = SPair !a !b`; i.e. upon construction, both arguments are evaluated. This distinction is difficult to see in Coq, since positive and negative pairs are logically equivalent, and Coq does not distinguish between them. (As a total language, it is indifferent to choice of evaluation strategy.) Furthermore, it's relatively common practice to extract pairs into their lazy variants when doing code extraction.

# Dependent pair types (Σ-types)

Dependent pair types are the generalization of product types to be dependent. As before, Σ-types cannot be directly expressed, except in the case where the first component is a type. In this case, there is an encoding trick utilizing data types which can be used to express so-called *existential types*:

    Definition p := exist bool not : { A : Type & A -> bool }

    data Ex = forall a. Ex (a -> Bool)
    p = Ex not

As was the case with polymorphism, the type argument to the dependent pair is implicit. It can be specified explicitly by way of an appropriately placed type annotation.

# Recursion

In Coq, all recursive functions must have a structurally decreasing argument, in order to ensure that all functions terminate. In Haskell, this restriction is lifted for the expression level; as a result, expression level functions may not terminate. At the type-level, by default, Haskell enforces that type level computation is decidable. However, this restriction can be lifted using the `UndecidableInstances` flag. It is generally believed that undecidable instances cannot be used to cause a violation of type safety, as nonterminating instances would simply cause the compiler to loop infinitely, and due to the fact that in Haskell, types cannot (directly) cause a change in runtime behavior.

# Inductive types/Recursive types

In Coq, one has the capacity to define inductive data types. Haskell has a similar-looking mechanism for defining data types, but there are a number of important differences which lead many to avoid using the moniker *inductive data types* for Haskell data types (although it’s fairly common for Haskellers to use the term anyway.)

Basic types like boolean can be defined with ease in both languages (in all cases, we will use the [GADT syntax](http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt) for Haskell data-types, as it is closer in form to Coq’s syntax and strictly more powerful):

    Inductive bool : Type :=
      | true : bool
      | false : bool.

    data Bool :: * where
      True :: Bool
      False :: Bool

Both also support recursive occurrences of the type being defined:

    Inductive nat : Type :=
      | z : nat
      | s : nat -> nat.

    data Nat :: * where
      Z :: Nat
      S :: Nat -> Nat

One has to be careful though: our definition of `Nat` in Haskell admits one more term: infinity (an infinite chain of successors). This is similar to the situation with products, and stems from the fact that Haskell is lazy.

Haskell’s data types support parameters, but these parameters may only be types, and not values. (Though, recall that data types can be promoted to the type level). Thus, the standard type family of vectors may be defined, assuming an appropriate type-level nat (as usual, explicit forall has been omitted):

    Inductive vec (A : Type) : nat -> Type :=
      | vnil  : vec A 0
      | vcons : forall n, A -> vec A n -> vec A (S n)

    data Vec :: Nat -> * -> * where
      VNil  :: Vec Z a
      VCons :: a -> Vec n a -> Vec (S n) a

As type-level lambda is not supported but partial application of data types is (in contrast to type families), the order of arguments in the type must be chosen with care. (One could define a type-level flip, but they would not be able to partially apply it.)

Haskell data type definitions do not have the [strict positivity requirement,](http://blog.ezyang.com/2012/09/y-combinator-and-strict-positivity/) since we are not requiring termination; thus, peculiar data types that would not be allowed in Coq can be written:

    data Free f a where
       Free :: f (Free f a) -> Free f a
       Pure :: a -> Free f a

    data Mu f where
       Roll :: f (Mu f) -> Mu f

# Inference

Coq has support for requesting that a term be inferred by the unification engine, either by placing an underscore in a context or by designating an argument as *implicit* (how one might implement in Coq the omission of type arguments of polymorphic functions as seen in Haskell). Generally, one cannot expect all inference problems in a dependently typed language to be solvable, and the inner-workings of Coq’s unification engines (plural!) are considered a black art (no worry, as the trusted kernel will verify that the inferred arguments are well-typed).

Haskell as specified in Haskell'98 enjoys principal types and full type inference under Hindley-Milner. However, to recover many of the advanced features enjoyed by Coq, Haskell has added numerous extensions which cannot be easily accomodated by Hindley-Milner, including type-class constraints, multiparameter type classes, GADTs and type families. The current state-of-the-art is an algorithm called [OutsideIn(X)](http://research.microsoft.com/en-us/um/people/simonpj/papers/constraints/jfp-outsidein.pdf). With these features, there are no completeness guarantee. However, if the inference algorithm accepts a definition, then that definition has a principal type and that type is the type the algorithm found.

# Conclusion

This article started as a joke over in OPLSS'13, where I found myself explaining some of the hairier aspects of Haskell’s type system to Jason Gross, who had internalized Coq before he had learned much Haskell. Its construction was iced for a while, but later I realized that I could pattern the post off of the first chapter of the homotopy type theory book. While I am not sure how useful this document will be for learning Haskell, I think it suggests a very interesting way of mentally organizing many of Haskell’s more intricate type-system features. Are proper dependent types simpler? Hell yes. But it’s also worth thinking about where Haskell goes further than most existing dependently typed languages...

# Postscript

Bob Harper [complained over Twitter](http://storify.com/ezyang/bob-harper-comments-on-haskell-for-coq-programmers) that this post suggested misleading analogies in some situations. I've tried to correct some of his comments, but in some cases I wasn't able to divine the full content of his comments. I invite readers to see if they can answer these questions:

1.  Because of the phase distinction, Haskell’s *type families* are not actually type families, in the style of Coq, Nuprl or Agda. Why?
2.  This post is confused about the distinction between elaboration (type inference) and semantics (type structure). Where is this confusion?
3.  Quantification over kinds is not the same as quantification over types. Why?
