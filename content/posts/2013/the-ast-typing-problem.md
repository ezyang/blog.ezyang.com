---
title: "The AST Typing Problem"
date: 2013-05-28 07:25:03
slug: the-ast-typing-problem
categories: [Compilers]
comments:
    - id: 6115
      author: Adam Chlipala
      date: "2013-05-28 09:48:48"
      content: "FWIW, Ur/Web actually uses \"explicit typing,\" and I think Coq does too, though I'm less sure on the latter."
    - id: 6116
      author: Edward Z. Yang
      date: "2013-05-28 15:29:54"
      content: "Good catch, I think you're right on both."
    - id: 6117
      author: Gabriel
      date: "2013-05-28 17:54:14"
      content: "You seem to have forgotten to delete (or rewrite) the paragraph starting with \"GHC - lots of IRs\". It looks like a bunch of scribbled notes."
    - id: 6118
      author: Edward Z. Yang
      date: "2013-05-28 17:55:11"
      content: "Haha, thanks. \"That's what I get for writing blog posts at 4am in the morning.\""
    - id: 6119
      author: Pseudonym
      date: "2013-05-28 22:52:02"
      content: "The two-level type approach is under-used, I think. I've used it for making mutable structures (by recursing through an MVar or IOVar) and to implement hash consing. You could also do value numbering using that approach."
    - id: 6120
      author: Pseudonym
      date: "2013-05-28 22:59:13"
      content: "Oh, having said that, a common scenario is that rich expressive IRs (where a close match between the source program and the IR must be maintained, say for reporting errors to the user) are typically compiled down to desugared lower-level IRs (where fewer operations is better). If you're using multiple IRs <i>anyway</i>, there's no point in making things too generic."
    - id: 6121
      author: José Pedro Magalhães
      date: "2013-05-29 03:13:13"
      content: |
        Generic programming can make the "two-level types" approach easier to use, in particular allowing you to define only the original Exp and Type datatypes, and getting ExpF for free. For example:
        http://hackage.haskell.org/package/Annotations (adding position information)
        http://hackage.haskell.org/package/rewriting (adding meta-variables)
        http://www.andres-loeh.de/GenericStorage/ (adding pointers)
    - id: 6122
      author: exDM69
      date: "2013-05-29 03:24:01"
      content: |
        I've encountered this problem too. And I've used yet another kind of solution for this.
        
        I used a type declaration expression <code>(x :: Int)</code>, so I added another type of expression to the AST, <code>data Exp = TypeDecl Type Exp | Num Int | If Exp Exp Exp | ....</code> After the type checking, I rebuild the AST and add TypeDecl nodes to every branch.
        
        I also tried not putting the type information in the AST in the first place, and passing the type and type environment along with the expression to the code generator tree walk and doing partial type checking as I go. This is crazy inefficient but it worked.
    - id: 6123
      author: Edward Z. Yang
      date: "2013-05-30 03:24:18"
      content: "Pseudonym: Hmm, I wonder if this approach would work for types, so that types can be temporarily be replaced with IORefs for unification."
    - id: 6127
      author: heatsink
      date: "2013-05-30 12:33:15"
      content: |
        The two-level type approach makes it awkward when your AST is several mutually recursive types, because each type in the recursive cycle becomes a type parameter.  Suppose that you wanted a separate AST type for functions.  Now everything is parameterized over two types.
        
        
            data ExpF e f = ... | Lambda f | App e e
            data FunF e f = Fun Var e
            newtype Exp = Exp (ExpF Exp Fun)
            newtype Fun = Fun (FunF Exp Fun)
            newtype ExpT = Exp (ExpF ExpT FunT, Type)
            newtype FunT = Fun (FunF ExpT FunT, Type)
        
        
        The two type parameters cannot vary independently, which makes me think there may be a single-parameter solution using type families.
    - id: 6128
      author: Pseudonym
      date: "2013-05-30 21:04:20"
      content: "Edward, you've inspired me to try it out. I've been looking for something small but nontrivial to try with automatically extracting Haskell from Coq. This looks perfect."
    - id: 6129
      author: Pseudonym
      date: "2013-05-30 21:05:09"
      content: "heatsink: That's the sort of situation where you really want O'Caml modules. :-)"
    - id: 6130
      author: Jimmy Koppel
      date: "2013-05-31 03:38:34"
      content: |
        heatsink:
        
        There is indeed a single-parameter solution. You've shown how to construct a two-sorted AST using a fixpoint for pairs. In this encoding, separate constructions would be needed for more sorts. However, pairs (*,*) are isomorphic to functions (2 -&gt; *), and an n-tuple is isomorphic to (n -&gt; *). If we approximate n with *, we can get away with the single, mono-kinded higher order fixpoint of kind ((* -&gt; *) -&gt; (* -&gt; *)) -&gt; (* -&gt; *).  We can thus rewrite this:
        
        newtype Fix f i = f (Fix f l) i
        data (:+:) f g e l = L (f e l) | R (g e l)
        
        data ExpL
        data FunL
        
        data ExpF e i where
          ...
          Lambda :: e FunL -&gt; ExpF e ExpL
          App :: e ExpL -&gt; e ExpL -&gt; ExpF e ExpL
        
        data FunF e i where
          Fun :: Var -&gt; e ExpL -&gt; FunF e FunL
        
        type Sig = FunF :+: ExpF
        
        type Term = Fix Sig
        
        type Exp = Term ExpL
        type Fun = Term FunL
        
        type (:&amp;:) f a e i = (f e i) :&amp;: a
        type TypeTerm = Fix (Sig :&amp;: Type)
        
        Here, Exp has the rather pleasing interpretation of labeled trees whose topmost label is "ExpL."
        
        This idea is laid out in more detail in "Generic programming with fixed points for mutually recursive datatypes" (ICFP '09), and is used in the multirec and compdata libraries.
    - id: 7228
      author: mpickering
      date: "2014-07-24 18:26:56"
      content: Note that now with Pattern Synonyms (in haskell since 7.8) have made it much easier to maintain an interface whilst performing these changes than before.
    - id: 19924
      author: Rich
      date: "2015-12-21 06:35:06"
      content: |
        I'll give you another solution I tried, with the caveat that it's *not* a good solution:
        
        Keep a weak hashtable around which maps your original (untyped) tree nodes, by address, to your node types.
        
        As I said, I tried this approach for an HTML/CSS parser I was writing, but I couldn't get the implementation of the weak hashtable to work well.  In particular for whatever reason my hashtable never freed any entries when the tree was freed.
    - id: 22046
      author: "Lessons learned building a toy compiler | Artificia Intelligence"
      date: "2017-07-07 21:28:13"
      content: "[&#8230;] Can you define your AST in a way that all semantic errors become type errors? This blog post The AST Typing Problem by Edward Z. Yang is a good introduction. I might write another post sometime later explaining a [&#8230;]"
    - id: 22144
      author: Edward Kmett
      date: "2017-11-15 18:18:48"
      content: |
        Tying this together with some of your more recent work:
        
        This encodes rather nicely if you adapt the "Trees that Grow" approach to Backpack!
    - id: 22150
      author: Benedict Gaster
      date: "2017-11-20 06:06:54"
      content: |
        Edward Kmett said:
        &gt; This encodes rather nicely if you adapt the “Trees that Grow” approach to Backpack!
        
        I'd been thinking about a solution to this problem for the last couple of days, found this blog via GHC comments and then found this paper via your comment, which was just the solution I was looking for. A couple of hours this morning refactoring... Thanks!
---

This [Lambda the Ultimate post (dated 2010)](http://lambda-the-ultimate.org/node/4170) describes a rather universal problem faced by compiler writers: how does one go about adding “extra information” (e.g. types) to an AST? (The post itself divides the problem into three components: adding the information to the data types, using the information to inform the construction of the node, and using the information to inform the destruction of a node—but I’m really only interested in the question of how you define your data type, not do things to it.) In this post, I want to sum up ways of solving the problem which were described in this post, and also take a look at what some real world compilers do. The running example lambda calculus looks like the following:

    data Exp = Num Int
             | Bool Bool
             | Var Var
             | If Exp Exp Exp
             | Lambda Var Exp
             | App Exp Exp
    data Type = TyInt | TyBool | TyArrow Type Type

# Separate IR where nodes are decorated with types

The low-tech solution: if you need a new version of the IR with more information, just define a new IR type where each node can also carry the information. A trick to make these definitions more concise is to make a mutually recursive data structure. [\[1\]](http://lambda-the-ultimate.org/node/4170#comment-63834)

    type TExp = (TExp', Type)
    data TExp' = TNum Int
               | TBool Bool
               | TVar Var
               | TIf TExp TExp TExp
               | TLambda Var TExp
               | TApp TExp TExp

Despite (or perhaps because of) it’s simplicity, this approach is extremely popular among many compilers, especially in the ML community. A few examples include OCaml (parsetree/typedtree), MLton (AST/CoreML) and Ikarus Scheme. Part of the reason for this is that the transition from frontend language to typed language also comes with some other changes, and when a new AST is defined those changes can be combined in too.

# Nullable field

The unprincipled solution: use one AST, but have an optional field in which you can slot in the information. [\[2\]](http://lambda-the-ultimate.org/node/4170#comment-63832)

    type TExp = (TExp', Maybe Type)
    data TExp' = TNum Int
               | TBool Bool
               | TVar Var
               | TIf TExp TExp TExp
               | TLambda Var TExp
               | TApp TExp TExp

Presented without further comment.

# Explicit typing

While closely related to the separate IR solution, an explicitly typed IR takes the approach of not decorating each node with a type, but arranging that the type of any given node can be quickly computed using only local information. [\[3\]](http://lambda-the-ultimate.org/node/4170#comment-63884)

    data TExp = TNum Int
              | TBool Bool
              | TVar Var
              | TIf TExp TExp TExp
              | TLambda Var Type TExp
              | TApp TExp TExp

Here, the difference between `TExp` and `Exp` is very slight; the `TLambda` is annotated with an explicit type for the binder. As far as type-checking is concerned, this makes a world of difference: we no longer need to look outside a lambda to figure out what the binder could be.

Forcing your IR to be explicitly typed is often a good idea for metatheoretic reasons, as complicated type systems often don’t have decidable inference algorithms. Both GHC’s core IR, Ur/Web's core and Coq are explicitly typed in this way.

# Two-level types

By deferring when you tie the knot of a recursive data-structure, you can arrange for the base functor to do double-duty for the untyped and typed representations. [\[4\]](http://lambda-the-ultimate.org/node/4170#comment-63836)

    data ExpF a = Num Int
                | Bool Bool
                | Var Var
                | If a a a
                | Lambda Var a
                | App a a
    newtype Exp = Exp (ExpF Exp)
    newtype TExp = TExp (ExpF TExp, Type)

The Coq kernel uses this to define its expression type, although it doesn’t use it to define an untyped variant.

# (Lazy) Attribute grammars

I don’t claim to understand this approach too well, but essentially it is a programming model distinct from usual algebraic data types which associates attributes over nodes of a tree. In some sense, it can be thought as a memoized function from AST nodes to the attributes. Many compilers do utlize maps, but only for top-level declarations. [\[5\]](http://lambda-the-ultimate.org/node/4170#comment-63903)

# Closing remarks

There were a few things that I did not mention here which came up in the discussion. One participant suggested using [polymorphic variants](http://lambda-the-ultimate.org/node/4170#comment-63842 (polymorphic variants)) to define the data type; this doesn’t help much with adding extra information but allows for different ways of writing traversal functions. Indeed, traversal is one of the big concerns, and the mention of [generic programming](http://lambda-the-ultimate.org/node/4170#comment-63846) also is targeted at this problem.

As for my preference? It’s hard to say. I’ve worked with compilers mostly written in the “define a new IR style”, and while the initial outlay of defining two data structures is quite annoying, it is mostly a fixed cost. What’s yours?

**Also, a question.** Is there a presentation of the conventional set of annotations needed to get explicitly typed System F?
