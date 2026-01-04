---
title: "Highlights of ICFP"
date: 2013-09-29 23:58:35
slug: 
draft: true
categories: [Miscellaneous]
---

This year's ICFP having come and gone, I’d like to reflect on the papers presented which I particularly liked, and give them some mini-plugs. This article is completely, 100% my own opinion, and I’ve divided it into three sections: “Things I want to use”, “Cool theoretical results” and “Unexpected gems”.

# Things I want to use

These papers presented results that made me think, “Wow, I want to use that technique, the next time I want to solve that problem!”

------------------------------------------------------------------------

Next time I want to implement a **type checker**, I want to use [Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism](http://www.cs.cmu.edu/~joshuad/papers/bidir/) by Joshua Dunfield and Neelakantan R. Krishnaswami. As the search for non Damas-Milner type inference proceeds afoot, bidirectional typechecking offers one particularly attractive mechanism to achieve it. The polymorphism problem shows up immediately when you attempt to implement a bidirectional type-checker: the problem is that you are asked to commit to an instantiation for a type parameter when you don't really want to. This paper offers a simple method of how to handle this by judicious application of existentials and careful engineering of definitions. I am told that Neel was investigating this topic because he needed a type checker for his FRP language, and came across Joshua's previous work on the subject. However, the proofs for his old algorithm *didn't actually work*, so they set off to do things properly: this paper is the result.

One of the important limitations of this work is that it cannot handle impredicative types *at all* (that is, even if you are willing to provide arbitrary type annotations, it will not work.) During some post-talk discussion, it was mentioned that you could probably work around this by building some support into your data types. Of course, Neel and Josh are off building even more complicated type checkers based on this method, so stay on the lookout here.

Purely theoretically speaking, I also learned something interesting from the talk: when building a type inference system, you can only pick two of these three desirable properties, due to undecidability results:

1.  Preservation of types under η-reduction (and expansion),
2.  Keeps standard types of System F,
3.  Allows impredicative instantiation of ∀

------------------------------------------------------------------------

a

# Cool theoretical results

Programmers argue a lot about whether or not call-by-value or lazy evaluation is better. In [Weak Optimality, and the Meaning of Sharing](http://dl.acm.org/citation.cfm?id=2500606), Thibaut Balabonski shows that lazy evaluation is in fact *optimal*... in an appropriate sense, of course. It’s a theory result: the sense of optimality is minimum number of reductions, and Balabonski considers this question in (a very reasonable) restriction of the weak lambda-calculus, where one is not allowed to evaluate *under* binders, since the problem of finding optimal reduction sequences is undecidable otherwise. In fact, it’s undecidable for the weak lambda-calculus without sharing too—but that didn’t stop Balabonski from showing that with sharing, lazy evaluation takes the same number of steps as the undecidable strategy without sharing.

------------------------------------------------------------------------

The very first time I encountered the multifarious recursion schemes of Haskell was probably [this catalogue](http://comonad.com/reader/2009/recursion-schemes/) of Edward Kmett’s. At the time, I thought, “Man, who came up with all of these random recursion schemes; there’s no structure or reason to them.” Little did I know that people were working on unifying these schemes. In [Unifying Structured Recursion Schemes](http://www.cs.ox.ac.uk/publications/publication6761-abstract.html), Ralf Hinze, Nicolas Wu and Jeremy Gibbons take the two preexisting unifying schemes, adjoint folds and comonadic folds, and show adjoint folds subsume comonadic folds! Bravo; now I can sleep well at night, knowing that there is no need to learn catamorphisms, anamorphisms, mutumorphisms, apomorphisms, or any of the other gaggle of recursion schemes; all I need to do is learn adjunctions and everything falls out naturally...

# Unexpected gems
