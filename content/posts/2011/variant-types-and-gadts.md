---
title: "Variant types and GADTs"
date: 2011-07-22 09:00:21
slug: variant-types-and-gadts
categories: [Haskell, OCaml]
comments:
    - id: 2838
      author: Sjoerd Visscher
      date: "2011-07-22 13:04:47"
      content: |
        This seems to work:
        
        data A
        data B
        data C
        data X
        data SumABC a b c where
          A :: SumABC A X X
          B :: SumABC X B X
          C :: SumABC X X C
        
        If you want to allow A or B, but not C, use SumABC a b X.
    - id: 2839
      author: Sjoerd Visscher
      date: "2011-07-22 13:15:45"
      content: |
        Or even shorter:
        
        data Y
        data N
        data SumABC a b c where
          A :: SumABC Y N N
          B :: SumABC N Y N
          C :: SumABC N N Y
    - id: 2841
      author: Edward Z. Yang
      date: "2011-07-22 19:38:13"
      content: "Heh, I suppose it does. GHC might get mad at you for using linearly many type variables though! :-)"
    - id: 2842
      author: Daniel Schüssler
      date: "2011-07-22 20:15:01"
      content: |
        Suggested solution: https://gist.github.com/1100723
        
        No change in the variant type needed, though making the subset definition boilerplate is a job for TH :)
    - id: 2843
      author: Anonymous
      date: "2011-07-22 22:22:11"
      content: "Are you aware of Wouter Swierstra's Data types à la carte paper? He describes a very clever method of solving your problem in general using Haskell."
    - id: 2844
      author: Anonymous
      date: "2011-07-22 23:51:30"
      content: "If you're on a 32-bit machine, those two bits needed for the closed version would probably be put into a 4-byte integer anyway, since that's the size of a word and the generated code has to obey alignment restrictions."
    - id: 2845
      author: Edward Z. Yang
      date: "2011-07-23 03:28:44"
      content: |
        Daniel: Classy. :-)
        
        Anonymous: Wouter Swierstra’s paper is very cute, but I think it solves something of a different problem. I'd love to be shown to be wrong.
        
        Anonymous 2: Right, the tag for the data is not much more efficient. The real benefit is dispatching on these tags. If the tags are open (range over all 32-bit integers), you cannot use jump tables; if you know they range over a small subset, you can.
---

OCaml supports anonymous variant types, of the form `` type a = [`Foo of int | `Bar of bool] ``, with the appropriate subtyping relations. Subtyping is, in general, kind of tricky, so I have been using these variant types fairly conservatively. (Even if a feature gives you too much rope, it can be manageable and useful if you use discipline.) Indeed, they are remarkably handy for one particular use-case for which I would have normally deployed GADTs. This is the “Combining multiple sum types into a single sum type” use-case.

Consider the following program in Haskell:

    data A = Foo Int | Bar Bool
    data B = Baz Char | Qux

If one would like to define the moral equivalent of A plus B, the most naive way to do this is:

    data AorB = A A | B B

But this kind of sucks: I would have preferred some kind of flat namespace by which I could refer to `A` and `B` (also, this encoding is not equivalent to `data AorB = Foo Int | Bar Bool | Baz Char | Qux` in the presence of laziness.) If you use normal sum types in OCaml, you’re similarly out of luck. However, you can handily manage this if you use variant types:

    type a = [`Foo of int | `Bar of bool]
    type b = [`Baz of char | `Quz]
    type a_or_b = [a | b]

Sweet! Note that we’re not using the full generality of variant types: I will only ever refer to these variant constructors in the context of `a`, `b` or `a_or_b`: anonymous variant types are right out. This prevents coercion messes.

I can actually pull this off in Haskell with GADTs, although it’s certainly not obvious for a beginning programmer:

    data A
    data B
    data AorB t where
      Foo :: Int -> AorB A
      Bar :: Bool -> AorB A
      Baz :: Char -> AorB B
      Quz :: AorB B

To pattern match against all constructors, I specify the type `AorB t`; to only do `A` I use `AorB A`, and to only do `B` I use `AorB B`. Don’t ask me how to specify arbitrary subsets of more than two combined sum types. (Solutions in the comment section welcome, though they will be graded on clarity.)

The Haskell approach does have one advantage, which is that the sum type is still closed. Since OCaml can make no such guarantee, things like `bin-prot` need to use up a full honking four-bytes to specify what variant it is (they hash the name and use that as a unique identifier) rather than the two bits (but more likely, one byte) needed here. This also means for more efficient generated code.
