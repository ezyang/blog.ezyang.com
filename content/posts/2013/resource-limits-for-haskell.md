---
title: "Resource limits for Haskell"
date: 2013-04-02 16:36:40
slug: resource-limits-for-haskell
categories: [Haskell]
comments:
    - id: 5994
      author: Shengyi Wang
      date: "2013-04-02 23:11:49"
      content: Congratulations and wish you good luck.
    - id: 5996
      author: gasche
      date: "2013-04-03 04:40:12"
      content: "Have you considered pushing your draft on arXiv? That would be great."
    - id: 5997
      author: Edward Z. Yang
      date: "2013-04-03 05:19:05"
      content: "Oh no, but that would mean I would have to clean up the LaTeX sources! :o) I'll look into it."
    - id: 5998
      author: Mihai Maruseac
      date: "2013-04-03 07:38:30"
      content: "Congrats :)"
    - id: 5999
      author: Omar Abuzzahab
      date: "2013-04-03 11:42:10"
      content: The little post about cost centers was a moment of clarity.  Thanks.
    - id: 6000
      author: Mikhail Glushenkov
      date: "2013-04-03 13:05:54"
      content: |
        Nice paper! One wart I found:
        
        `withCC ccs x (evaluate (f x)) &gt;&gt;= sandboxThunk k` - surely that was meant to be `withCC ccs (evaluate (f x)) ...` since the type of `withCC` is `withCC :: CostCenter -&gt; IO a -&gt; IO a` ? Also you might want to put parentheses around the first half for the sake of people who are hazy on the details of Haskell's operator precedence.
        
        There was also another typo somewhere, but I can't find it now.
    - id: 6002
      author: Edward Z. Yang
      date: "2013-04-03 16:57:38"
      content: "Thanks, I fixed that typo."
    - id: 6003
      author: Jimmy Koppel
      date: "2013-04-04 07:47:00"
      content: "Memory, information flow, and patches to GHC? That's an impressive amount of ezyang in one paper."
    - id: 6375
      author: Paolo G. Giarrusso
      date: "2014-02-04 16:51:06"
      content: "Well, congratulations on getting this accepted at PLDI (http://conferences.inf.ed.ac.uk/pldi2014/acceptedpapers.html)!"
    - id: 6376
      author: Edward Z. Yang
      date: "2014-02-04 17:25:54"
      content: "Thanks Paolo! :)"
---

Last week, I made my very first submission to ICFP! The topic? An old flame of mine: how to bound space usage of Haskell programs.

> We describe the first iteration of a resource limits system for Haskell, taking advantage of the key observation that resource limits share semantics and implementation strategy with profiling. We pay special attention to the problem of limiting resident memory usage: we describe a simple implementation technique for carrying out incremental heap censuses and describe a novel information-flow control solution for handling forcible resource reclamation. This system is implemented as a set of patches to GHC.

You can get [a copy of the submission here.](http://ezyang.com/papers/ezyang13-rlimits.pdf) I've reproduced below the background section on how profiling Haskell works; if this tickles your fancy, check out the rest of the paper!

------------------------------------------------------------------------

Profiling in Haskell is performed by charging the costs of computation to the “current cost center.” A *cost center* is an abstract, programmer-specified entity to which costs can be charged; only one is active per thread at any given time, and the *cost semantics* determines how the current cost center changes as the program executes. For example, the `scc cc e` expression (set-cost-center) modifies the current cost center during evaluation of `e` to be `cc`. Cost centers are defined statically at compile time.

A cost semantics for Haskell was defined by Sansom et al. (1995) Previously, there had not been a formal account for how to attribute costs in the presence of lazy evaluation and higher-order functions; this paper resolved these questions. The two insights of their paper were the following: first, they articulated that cost attribution should be independent of evaluation order. For the sake of understandability, whether a thunk is evaluated immediately or later should not affect who is charged for it. Secondly, they observed that there are two ways of attributing costs for functions, in direct parallel to the difference between lexical scoping and dynamic scoping.

The principle of order-independent cost-attribution can be seen by this program:

    f x = scc "f" (Just (x * x))
    g x = let Just y = f x in scc "g" y

When `g 4` is invoked, who is charged the cost of evaluating `x * x`? With strict evaluation, it is easy to see that `f` should be charged, since `x * x` is evaluated immediately inside the `scc` expression. Order-independence dictates, then, that even if the execution of `x * x` is deferred to the inside of `scc "g" y`, the cost should *still* be attributed to `f`. In general, `scc "f" x` on a variable `x` is a no-op. In order to implement such a scheme, the current cost-center at the time of the allocation of the thunk must be recorded with the thunk and restored when the thunk is forced.

The difference between lexical scoping and dynamic scoping for function cost attribution can be seen in this example:

    f = scc "f" (\x -> x * x)
    g = \x -> scc "g" (x * x)

What is the difference between these two functions? We are in a situation analogous to the choice for thunks: should the current cost-center be saved along with the closure, and restored upon invocation of the function? If the answer is yes, we are using lexical scoping and the functions are equivalent; if the answer is no, we are using dynamic scoping and the `scc` in `f` is a no-op. The choice GHC has currently adopted for `scc` is dynamic scoping.
