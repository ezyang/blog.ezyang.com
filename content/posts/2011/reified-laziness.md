---
title: "Reified laziness"
date: 2011-05-13 21:45:11
slug: reified-laziness
categories: [Haskell]
comments:
    - id: 2425
      author: Wu Xingbo
      date: "2011-05-14 23:42:20"
      content: "Very interesting. When blocked, it said \".......\" in mind:(\"I really don't know what it is!\"). But after a while it returns and say \"what you want is xxx\". regardless of how long you have been waiting for, you get a \"pure\" logical interface."
    - id: 2426
      author: Edward Z. Yang
      date: "2011-05-15 07:25:08"
      content: |
        Yeah! So the caller can't actually tell if it started trying to calculate the value, or it's just waiting for someone to tell you what it is.
        
        It should be noted that you do need to wrap IVars in another interface to produce a more traditional laziness mechanism (since, on the first get to an IVar, the lazy thread of execution should be started up.) Par is all about letting you move these executions around to be earlier or later, depending on your knowledge of the problem.
    - id: 2481
      author: Paul Liu
      date: "2011-05-20 15:57:29"
      content: "IVar may be lazy, but not call-by-need."
---

Short post, longer ones in progress.

One of the really neat things about the [Par monad](http://hackage.haskell.org/packages/archive/monad-par/0.1.0.1/doc/html/Control-Monad-Par.html) is how it explicitly reifies laziness, using a little structure called an `IVar` (also known in the literature as *I-structures*). An `IVar` is a little bit like an `MVar`, except that once you’ve put a value in one, you can never take it out again (and you’re not allowed to put another value in.) In fact, this precisely corresponds to lazy evaluation.

![image](/img/ivar.png)

The key difference is that an `IVar` splits up the naming of a lazy variable (the creation of the `IVar`), and specification of whatever code will produce the result of the variable (the `put` operation on an `IVar`). Any `get` to an empty `IVar` will block, much the same way a second attempt to evaluate a thunk that is being evaluated will block (a process called blackholing), and will be fulfilled once the “lazy computation” completes (when the `put` occurs.)

It is interesting to note that this construction was adopted precisely because laziness was making it really, really hard to reason about parallelism. It also provides some guidance for languages who might want to provide laziness as a built-in construct (hint: implementing it as a memoized thunk might not be the best idea!)
