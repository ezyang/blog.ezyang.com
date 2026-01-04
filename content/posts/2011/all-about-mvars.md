---
title: "All about MVars"
date: 2011-02-02 09:00:37
slug: all-about-mvars
categories: [Haskell]
comments:
    - id: 1747
      author: Neil Brown
      date: "2011-02-02 11:17:47"
      content: "Your program deadlocks (and probably throws an exception for the deadlock?), assuming the puts are queued in FIFO order.  A full MVar, one take and two puts doesn't add up :-)  I always thought readMVar, modifyMVar &amp; co. should be removed, because takeMVar/putMVars are the real atomic primitives, and the rest are confusing compositions with race hazards, and obscure what MVars really are: one-place-buffered channels."
    - id: 1748
      author: snk_kid
      date: "2011-02-02 12:46:01"
      content: "I have some examples which I ported from the lazyfoo tutorials (SDL),  lesson33, lesson34, lesson35: https://github.com/snkkid/LazyFooHaskell"
    - id: 1749
      author: Steve
      date: "2011-02-02 15:31:53"
      content: "Re the delayed evaluation causing it to happen in the reading thread.  Does there exist some similar construct on top of MVars that forces evaluation on put?"
    - id: 1750
      author: Edward Z. Yang
      date: "2011-02-02 16:25:27"
      content: "Steve: Yep! Check out the strict-concurrency package."
    - id: 1751
      author: Antoine
      date: "2011-02-03 20:45:54"
      content: |
        I've always thought of the MVar functions as being in two separate families:
        
        1. withMVar, readMVar, modifyMVar, &amp;c.
        
        2. takeMVar &amp; putMVar
        
        And everything will be okay as long as you don't mix &amp; match functions between families.
        
        I guess these fit points one and two of your use-cases above.
        
        Sometimes I put together a type for the binary semaphore:
        
        &gt; data Lock a = L (MVar ()) a
    - id: 1755
      author: Paul
      date: "2011-02-04 15:52:58"
      content: "Regarding your program, I focussed on the putMVar and the readMVar, and just missed the newMVar bit, thinking is was created empty. I couldn't then understand the reason for the deadlock."
    - id: 1760
      author: Applicativity...
      date: "2011-02-08 08:36:31"
      content: "There's another point in favor comparing MVars to STM; they will (or at least, I think so) generally be faster.  Specifically, STM has  (can have?) an overhead that's quadratic in the number of TVars touched, while MVars should be linear (i.e. a constant cost per \"touch\")."
    - id: 1761
      author: Edward Z. Yang
      date: "2011-02-08 12:49:24"
      content: "Applicativity, that's an interesting statement. John Launchbury did some experiments a while back (unfortunately, I don't think they were ever published, so if someone wants to reproduce them that would be killer) which showed that MVars actually performed worse than STM for large amounts of variables. It would be good to see some data on the matter."
    - id: 20919
      author: David Turner
      date: "2016-06-29 11:38:02"
      content: |
        The answer to your "What does it do?" seems to have changed with base-4.7. Previously the child completed and the parent deadlocked, but for base-4.7 and later the parent completes cleanly because readMVar became atomic.
        
        Just noting it here for posterity...
        
        Cheers,
---

I recently took the time out to rewrite [the MVar documentation](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-MVar.html), which as it stands is fairly sparse (the introduction section rather tersely states "synchronising variables"; though to the credit of the original writers the inline documentation for the data type and its fundamental operations is fairly fleshed out.) I've reproduced my new introduction here.

While researching this documentation, I discovered something new about how MVars worked, which is encapsulated in this program. What does it do? :

    import Control.Concurrent.MVar
    import Control.Concurrent     
    main = do
        x <- newMVar 0
        forkIO $ do
            putMVar x 1
            putStrLn "child done"
        threadDelay 100
        readMVar x
        putStrLn "parent done"

------------------------------------------------------------------------

An `MVar t` is mutable location that is either empty or contains a value of type `t`. It has two fundamental operations: `putMVar` which fills an MVar if it is empty and blocks otherwise, and `takeMVar` which empties an MVar if it is full and blocks otherwise. They can be used in multiple different ways:

1.  As synchronized mutable variables,
2.  As channels, with `takeMVar` and `putMVar` as receive and send, and
3.  As a binary semaphore `MVar ()`, with `takeMVar` and `putMVar` as wait and signal.

They were introduced in the paper "Concurrent Haskell" by Simon Peyton Jones, Andrew Gordon and Sigbjorn Finne, though some details of their implementation have since then changed (in particular, a put on a full MVar used to error, but now merely blocks.)

# Applicability

MVars offer more flexibility than IORefs, but less flexibility than STM. They are appropriate for building synchronization primitives and performing simple interthread communication; however they are very simple and susceptible to race conditions, deadlocks or uncaught exceptions. Do not use them if you need perform larger atomic operations such as reading from multiple variables: use 'STM' instead.

In particular, the "bigger" functions in this module (`readMVar`, `swapMVar`, `withMVar`, `modifyMVar_` and `modifyMVar`) are simply compositions a `takeMVar` followed by a `putMVar` with exception safety. These only have atomicity guarantees if all other threads perform a `takeMVar` before a `putMVar` as well; otherwise, they may block.

# Fairness

The original paper specified that no thread can be blocked indefinitely on an MVar unless another thread holds that MVar indefinitely. This implementation upholds this fairness property by serving threads blocked on an MVar in a first-in-first-out fashion.

# Gotchas

Like many other Haskell data structures, MVars are lazy. This means that if you place an expensive unevaluated thunk inside an MVar, it will be evaluated by the thread that consumes it, not the thread that produced it. Be sure to `evaluate` values to be placed in an MVar to the appropriate normal form, or utilize a strict MVar provided by the [strict-concurrency package](http://hackage.haskell.org/package/strict-concurrency).

# Example

Consider the following concurrent data structure, a skip channel. This is a channel for an intermittent source of high bandwidth information (for example, mouse movement events.) Writing to the channel never blocks, and reading from the channel only returns the most recent value, or blocks if there are no new values. Multiple readers are supported with a `dupSkipChan` operation.

A skip channel is a pair of MVars: the second MVar is a semaphore for this particular reader: it is full if there is a value in the channel that this reader has not read yet, and empty otherwise.

    import Control.Concurrent.MVar
    import Control.Concurrent     

    data SkipChan a = SkipChan (MVar (a, [MVar ()])) (MVar ())

    newSkipChan :: IO (SkipChan a)
    newSkipChan = do
        sem <- newEmptyMVar
        main <- newMVar (undefined, [sem])
        return (SkipChan main sem)

    putSkipChan :: SkipChan a -> a -> IO ()
    putSkipChan (SkipChan main _) v = do
        (_, sems) <- takeMVar main
        putMVar main (v, [])
        mapM_ (\sem -> putMVar sem ()) sems

    getSkipChan :: SkipChan a -> IO a
    getSkipChan (SkipChan main sem) = do
        takeMVar sem
        (v, sems) <- takeMVar main
        putMVar main (v, sem:sems)
        return v

    dupSkipChan :: SkipChan a -> IO (SkipChan a)
    dupSkipChan (SkipChan main _) = do
        sem <- newEmptyMVar
        (v, sems) <- takeMVar main
        putMVar main (v, sem:sems)
        return (SkipChan main sem)

This example was adapted from the original Concurrent Haskell paper. For more examples of MVars being used to build higher-level synchronization primitives, see [Control.Concurrent.Chan](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-Chan.html) and [Control.Concurrent.QSem](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Concurrent-QSem.html).
