---
title: "Unintended consequences: Bound threads and unsafe FFI calls "
date: 2014-12-08 19:09:33
slug: unintended-consequences-bound-threads-and-unsafe-ffi-calls
categories: [GHC]
comments:
    - id: 10864
      author: Simon Marlow
      date: "2014-12-09 03:07:41"
      content: "Bound and unbound threads are irrelevant here, it's all about which capability the threads are running on.  Even when you make an unsafe call in a bound thread, it is still holding the capability and will prevent other threads on that capability from running (and will block the other capabilities if they want to GC)."
    - id: 10884
      author: Edward Z. Yang
      date: "2014-12-09 19:35:43"
      content: "Hmm, maybe I should have made the explanation clearer: what I meant to say was that you care about bound threads because they need to run on a specific capability, so if someone has gone out to lunch with the capability, you have no recourse (whereas if you weren't bound, you would eventually get migrated to another capability.) I agree that whether or not the unsafe FFI call is running on an unbound thread or a bound thread makes no difference."
    - id: 10906
      author: Simon Marlow
      date: "2014-12-10 08:44:06"
      content: |
        I'm not following you here.
        
        "you care about bound threads because they need to run on a specific capability"
        
        No - you care about bound threads because they need to run on a specific OS thread; the capability is irrelevant.  
        
        "if you weren’t bound, you would eventually get migrated to another capability." 
        
        No - bound threads get migrated in exactly the same way as unbound threads.
        
        Are you mixing up bound threads and forkOn?
    - id: 10917
      author: Edward Z. Yang
      date: "2014-12-10 13:57:41"
      content: "Nah, it sounds like I'd forgotten how bound threads work (despite having studied this a bit back) and said wrong things. Let me try again: we care about bound threads because they need to run on a specific operating system thread. If it wasn't bound, operation of the capability would not block when trying to run the bound thread, when the original operating system thread was not available."
---

A while ago, I wrote a post describing how [unsafe FFI calls could block your entire system](http://blog.ezyang.com/2010/07/safety-first-ffi-and-threading/), and gave the following example of this behavior:

    /* cbit.c */
    #include <stdio.h>
    int bottom(int a) {
        while (1) {printf("%d\n", a);sleep(1);}
        return a;
    }

    /* cbit.h */
    int bottom(int a);

    /* UnsafeFFITest.hs */
    {-# LANGUAGE ForeignFunctionInterface #-}

    import Foreign.C
    import Control.Concurrent

    main = do
        forkIO $ do
            safeBottom 1
            return ()
        yield
        print "Pass (expected)"
        forkIO $ do
            unsafeBottom 2
            return ()
        yield
        print "Pass (not expected)"

    foreign import ccall "cbit.h bottom" safeBottom :: CInt -> IO CInt
    foreign import ccall unsafe "cbit.h bottom" unsafeBottom :: CInt -> IO CInt

In the post, I explained that the reason this occurs is that unsafe FFI calls are not preemptible, so when unsafeBottom loops forever, the Haskell thread can't proceed.

This explanation would make perfect sense except for one problem: the code also hangs even when you run with the multi-threaded runtime system, with multiple operating system threads. David Barbour wrote in wondering if my claim that unsafe calls blocked the entire system was out of date. But the code example definitely does hang on versions of GHC as recent as 7.8.3. Based on the title of this post, can you guess the reason? If you think you know, what do these variants of the program do?

1.  Change `main =` to `main = runInUnboundThread`
2.  Change the second `forkIO` to `forkOn 2`
3.  Add a `yield` before `unsafeBottom`, and another `yield` before `print "Pass (not expected)"`

------------------------------------------------------------------------

The reason why the code blocks, or, more specifically, why the main thread blocks, is because the unsafe FFI call is unpreemptibly running on the operating system thread which the main thread is bound to. Recall, by default, the main thread runs in a bound operating system thread. This means that there is a specific operating system thread which must be used to run code in main. If that thread is blocked by an FFI call, the main thread cannot run, even if there are other worker threads available.

We can thus explain the variants:

1.  `main` is run in an unbound thread, no blocking occurs, and thus the second print runs.
2.  By default, a forked thread is run on the same capability as the thread that spawned it (this is good, because it means no synchronization is necessary) so forcing the bad FFI call to run on a different worker prevents it from blocking main.
3.  Alternately, if a thread yields, it might get rescheduled on a different worker thread, which also prevents main from getting blocked.

So, perhaps the real moral of the story is this: be careful about unsafe FFI calls if you have bound threads. And note: every Haskell program has a bound thread: main!
