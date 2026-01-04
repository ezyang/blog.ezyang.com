---
title: "Safety first: FFI and threading"
date: 2010-07-09 09:00:05
slug: safety-first-ffi-and-threading
categories: [C, Haskell]
comments:
    - id: 655
      author: Simon Marlow
      date: "2010-07-09 15:52:47"
      content: |
        It's true that an unsafe foreign call can't be pre-empted, but that's not the same as a callback - a callback from an unsafe foreign call will result in a crash.
        
        Full details about the design of the system are in this paper:
        http://www.haskell.org/~simonmar/bib/concffi04_abstract.html
        
        I have to point out that forkOS doesn't do what you think it does.  Admittedly it is very badly named, and that has lead to a lot of misconceptions.  Here are the docs:
        
        http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Concurrent.html#v%3AforkOS
        
        note the part "Using forkOS  instead of forkIO  makes no difference at all to the scheduling behaviour of the Haskell runtime system".  A forkOS thread makes all its foreign calls using the same OS thread; that's the only difference between forkOS and forkIO.
        
        Regarding your question "I believe that before an unsafe call, Haskell suspends the operation of all real operating system threads", that's not the case.  An unsafe foreign call is implemented as an inline C function call, no synchronisation or anything happens around it.  In contrast, safe calls are more expensive and involve taking and releasing locks, and possibly creating a new OS thread.
    - id: 656
      author: Edward Z. Yang
      date: "2010-07-09 15:59:42"
      content: "Thanks for the comment Simon! I’ll go and fix the post contents."
    - id: 657
      author: Maciej Piechotka
      date: "2010-07-09 17:02:05"
      content: |
        I disagree that "Don’t use unsafe in your FFI imports! We really mean it!". If call is short (for example 20 or 100 execution-line C without I/O) the unsafe is overkill - you want to do this as fast as possible.
        
        Generally - I think that C-methods-that-don't-call-back-and-are-long are rather exceptions then rule. Not using unsafe is extream that will slow down FFI. Of course given its name it should be handled with care but FFI have other methods of shooting in feet (like Int -&gt; Int vs. Int -&gt; IO Int).
    - id: 658
      author: Edward Z. Yang
      date: "2010-07-09 17:14:57"
      content: "Maciej, perhaps my perception is colored by the codebase (a SAT solver, essentially) I’m currently working in. :-) My FFI calls are low frequency and take a long time. If you’re building FFI bindings to some library that gives you access to a processor’s vector operations, yes, the point of the binding is to be fast, and you need to use unsafe. But if your building FFI bindings to hook up to some large, existing system, better play it safe first and then mark things unsafe when you're tuning."
    - id: 10698
      author: David Barbour
      date: "2014-12-05 19:47:38"
      content: |
        This blog post seems to be out of date now. 'unsafe' blocks only one OS thread in a `-threaded` application.  See:
        
        http://blog.melding-monads.com/2011/10/24/concurrency-and-foreign-functions-in-the-glasgow-haskell-compiler/
    - id: 10850
      author: Edward Z. Yang
      date: "2014-12-08 16:56:27"
      content: "David: Your comment led me to revisit the code sample in this post. As it turns out, you are right that an unsafe call will block only one capability, but this was true even when this post was written. The code in this post still blocks. So what is going on? The answer is: bound threads. I'll write a follow up post explaining."
    - id: 16393
      author: Fylwind
      date: "2015-09-02 17:26:30"
      content: |
        Link to the paper has died. This one is still available:
        http://community.haskell.org/~simonmar/bib/concffi04_abstract.html
---

**Update.** While this blog post presents two true facts, it gets the causal relationship between the two facts wrong. [Here is the correction.](http://blog.ezyang.com/2014/12/unintended-consequences-bound-threads-and-unsafe-ffi-calls/)

*Attention conservation notice.* Don’t use `unsafe` in your FFI imports! We really mean it!

Consider the following example in from an old version of Haskellwiki’s [FFI introduction](http://www.haskell.org/haskellwiki/?title=FFI_Introduction&oldid=33660):

    {-# INCLUDE <math.h> #-}
    {-# LANGUAGE ForeignFunctionInterface #-}
    module FfiExample where
    import Foreign.C -- get the C types

    -- pure function
    -- "unsafe" means it's slightly faster but can't callback to haskell
    foreign import ccall unsafe "sin" c_sin :: CDouble -> CDouble
    sin :: Double -> Double
    sin d = realToFrac (c_sin (realToFrac d))

The comment blithely notes that the function can’t “callback to Haskell.” Someone first learning about the FFI might think, “Oh, that means I can put most `unsafe` on most of my FFI declarations, since I’m not going to do anything advanced like call back to Haskell.”

Oh my friend, if only it were that simple!

Recall that when you create a thread in Haskell with `forkIO`, you’re not creating a real operating system thread; you’re creating a green thread that Haskell’s runtime system manages across its pool of operating system threads. This is usually very good: real threads are heavyweight, but Haskell threads are light and you can use a lot of them without paying too much. But here’s the rub:

<div class="container center">

The runtime system cannot preempt unsafe FFI calls!

</div>

In particular, when you invoke an `unsafe` FFI import, you effectively suspend everything else going on in the system: Haskell is not able to preempt it (in particular `unsafe` indicated that there was no need to save the state of the RTS), and the foreign code will keep running by itself until it finishes.

Don’t believe me? Try it out yourself (I conducted my tests on 6.12.1). You’ll need a few files:

    /* cbit.c */
    #include <stdio.h>
    int bottom(int a) {
        while (1) {printf("%d\n", a);sleep(1);}
        return a;
    }

    /* cbit.h */
    int bottom(int a);

And `UnsafeFFITest.hs`:

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

Compile and run the relevant files with:

    gcc -c -o cbit.o cbit.c
    ghc -threaded --make UnsafeFFITest.hs cbit.o
    ./UnsafeFFITest +RTS -N4

The output you see should be similar to this:

    ezyang@javelin:~/Dev/haskell/unsafe-ffi$ ./UnsafeFFITest +RTS -N2
    1
    "Pass (expected)"
    2
    1
    2
    1
    2

The first call played nice and let Haskell move along, but the second call didn’t. Some things to try for yourself include swapping the order of the forks, using `forkOS` (which many people, including myself, incorrectly assumed creates another operating system call) and changing the RTS option `-N`.

What does this mean? Essentially, only if you’re *really* sure Haskell will never have to preempt your C call (which I would not be comfortable saying except for the smallest, purest C functions), don’t use `unsafe`. It’s not worth it. Safety first!

*Postscript.* Thanks `#haskell` for helping me hash out this line of thought (I’d run into this behavior earlier, but it hadn’t occurred to me that it was bloggable.)

*Postscript 2.* Thanks to Simon Marlow for clarifying some mistakes that I made in my original treatment of the topic. If you’re interested in more details about the interaction of concurrency and the FFI, check out the paper he pointed to: [Extending the Haskell Foreign Function Interface with Concurrency](http://www.haskell.org/~simonmar/bib/concffi04_abstract.html).
