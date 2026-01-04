---
title: "BlockedIndefinitelyOnMVar"
date: 2011-07-27 09:00:08
slug: blockedindefinitelyonmvar
categories: [GHC]
comments:
    - id: 2860
      author: Brandon Simmons
      date: "2011-07-27 18:29:15"
      content: |
        I'm glad my question inspired such a great post! This was very illuminating, although as a humble haskell user I can't say I'm satisfied with the behavior here. 
        
        For instance in 'main2': surely by the time there are "no other references" to the MVar 'complete' then the main thread should no longer be blocked?
        
        I don't think the fact that certain cases of deadlock are undecidable is  my issue (i.e. that if you write code that deadlocks it might deadlock is understandable), instead I think my issues are that:
        
        1) read vs. write on an MVar is not part of the analysis, which would clear up 'main3'
        
        2) GHC seems overzealous in raising BlockedIndefinitelyOnMVar. I still don't really see why 'main2' behaves as it does
        
        Again, just my layhaskellperson's view. Things may click for me more later.
        
        B.t.w. If you want to check in on my SO question with a link here I'll give you the karma, otherwise point people here in my original question
        
        http://stackoverflow.com/questions/6847307/understanding-blockedindefinitelyonmvar-in-concurrent-code/6847609#6847609
        
        Thanks once again
    - id: 2861
      author: Edward Z. Yang
      date: "2011-07-27 18:49:23"
      content: |
        main2: Oh, that's a good catch, which I did not notice when I was originally writing the program. Here is the illustrative counter-example:
        
        <pre>main = do
            lock <- newEmptyMVar
            complete <- newEmptyMVar
            forkIO $ takeMVar lock `finally` putMVar complete ()
            yield
            performGC
            takeMVar complete</pre>
        
        So if the other thread manages to become blocked (thus removing its reference to complete) and a GC manages to happen before the main thread gets blocked, we successfully throw BlockedIndefinitelyOnMVar.
        
        One of the main points to see here is that you are very much subject to the whimsy of the garbage collector. GHC doesn't guarantee that it will perform analysis in a timely manner; it just guarantees that it will be done eventually. (Something similar applies for finalizers.)
        
        main3: For this particular one, yes.
        
        I'd like to reemphasize: this exception is an artifact of something that was, under-the-hood, easy to do. If the code doesn't know anything about these exceptions, its behavior is exactly equivalent to that of a deadlocked program. But it was not written with "heavy use" in mind, and these strange examples are a good demonstration of that.
    - id: 2866
      author: Brandon Simmons
      date: "2011-07-29 13:17:03"
      content: |
        &gt; If the code doesn't know anything about these exceptions, its behavior is exactly equivalent to that of a deadlocked program.
        
        Excellent point. Thanks.
    - id: 2874
      author: Simon Marlow
      date: "2011-08-01 08:40:13"
      content: |
        Edward: as usual, you're spot on with this article.  I think of BlockedIndefinitelyOnMVar as a debugging tool, nothing more.  Perhaps we should clarify that in the documentation.
        
        There's no way to separate reading from writing.  Consider
        
          thread m = if  then takeMVar m else putMVar m ()
    - id: 6342
      author: Edward Z. Yang
      date: "2014-01-06 00:15:01"
      content: "Several years later, I rehappened upon this post, and realized something: while it is true that it is not decidable to decide whether or not a thread is indefinitely blocked on an MVar, it is decidable to determine if a set of threads is in deadlock, for an appropriate definition of deadlock: that is, if the resource-requested graph of the threads contains a cycle. Such a notion doesn't exactly exist for MVars, because it's always possible for someone to waltz along and put into an MVar they have no business putting into."
---

*This post was adapted from a post I made to the glasgow-haskell-users list.*

According to [Control.Exception](http://haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Exception.html#t%3ABlockedIndefinitelyOnMVar), the `BlockedIndefinitelyOnMVar` exception (and related exception `BlockedIndefinitelyOnSTM`) is thrown when “the thread is blocked on an MVar, but there are no other references to the MVar so it can't ever continue.” The description is actually reasonably precise, but it is easy to misinterpret. Fully understanding how this exception works requires some extra documentation from [Control.Concurrent](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent.html) as well as an intuitive feel for how garbage collection in GHC works with respects to Haskell’s green threads.

Here’s the litmus test: can you predict what these three programs will do? :

    main1 = do
        lock <- newMVar ()
        forkIO $ takeMVar lock
        forkIO $ takeMVar lock
        threadDelay 1000 -- let threads run
        performGC -- trigger exception
        threadDelay 1000

    main2 = do
        lock <- newEmptyMVar
        complete <- newEmptyMVar
        forkIO $ takeMVar lock `finally` putMVar complete ()
        takeMVar complete

    main3 = do
        lock <- newEmptyMVar
        forkIO $ takeMVar lock `finally` putMVar lock ()
        let loop = do
            b <- isEmptyMVar lock
            if b
                then yield >> performGC >> loop
                else return ()
        loop

Try not to peek. For a hint, check the documentation for [forkIO](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent.html#v:forkIO).

------------------------------------------------------------------------

The first program gives no output, even though the `threadDelay` ostensibly lets both forked threads get scheduled, run, and deadlocked. In fact, `BlockedIndefinitelyOnMVar` is raised, and the reason you don’t see it is because `forkIO` installs an exception handler that mutes this exception, along with `BlockedIndefinitelyOnSTM` and `ThreadKilled`. You can install your own exception handler using `catch` and co.

There is an interesting extra set of incants at the end of this program that ensure, with high probability, that the threads get scheduled and the `BlockedIndefinitelyOnMVar` exception gets thrown. Notice that the exception only gets thrown when “no references are left to the MVar.” Since Haskell is a garbage collected language, the only time it finds out references are gone are when garbage collections happen, so you need to make sure one of those occurs before you see one of these errors.

One implication of this is that GHC does not magically know which thread to throw the exception at to “unwedge” the program: instead, it will just throw `BlockedIndefinitelyOnMVar` at all of the deadlocked threads, including (if applicable) the main thread. This behavior is demonstrated in the second program, where the program terminates with `BlockedIndefinitelyOnMVar` because the main thread gets a copy of the exception, even though the `finally` handler of the child thread would have resolved the deadlock. Try replacing the last line with `` takeMVar complete `catch` \BlockedIndefinitelyOnMVar -> takeMVar complete >> putStrLn "done" ``. It’s pretty hilarious.

The last program considers what it means for an `MVar` to be “reachable”. As it deadlocks silently, this must mean the `MVar` stayed reachable; and indeed, our reference `isEmptyMVar` prevents the `MVar` from ever going dead, and thus we loop infinitely, *even though* there was no possibility of the `MVar` getting filled in. GHC only knows that a thread can be considered garbage (which results in the exception being thrown) if there are no references to it. Who is holding a reference to the thread? The `MVar`, as the thread is *blocking* on this data structure and has added itself to the blocking list of this. Who is keeping the `MVar` alive? Why, our closure that contains a call to `isEmptyMVar`. So the thread stays. The general rule is as follows: if a thread is blocked on an `MVar` which is accessible from a non-blocked thread, the thread sticks around. While there are some obvious cases (which GHC doesn’t manage) where the `MVar` is obviously dead, even if there are references sticking around to it, figuring this out in general is undecidable. (Exercise: Write a program that solves the halting problem if GHC was able to figure this out in general.)

To conclude, without a bit of work (which would be, by the way, quite interesting to see), `BlockedIndefinitelyOnMVar` is not an obviously useful mechanism for giving your Haskell programs deadlock protection. Instead, you are invited to think of it as a way of garbage collecting threads that would have otherwise languished around forever: by default, a deadlocked thread is silent (except in memory usage.) The fact that an exception shows up was convenient, operationally speaking, but should not be relied on.
