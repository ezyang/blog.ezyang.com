---
title: "The GHC scheduler"
date: 2013-01-28 03:00:48
slug: the-ghc-scheduler
categories: [GHC]
comments:
    - id: 5944
      author: Thiago Negri
      date: "2013-01-29 05:29:21"
      content: |
        What is the main difference between `safe` and `unsafe` foreign imports? Why does an `unsafe` foreign import blocks other threads?
        
        Thanks!
    - id: 5945
      author: Jan Stolarek
      date: "2013-01-29 07:38:03"
      content: |
        Thiago: take a look at "The Haskell 98 Foreign Function Interface 1.0 - An Addendum to the Haskell 98 Report", section 3.3, last paragraph.
        
        Ed: Nice post. I don't understand some of it since I'm not hacking on the scheduler right now, but if I ever will your notes will be very valuable.
    - id: 5946
      author: Edward Z. Yang
      date: "2013-01-29 13:23:52"
      content: "Thiago: I’ve also written about this previously http://blog.ezyang.com/2010/07/safety-first-ffi-and-threading/"
    - id: 5949
      author: Alexander Kjeldaas
      date: "2013-01-29 20:33:31"
      content: "Could you comment on when a TSO is *popped* off the run-queue.  The RTS looks great, but I am a little worried about work-stealing or the lack thereof."
    - id: 5950
      author: Edward Z. Yang
      date: "2013-01-29 20:42:54"
      content: "Right; so, TSOs are popped off the run queue after you finish running your previous one, so the work stealing doesn’t work directly that way. However, each round around the scheduler loop, we look and see if there are any idle capabilities, and push our work to them (this is the thread migration case). For sparks, we actually have a work-stealing deque which allows other capabilities to immediately attempt to steal a spark, though I’m not 100% clear on the details."
    - id: 5955
      author: Simon Marlow
      date: "2013-01-31 08:04:17"
      content: |
        Great writeup, would you mind adding a link from the GHC commentary?
        
        One thing that stuck out when reading this is that you're missing the intuition for the difference between adding a thread to the front or the end of the run queue. It's quite simple: if the thread has exhausted its time slice (context_switch /= 0), then it goes to the back of the queue, otherwise, it goes on the front and we keep running it.  It would probably help to distinguish the cases that apply when we have just finished running a thread from the other cases: unblocking a thread and creating a new thread.
    - id: 5960
      author: Edward Z. Yang
      date: "2013-01-31 16:24:27"
      content: |
        I think the best thing to do is to format shift this onto the wiki, so future hackers can update it as necessary. Alas, I don't exactly know how it should be fit into the current http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Scheduler
        
        Re intuition, I think at some point I convinced myself that there were certain pathological cases where a thread would still get put in front even if it had exhausted its time slice. I think this is primarily if a thread continually allocates large blocks. I think I'll go and make this clearer.
    - id: 5973
      author: xxx
      date: "2013-02-08 15:57:45"
      content: "can you explain what happens when the 1kb stack is overflowed?"
    - id: 5974
      author: Dan Haraj
      date: "2013-02-08 18:51:44"
      content: |
        These nitty gritty implementation posts are always delightful to read!
        
        Thread scheduling is potentially related to strange behavior I am dealing with in a program that is compiled with GHC 7.6.1 - threaded -O2. It is very difficult to describe the program without providing the entire rather large and hackish code base, but the phenomenon is quite simple: I have a gui program that I am stress testing by forcing it to process thousands of objects at once. The program has two threads: the main thread and a thread that processes events. Objects are synchronized between the threads by being glorified MVars with attached callbacks.
        
        With +RTS -N1 enabled, around ~1000 objects, the program runs at exactly 50 fps. With +RTS -N2 enabled, it runs at 215 fps. With -N3, there is no change, and with -N4 the program runs at 200 fps. 
        
        As the number of objects increases, the factor reduces until -N2 provides approximately a 2-fold increase in speed. However, interestingly enough, the program runs at exactly 50 fps even when the number of test objects is doubled to 2000.
        
        The fact that I am getting a 4-fold increase in performance with one extra thread and no increase or decrease with more threads makes me think there is some subtlety with thread scheduling I am not getting. The suspicious behavior around 1000-2000 objects, and the fact that the discrepancy eventually disappears for large N only shores up my suspicion.
        
        I'm not sharing this asking for a troubleshoot, but mostly because I am curious whether there is a big difference between -threaded -N1 and -threaded -N2+?
---

I’d like to talk about some nitty-gritty details of GHC’s thread scheduling, discovered over the course of working on stride scheduling for GHC. Most of these choices are merely *implementation* details and are not part of any specification. While these choices shouldn’t be relied upon, they are worth knowing, since many of these details were accreted over the course of many performance bugs, benchmark tests and other battles. In this post, I’ll attempt to give some historical insight into why many choices were made. These insights should generalize to any system that would like to implement *green threads*, lightweight threads that use less memory than traditional operating system threads. For space reasons, I’m not going to talk about STM or sparks (though they are also quite interesting).

**Update:** A large portion of this material has been incorporated into the [scheduler page in the GHC commentary](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Scheduler)

# Anatomy of a thread

I’d first like to discuss some brief background about the runtime system first and point out some perhaps nonintuitive design choices. A thread is represented by a TSO (thread-state object) by GHC, i.e. the `StgTSO` struct in `includes/rts/storage/TSO.h`. \[1\] In Haskell, TSOs can be passed around as `ThreadId` objects. The `Stg` in front of the struct name indicates that TSOs are *garbage collected*, like other closures in Haskell. The TSO, along with the stack allocated with it (STACK), constitute the primary memory overhead of a thread. Default stack size, in particular, is controlled by the GC flag `-ki`, and is 1k by default. \[2\] Threads are run by Capabilities, which can be thought of virtual cores managed by GHC. Capabilities are, in turn, mapped to true operating system threads, or Tasks, though we won’t talk about them much.

Being garbage collected has two major implications for TSOs. First, TSOs are *not* GC roots, so they will get GC'd if there is nothing holding on to them (e.g. [in the case of deadlock](http://blog.ezyang.com/2011/07/blockedindefinitelyonmvar/)), and their space is not *automatically* reclaimed when they finish executing \[3\]. Usually, a TSO will be retained by a Capability’s run queue (a GC root), or in the list of waiting threads of some concurrency variable, e.g. an MVar. Second, a TSO must be considered a *mutable* object, and is thus subject to the conventional GC write barriers necessary for any mutable object in a generational garbage collector. \[4\] The `dirty` bit tracks whether or not a TSO has been modified; it is always set when a thread is run and also when any of the pointer fields on a TSO are modified. Two fields, set by `setTSOLink` and `setTSOPrev`, are of particular interest to the scheduler.

# Run queue

The run queue is at the heart of the scheduler, as any runnable thread will hit the run queue before the scheduler actually pops it off the queue and runs it. There’s one per capability `rts/Capability.h` (in the bad old days, there was a global run queue, but this performed badly for multithreaded processes), and it is implemented as a doubly-linked list `run_queue_hd` and `run_queue_tl`. \[6\] The head and tail pointers mean that the queue is actually a deque: this is important because the scheduler will often have to handle threads that were interrupted in some way, and should let the threads get back on. The links themselves are on the TSOs and modified with `setTSOLink` and `setTSOPrev`, so modifying the queue dirties the TSOs involved. \[7\] Otherwise, the run queue is exclusively owned by the scheduler. If there are idle capabilities and if we have more than one thread left in our run queue, threads will be pushed to other queues with `schedulePushWork`.

Threads are put in *front* (`pushOnRunQueue`) if:

- A stack overflow occurs;
- A heap overflow occurs; \[8\]
- A task attempts to run a thread, but it is [bound](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent.html#v:forkOS) and the current task is the wrong one;
- A thread is associated with a black hole (a thunk that is being evaluated), and another thread, possibly on another capability, has blocked on its evaluation (see [ticket \#3838](http://hackage.haskell.org/trac/ghc/ticket/3838));
- In the threaded runtime, if a thread was interrupted because another Capability needed to do a stop-the-world GC (see commit `6d18141d8`);
- In the non-threaded runtime, when a thread waiting on IO unblocks.

Threads are put in *back* (`appendToRunQueue`) in the case of pre-emption, or if it’s new; particularly, if

- A thread was pre-empted via the context switch flag (e.g. incoming message from another thread, the timer fired, the thread cooperatively yielded, etc; see also \[8\] on how this interacts with heap overflows);
- It is a new thread (so large amounts of thread creation do not starve old threads, see `conc004` and commit `05881ecab`);
- A thread becomes unblocked;
- A thread is migrated to another capability (though, in this case, the queue was empty anyway);
- A thread finishes, but for some reason we need to keep it around (this is related to in-calls, though I’m not a 100% sure what is going on here; if you know, please tell me!)

# Benchmarks

Benchmarks like nofib are very important, even if they are synthetic, as they will often be construed as primary evidence whether or not a change to the scheduler speeds or slows things down. One reason is that it is much easier to tell why a short program that torture tests threads has slowed down than it is to tell why a large, complicated multithreaded program no longer seems very snappy. But really, the main motivation is convenience: nofib programs are easy to measure and easy to compare. Fortunately, the tests often measure something quite specific, so I’d like to describe the tests that compose the `smp` nofib suite here:

- `callback001` (also known as `ffi014`) performs a large number of incalls to Haskell from C from a large number of threads. This is a rather specific test related to how we place threads in the run queue even if they’ve finished, if they finished in an in-call.
- `callback002` measures how quickly we can perform incalls to Haskell from C.
- `chan` measures how scheduling order effects memory usage: if threads are allowed to run for a bit without getting context switched, they build up data in channels. This is related to when we reset the context switch flag (see \[8\]).
- `sieve` implements the Sieve of Eratosthenes, spawning many threads to evaluate thunks of a lazy list in parallel. It performs a bit of allocation, and sensitive to what happens to threads after a HeapOverflow.
- `threads001` tests how quickly we can create a thread and then context switch to it.
- `threads003` tests how quickly many threads can communicate by reading and writing MVars. It is a bit sensitive to what happens to threads after they wake up from sleeping.
- `threads006` tests how quickly threads can be created and destroyed, as well as `throwTo` blocking performance. It is very sensitive to the number of major GCs that occur (which can be influenced if TSO size changes).
- `threads007` generates a lot of threads waiting on MVars, and then sees how shutdown behavior is affected. It was due to bad behavior in the MVar queue and fixed in `f4692220c7`.

# Conclusion

The GHC scheduler is pretty complicated! Much of the current behavior was created in response to specific problems: the right choices are *not* obvious a priori! I hope this post will serve as a valuable reference for any future GHC hackers interested in playing around with the scheduler, as well as for anyone else who needs to implement a scheduler for their runtime system. Much of the historical data was gleaned from comments (though I found some out-of-date ones), liberal use of `git blame`, and cross-referencing with the bug tracker—these are all useful places to figure out, “Well, why does that code do that?” In this post, I hope I’ve answered that question, to some degree.

------------------------------------------------------------------------

\[1\] Initialization of `StgTSO` is handled in `createThread` in `rts/Threads.c`; this function is in turn invoked by `createGenThread`, `createIOThread` and `createStrictIOThread` in `rts/RtsAPI.c`. These functions setup the initial stack state, which controls what the thread executes when it actually gets run. These functions are the ones invoked by the `fork#` and other primops (entry-points for primops are located in `rts/PrimOps.cmm`).

\[2\] Actually, your usable stack will be a little smaller than that because this size also includes the size of the `StgTSO` struct. (This is only really for allocating lots of threads into one block, however, as once a GC occurs the TSOs and stacks will no longer be adjacent.)

\[3\] Here is a sample program which demonstrates how holding onto `ThreadId` using stable pointers (which force the object their pointing to to never be GC'd) can leak memory:

    import Control.Concurrent
    import Control.Monad
    import Foreign.StablePtr

    n = 400000
    main = do
        ms <- replicateM n (newEmptyMVar >>= \m -> (forkIO (putMVar m ()) >>= newStablePtr) >> return m)
        mapM_ takeMVar ms

The [heap profile of the run](http://heap.ezyang.com/view/6e310e2e2e2c11ff3a7cc8ff0f5c205e51a8a188) shows none of the TSO/STACK objects being deallocated, even when the MVars drain out as threads finish executing.

\[4\] The write barrier for generational GCs refers not to [memory barrier](http://en.wikipedia.org/wiki/Memory_barrier) of multithreaded execution, but rather, notification for the garbage collector when a mutable reference in the old generation changes, and may now possibly point to an object in the young generation. Write barriers are necessary because the old generation will not be traversed during a minor collection, and thus if old generations may point to an object in a young generation, we may miss the fact that a young object is still alive even though it has no references from other young objects. In GHC, a write barrier is implemented by adding an object to the [mutable list](http://hackage.haskell.org/trac/ghc/wiki/StgObjectTypes) (`mut_list`) of a Capability if it is not in the youngest generation. (Some objects, like `MutArr#`, are *permanently* on the mutable list; in such a case, a write barrier may not be necessary. But see \[5\] for more details.) Objects will usually track their dirty status, so that they don’t add themselves to the mutable list multiple times. (Accidentally adding an object multiple times is harmless, but means the GC has to do extra work traversing the mutable list.) Additionally, if we can guarantee that the new reference does not point to the young generation (for instance, it is a static closure like `END_TSO_QUEUE`), then dirtying the object is not necessary. Getting this stuff right is tricky, to say the least!

\[5\] There is a bit of a sordid story here. Keeping an object permanently on the mutable list is done by `scavenge_mutable_list` in `rts/sm/Scav.c`, which will unconditionally re-add such an object to the mutable list if it sees it there. How does the object get on the mutable list in the first place? It’s not placed on the list upon creation; rather, upon the first minor GC on the youngest generation, the scavenging GC notices the object and places it on the mutable list by `gct->failed_to_evac = rtsTrue`. How do we end up freeing the object? The mutable list is considered a set of root pointers, but it is only *scavenged*, not evacuated. If an item on the mutable list ends up not being evacuated, it will be blown away regardless. (This does mean, however, that its elements will not be freed until the next GC.) Isn’t it really inefficient to always be scanning these arrays? Yes, and this used to [be a problem (ticket \#650)](http://hackage.haskell.org/trac/ghc/ticket/650), nowadays mitigated by card marking. The same story applied to [TSOs (ticket \#1589)](http://hackage.haskell.org/trac/ghc/ticket/1589), but the fix here was to properly apply a write barrier and not keep the objects permanently on the mutable list; this improved performance quite a bit when there were a lot of threads (even if you don’t scavenge their pointers, traversing a huge mutable list is still a pain.) Creating a lot of small mutable arrays is apt to be painful.

\[6\] It used to be singly linked, but fixing [ticket \#3838](http://hackage.haskell.org/trac/ghc/ticket/3838) required the ability to remove TSOs from the run queue.

\[7\] Since these fields are always traversed by the GC, it’s important that they do not contain NULL pointers or garbage. Instead, we set them to the static closure `END_TSO_QUEUE`. Because this is guaranteed not to be in the young generation, this is why you do not need to dirty the TSO after setting this field.

\[8\] Sometimes, a heap overflow and a context switch occur simultaneously. If the thread requested a large block, we still always push it in front (because we don’t want another thread to steal our large block); however, otherwise, the context switch takes precedence and the thread is booted to the end of the queue—the context switch is checked as *late* as possible. (See commit `05881ecab`)
