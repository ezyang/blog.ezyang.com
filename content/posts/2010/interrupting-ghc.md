---
title: "Interrupting GHC"
date: 2010-08-27 09:00:27
slug: interrupting-ghc
categories: [C, GHC, Hack, Haskell]
math: true
comments:
    - id: 1088
      author: Baughn
      date: "2010-08-27 17:53:53"
      content: |
        Consider what happens if the FFI call ends between you finding the OS thread and calling pthread_cancel.
        
        Race conditions are fun, yes? I wonder if this actually is one..
    - id: 1089
      author: Edward Z. Yang
      date: "2010-08-27 17:59:07"
      content: "That’s... complicated. After the FFI call ends, the OS thread doesn’t start doing something else until it returns to Haskell, which requires the capability. So it might not be a problem, though it certainly seems like it could be! I wonder what the Simons have to say on this matter."
    - id: 1090
      author: Baughn
      date: "2010-08-27 18:10:47"
      content: "In that case, the exception still gets lost; and it *might* start something else."
    - id: 1116
      author: Anonymous
      date: "2010-09-02 12:42:24"
      content: |
        Someone praised your patches and I was telling them how much I liked your approach:
        
           1. You discovered something you didn't like (uninterruptable FFI calls)
           2. Researched the problem, created minimal examples
           3. Sought feedback from senior developers (SPJ/SimonM)
           4. Published a proposal for what to change, what you learned, and the problem itself (used your blog, but public mailing lists also work well)
           5. Submitted patches
        
        It's a good example for all of us to follow when doing open source collaboration.  Speaking for myself, I know I struggle to remember the (3) and (4) and I've learned the hard way how important those two steps are.
        
        Keep up the good work!
    - id: 1334
      author: Brian
      date: "2010-10-23 15:24:38"
      content: |
        Hi,
        
        I'm writing an interactive Haskell program using the GNU readline library, and I've been frustrated with the current interaction of FFI and signals.  I've come across your post and am very encouraged to see that someone is looking into this particular problem in great detail, and I <i>very</i> much hope to see some improvements integrated into the GHC runtime environment.
        
        In my situation, when an interactive GNU readline function is called, it installs its own signal handlers for SIGINT, SIGTERM, and so on, then proceeds to muck with the terminal state and perform IO.  If called, its signal handlers return the terminal settings to their prior state, reset the signal handler to its previous value, and re-throw the signal.  Despite the seeming portability of this strategy, it leads to trouble in a Haskell program.
        
        At present, it looks like the readline library is handling SIGINT (from a control-C press) correctly, but the re-throwing gets lost somewhere in the Haskell runtime environment.  Sometimes the re-thrown signal is handled at a <i>much</i> later date (after multiple reads/writes to the terminal), and sometimes it gets internally queued forever, taking up space in the runtime environment's signal queue (which can lead to a "too many pending signals" error).
        
        Unfortunately, GNU readline is not pthread-aware, and assumes single-threaded operation with a linear chain of signal handlers that it can insert itself into via system calls.  I'm curious to know if your approach will work well with such legacy libraries?
        
        I'll include an example below in case you're tempted to experiment.
        
        Thanks,
        Brian
        
        
        <pre>import Prelude hiding (catch)
        import Control.Exception (catch, throwIO, AsyncException(UserInterrupt))
        import System.Console.Readline
        
        main = do catch printread
                    (\e -&gt; if e == UserInterrupt
                           then putStrLn "Signal caught, exiting"
                           else throwIO e)
        
        printread = sequence_ $ repeat ((readline "&gt; ") &gt;&gt;= putStrLn . show)</pre>
    - id: 1335
      author: Edward Z. Yang
      date: "2010-10-23 15:33:59"
      content: "Brian, that's really fascinating. My understanding is that the RTS assumes that its signal handler thread will catch all of its signals, but when readline rethrows its signal using raise (I assume), the signal gets sent to that thread, not the GHC signal handler thread. My patch doesn't really deal with your use-case at all, unfortunately, but it sounds like what we want to do in your case is when the FFI call comes back to Haskell, check for pending signals for that OS thread and baton them to the signal handler thread."
    - id: 1482
      author: Edward Z. Yang
      date: "2010-11-10 19:38:51"
      content: |
        Brian,
        
        You might find my follow up on this list interesting:
        
        http://www.haskell.org/pipermail/glasgow-haskell-users/2010-November/019531.html
        
        Edward
    - id: 1622
      author: Brian Johnson
      date: "2010-12-06 20:08:40"
      content: "Thanks for thinking about this and looking into the runtime environment's (mis)behavior."
---

In my [tech talk about abcBridge](http://blog.ezyang.com/2010/08/galois-tech-talk-abcbridge-functional-interfaces-for-aigs-and-sat-solving/), one of the “unsolved” problems I had with making FFI code usable as ordinary Haskell code was interrupt handling. Here I describe an experimental solution involving a change to the GHC runtime system as suggested by [Simon Marlow](http://permalink.gmane.org/gmane.comp.lang.haskell.glasgow.user/18771). The introductory section may be interesting to practitioners looking for working examples of code that catches signals; the later section is a proof of concept that I hope will turn into a fully fleshed out patch. :

    > {-# LANGUAGE ForeignFunctionInterface #-}
    > {-# LANGUAGE DeriveDataTypeable #-}
    > {-# LANGUAGE ScopedTypeVariables #-}
    > 
    > import qualified Control.Exception as E
    > 
    > import Foreign.C.Types (CInt)
    > 
    > import Control.Monad
    > import Control.Concurrent (threadDelay, myThreadId, throwTo, forkIO)
    > import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
    > 
    > import System.IO (hPutStrLn, stderr)
    > import System.Posix.Signals (installHandler, sigINT, Handler(..))

------------------------------------------------------------------------

In many interactive applications (especially for REPLs), you would like to be able to catch when a user hits `^C` and terminate just the current computation, not the entire program. `fooHs` is some function that may take a long time to run (in this case, `fooHs` never terminates). :

    > fooHs :: Int -> IO Int
    > fooHs n = do
    >     putStrLn $ "Arf HS " ++ show n
    >     threadDelay 1000000
    >     fooHs n

By default, GHC generates an asynchronous exception which we can catch using the normal exception handling facilities to say “don’t exit yet”:

    > reallySimpleInterruptible :: a -> IO a -> IO a
    > reallySimpleInterruptible defaultVal m = do
    >     let useDefault action =
    >             E.catch action
    >                 (\(e :: E.AsyncException) ->
    >                     return $ case e of
    >                         E.UserInterrupt -> defaultVal
    >                         _ -> E.throw e
    >                         )
    >     useDefault m
    > 
    > reallySimpleMain = do
    >     r <- reallySimpleInterruptible 42 (fooHs 1)
    >     putStrLn $ "Finished with " ++ show r

Sometimes, you don’t want an exception generated at all and would like to deliberate on the signal as soon as it arrives. You might be in some critical section of the program that should not be interrupted! In such a case, you can install a signal handler with `installHandler` from [System.Posix.Signals](http://www.haskell.org/ghc/docs/6.12-latest/html/libraries/unix-2.4.0.1/System-Posix-Signals.html).

    > installIntHandler :: Handler -> IO Handler
    > installIntHandler h = installHandler sigINT h Nothing

Care should be taken to make sure you restore the original signal handler when you’re done.

If you do decide you want to generate an exception from inside a signal handler, a little care must be taken: if we try to do just a simple throw, our exception will seemingly vanish into the void! This is because the interrupt handler is run on a different thread, and we have to use `throwTo` from [Control.Concurrent](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Concurrent.html) to ensure our exception is sent to the right thread. :

    > simpleInterruptible :: a -> IO a -> IO a
    > simpleInterruptible defaultVal m = do
    >     tid <- myThreadId
    >     let install = installIntHandler (Catch ctrlc)
    >         ctrlc = do
    >             -- This runs in a different thread!
    >             hPutStrLn stderr "Caught signal"
    >             E.throwTo tid E.UserInterrupt
    >         cleanup oldHandler = installIntHandler oldHandler >> return ()
    >         useDefault action =
    >             E.catch action
    >                 (\(e :: E.AsyncException) ->
    >                     return $ case e of
    >                         E.UserInterrupt -> defaultVal
    >                         _ -> E.throw e
    >                         )
    >     useDefault . E.bracket install cleanup $ const m
    > 
    > simpleMain = do
    >     r <- simpleInterruptible 42 (fooHs 1)
    >     putStrLn $ "Finished with " ++ show r

This code works fine for pure Haskell work.

------------------------------------------------------------------------

However, our question is whether or not we can interrupt Haskell threads that are inside the FFI, not just pure Haskell code. That is, we’d like to replace `fooHs` with:

    > foreign import ccall "foo.h" foo :: CInt -> IO ()

where `foo.h` contains:

    void foo(int);

and `foo.c` contains:

    #include <stdio.h>
    #include "foo.h"

    void foo(int d) {
        while (1) {
            printf("Arf C %d!\n", d);
            sleep(1);
        }
    }

In real practice, `foo` will be some highly optimized function written in C that may take a long time to run. We also can’t kill functions willy nilly: we should be able to forcibly terminate it at any time without corrupting some global state.

If we try our existing `interruptible` functions, we find they don’t work:

- `reallySimpleInterruptible` registers the SIGINT, but the foreign call continues. On the second SIGINT, the program terminates. This is the [default behavior of the runtime system](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Signals): the RTS will attempt to gracefully abort the computation, but has no way of killing an FFI call, and forcibly terminates the program when the second SIGINT arrives.
- `simpleInterruptible` fares even worse: without the “exit on the second signal” behavior, we find that we can’t kill the program by pressing `^C`! The thread that requested the FFI call is ignoring our exceptions.

------------------------------------------------------------------------

*Nota bene.* Please let the author know of any factual inaccuracies in this section.

Time to dive into the runtime system! The code that manages asynchronous exception lives in `RaiseAsync.c` in the `rts` directory. In particular, there is the function:

    nat throwToMsg (Capability *cap, MessageThrowTo *msg)

Which is called when a thread invokes `throwTo` to create an exception in another thread.

It’s instructive to first look at what happens when there is no funny business going along, that is, when the thread is not blocked:

    case NotBlocked:
    {
        if ((target->flags & TSO_BLOCKEX) == 0) {
            // It's on our run queue and not blocking exceptions
            raiseAsync(cap, target, msg->exception, rtsFalse, NULL);
            return THROWTO_SUCCESS;
        } else {
            blockedThrowTo(cap,target,msg);
            return THROWTO_BLOCKED;
        }
    }

If the thread is running normally, we use `raiseAsync` to raise the exception and we’re done! However, the thread may have called `block` (from [Control.Exception](http://haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Exception.html#v%3Ablock)), in which case we add the exception to the target’s blocked exceptions queue, and wait for the target to become unblocked.

Another state that a Haskell thread can be in is this:

    case BlockedOnCCall:
    case BlockedOnCCall_NoUnblockExc:
    {
        blockedThrowTo(cap,target,msg);
        return THROWTO_BLOCKED;
    }

The runtime system waits for the thread to stop being blocked on the FFI call before delivering the exception—it will get there eventually! But if the FFI call takes a long time, this will be too late. We could replace this call with `raiseAsync`, but what we find is that, while the exception gets raised and the Haskell thread resumes normal execution, the *FFI computation continues*!

------------------------------------------------------------------------

If this seems mysterious, it’s useful to review how [the multithreaded scheduler](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Scheduler) in the GHC runtime system works. Haskell threads are light-weight, and don’t have a one-to-one corresponding with OS threads. Instead, Haskell threads, represented with a TSO (thread-state object), are scheduled on a smaller number of OS threads, abstracted in the RTS as Tasks. Each OS thread is associated with a CPU core, abstracted in the RTS as a Capability.

At the very start of execution, the number of OS threads is the same as the number of virtual cores (as specified by the `-N` RTS option): in terms of Haskell code, we gain parallelism by having multiple capabilities, *not* multiple tasks! A capability can only belong to one task at a time. However, if a task blocks on the operating system, it may give up it’s capability to another task, which can continue running Haskell code, thus we frequently refer to these tasks as worker threads.

A Task (OS thread) does work by executing InCalls requested by a TSO (Haskell thread) in the run queue, scheduling them in a round-robin fashion. During the course of this execution, it may run across an FFI call. The behavior here diverges depending on whether or not the FFI call is safe or unsafe.

- If the call is unsafe, we just make the call, without relinquishing the capability! This means no other Haskell code can run this virtual core, which is bad news if the FFI call takes a long time or blocks, but if it’s really fast, we don’t have to give up the capability only to snatch it back again.
- If the call is safe, we release the capability (allowing other Haskell threads to proceed), and the Haskell thread is suspended as waiting on a foreign call. The current OS thread then goes and runs the FFI call.

Thus, if we attempt to directly wake up the original Haskell thread by throwing it an exception, it will end up getting scheduled on a *different* OS thread (while the original thread continues running the FFI call!)

The trick is to kill the OS thread that is running the FFI call. :

    case BlockedOnCCall:
    case BlockedOnCCall_NoUnblockExc:
    {
    #ifdef THREADED_RTS
        Task *task = NULL;
        if (!target->bound) {
            // walk all_tasks to find the correct worker thread
            for (task = all_tasks; task != NULL; task = task->all_link) {
                if (task->incall->suspended_tso == target) {
                    break;
                }
            }
            if (task != NULL) {
                raiseAsync(cap, target, msg->exception, rtsFalse, NULL);
                pthread_cancel(task->id);
                task->cap = NULL;
                task->stopped = rtsTrue;
                return THROWTO_SUCCESS;
            }
        }
    #endif
        blockedThrowTo(cap,target,msg);
        return THROWTO_BLOCKED;
    }

Which OS thread is it, anyhow? It couldn’t possibly be thread attempting to throw the exception and it doesn’t have anything to do with the suspended Haskell thread, which is waiting to be woken up but doesn’t know what it’s waiting to be woken up from. However, the task running the FFI call knows which Haskell thread is waiting on it, so we can just walk the list of all tasks looking for the one that matches up with the target of our exception. Once we find it, we kill the thread with fire (`pthread_cancel`) and wakeup the orignating Haskell thread with an exception.

There is one subtlety that Marlow pointed out: we do not want to destroy bound threads, because they may contain thread local state. Worker threads are identical and thus expendable, but bound threads cannot be treated so lightly.

------------------------------------------------------------------------

We’ve been a bit mean: we haven’t given the library a chance to clean up when it got interrupted. Fortunately, the library can use `pthread_setcancelstate` and `pthread_setcanceltype`, to give it a chance to cleanup before exiting.

------------------------------------------------------------------------

It turns out that even with the RTS patch, we still aren’t quite able to interrupt FFI calls. If we add in an explicit new Haskell thread, hwoever, things work:

    > interruptible :: a -> IO a -> IO a
    > interruptible defaultVal m = do
    >     mresult <- newEmptyMVar -- transfer exception to caller
    >     mtid    <- newEmptyMVar
    >     let install = installIntHandler (Catch ctrlc)
    >         cleanup oldHandler = installIntHandler oldHandler >> return ()
    >         ctrlc = do
    >             hPutStrLn stderr "Caught signal"
    >             tid <- readMVar mtid
    >             throwTo tid E.UserInterrupt
    >         bracket = reportBracket . E.bracket install cleanup . const
    >         reportBracket action = do
    >             putMVar mresult =<< E.catches (liftM Right action)
    >                 [ E.Handler (\(e :: E.AsyncException) ->
    >                     return $ case e of
    >                         E.UserInterrupt -> Right defaultVal
    >                         _ -> Left (E.toException e)
    >                     )
    >                 , E.Handler (\(e :: E.SomeException) -> return (Left e))
    >                 ]
    >     putMVar mtid =<< forkIO (bracket m)
    >     either E.throw return =<< readMVar mresult -- one write only
    > 
    > main = main' 3
    > 
    > main' 0 = putStrLn "Quitting"
    > main' n = do
    >     interruptible () $ do
    >         (r :: Either E.AsyncException ()) <- E.try $ foo n
    >         putStrLn $ "Thread " ++ show n ++ " was able to catch exception"
    >     main' (pred n)

The output of this literate Haskell file, when compiled with `-threaded` on the patched RTS is as follows:

    Arf C 3!
    Arf C 3!
    ^CCaught signal
    Thread 3 was able to catch exception
    Arf C 2!
    Arf C 2!
    Arf C 2!
    ^CCaught signal
    Thread 2 was able to catch exception
    Arf C 1!
    Arf C 1!
    ^CCaught signal
    Thread 1 was able to catch exception
    Quitting

Proof of concept accomplished! Now to make it work on Windows...
