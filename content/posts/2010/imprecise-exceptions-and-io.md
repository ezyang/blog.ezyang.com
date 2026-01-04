---
title: "Lazy exceptions and IO"
date: 2010-05-24 09:00:41
slug: imprecise-exceptions-and-io
categories: [Haskell]
comments:
    - id: 428
      author: "Walt \"BMeph\" Rorie-Baety"
      date: "2010-05-24 11:14:22"
      content: |
        Actually, I believe there is a different problem in that program: that, even though you "know better," years of procedural programming have "warped your brain" and you fail to notice that in Haskell, "return" does not do what you expect. I think it (the decision to call the monadic injection function "return") was a hilarious "inside joke" to the Committee, but that this use shows how it causes cognitive dissonance, and interferes with clear understanding of programs' meanings.
        
        On another note, I enjoyed the post in general, and the morals, even though, as I said, I have a different interpretation on what (some of) those morals should be.
    - id: 429
      author: Edward Z. Yang
      date: "2010-05-24 11:27:40"
      content: "BMeph, I agree that return was a terrible terrible choice for the monadic injection function. However, there is an interesting point to be made here: we usually consider bind to result in a sequencing of IO action, so that whenever we perform the next action, we can assume that the previous action was performed. This is strictly true, but the IO monad says nothing about the sequencing of pure computation, which is what's emitting the imprecise exception. And, more practically, no funny business results if I omit the return. :-)"
    - id: 430
      author: Sjoerd Visscher
      date: "2010-05-24 11:52:28"
      content: "If everybody knows that error is evil, why is it still used everywhere? F.e. iteratee uses ListLike, which requires instances to implement head and tail."
    - id: 431
      author: Edward Z. Yang
      date: "2010-05-24 12:13:00"
      content: "Sjoerd, perhaps the multiple layers of indirection has something to do it. \"But, I didn't know that this tasty API was using error behind the scenes!\""
    - id: 432
      author: Dan Doel
      date: "2010-05-24 15:10:19"
      content: |
        I think I have to agree with BMeph. It's not uncommon to think of _|_ as a value in Haskell, and from that perspective 'return (error "whatever")' is just a well-defined action that yields the value bottom. Special combinators, like evaluate, are supposed to be used to inspect the pure values and turn certain observably different (from IO) bottoms into IO exceptions.
        
        What is more odd is that evaluate is actually redundant. You can just write:
        
        return $! x `catch` \e -&gt; ...
        
        and it will work exactly as if you had said
        
        evaluate x `catch` \e -&gt; ...
        
        So catch is not merely catching exceptions that have been promoted into the IO exception mechanism via some other magic; catch is itself magic (and evaluate is probably just an internally sort-of-eta-expanded strict return), and attempts to recover from _|_ :: IO a, rather than just a thrownIO exception.
    - id: 434
      author: Zygoloid
      date: "2010-05-25 18:51:02"
      content: |
        Great observation, but I have a terminology quibble: that's not what "imprecise exception" means.
        
        Imprecise exceptions refers to the handling of expressions which are denotationally _|_, but where that value can arise in different ways. For instance, consider (error "A" `seq` error "B"). In this case, there's no guarantee which of the errors happens first.
        
        Imprecise exceptions extends the semantics to say that a _|_ value represents a /set/ of possible exceptions (in this case error "A" and error "B"), from which a representative is (potentially nondeterministically) chosen at runtime.
    - id: 435
      author: Edward Z. Yang
      date: "2010-05-25 19:46:35"
      content: |
        Zygoloid, interesting point, and one that I probably ought to sort out soon (this post is somehow fourth in a Google search for imprecise exception). The documentation for Control.Exception pointed to a paper "A semantics for imprecise exceptions", and I went and checked the paper out:
        
        <blockquote>All current programming languages that support exceptions take it for granted that the language definition should specify, for a given program, what exception, if any, is raised when the program is executed. That used to be the case in microprocessor architecture too, but it is no longer so.  Some processors, notably the Alpha, provide so-called <i>imprecise exceptions</i>. These CPUs execute many instructions in parallel, and perhaps out of order; it follows that the first exception (divide-by-zero, say) that is encountered is not necessarily the first that would be encountered in simple sequential execution.</blockquote>
        
        So, yeah, I am abusing terminology a little bit here. I will note, though, that they do spend a little time discussing the difference between an exceptional call and value (which Dan Doel helpfully commented about):
        
        <blockquote>To repeat: it is <i>values</i> not <i>calls</i> that may be exceptional, and exceptional values may, for example, hide inside lazy data structures.</blockquote>
        
        What would you suggest a renaming be?
    - id: 436
      author: andersk
      date: "2010-05-25 23:46:12"
      content: "The equivalent of (say) Python’s raise in Haskell is not throw but <a href=\"http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html#v%3AthrowIO\" rel=\"nofollow\">throwIO</a>."
    - id: 437
      author: Zygoloid
      date: "2010-05-26 06:37:07"
      content: "I guess a better name for what's going on here might be \"lazy exceptions\"."
    - id: 445
      author: Edward Z. Yang
      date: "2010-05-26 11:18:13"
      content: "Zygoloid; ok, updated! Anders, that is correct, and I'm purposely conflating the two in this example."
---

Consider the following piece of code:

    import Prelude hiding (catch)
    import Control.Exception

    main :: IO ()
    main = do
        t <- safeCall
        unsafeCall t
        putStrLn "Done."

    safeCall :: IO String
    safeCall = do
        return alwaysFails `catch` errorHandler

    --alwaysFails = throw (ErrorCall "Oh no!")
    alwaysFails = error "Oh no!"

    errorHandler :: SomeException -> IO String
    errorHandler e = do
        putStrLn "Caught"
        return "Ok."
    errorHandler_ e = errorHandler e >> return ()

    unsafeCall :: String -> IO ()
    unsafeCall = putStrLn

What might you expect the output to be? A straightforward transcription to Python might look like:

    def main():
        t = safeCall()
        unsafeCall(t)
        print "Done"

    def safeCall():
        try:
            return alwaysFails()
        except:
            return errorHandler()

    def alwaysFails():
        raise Exception("Oh no!")

    def errorHandler():
        print "Caught."
        return "Ok."

    def unsafeCall(output):
        print output

and anyone with a passing familiarity with the any strict language will say, "Of course, it will output:"

    Caught.
    Ok.
    Done.

Of course, lazy exceptions (which is what `error` emits) aren't called lazy for no reason; the Haskell code outputs:

    *** Exception: Oh no!

What happened? Haskell was lazy, and didn't bother evaluating the pure insides of the IO `return alwaysFails` until it needed it for unsafeCall, at which point there was no more `catch` call guarding the code. If you don't believe me, you can add a trace around `alwaysFails`. You can also try installing `errorHandler_` on `unsafeCall`.

What is the moral of the story? Well, one is that `error` is evil, but we already knew that...

- You may install exception handlers for most IO-based errors the obvious way. (If we had replaced `return alwaysFails` with `alwaysFails`, the result would have been the strict one.) You may not install exception handlers for errors originating from pure code, since GHC reserves the right to schedule arbitrarily the time when your code is executed.
- If pure code is emitting exceptions and you would like it to stop doing that, you'll probably need to force strictness with `$!` `deepseq` or `rnf`, which will force GHC to perform the computation inside your guarded area. As my readers point out, a good way to think about this is that the *call* is not what is exceptional, the *structure* is.
- If you are getting an imprecise exception from pure code, but can't figure out where, good luck! I don't have a good recipe for figuring this out yet. (Nudge to my blog readers.)

*Postscript.* Note that we needed to use `Control.Exception.catch`. `Prelude.catch`, as per Haskell98, only catches IO-based errors.
