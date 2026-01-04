---
title: "monad-control is tricky"
date: 2012-01-23 12:39:00
slug: monadbasecontrol-is-unsound
categories: [Haskell]
comments:
    - id: 3357
      author: Anders Kaseorg
      date: "2012-01-23 15:44:20"
      content: |
        What you’ve done here is successfully doubled the _IO side effects_ of incState, of which there are none.  This is potentially confusing to new users and deserving of better documentation, but I wouldn’t go so far as to call it a fatal flaw.
        
        I did document some of the consequences of this behavior in <a href="http://hackage.haskell.org/packages/archive/monad-peel/0.1/doc/html/Control-Exception-Peel.html" rel="nofollow">Control.Exception.Peel</a> (search for Note), and these notes were inherited by <a href="http://hackage.haskell.org/packages/archive/lifted-base/0.1.0.3/doc/html/Control-Exception-Lifted.html" rel="nofollow">Control.Exception.Lifted</a>.
        
        Still, I absolutely second your desire for a rigorous theoretical foundation.
    - id: 3358
      author: Anders Kaseorg
      date: "2012-01-23 18:08:55"
      content: |
        It’s not true, by the way, that it “doesn’t work on functions which invoke the callback multiple times”.  You just have to lift the right function.  This prints 2 as you’d expect:
        
        
        double' :: (a -&gt; IO a) -&gt; a -&gt; IO a
        double' m a = m a &gt;&gt;= m
        
        doubleG' :: MonadBaseControl IO m =&gt; (a -&gt; m a) -&gt; a -&gt; m a
        doubleG' m a = control $ \run -&gt;
          run (return a) &gt;&gt;= double' (\b -&gt; run (restoreM b &gt;&gt;= m))
        
        main = execStateT (doubleG' (\() -&gt; incState) ()) 0 &gt;&gt;= print
    - id: 3360
      author: Edward Z. Yang
      date: "2012-01-23 22:23:55"
      content: |
        That's true. So this "problem" suggests that IO actions need to be written in some "sufficiently general" fashion (something like the "threading" which is seen in double') in order to accommodate lifting via MonadBaseControl. This is tricky, because these functions are not always endomorphic, so I still need to communicate the intermediate results. Edward Kmett has suggested that any function you want to lift should be universally quantified over all monads (that is to say, a monad transformation), but I don't think we've worked out the details properly on this scheme.
        
        It's not a fatal flaw. But it's very important to have the right conception of what MonadBaseControl does, and in my opinion it's a nice trick for making implementing lifts easier, but not a proper interface.
    - id: 21542
      author: Michael Snoyman
      date: "2016-11-10 04:52:50"
      content: |
        In my opinion, monad-control is most useful for the subset of monad transformers which are isomorphic to ReaderT, since there are no side-effects to be accidentally discarded or doubled. That's why I wrote the monad-unlift package (https://www.stackage.org/package/monad-unlift). There's also the related monad-unlift-ref package (https://www.stackage.org/package/monad-unlift-ref), which provides replacements for StateT, WriterT, and RWST that use mutable references and therefore remain isomorphic to ReaderT.
        
        Finally, the lifted-async package provides a very nice module Control.Concurrent.Async.Lifted.Safe (https://www.stackage.org/haddock/lts-7.8/lifted-async-0.9.0/Control-Concurrent-Async-Lifted-Safe.html) which uses the exact same concept to provided safely lifted async/concurrency primitives.
---

*Editor's note.* I've toned down some of the rhetoric in this post. The original title was "monad-control is unsound".

MonadBaseControl and MonadTransControl, from the [monad-control](http://hackage.haskell.org/package/monad-control) package, specify an appealing way to automatically lift functions in IO that take "callbacks" to arbitrary monad stacks based on IO. Their appeal comes from the fact that they seem to offer a more general mechanism than the alternative: picking some functions, lifting them, and then manually reimplementing generic versions of all the functions built on top of them.

Unfortunately, monad-control has rather surprising behavior for many functions you might lift.

For example, it doesn't work on functions which invoke the callback multiple times:

    {-# LANGUAGE FlexibleContexts #-}

    import Control.Monad.Trans.Control
    import Control.Monad.State

    double :: IO a -> IO a
    double m = m >> m

    doubleG :: MonadBaseControl IO m => m a -> m a
    doubleG = liftBaseOp_ double

    incState :: MonadState Int m => m ()
    incState = get >>= \x -> put (x + 1)

    main = execStateT (doubleG (incState)) 0 >>= print

The result is `1`, rather than `2` that we would expect. If you are unconvinced, suppose that the signature of double was `Identity a -> Identity a`, e.g. `a -> a`. There is only one possible implementation of this signature: `id`. It should be obvious what happens, in this case.

If you look closely at the types involved in MonadBaseControl, the reason behind this should become obvious: we rely on the polymorphism of a function we would like to lift in order to pass `StM m` around, which is the encapsulated “state” of the monad transformers. If this return value is discarded by `IO`, as it is in our function `double`, there is no way to recover that state. (This is even alluded to in the `liftBaseDiscard` function!)

My conclusion is that, while monad-control may be a convenient implementation mechanism for lifted versions of functions, the functions it exports suffer from serious semantic incoherency. End-users, take heed!

*Postscript.* A similar injunction holds for the previous versions of MonadBaseControl/MonadTransControl, which went by the names MonadPeel and MonadMorphIO.
