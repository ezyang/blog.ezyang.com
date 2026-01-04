---
title: "Rank-2 types and type family pitfalls"
date: 2010-08-13 00:48:49
slug: 
draft: true
categories: [Haskell]
---

This is a bit of type family hackery that gave me trouble at work today, and thus I share it with you.

Suppose that I want to generalize over monads with phantom types and the rank-2 functions that run them, e.g. monads that look like [ST s](http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Monad-ST.html). You might want to do this if you have multiple such monads that share a common API, but have different backends which give different results. We might write the following data family:

    class PhantomResult r where
      data PhantomMonad r :: * -> * -> *
      runLeakyPhantomMonad :: (Monad (PhantomMonad r s)) => 
          PhantomMonad r s a -> a

There is something suboptimal about this code: in particular, because we’ve had no way of saying that the result of the `PhantomMonad` type family should be a `Monad`; we instead have to push it into every function definition that might use a `PhantomMonad`. Our function leaks both variables of type `s` as well as class constraints! We should very much like our function not to leak, so let’s add another definition with the power of Rank-2 types.

This is the point at which we enter the rabbit hole of types.

We can, for example, write the following incorrect type signature:

    runPhantomMonad :: (Monad (PhantomMonad r s0)) =>
        (forall s. PhantomMonad r s a) -> a

After all, class constraints usually go at the beginning of a function, right? We then code up an implementation:

    instance PhantomResult Int where
      data PhantomMonad Int s a = IntPhantom { unIntPhantom :: ST s a }
      runPhantomMonad = runST . unIntPhantom

Actually, that doesn’t work:

    Couldn't match expected type `forall s. PhantomMonad Int s a'
           against inferred type `PhantomMonad Int s a1'

since Rank-2 types don’t play nice with regular function composition (this error has nothing to do with type families, by the way). The mismatch of `a` and `a1` is a red herring. We’ll need to explicitly pattern-match `IntPhantom`:

    runPhantomMonad (IntPhantom x) = runST x

But that gives us a different error:

    Couldn't match expected type `forall s. PhantomMonad Int s a'
           against inferred type `PhantomMonad r ($a) ($b)'

As it turns out, dropping the original `s0` phantom type variable was the wrong thing to do: the class constraint as well as the quantification needs to be a higher rank:

    runPhantomMonad ::
        (forall s. (Monad (PhantomMonad r s)) => PhantomMonad r s a) -> a

With the correct type signature in place, GHC correctly complains about a missing Monad typeclass:

    Couldn't match expected type `forall s.
                                  (Monad (PhantomMonad Int s)) =>
                                  PhantomMonad Int s a'
           against inferred type `PhantomMonad r ($a) ($b)'

So we add it (generalized newtype deriving won’t help us here; it’s not supported on type families):

    instance Monad (PhantomMonad Int s) where
      (IntPhantom m) >>= f = IntPhantom $ m >>= (unIntPhantom . f)
      return = IntPhantom . return

Unfortunately, the error persists. It turns out, whatever we do
