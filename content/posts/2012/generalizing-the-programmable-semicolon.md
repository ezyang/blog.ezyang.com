---
title: "Generalizing the programmable semicolon"
date: 2012-10-03 17:49:28
slug: generalizing-the-programmable-semicolon
categories: [Programming Languages]
comments:
    - id: 4297
      author: Tomas Petricek
      date: "2012-10-03 19:56:38"
      content: |
        Interesting article, I was hoping you'll share the details after the Twitter discussion yesterday!
        
        I think I maybe did something that might be relevant here - my initial motivation was similar - you can write monadic code in call-by-name or call-by-value style (i.e. do effects repeatedly or too eagerly). As you say call-by-need effects would be a bit weird, but I tried to encode that anyway :-). I suppose your implementation does something like call-by-need on the pure component, but call-by-name style effects, which sounds quite neat.
        
        My attempt was to define an operation "malias: m a -&gt; m (m a)" which is inserted in the monadic code and lets you parameterize it by evaluation order. Intuitively, it splits the effects of "m a" into effects that happen before function call (the outer "m" in "m (m a)") and effects that happen when you actually use the value (the inner "m"). You can define:
        
        <pre>
            malias m = return m             -- runs effects repeatedly as in call-by-name
            malias m = fmap return m    -- runs effects just once as in call-by-value
        </pre>
        
        If you're doing this in a monad that supports some form of state to do the "caching", then you can implement call-by-need. I suppose you could also implement the behaviour you want (which is like call-by-need in the sense that it caches the thunk, but runs the effects repeatedly every time).
        
        If I had the "extract" operation that you're using, then I could certainly do:
        
        <pre>
            malias m = 
                let ~(a, e) = extract m
                in return $ e &gt;&gt; return a
        </pre>
        
        But perhaps this scheme would also allow you to use "f a -&gt; f (a, f())", but then the laws would somehow have to specify that the effects of the argument should be pushed into the inner "f ()". Maybe this would allow the outer computation to do some un-observable effects? Or maybe you could implement this to do a strategy that runs the effects both when CBN and when CBV would run them (so the effects happen even more often, but at least in a predictable way).
        
        A paper that describes my "malias" is here:
        http://www.cl.cam.ac.uk/~tp322/papers/malias.html
        
        PS: Not sure how to do code formatting in this editor :-) hope it does not look too bad!
    - id: 4298
      author: Edward Z. Yang
      date: "2012-10-04 03:53:14"
      content: |
        Yes, I've thought about 'f a -> f (a, f())'; it seems like it might be more accurate to have something like: 's (t a) -> t (a, s ())', showing that the effects could be disjoint, and not related to each other. (I'm not sure if the order should be commuted.)
        
        Another paper along the lines of malias is the explicit sharing paper (http://sebfisch.github.com/explicit-sharing/) which implements malias for exactly the call-by-need case, and in a way which is compatible with non-determinism (which, remarkably and for reasons I don't quite understand, does seem to play well with call-by-need.)
    - id: 4300
      author: Ywen
      date: "2012-10-04 12:23:39"
      content: |
        let ~(x,w) = run (cbneed e)
        
        Do you really need to explicitely declare a pattern as irrefutable in a let-binding? Isn't it irrefutable by default?
    - id: 4301
      author: Edward Z. Yang
      date: "2012-10-04 12:53:35"
      content: "Ywen: Yes, I think you are right, but I wanted to be explicit that something \"funny\" was going on here."
    - id: 4303
      author: Sjoerd Visscher
      date: "2012-10-04 15:52:23"
      content: "Maybe it is related, most likely it is not, but a left adjoint functor supports the extract function: splitL in http://hackage.haskell.org/packages/archive/adjunctions/3.0.0.1/doc/html/Data-Functor-Adjunction.html, and every adjunction defines a monad."
    - id: 4304
      author: Edward Z. Yang
      date: "2012-10-04 18:25:01"
      content: "Actually, it looks like there is a very close relation: we're looking for left adjoint functors, it seems. Unfortunately, I don't have very good intuition for adjoint functors :("
    - id: 4310
      author: Wouter
      date: "2012-10-05 13:51:32"
      content: "Do you have a reference for the Levy quote?"
    - id: 4311
      author: Edward Z. Yang
      date: "2012-10-05 14:21:04"
      content: "Yep, it's in Levy's thesis, page 16. http://www.cs.bham.ac.uk/~pbl/papers/thesisqmwphd.pdf"
    - id: 4346
      author: Edward Z. Yang
      date: "2012-10-12 03:36:38"
      content: "Oh, not bad! The taint monad http://blog.sigfpe.com/2007/04/homeland-security-threat-level-monad.html is just the writer monad. Maybe this will come in handy after all..."
---

*Caveat emptor: half-baked research ideas ahead.*

What is a monad? One answer is that it is a way of sequencing actions in a non-strict language, a way of saying “this should be executed before that.” But another answer is that it is programmable semicolon, a way of implementing custom side-effects when doing computation. These include bread and butter effects like state, control flow and nondeterminism, to more exotic ones such as [labeled IO](http://hackage.haskell.org/package/lio). Such functionality is useful, even if you don’t need monads for sequencing!

Let’s flip this on its head: what does a programmable semicolon look like for a call-by-need language? That is, can we get this extensibility without sequencing our computation?

At first glance, the answer is no. Most call-by-value languages are unable to resist the siren’s song of side effects, but in call-by-need side effects are sufficiently painful that Haskell has managed to avoid them (for the most part!) Anyone who has worked with `unsafePerformIO` with `NOINLINE` pragma can attest to this: depending on optimizations, the effect may be performed once, or it may be performed many times! As Paul Levy says, “A third method of evaluation, call-by-need, is useful for implementation purposes. but it lacks a clean denotational semantics—at least for effects other than divergence and erratic choice whose special properties are exploited in \[Hen80\] to provide a call-by-need model. So we shall not consider call-by-need.” Paul Levy is not saying that for pure call-by-need, there are no denotational semantics (these semantics coincide exactly with call-by-name, call-by-need’s non-memoizing cousin), but that when you add side-effects, things go out the kazoo.

But there’s a hint of an angle of attack here: Levy goes on to show how to discuss side effects in call-by-name, and has no trouble specifying the denotational semantics here. Intuitively, the reason for this is that in call-by-name, all uses (e.g. case-matches) on lazy values with an effect attached cause the effect to manifest. Some effects may be dropped (on account of their values never being used), but otherwise, the occurrence of effects is completely deterministic.

Hmm!

Of course, we could easily achieve this by giving up memoization, but that is a bitter pill to swallow. So our new problem is this: *How can we recover effectful call-by-name semantics while preserving sharing?*

In the case of the `Writer` monad, we can do this with all of the original sharing. The procedure is very simple: every thunk `a` now has type `(w, a)` (for some fixed monoidal `w`). This tuple can be shared just as the original `a` was shared, but now it also has an effect `w` embedded with it. Whenever `a` would be forced, we simply append effect to the `w` of the resulting thunk. Here is a simple interpreter which implements this:

    {-# LANGUAGE GADTs #-}
    import Control.Monad.Writer

    data Expr a where
        Abs :: (Expr a -> Expr b) -> Expr (a -> b)
        App :: Expr (a -> b) -> Expr a -> Expr b
        Int :: Int -> Expr Int
        Add :: Expr Int -> Expr Int -> Expr Int
        Print :: String -> Expr a -> Expr a

    instance Show (Expr a) where
        show (Abs _) = "Abs"
        show (App _ _) = "App"
        show (Int i) = show i
        show (Add _ _) = "Add"
        show (Print _ _) = "Print"

    type M a = Writer String a
    cbneed :: Expr a -> M (Expr a)
    cbneed e@(Abs _) = return e
    cbneed (App (Abs f) e) =
        let ~(x,w) = run (cbneed e)
        in cbneed (f (Print w x))
    cbneed e@(Int _) = return e
    cbneed (Add e1 e2) = do
        Int e1' <- cbneed e1
        Int e2' <- cbneed e2
        return (Int (e1' + e2'))
    cbneed (Print s e) = do
        tell s
        cbneed e

    sample = App (Abs (\x -> Print "1" (Add x x))) (Add (Print "2" (Int 2)) (Int 3))
    run = runWriter

Though the final print out is `"122"` (the two shows up twice), the actual addition of 2 to 3 only occurs once (which you should feel free to verify by adding an appropriate tracing call). You can do something similar for `Maybe`, by cheating a little: since in the case of `Nothing` we have no value for `x`, we offer bottom instead. We will never get called out on it, since we always short-circuit before anyone gets to the value.

There is a resemblance here to applicative functors, except that we require even more stringent conditions: not only is the control flow of the computation required to be fixed, but the value of the computation must be fixed too! It should be pretty clear that we won’t be able to do this for most monads. Yesterday [on Twitter](https://twitter.com/ezyang/status/253258690688344064), I proposed the following signature and law (reminiscent of inverses), which would need to be implementable by any monad you would want to do this procedure on (actually, you don’t even need a monad; a functor will do):

    extract :: Functor f => f a -> (a, f ())
    s.t.  m == let (x,y) = extract m in fmap (const x) y

but it seemed only `Writer` had the proper structure to pull this off properly (being both a monad and a comonad). This is a shame, because the application I had in mind for this bit of theorizing needs the ability to allocate cells.

Not all is lost, however. Even if full sharing is not possible, you might be able to pull off partial sharing: a sort of bastard mix of full laziness and partial evaluation. Unfortunately, this would require substantial and invasive changes to your runtime (and I am not sure how you would do it if you wanted to CPS your code), and so at this point I put away the problem, and wrote up this blog post instead.
