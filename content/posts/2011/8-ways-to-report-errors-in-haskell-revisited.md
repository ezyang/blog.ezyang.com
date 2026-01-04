---
title: "8 ways to report errors in Haskell revisited"
date: 2011-08-29 12:30:02
slug: 8-ways-to-report-errors-in-haskell-revisited
categories: [Haskell]
math: true
comments:
    - id: 2932
      author: elaforge
      date: "2011-08-29 12:44:30"
      content: "I use Either String quite a lot.  Basically it's equivalent to Maybe + log a warning.  An ADT whose only use is to be converted to a String would be complicated and unnecessary."
    - id: 2933
      author: Edward Z. Yang
      date: "2011-08-29 13:23:06"
      content: "It’s not really like logging warnings, because you stop executing when an error condition is triggered. If you want logging of errors, you can pretty easily do that with a top-level exception handler and error."
    - id: 2936
      author: Ivan Miljenovic
      date: "2011-08-29 18:30:01"
      content: "For point 4, isn't usage of \"fail\" suitable with parsing combinator libraries as well?"
    - id: 2937
      author: Edward Z. Yang
      date: "2011-08-29 19:21:47"
      content: "\"Depends.\" Did the parser combinator library provide a sensible implementation of fail? Is a string error message the only thing you'd ever want to transmit?"
    - id: 2938
      author: Tim
      date: "2011-08-29 20:07:07"
      content: |
        I agree with elaforge that Either String can be a useful error mode. It lets you:
        
                - handle errors in pure code as you would with Maybe
                - log errors at an outer (IO) level, where appropriate.
        
        A common case for me is that I want to do some complex pure calculations over doubles and want either a result, or a reason why the result could not be calculated (for presentation to a user). Either String captures this perfectly.
    - id: 2940
      author: elaforge
      date: "2011-08-30 03:34:24"
      content: |
        Actually it is like logging warnings.  It just means that the particular monadic computation aborted, but execution in general continues.  E.g.
        
        xs  Log.warn ("failed to do x: "++err) &gt;&gt; return []) return $ do_something xyz
        
        This is very common for short ad-hoc Either blocks, but I also do it for larger monads with large libraries that run complicated computations.
        
        IO errors would be inappropriate because the monad with Log.warn is also pure, and it can't catch IO exceptions.  And I think in general catching IO exceptions is unpleasant, they should crash the program, but it would be better were they never thrown at all!  They interact badly with laziness.  I would only use one  to indicate an assertion failure because I failed to adequately constrain things with types.
    - id: 2941
      author: ketil
      date: "2011-08-30 08:49:36"
      content: |
        I use 'error' a lot.  I use it when something is irrevocably wrong, but not necessarily a programmer error, just that something is wrong enough that there is nothing to do but give up.
        
        I don't think you should use 'error' when you expect the error to be dealt with in some way, and in that sense 'catch' is a hack to work around that, and should be used sparingly and with caution.
    - id: 3089
      author: "Link Library - Saturday November 5 | Codebloat"
      date: "2011-11-05 13:06:15"
      content: "[...] 8 Ways to Report Errors in Haskell Revisited [...]"
    - id: 6153
      author: greg
      date: "2013-06-12 18:28:14"
      content: "Great post!  Issues like 'which form of error handling should I be using here?  I vaguely remember that what I'm doing is kind of like another possibility - am  I making the wrong choice?' often paralyze me really early in a project.  Having these clearly articulated and up-to-date recommendations gives me a lot of psychic comfort :)"
    - id: 6279
      author: "Скандальная правда об обработке исключений в Haskell | Записки программиста"
      date: "2013-10-30 05:24:01"
      content: "[&#8230;] обзор способов обработки ошибок в [&#8230;]"
    - id: 6362
      author: dsf
      date: "2014-01-26 10:42:57"
      content: "Something to note - error e is no longer an abbreviation for throw (UserError e), it is now an abbreviation for throw (ErrorCall e)."
    - id: 6363
      author: Edward Z. Yang
      date: "2014-01-26 20:31:12"
      content: "Thanks! I've updated the blog post."
    - id: 7183
      author: Anonymous
      date: "2014-07-18 19:57:25"
      content: "Isn't there a missing Either MyError a option here?"
---

In 2007, Eric Kidd wrote a quite popular article named [8 ways to report errors in Haskell](http://www.randomhacks.net/articles/2007/03/10/haskell-8-ways-to-report-errors/). However, it has been four years since the original publication of the article. Does this affect the veracity of the original article? Some names have changed, and some of the original advice given may have been a bit... dodgy. We’ll take a look at each of the recommendations from the original article, and also propose a new way of conceptualizing all of Haskell’s error reporting mechanisms.

I recommend reading this article side-to-side with the old article.

# 1. Use error

No change. My personal recommendation is that you should only use `error` in cases which imply *programmer* error; that is, you have some invariant that only a programmer (not an end-user) could have violated. And don’t forget, you should probably see if you can enforce this invariant in the type system, rather than at runtime. It is also good style to include the name of the function which the `error` is associated with, so you say “myDiv: division by zero” rather than just “Division by zero.”

Another important thing to note is that `error e` is actually an abbreviation for `throw (ErrorCall e)`, so you can explicitly pattern match against this class of errors using:

    import qualified Control.Exception as E

    example1 :: Float -> Float -> IO ()
    example1 x y =
      E.catch (putStrLn (show (myDiv1 x y)))
              (\(ErrorCall e) -> putStrLn e)

However, testing for string equality of error messages is bad juju, so if you do need to distinguish specific `error` invocations, you may need something better.

# 2. Use Maybe a

No change. Maybe is a convenient, universal mechanism for reporting failure when there is only one possible failure mode and it is something that a user probably will want to handle in pure code. You can easily convert a returned Maybe into an error using `fromMaybe (error "bang") m`. Maybe gives no indication what the error was, so it’s a good idea for a function like `head` or `tail` but not so much for `doSomeComplicatedWidgetThing`.

# 3. Use Either String a

I can’t really recommend using this method in any circumstance. If you don’t need to distinguish errors, you should have used `Maybe`. If you don’t need to handle errors while you’re in pure code, use exceptions. If you need to distinguish errors in pure code, for the love of god, don’t use strings, make an enumerable type!

However, in base 4.3 or later (GHC 7), this monad instance comes for free in `Control.Monad.Instances`; you no longer have to do the ugly `Control.Monad.Error` import. But there are some costs to having changed this: see below.

# 4. Use Monad and fail to generalize 1-3

If you at all a theoretician, you reject `fail` as an abomination that should not belong in `Monad`, and refuse to use it.

If you’re a bit more practical than that, it’s tougher to say. I’ve already made the case that catching string exceptions in pure code isn’t a particularly good idea, and if you’re in the `Maybe` monad `fail` simply swallows your nicely written exception. If you’re running base 4.3, `Either` will not treat `fail` specially either:

    -- Prior to base-4.3
    Prelude Control.Monad.Error> fail "foo" :: Either String a
    Loading package mtl-1.1.0.2 ... linking ... done.
    Left "foo"

    -- After base-4.3
    Prelude Control.Monad.Instances> fail "foo" :: Either String a
    *** Exception: foo

So you have this weird generalization that doesn’t actually do what you want most of the time. It just might (and even so, only barely) come in handy if you have a custom error handling application monad, but that’s it.

It’s worth noting `Data.Map` does not use this mechanism anymore.

# 5. Use MonadError and a custom error type

`MonadError` has become a lot more reasonable in the new world order, and if you are building your own application monad it’s a pretty reasonable choice, either as a transformer in the stack or an instance to implement.

Contrary to the old advice, you can use `MonadError` on top of `IO`: you just transform the `IO` monad and lift all of your IO actions. I’m not really sure why you’d want to, though, since IO has it’s own nice, efficient and extensible error throwing and catching mechanisms (see below.)

I’ll also note that canonicalizing errors that the libraries you are interoperating is a good thing: it makes you think about what information you care about and how you want to present it to the user. You can always create a `MyParsecError` constructor which takes the parsec error verbatim, but for a really good user experience you should be considering each case individually.

# 6. Use throw in the IO monad

It’s not called `throwDyn` and `catchDyn` anymore (unless you import `Control.OldException`), just `throw` and `catch`. You don’t even need a Typeable instance; just a trivial `Exception` instance. I highly recommend this method for unchecked exception handling in IO: despite the mutation of these libraries over time, the designers of Haskell and GHC’s maintainers have put a lot of thought into how this exceptions should work, and they have broad applicability, from normal synchronous exception handling to *asynchronous* exception handling, which is very nifty. There are a load of bracketing, masking and other functions which you simply cannot do if you’re passing Eithers around.

Make sure you do use `throwIO` and not `throw` if you are in the IO monad, since the former guarantees ordering; the latter, not necessarily.

# 7. Use ioError and catch

No reason to use this, it’s around for hysterical raisins.

# 8. Go nuts with monad transformers

This is, for all intents and purposes, the same as 5; just in one case you roll your own, and in this case you compose it with transformers. The same caveats apply. Eric does give good advice here shooing you away from using this with IO.

------------------------------------------------------------------------

Here are some new mechanisms which have sprung up since the original article was published.

# 9. Checked exceptions

Pepe Iborra wrote a nifty [checked exceptions library](http://hackage.haskell.org/package/control-monad-exception) which allows you to explicitly say what `Control.Exception` style exceptions a piece of code may throw. I’ve never used it before, but it’s gratifying to know that Haskell’s type system can be (ab)used in this way. Check it out if you don’t like the fact that it’s hard to tell if you caught all the exceptions you care about.

# 10. Failure

The [Failure typeclass](http://hackage.haskell.org/packages/archive/failure/0.1.0.1/doc/html/Control-Failure.html) is a really simple library that attempts to solve the interoperability problem by making it easy to wrap and unwrap third-party errors. I’ve used it a little, but not enough to have any authoritative opinion on the matter. It’s also worth taking a look at the [Haskellwiki page](http://www.haskell.org/haskellwiki/Failure).

# Conclusion

There are two domains of error handling that you need to consider: *pure errors* and *IO errors*. For IO errors, there is a very clear winner: the mechanisms specified in `Control.Exception`. Use it if the error is obviously due to an imperfection in the outside universe. For pure errors, a bit more taste is necessary. `Maybe` should be used if there is one and only one failure case (and maybe it isn’t even that big of a deal), `error` may be used if it encodes an *impossible* condition, string errors may be OK in small applications that don’t need to react to errors, custom error types in those that do. For interoperability problems, you can easily accomodate them with your custom error type, or you can try using some of the frameworks that other people are building: maybe one will some day gain critical mass.

It should be clear that there is a great deal of choice for Haskell error reporting. However, I don’t think this choice is unjustified: each tool has situations which are appropriate for its use, and one joy of working in a high level language is that error conversion is, no really, not that hard.
