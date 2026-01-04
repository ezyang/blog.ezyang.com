---
title: "Reader monad and implicit parameters"
date: 2010-07-26 12:46:43
slug: implicit-parameters-in-haskell
categories: [Haskell]
comments:
    - id: 757
      author: Dan Doel
      date: "2010-07-26 20:40:12"
      content: |
        For completeness: Oleg argues somewhere (possibly in some of the material related to Delimited Dynamic Binding, although I'm not certain) that implicit parameters are not *really* the same as dynamic scope ala Lisp. Not being particularly excited by dynamic scope, I have trouble remembering the alleged difference, but I think it's related to stuff like:
        
            let ?x = 5 :: Int in \() -&gt; ?x
        
        That will have type '() -&gt; Int' using implicit parameters, but I think Oleg's conception of dynamic scope would leave it as '?x :: Int =&gt; () -&gt; Int', because you don't bind dynamic variables under lambdas or something.
        
        Note that one 'advantage' of the reader monad here is that it makes it unambiguous (and up to the programmer) when binding happens.
        
          local (const 5) (return (\() -&gt; ask)) :: R (() -&gt; R Int)
        
        The function in question returns an R Int, so the variables therein are bound where you *use* the result.
        
          local (const 5) (ask &gt;&gt;= \v -&gt; return (\() -&gt; v)) :: R (() -&gt; Int)
        
        The function's return type doesn't mention R, so any variables it uses has been bound at the it's creation. Note that the syntax is noticeably different, as well.
        
        Implicit parameters only give you the latter option, I believe, and Oleg argues (if I recall correctly) that the former is how dynamic scope is supposed to work.
    - id: 758
      author: Edward Z. Yang
      date: "2010-07-26 20:56:38"
      content: |
        Very interesting point: this is definitely an advantage of the Reader monad. In many ways, this feels like it stems from the fact that without monads, we can't control the order of execution.
        
        I've been playing around in my head, however, with the idea of using implicit variables to carry around labels for monadic regions, and unifying the two worlds. No working code yet though :-)
    - id: 759
      author: Dan Doel
      date: "2010-07-26 22:32:47"
      content: |
        Thinking a bit more, I guess one way to distinguish existing implicit parameters from Oleg's dynamic scope would be the following typing judgment:
        
            (\() -&gt; ?x) :: () -&gt; (?x :: Int =&gt; Int)
        
        And have 'let ?x = ... in ...' only eliminate top-level contexts. 
        
        Having just tried that now, it doesn't work that way by default. That's a legal type (to my surprise) but a let eliminates the constraint. However, if I write:
        
            (let ?x = 5 in \() -&gt; ?x) :: (?x :: Int) =&gt; () -&gt; Int
        
        That signature is accepted! And:
        
            (let ?x = 4 in ((let ?x = 5 in \() -&gt; ?x) :: (?x :: Int) =&gt; () -&gt; Int)) ()
        
        yields 4. So you can fool with binding time by specifying type signatures, apparently, which is quite weird.
        
        As to the first point, yes. One of the nice things in general about using monads for effects it that they make it clear where and in what order effects happen, both in the types and in terms (implicit parameters only in the former). Another area where this comes up (in my opinion) is with continuations. Frequently you'll see tutorials on them in scheme with constructs like:
        
        (reset (list 1 2 3 (shift k ...) 4 5))
        
        And understanding what happens isn't just a matter of understanding continuations, but also understanding in what order the implicit evaluation strategy causes things to occur. By contrast, a monadic presentation (at least, in terms of the base combinators) doesn't have this extra complication.
        
        reset (return [4, 5] &gt;&gt;= \tl -&gt; shift (\k -&gt; ...) &gt;&gt;= \e -&gt; return (e : tl) &gt;&gt;= \tl' -&gt; return (1:2:3:tl')) -- or something like that.
    - id: 760
      author: Dan Doel
      date: "2010-07-26 22:52:09"
      content: |
        For additional fun, I just discovered that:
        
        (?x :: Int) =&gt; () -&gt; Int
        
        causes definition-site binding (it's a function that requires an implicit ?x, which it ignores), while:
        
        () -&gt; ((?x :: Int) =&gt; Int)
        
        causes use-site binding.
    - id: 761
      author: Edward Z. Yang
      date: "2010-07-27 01:55:24"
      content: |
        Hmm, I swear I played with this and wasn't able to cause use-site binding. However, looking at my Haskell code, I had:
        
        baz :: (?x :: Int) => () -> Int
        baz = let ?x = 5 :: Int in (\() -> ?x)
        
        which in retrospect just takes in an implicit parameter but then ignores it immediately.
        
        (The extra parallel about continuations is very good.)
    - id: 762
      author: Dan Doel
      date: "2010-07-27 03:16:53"
      content: |
        I filed a bug report, and SPJ confirmed that the user-site binding for the function typed
        
        () -&gt; ((?x :: Int) =&gt; Int)
        
        is not the expected behavior (or at least, that it shouldn't behave differently than putting the constraint first). So don't get too attached to it. :)
        
        http://hackage.haskell.org/trac/ghc/ticket/4226
    - id: 765
      author: Edward Z. Yang
      date: "2010-07-27 23:57:25"
      content: "Your first example doesn't do what you described on the tin ((let ?x = 4 in ((let ?x = 5 in \\() -> ?x) :: (?x :: Int) => () -> Int)) () does definition site binding) but then you nail that the higher-“ranked” binding causes evaluation time binding."
    - id: 766
      author: Edward Z. Yang
      date: "2010-07-28 00:23:10"
      content: |
        Oleg’s comment that Haskell’s implicit parameters are not dynamically scoped is on this page:
        
        http://okmij.org/ftp/Computation/dynamic-binding.html
---

<div class="container center">

*For when the Reader monad seems hopelessly clunky*

</div>

The Reader monad (also known as the Environment monad) and implicit parameters are remarkably similar even though the former is the standard repertoire of a working Haskell programmer while the latter is a GHC language extension used sparingly by those who know about it. Both allow the programmer to code as if they had access to a global environment that can still change at runtime. However, implicit parameters are remarkably well suited for cases when you would have used a stack of reader transformers. Unfortunately, unlike many type system extensions, GHC cannot suggest that you enable `ImplicitParams` because the code you innocently wrote is not valid Haskell98 but would be valid if you enabled this extension. This post intends to demonstrate one way to discover implicit parameters, with a little nudging.

*Reader monad in practice.* The Reader monad is really quite simple: after all, it is isomorphic to `(->) r`, the only real difference being a newtype. Because of this, in engineering contexts, it is rarely used as-is; in particular:

- It is used as a transformer, endowing an “environment” to whatever application-specific monad you are building, and
- It is used with a record type, because an environment of only one primitive value is usually not very interesting.

These choices impose some constraints on how code written for a Reader monad can be used. In particular, baking in the environment type `r` of `ReaderT r` means that your monadic code will not play nicely with some other monadic code `ReaderT r2` without some coaxing; additionally, I can’t build up a complicated record type `Record { field1 :: Int; field2 :: String; field3 :: Bool}` incrementally as I find out values of the environment. I could have my record type be a map of some sort, in which case I could place arbitrarily values in it, but in this case I have no static assurances of what values will or will not be in the map at a given point in time.

*Stacked Reader transformers.* To allow ourselves to incrementally build up our environment, one might consider stacking the Reader monad transformers. Consider the type `ReaderT a (ReaderT b (ReaderT c IO)) ()`. If we desugar this into function application, we find `a -> (b -> (c -> IO ()))`, which can be further simplified to `a -> b -> c -> IO ()`. If `a`, `b` and `c` happen to be the same type, we don’t have any way of distinguishing the different values, except for the location in the list of arguments. However, instead of writing out the parameters explicitly in our function signature (which, indeed, we are trying to avoid with the reader monad), we find ourselves having to lift `ask` repeatedly (zero times for `a`, once for `b` and twice for `c`). Unlike the record with three fields, there is no name for each environment variable: we have to refer to them by using some number of lifts.

> *Aside*. In fact, this is a [De Bruijn index](http://en.wikipedia.org/wiki/De_Bruijn_index), which [Oleg](http://okmij.org/ftp/) helpfully pointed in out in an email conversation we had after my post about [nested loops and continuations](http://blog.ezyang.com/2010/02/nested-loops-and-continuation/). The number of lifts is the index (well, the Wikipedia article is 1-indexed, in which case add one) which tells us how many reader binding scopes we need to pop out of. So if I have:
>
>     runReaderT (runReaderT (runReaderT (lift ask) c) b) a
>     \------- outermost/furthest context (3) ------------/
>                \--- referenced context (2; one lift) -/
>                            \--- inner context (1) -/
>
> I get the value of `b`. This turns out to be wonderful for the lambda-calculus theoreticians (who are cackling gleefully at trouble-free α-conversion), but not so wonderful for software engineers, for whom De Bruijn indexes are equivalent to the famous antipattern, the magic number.

With typeclass tricks, we can get back names to some extent: for example, Dan Piponi [renames the transformers with singleton data types or “tags”](http://blog.sigfpe.com/2010/02/tagging-monad-transformer-layers.html), bringing in the heavy guns of `OverlappingInstances` in the process. Oleg [uses lexical variables that are typed to the layer they belong to](http://okmij.org/ftp/Haskell/regions.html#light-weight) to identify different layers, although such an approach is not really useful for a Reader monad stack, since the point of the Reader monad is not to have to pass any lexical variables around, whether or not they are the actual variables or specially typed variables.

*Implicit parameters.* In many ways, implicit parameters are a cheat: while Dan and Oleg’s approaches leverage existing type-level programming facilities, implicit parameters define a “global” namespace (well known to Lispers as the dynamic scope) that we can stick variables in, and furthermore it extends the type system so we can express what variables in this namespace any given function call expects to exist (without needing to use monads, the moxy!)

Instead of an anonymous environment, we assign the variable a name:

    f :: ReaderT r IO a
    f' :: (?implicit_r :: r) => IO a

`f'` is still monadic, but the monad doesn’t express what is in the environment anymore: it’s entirely upon the type signature to determine if an implicit variable is passed along:

    f  = print "foobar" >> g 42 -- Environment always passed on
    f' = print "foobar" >> g 42 -- Not so clear!

Indeed, `g` could have just as well been a pure computation:

    f' = print (g 42)

However, if the type of is:

    g :: IO a

the implicit variable is lost, while if it is:

    g :: (?implicit_r :: r) => IO a

the variable is available.

While `runReader(T)` was our method for specifying the environment, we now have a custom let syntax:

    runReaderT f value_of_r
    let ?implicit_r = value_of_r in f

Besides having ditched our monadic restraints, we can now easily express our incremental environment:

    run = let ?implicit_a = a
              ?implicit_b = b
              ?implicit_c = c
          in h

    h :: (?implicit_a :: a, ?implicit_b :: b, ?implicit_c :: c) => b
    h = ?implicit_b

You can also use `where`. Note that, while this looks deceptively like a normal `let` binding, it is quite different: you can’t mix implicit and normal variable bindings, and if you have similarly named implicit bindings on the right-hand side, they refer to their values *outside* of the `let`. No recursion for you! (Recall `runReaderT`: the values that we supply in the second argument are pure variables and not values in the Reader monad, though with `>>=` you could instrument things that way.)

*Good practices.* With monadic structure gone, there are fewer source-level hints on how the monomorphism restriction and polymorphic recursion apply. Non-polymorphic recursion *will* compile, and cause unexpected results, such as your implicit parameter not changing when you expect it to. You can play things relatively safely by making sure you always supply type signatures with all the implicit parameters you are expecting. I hope to do a follow-up post explaining more carefully what these semantics are, based off of formal description of types in the [relevant paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.46.9849).
