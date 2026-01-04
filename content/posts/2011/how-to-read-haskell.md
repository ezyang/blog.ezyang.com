---
title: "How to read Haskell like Python"
date: 2011-11-14 09:00:26
slug: how-to-read-haskell
categories: [Haskell]
math: true
comments:
    - id: 3102
      author: Anonymous
      date: "2011-11-14 14:15:28"
      content: |
        Nice Stuff! But i believe thèrese a small error in the following:
        
        Dot. f . g $ a + b translates to f(g(a + b)). Actually, in a Python program you'd probably have been more likely to see:
        
        x = f(a + b)
        y = g(x)
        But Haskell programmers are allergic to extra variables.
        
        Should be:
        
        x = g(a + b)
        y = f(x)
    - id: 3103
      author: Edward Z. Yang
      date: "2011-11-14 14:26:27"
      content: "Thanks, fixed."
    - id: 3106
      author: Barnaby Robson
      date: "2011-11-14 17:18:28"
      content: |
        def runCommand(env, m):
        
        should be
        
        def runCommand(env, m, state):
        
        also there's an extraneous doCommand function probably from another revision.
        
        instead of just Environment(currentTerm, opts) you could make it even more similar with keyword arguments like: Environment(envCurrentTerm=currentTerm, envOpts=opts) 
        
        thanks for the post.
    - id: 3107
      author: Edward Z. Yang
      date: "2011-11-14 19:59:15"
      content: "Thanks Barnaby, fixed."
    - id: 3108
      author: Adrusi
      date: "2011-11-14 21:02:01"
      content: |
        While I see the good intention behind this post, I feel it's harmful. On the HN thread, someone said that this makes Haskell's seem as bad as perl's and that it makes him not want to learn Haskell. Obviously a single post can't hope to teach Haskell from the ground up, which would be the best way to show that Haskell actually has less syntax than python, but the way this post dismisses so much as line noise and pretends that so much is similar to python when it really isn't.
        
        I think this post would be better broken up into two parts, still looking at the code from the top down, but the first post explaining pure code and the second post explaining monadic IO code and not pretending that monads don't exist.
        
        Although I am impressed that you were able to fit so much into a single post!
    - id: 3110
      author: Edward Z. Yang
      date: "2011-11-14 21:07:41"
      content: "I love Haskell, but I'm not trying to get people to learn Haskell in this post. I just want them to know enough syntax to be able to read uninteresting glue code written in Haskell without bleeding. There is a lot different, and a lot of value, and there's a time to learn it... later."
    - id: 3112
      author: nun
      date: "2011-11-14 21:13:13"
      content: But that Perl comment is right after all.
    - id: 3114
      author: Rafael
      date: "2011-11-14 22:30:14"
      content: |
        Edward,
        
        excellent post. I think it's useful (as a sort of checklist) even for people like me who are learning Haskell but aren't necessarily Pythonistas. 
        
        The only part I didn't get is "There's not really an equivalent for regular $". What do you mean?
    - id: 3115
      author: Edward Z. Yang
      date: "2011-11-14 22:35:48"
      content: "As in, there's no way to write a \"comma\" if you're using $, like there is for <$> (namely <*>). You could do it if you redefined $ to be left associative."
    - id: 3116
      author: Oliver Batchelor
      date: "2011-11-14 22:52:21"
      content: "It's only correct insofar to say that it cab be alien syntax if you are not familiar with it. It shows that the haskell is as much defined by it's standard library for things like flow control - this is something which is being hammered away at constantly. Haskeller's take pride in being able to write clean readable concise code, as opposed to the perception of a  perl programmer as delighting in complex dense unreadable code."
    - id: 3121
      author: Arun Ravindran
      date: "2011-11-15 05:14:20"
      content: "Thanks! Have been always looking for cheatsheet like this, during my early days of understanding Haskell syntax. Bookmarked!"
    - id: 3122
      author: Anonymous
      date: "2011-11-15 06:01:58"
      content: "Great article! I second Rafael in every word!"
    - id: 3124
      author: Anonymous
      date: "2011-11-15 10:04:40"
      content: |
        1) This has been helpful.
        2) Haskell has a truly disgusting amount of syntax.
    - id: 3126
      author: Anonymous
      date: "2011-11-15 21:27:29"
      content: "@Anonymous 2) Reading comprehension fail. You, particularly, should interpret this syntax as \"deeply magical things.\""
    - id: 3130
      author: joaquin
      date: "2011-11-17 05:16:12"
      content: |
        Great !
        This will help on my Haskell learning curve (after two years periodically trying it, it looks like I am still walking before the initial slope... and there is a wall in front).
        Thanks !
    - id: 3131
      author: Carlo Pires
      date: "2011-11-17 21:41:06"
      content: "This is fantastic! Always wanted to know more of haskell and you make it easy."
    - id: 3132
      author: Tommy
      date: "2011-11-17 21:55:38"
      content: That was a fun read.
    - id: 3135
      author: Kitmanov
      date: "2011-11-18 18:45:02"
      content: |
        &gt; which requires knowing what a fold is
        Python has built-in function reduce, so, probably, Pythonistas knows what a fold is?
        
        Thanks for this post. I'm translating it to Russian right now.
    - id: 3250
      author: "2012 python meme &laquo; the meta cloud"
      date: "2011-12-22 05:45:19"
      content: "[...] experimental, either learn some Haskell (starting here then continuing here) or maybee finally get around to learn some narrow AI/machine learning [...]"
    - id: 3269
      author: mathk
      date: "2011-12-28 11:38:21"
      content: "Isn't the lazy evaluation will not call doSomething here: \"when(x == y, doSomething(x))\""
    - id: 3272
      author: Edward Z. Yang
      date: "2011-12-29 04:03:20"
      content: "mathk: It depends on whether or not x == y."
    - id: 3876
      author: rodmoorhead
      date: "2012-07-24 22:11:35"
      content: |
        "Ignore everything you see after ::" and you have no idea what Haskell is about. Among other things, strong typing will keep you from shooting yourself in the foot or somebody else in the head.  It insures that if the code compiles, the results will 99% of the time be what you were looking for.  
        
        So disregard the type references at your peril.  If you can't correctly write the type statement, then you don't know what you're actually doing.  
        
        And there are times when that is true.  And that's when you go to ghci and enter &gt; :t myfunction, and it will enlighten you.  Assuming it compiles :t will tell you what you did.
    - id: 3877
      author: Edward Z. Yang
      date: "2012-07-24 22:15:34"
      content: "Hello rodmoorhead: The point is not to teach people what Haskell is about, just to get the comfortable enough to read it. If I have to explain what \"IO a\" and \"(forall s. ST s a) -> a\" mean I lose. The great thing about erasure is you really can ignore the types!"
    - id: 4340
      author: Cory
      date: "2012-10-11 13:38:14"
      content: |
        I'm a little late to the party, but I feel like something out for future readers:
        
        Almost none of what is explained in this post is actually syntax: it's basically all library functions. Yes, including the fish operators. "do", list things, lambdas and left arrows are the only things that are really, truly syntax.
        
        The fish operators (love that name, by the way) are used in place of "named" functions because they are used to express data flow (or even control flow!) and arrow-like symbols make sense for this.
        
        Great post!
    - id: 6072
      author: Lee
      date: "2013-05-03 23:31:04"
      content: |
        This post is nice! I think if the Dot part is written to:
        
        def composition(x):
            return f(g(x))
        composition(a + b)
        
        might be better, because the dot is used to compose functions. Otherwise I cannot get the difference if I change the dot to dollar sign. What's your opinion?
    - id: 6073
      author: Edward Z. Yang
      date: "2013-05-04 23:49:22"
      content: |
        Lee: Well, your code fragment is not well formed Python; do you mean:
        
        <pre>def composition(f, g):
          def h(x):
            return f(g(x))
          return h</pre>
        
        Dollar and dot are frequently interchangeable. However, it's considered good practice to use dot when possible, because it is easier to cut and paste function pipelines written that way.
    - id: 6074
      author: Lee
      date: "2013-05-05 13:19:00"
      content: |
        Edward: Sorry to give you bad code fragment. Actually, I want to use function composition to represent function h not dot. And yes, dollar and dot are frequently interchangeable, but you cannot write "f . g . 1 + 2", right? That's what I mean, if we get the purpose, we can understand the code more easier, although this is not a tutorial of haskell. But, maybe you are right, it may be better to those who don't know haskell that ignoring the difference between dollar and dot. Thanks for your answer :)
        BTW, I'm new to haskell, I've been reading "learn you a haskell" a couple of weeks. Any suggestions?
    - id: 6075
      author: Edward Z. Yang
      date: "2013-05-05 19:16:25"
      content: |
        Lee: Yep. The general rule is you can use dots all the way up to the point where you have something that is not a function.
        
        As for learning Haskell, keep reading, join #haskell (or find your local Haskell enthusiast) and keep asking questions.
    - id: 6154
      author: Anonymous
      date: "2013-06-17 03:03:57"
      content: |
        Totally awesome post. Thank you for writing this.
        
        @Cory: Check out the Lambda Calculus to learn the why almost everything is a "function," I literally just discovered Haskell and Functional Programming but understanding Lambda Calculus seems to me the key to grasping all this totally aweosme functional programming stuff.
        http://en.wikipedia.org/wiki/Lambda_calculus
    - id: 6264
      author: Thomas Geraghty
      date: "2013-10-23 06:40:52"
      content: "Hey, great article, but wouldn't WithNames be more suited to a NamedTuple in Python? Classes should only really be used if there are public methods that operate on the data's state. PyLint complains about this and I agree, I think classes should only be used when you actually need them."
    - id: 6265
      author: Edward Z. Yang
      date: "2013-10-23 16:46:43"
      content: "Sure; but I'm not prescribing how to translate Haskell code into Python code; just how to read it, and \"reading it\" as an object is conceptually simpler than having to know what named tuples are."
    - id: 6301
      author: Anonymous
      date: "2013-11-19 19:22:56"
      content: "Great Work! It would be very helpful if you could post a complete Haskell program covering major functions and an equivalent python code. I understand you are just helping to read Haskell code like python, though."
    - id: 11293
      author: chekkal
      date: "2014-12-19 08:05:57"
      content: Awesome
    - id: 14173
      author: Beth
      date: "2015-05-18 11:19:27"
      content: |
        If people ignore the types they will never be able to read Haskell. Most haskellers will probably send them the types instead of code. Without the types, when can not really use documentation or Hoogle properly as well.
        
        A better advice would be, if you are new to Haskell and want to learn how to read it, ignore everything under the types and read only the types.
    - id: 16318
      author: "Haskell Web编程（Yesod简要教程） | cipchk"
      date: "2015-08-29 08:15:12"
      content: "[&#8230;] 如何阅读Haskell代码（英文） [&#8230;]"
---

**tl;dr** — Save this page for future reference.

Have you ever been in the situation where you need to quickly understand what a piece of code in some unfamiliar language does? If the language looks a lot like what you’re comfortable with, you can usually guess what large amounts of the code does; even if you may not be completely familiar how all the language features work.

For Haskell, this is a little more difficult, since Haskell syntax looks very different from traditional languages. But there's no really deep difference here; you just have to squint at it just right. Here is a fast, mostly incorrect, and hopefully useful guide for interpreting Haskell code like a Pythonista. By the end, you should be able to interpret this fragment of Haskell (some code elided with `...`):

    runCommand env cmd state = ...
    retrieveState = ...
    saveState state = ...

    main :: IO ()
    main = do
        args <- getArgs
        let (actions, nonOptions, errors) = getOpt Permute options args
        opts <- foldl (>>=) (return startOptions) actions
        when (null nonOptions) $ printHelp >> throw NotEnoughArguments
        command <- fromError $ parseCommand nonOptions
        currentTerm <- getCurrentTerm
        let env = Environment
                { envCurrentTerm = currentTerm
                , envOpts = opts
                }
        saveState =<< runCommand env command =<< retrieveState

------------------------------------------------------------------------

*Types.* Ignore everything you see after `::` (similarly, you can ignore `type`, `class`, `instance` and `newtype`. Some people claim that types help them understand code; if you're a complete beginner, things like `Int` and `String` will probably help, and things like `LayoutClass` and `MonadError` won't. Don't worry too much about it.)

------------------------------------------------------------------------

*Arguments.* `f a b c` translates into `f(a, b, c)`. Haskell code omits parentheses and commas. One consequence of this is we sometimes need parentheses for arguments: `f a (b1 + b2) c` translates into `f(a, b1 + b2, c)`.

------------------------------------------------------------------------

*Dollar sign.* Since complex statements like `a + b` are pretty common and Haskellers don't really like parentheses, the dollar sign is used to avoid parentheses: `f $ a + b` is equivalent to the Haskell code `f (a + b)` and translates into `f(a + b)`. You can think of it as a big opening left parenthesis that automatically closes at the end of the line (no need to write `))))))` anymore!) In particular, if you stack them up, each one creates a deeper nesting: `f $ g x $ h y $ a + b` is equivalent to `f (g x (h y (a + b)))` and translates into `f(g(x,h(y,a + b))` (though some consider this bad practice).

In some code, you may see a variant of `$`: `<$>` (with angled brackets). You can treat `<$>` the same way as you treat `$`. (You might also see `<*>`; pretend that it's a comma, so `f <$> a <*> b` translates to `f(a, b)`. There's not really an equivalent for regular `$`)

------------------------------------------------------------------------

*Backticks.* `` x `f` y `` translates into `f(x,y)`. The thing in the backticks is a function, usually binary, and the things to the left and right are the arguments.

------------------------------------------------------------------------

*Equals sign.* Two possible meanings. If it's at the beginning of a code block, it just means you're defining a function:

    doThisThing a b c = ...
      ==>
    def doThisThing(a, b, c):
      ...

Or if you see it to near a `let` keyword, it’s acting like an assignment operator:

    let a = b + c in ...
      ==>
    a = b + c
    ...

------------------------------------------------------------------------

*Left arrow.* Also acts like an assignment operator:

    a <- createEntry x
      ==>
    a = createEntry(x)

Why don't we use an equals sign? Shenanigans. (More precisely, `createEntry x` has side effects. More accurately, it means that the expression is monadic. But that’s just shenanigans. Ignore it for now.)

------------------------------------------------------------------------

*Right arrow.* It's complicated. We'll get back to them later.

------------------------------------------------------------------------

*Do keyword.* Line noise. You can ignore it. (It does give some information, namely that there are side effects below, but you never see this distinction in Python.)

------------------------------------------------------------------------

*Return.* Line-noise. Also ignore. (You’ll never see it used for control flow.)

------------------------------------------------------------------------

*Dot.* `f . g $ a + b` translates to `f(g(a + b))`. Actually, in a Python program you'd probably have been more likely to see:

    x = g(a + b)
    y = f(x)

But Haskell programmers are allergic to extra variables.

------------------------------------------------------------------------

*Bind and fish operators.* You might see things like `=<<`, `>>=`, `<=<` and `>=>`. These are basically just more ways of getting rid of intermediate variables:

    doSomething >>= doSomethingElse >>= finishItUp
      ==>
    x = doSomething()
    y = doSomethingElse(x)
    finishItUp(y)

Sometimes a Haskell programmer decides that it's prettier if you do it in the other direction, especially if the variable is getting assigned somewhere:

    z <- finishItUp =<< doSomethingElse =<< doSomething
      ==>
    x = doSomething()
    y = doSomethingElse(x)
    z = finishItUp(y)

The most important thing to do is to reverse engineer what's actually happening by looking at the definitions of `doSomething`, `doSomethingElse` and `finishItUp`: it will give you a clue what's “flowing” across the fish operator. If you do that, you can read `<=<` and `>=>` the same way (they actually do function composition, like the dot operator). Read `>>` like a semicolon (e.g. no assignment involved):

    doSomething >> doSomethingElse
      ==>
    doSomething()
    doSomethingElse()

------------------------------------------------------------------------

*Partial application.* Sometimes, Haskell programmers will call a function, but they *won't pass enough arguments.* Never fear; they've probably arranged for the rest of the arguments to be given to the function somewhere else. Ignore it, or look for functions which take anonymous functions as arguments. Some of the usual culprits include `map`, `fold` (and variants), `filter`, the composition operator `.`, the fish operators (`=<<`, etc). This happens a lot to the numeric operators: `(+3)` translates into `lambda x: x + 3`.

------------------------------------------------------------------------

*Control operators.* Use your instinct on these: they do what you think they do! (Even if you think they shouldn't act that way.) So if you see: `when (x == y) $ doSomething x`, it reads like “When x equals y, call doSomething with x as an argument.”

Ignore the fact that you couldn’t actually translate that into `when(x == y, doSomething(x))` (Since, that would result in `doSomething` always being called.) In fact, `when(x == y, lambda: doSomething x)` is more accurate, but it might be more comfortable to just pretend that `when` is also a language construct.

`if` and `case` are built-in keywords. They work the way you’d expect them to.

------------------------------------------------------------------------

*Right arrows (for real!)* Right arrows have nothing to do with left arrows. Think of them as colons: they're always nearby the `case` keyword and the backslash symbol, the latter of which is lambda: `\x -> x` translates into `lambda x: x`.

Pattern matching using `case` is a pretty nice feature, but a bit hard to explain in this blog post. Probably the easiest approximation is an `if..elif..else` chain with some variable binding:

    case moose of
      Foo x y z -> x + y * z
      Bar z -> z * 3
      ==>
    if isinstance(moose, Foo):
      x = moose.x # the variable binding!
      y = moose.y
      z = moose.z
      return x + y * z
    elif isinstance(moose, Bar):
      z = moose.z
      return z * 3
    else:
      raise Exception("Pattern match failure!")

------------------------------------------------------------------------

*Bracketing.* You can tell something is a bracketing function if it starts with `with`. They work like contexts do in Python:

    withFile "foo.txt" ReadMode $ \h -> do
      ...
      ==>
    with open("foo.txt", "r") as h:
      ...

(You may recall the backslash from earlier. Yes, that's a lambda. Yes, `withFile` is a function. Yes, you can define your own.)

------------------------------------------------------------------------

*Exceptions.* `throw`, `catch`, `catches`, `throwIO`, `finally`, `handle` and all the other functions that look like this work essentially the way you expect them to. They may look a little funny, however, because none of these are keywords: they’re all functions, and follow all those rules. So, for example:

    trySomething x `catch` \(e :: IOException) -> handleError e
      ===
    catch (trySomething x) (\(e :: IOException) -> handleError e)
      ==>
    try:
      trySomething(x)
    except IOError as e:
      handleError(e)

------------------------------------------------------------------------

*Maybe.* If you see Nothing, it can be thought of as `None`. So `isNothing x` tests if `x is None`. What's the opposite of it? `Just`. For example, `isJust x` tests if `x is not None`.

You might see a lot of line noise associated with keeping `Just` and `None` in order. Here's one of the most common ones:

    maybe someDefault (\x -> ...) mx
      ==>
    if mx is None:
      x = someDefault
    else:
      x = mx
    ...

Here's one specific variant, for when a null is an error condition:

    maybe (error "bad value!") (\x -> ...) x
      ==>
    if x is None:
      raise Exception("bad value!")

------------------------------------------------------------------------

*Records.* The work they way you'd expect them too, although Haskell lets you create fields that have no names:

    data NoNames = NoNames Int Int
    data WithNames = WithNames {
      firstField :: Int,
      secondField :: Int
    }

So `NoNames` would probably be represented as a tuple `(1, 2)` in Python, and `WithNames` a class:

    class WithNames:
      def __init__(self, firstField, secondField):
        self.firstField = firstField
        self.secondField = secondField

Then creation is pretty simple `NoNames 2 3` translates into `(2, 3)`, and `WithNames 2 3` or `WithNames { firstField = 2, secondField = 3 }` translates into `WithNames(2,3)`.

Accessors are a little more different. The most important thing to remember is Haskellers put their accessors before the variable, whereas you might be most familiar with them being after. So `field x` translates to `x.field`. How do you spell `x.field = 2`? Well, you can’t really do that. You can copy one with modifications though:

    return $ x { field = 2 }
      ==>
    y = copy(x)
    y.field = 2
    return y

Or you can make one from scratch if you replace `x` with the name of the data structure (it starts with a capital letter). Why do we only let you copy data structures? This is because Haskell is a *pure* language; but don't let that worry you too much. It's just another one of Haskell’s quirks.

------------------------------------------------------------------------

*List comprehensions.* They originally came from the Miranda-Haskell lineage! There are just more symbols. :

    [ x * y | x <- xs, y <- ys, y > 2 ]
      ==>
    [ x * y for x in xs for y in ys if y > 2 ]

It also turns out Haskellers often prefer list comprehensions written in multi-line form (perhaps they find it easier to read). They look something like:

    do
      x <- xs
      y <- ys
      guard (y > 2)
      return (x * y)

So if you see a left arrow and it doesn't really look like it's doing side effects, maybe it's a list comprehension.

------------------------------------------------------------------------

*More symbols.* Lists work the way you would expect them to in Python; `[1, 2, 3]` is in fact a list of three elements. A colon, like `x:xs` means construct a list with `x` at the front and `xs` at the back (`cons`, for you Lisp fans.) `++` is list concatenation. `!!` means indexing. Backslash means lambda. If you see a symbol you don't understand, try looking for it on [Hoogle](http://haskell.org/hoogle/) (yes, it works on symbols!).

------------------------------------------------------------------------

*More line noise.* The following functions are probably line noise, and can probably be ignored. `liftIO`, `lift`, `runX` (e.g. `runState`), `unX` (e.g. `unConstructor`), `fromJust`, `fmap`, `const`, `evaluate`, an exclamation mark before an argument (`f !x`), `seq`, a hash sign (e.g. `I# x`).

------------------------------------------------------------------------

*Bringing it all together.* Let’s return to the original code fragment:

    runCommand env cmd state = ...
    retrieveState = ...
    saveState state = ...

    main :: IO ()
    main = do
        args <- getArgs
        let (actions, nonOptions, errors) = getOpt Permute options args
        opts <- foldl (>>=) (return startOptions) actions
        when (null nonOptions) $ printHelp >> throw NotEnoughArguments
        command <- fromError $ parseCommand nonOptions
        currentTerm <- getCurrentTerm
        let env = Environment
                { envCurrentTerm = currentTerm
                , envOpts = opts
                }
        saveState =<< runCommand env command =<< retrieveState

With some guessing, we can pop out this translation:

    def runCommand(env, cmd, state):
       ...
    def retrieveState():
       ...
    def saveState(state):
       ...

    def main():
      args = getArgs()
      (actions, nonOptions, errors) = getOpt(Permute(), options, args)
      opts = **mumble**
      if nonOptions is None:
         printHelp()
         raise NotEnoughArguments
      command = parseCommand(nonOptions)

      currentTerm = getCurrentTerm()
      env = Environment(envCurrentTerm=currentTerm, envOpts=opts)

      state = retrieveState()
      result = runCommand(env, command, state)
      saveState(result)

This is not bad, for a very superficial understanding of Haskell syntax (there's only one obviously untranslatable bit, which requires knowing what a fold is. Not all Haskell code is folds; I’ll repeat, don’t worry about it too much!)

Most of the things I have called “line noise” actually have very deep reasons behind them, and if you’re curious behind the actual reasons behind these distinctions, I recommend learning how to *write* Haskell. But if you’re just reading Haskell, I think these rules should be more than adequate.

*Thanks* to Keegan McAllister, Mats Ahlgren, Nelson Elhage, Patrick Hurst, Richard Tibbetts, Andrew Farrell and Geoffrey Thomas for comments. Also thanks to two kind denizens of `#python`, `asdf`<span class="title-ref"> and </span><span class="title-ref">talljosh</span>\`, for acting as Python-using guinea pigs.

*Postscript.* If you're really curious what `foldl (>>=) (return startOptions) actions` does, it implements the [chain of responsibility](http://en.wikipedia.org/wiki/Chain-of-responsibility_pattern) pattern. Hell yeah.
