---
title: "Keyword arguments in Haskell"
date: 2010-09-13 09:00:24
slug: keyword-arguments-in-haskell
categories: [Haskell, Python]
comments:
    - id: 1146
      author: The 27th Comrade
      date: "2010-09-13 11:47:45"
      content: "I like SML’s anonymous records, with which the above Python features are all possible. In my opinion, this is Haskell’s missing feature. But perhaps Haskell should be Haskell. :-)"
    - id: 1149
      author: Robert Massaioli
      date: "2010-09-13 18:35:09"
      content: "Good advice, newtypes are well suited to this purpose, and I really do wish that there was a way to not make record element names appear in the global scope; it is especially apparent when I have two different data types that should have the same record name."
    - id: 1150
      author: Alessandro Stamatto
      date: "2010-09-13 19:10:58"
      content: "Very Cool advice, a code like that is great to follow =)"
    - id: 1151
      author: Tim
      date: "2010-09-13 22:18:16"
      content: |
        Using newtypes like this is tidy but misses a couple of key features of python/haskell record style keyword arguments:
        
             - flexible argument ordering
             - the ability to specify default values, used when a keyword is not specified.
        
        The second of these is important enough to me that I use haskell records to simulate keyword args, but the single namespace for record fields is a significant irritation.
    - id: 1153
      author: Eyal Lotem
      date: "2010-09-14 05:38:34"
      content: |
        Tim: You could use a Default class:
        
        class Default d where
          default :: d
        
        And then instantiate defaults for each newtype.
    - id: 1155
      author: Sjoerd Visscher
      date: "2010-09-14 13:00:24"
      content: |
        If you think you need keyword arguments, you need to fix your function. A function should never need more than two arguments, three is the max. A function with more arguments usually is a sign that you need a datatype to hold some of the arguments which belong together. 
        
        renderBox should obviously be a function with one argument: a box.
    - id: 1156
      author: Tim
      date: "2010-09-14 19:27:08"
      content: |
        Eyal:
        
        Agreed, you could use a typeclass for defaulting, but this means you'd end up with code like:
        
            renderBox (XPos 2) (YPos 4) default default
        
        which misses out on a key benefit of defaults - not having to mention them. Sometime I want to make an API extensible, adding function parameters without needing to change existing call sites. Using records is the only way I know to do this in haskell.
    - id: 1170
      author: Andrew Pennebaker
      date: "2010-09-17 11:53:24"
      content: |
        Default values are also useful (see http://www.haskell.org/haskellwiki/Default_values_in_records).
        
        data Foo = Foo { bar :: Int, baz :: Int, quux :: Int }
         
        fooDefault = Foo { bar = 1, baz = 2, quux = 3 }
         
        newRecord = fooDefault { quux = 42 }
    - id: 1187
      author: Ben
      date: "2010-09-18 09:14:36"
      content: |
        I'm fairly new to Haskell, and I'm not really following why you're using a newtype instead of a data. (I'm generally not sure what the purpose of newtype is.)
        
        I understand that a data declaration requires a separate type and constructor, but it still seems like it would work the same.
    - id: 1188
      author: Edward Z. Yang
      date: "2010-09-18 12:16:44"
      content: "Hello Ben. Newtypes are a restricted form of data declarations (only one constructor is allowed and it may only have one field), but the benefit is that they are optimized away at compile time, so it really is just a static check."
    - id: 6290
      author: "Is there a nice way to make function signatures more informative in Haskell? | Ask Programming &amp; Technology"
      date: "2013-11-08 10:50:30"
      content: "[&#8230;] Googling around, and I even found this little post that suggests named parameters, specifically, spoofing named parameters via newtype, but that seems to be a bit [&#8230;]"
    - id: 6373
      author: Anonymous
      date: "2014-02-04 01:27:55"
      content: "Awesome!  Thanks."
    - id: 23565
      author: Kiara Grouwstra
      date: "2020-03-26 14:45:07"
      content: |
        &gt; If the type already says all you need to say about an argument, there’s no need to newtype it again. Thus, you can have a mix of regular and newtype arguments.
        
        To be pedantic here, this isn't as nice as in Python -- over there the user still has some choice in which arguments to specify in which way, whereas in Haskell, they don't, as instead the choice is made by the one who wrote the function the user wishes to call.
    - id: 27495
      author: Evi1M4chine
      date: "2022-03-23 17:26:22"
      content: |
        I used to be a big fan of Haskell. Before I had to actually use it in practice, to get things done.
        
        Haskell has GOT to be the most obfuscated language that is not a joke language. It does everything wrong that mathematicians do wrong, and for the same snobist reasons.
        
        Everything is named in the most obscure and cryptic way possible, and if possible, not named at all. And you have to guess what something is for through as many levels of abstraction and indirection as possible. Like “Make implicit ALL the things!”
        
        Like type classes of various orders and monads with transformers that are more than meets the eye you you’re always stuck for way too long to ponder which concrete function is actually executed for these given types.
        
        And it seems, the creators, in their institutes, cannot imagine functions with two parameters of equal types where you have to know which of your variables goes into which slot. No, being “typeful” is not magic. It does not solve it! And using newtypes there is exactly the kind of boilerplate crap that I fled to Haskell to avoid!
        
        That’s another thing that’s stunning for Haskell: How much boilerplate you have to write, just to get things done. Template Haskell’s existence is hard proof that Haskell failed. Everything that it does should be part of Haskell proper, without such horrible PHP(reprocessor)-level hacks.
---

Keyword arguments are generally considered a good thing by language designers: positional arguments are prone to errors of transposition, and it’s absolutely no fun trying to guess what the `37` that is the third argument of a function *actually* means. Python is one language that makes extensive use of keyword arguments; they have the following properties:

1.  Functions are permitted to be a mix of positional and keyword arguments (a nod to the compactness of positional arguments),
2.  Keywords are local to any given function; you can reuse a named function argument for another function,
3.  In Python 3.0, you can force certain arguments to *only* be specifiable with a keyword.

Does Haskell have keyword arguments? In many ways, they’re much less necessary due to the static type system: if you accidentally interpose an `Int` and `Bool` your compiler will let you know about it. The type signature guides you!

Still, if we were to insist (perhaps our function took many arguments of the same type), one possibility is to pass a record data type in as the sole argument, but this is a little different than Python keyword arguments in the following ways:

1.  There is a strict delineation between positional and keywords: either you can specify your record entirely with keywords or entirely with positional arguments, but you can’t do both,
2.  Record fields go into the global namespace, so you have to prefix/suffix them with some unique identifier, and
3.  Even with named records, a user can still choose to construct the record without specifying keyword arguments. For large argument lists, this is not as much of an issue, but for short argument lists, the temptation is great.

I find issue two to be the reason why I don’t really employ this trick; I would find it quite annoying to have to make a data structure for every function that I wanted to use named arguments with.

I’d like to suggest another trick to simulate named arguments: use newtypes! Consider this undertyped function:

    renderBox :: Int -> Int -> Int -> Int -> IO ()
    renderBox x y width height = undefined

    main = renderBox 2 4 50 60

We can convert it to use newtypes like this:

    newtype XPos = XPos Int
    newtype YPos = YPos Int
    newtype Width = Width Int
    newtype Height = Height Int

    renderBox :: XPos -> YPos -> Width -> Height -> IO ()
    renderBox (XPos x) (YPos y) (Width width) (Height height) = undefined

    main = renderBox (XPos 2) (YPos 4) (Width 50) (Height 60)

Unlike the usual use of newtypes, our newtypes are extremely short-lived: they last just long enough to get into the body of `renderBox` and then they are pattern matched to oblivion: the function body can rely on good local variable names to do the rest. But it still manages to achieve the goals of keyword arguments: any call to `renderBox` makes it crystal clear what each integer means. We also maintain the following good properties:

1.  If the type already says all you need to say about an argument, there’s no need to newtype it again. Thus, you can have a mix of regular and newtype arguments.
2.  Newtypes can be reused. Even further, they are only to be reused when the semantic content of their insides is the same, which encourages good naming practices.
3.  The user is forced to do the newtype wrapping: there’s no way around it. If you publish smart constructors instead of the usual constructors, you can factor out validation too.

Newtypes are so versatile!
