---
title: "Straitjacket programming"
date: 2010-03-15 09:00:41
slug: straitjacket-programming
categories: [Haskell]
comments:
    - id: 185
      author: gwern
      date: "2010-03-15 11:54:47"
      content: |
        "Every task involves constraint,
        Solve the thing without complaint;
        There are magic links and chains
        Forged to loose our rigid brains.
        Structures, strictures, though they bind,
        Strangely liberate the mind."
        
        James Falen, _Le Ton beau de Marot_
    - id: 186
      author: Xavier Noria
      date: "2010-03-15 12:05:39"
      content: "I don't think the opening analogy is valid. Dynamically typed languages are not unconstrained. Following your examples, I'd say they are the jazz of programming languages."
    - id: 187
      author: Edward Z. Yang
      date: "2010-03-15 12:37:13"
      content: "Xavier, runtime improvisation; sure, I'd buy that!"
    - id: 189
      author: Berengal
      date: "2010-03-15 13:16:23"
      content: |
        Hear hear!
        
        My main reason for liking Haskell is expressiveness, safety is just a bonus. I came from a Python background, so I was pretty used to doing whatever I wanted wherever I wanted, but Haskell soon started to feel even more so, with more opportunities for abstraction.
        
        After a while it even started to feel like I was having conversations with the compiler. It doesn't just tell me wether my program typechecks or not, it actually starts giving me comments and insights on my programs. It could be better at communicating its ideas though (see Agda.)
    - id: 190
      author: Greg Wilson
      date: "2010-03-15 13:44:06"
      content: "Is there any empirical data showing that static vs. dynamic typing makes any difference to programmers' productivity on non-trivial projects?  If not, isn't the debate about their intrinsic or relative values rather theological in nature?"
    - id: 191
      author: Poodle Shoes
      date: "2010-03-15 13:46:55"
      content: I like to color inside the lines. Sometimes restrictions squeeze out the best art.
    - id: 192
      author: Edward Z. Yang
      date: "2010-03-15 13:53:55"
      content: "Greg Wilson, I recall reading once about a military project that was implemented twice, once in a static language and once in a dynamic language. Unfortunately, I don't remember where I read about it, what the result was or what the relevant externalities were. ;-) I certainly don't have the empirical data, just my gut feeling when I need to make sweeping changes to a large codebase that I didn't write myself."
    - id: 193
      author: Luke Palmer
      date: "2010-03-15 16:31:47"
      content: |
        ... Structures, strictures, though they bind
        strangely liberate the mind.
        Shit!  Prelude.undefined!
        
        (The NullPointerException of Haskell)
    - id: 194
      author: Anonymous
      date: "2010-03-15 18:43:32"
      content: |
        Be careful. People use exactly the same argument to support proprietary computing with Apple computers and Apple devices. If that rings true then you should consider the validity of this line of reasoning.
        
        That said you're not talking about excessively proprietary apple products, so your point might still stand.
    - id: 196
      author: Edward Z. Yang
      date: "2010-03-15 18:52:11"
      content: "Hi Anonymous, I thought about your comment a bit, and to a certain extent, constraint on, say, design elements, is one of the reasons why Apple is able to deliver such a consistent and beautiful (in some eyes) user experience. And I would go as far to say that there's nothing wrong with making technology that encourages operation within these constraints and fostering social conventions that do the same. The problem is when you don't let people escape from the constraint, and when you enforce the constraints by legal mechanisms."
    - id: 200
      author: Joe Buehler
      date: "2010-03-16 07:39:44"
      content: "Thanks for the snippet about .  I am to the point where I am learning Functor, Monoid etc. and that helped illuminate what they are about."
    - id: 201
      author: Colin
      date: "2010-03-16 10:03:50"
      content: |
        You can see the static typing as something that places constraints on what you can write, but I do not find that point of view very appropriate or enlightening.
        
        A static type system gives you tools and bricks with which you can model something, and then work with that model; a dynamically typed language does not have these possibilities, you can "only" build with code.
    - id: 204
      author: Edward Z. Yang
      date: "2010-03-16 16:35:00"
      content: "Hi Colin, static types for modeling are indeed an excellent practice (I find myself constantly asking people \"what's the type?\" when they come to me with questions). The enforcement of the static types when you work with the model is what I primarily was talking about in this post: you can still model code with types in dynamically typed languages (using docblocks and such) but you loose a lot that you compiler has to offer."
    - id: 206
      author: Revzala Haelmic
      date: "2010-03-16 17:34:47"
      content: |
        Edward, how do you feel about holes in the straitjacket? 
        
        In Prelude and in Data.List modules there are such functions as head, tail, last, init, foldr1, cycle and (!!) that just throw an error for wrong arguments. Sometimes I imagine a reverse pyramid of functions based upon these, and wonder if Haskell has things that "strike terror in the hearts of the programmer" as much as in Java.
        
        One more thing is that dealing with exceptions in Java looks usual. Don't you think that in Haskell dealing with exceptions of the same kind looks a bit... alien, saying softly?
    - id: 207
      author: Edward Z. Yang
      date: "2010-03-16 19:09:33"
      content: |
        > if Haskell has things that "strike terror in the hearts of the programmer"
        
        Oh yes it does! Of late the library writers have gotten much better at prefixing such things that are not to be named with "unsafe", but we're still stuck with the original prelude and all of its warts. 
        
        > Don’t you think that in Haskell dealing with exceptions of the same kind looks a bit… alien, saying softly?
        
        Yes, and you shouldn't be using that mechanism anyway for pure code; the MonadFailure typeclass can give you a much more flexible mechanism for bubbling up errors to the point where you can handle it.
    - id: 299
      author: Frank Atanassow
      date: "2010-04-12 03:56:41"
      content: |
        Please stop perpetuating the myth that static typing is a straitjacket.
        
        The one-line refutation of this is that every computable function is representable by typed languages. How is that a straitjacket?
        
        The biggest reason this idea exists is that most people only ever bother to compare typed and untyped languages via one translation, namely type erasure. Type erasure exhibits a typed language as an untyped one by forgetting types, and that information is not in general recoverable (else type inference would be decidable). It also embeds typed programs in along with a bunch of other junk, most of which has no computational meaning except "bottom".
        
        There is at least one other translation from typed to untyped languages, and that one exhibits typed programs as two-phase untyped programs, the first phase being essentially compile-time and the second run-time. This translation does not forget the types and it is essentially surjective in the sense that "untypeable" programs can still be represented by making a canonical choice of a universal type (like "Dynamic" or what I often called "Univ").
        
        If you look at it from the other direction, also you can make a cogent argument that there is no straitjacket. There is a perfectly good translation from untyped languages to typed languages which exhibits untyped programs as programs of type Dynamic/Univ. To put it differently, according to this encoding, untyped programs are programs which can only ever make use of one type. I might well argue that THIS is a straitjacket. Hey, why do I have to use this one type to do all my programming when there are so many others?
        
        Even the idea that computation is fundamentally untyped is just a historical accident. There are perfectly good models of computation which are typed; they include every computable function.
        
        What about the real world? Is it untyped? No, we don't just have integers, or real numbers or whatever. We have units of mass AND units of charge AND units of length AND units of time. Velocity is not even a scalar. Maybe someday we will have a Grand Unified Theory and everything can be explained with one type, but until that day it is an article of faith. Even if that day comes, it would still just be a model, and a "typed GUT" would probably be just as useful and I am guessing even more insightful.
    - id: 301
      author: Edward Z. Yang
      date: "2010-04-12 12:30:56"
      content: |
        Hi Frank, thanks for the thoughtful comment.
        
        I suspect there may be a terminology mismatch in play here. In my opinion, you don't get to use a universal type like Dynamic and still call the program statically typed: the point behind "static" is that it happens compile-time. To this end, there is all sorts of research going on exploring how we can push more information to compile time without making the type system too unwieldy to use. Rather than "every computable function can be represented in a typed language" (which is vacuously true), I'd want to say "there are programs which are statically typed, which are a subset of all programs. There are some correct, perhaps even elegant, programs that are not statically typed, but we can't have compile time guarantees on them."
    - id: 307
      author: Frank Atanassow
      date: "2010-04-14 11:17:32"
      content: |
        &gt;  In my opinion, you don’t get to use a universal type like Dynamic and still call the program statically typed: the point behind “static” is that it happens compile-time.
        
        Then by your definition there is no such thing as a statically typed language because a universal type is always definable. (It has to be, for Turing-completeness.) I'm not being tricky or clever or positing some special language feature.
        
        The type Dynamic in GHC, for example, is only special because GHC generates embeddings to it automatically for user-defined types. But you can always write those functions yourself.
        
        I explained this long ago on LtU: http://lambda-the-ultimate.org/node/100  Search for "showsPrec" and read that post/subthread.
        
        No offense but: if you don't understand this then you don't understand what a "dynamically typed" language is. Of course, that would just put you in the same camp as just about everyone who uses those languages. There is nothing a dynamically typed language can do that a statically typed language cannot do. Nothing. A dynamically typed language is just a un(i)typed language.
    - id: 308
      author: Edward Z. Yang
      date: "2010-04-14 11:59:25"
      content: |
        Putting on my computer scientist hat, you're absolutely correct. Both typed and untyped languages can express all computable functions.
        
        Putting on my programmer hat, there's a difference between "it's possible" and "it's natural or convenient." It's a soft metric, but as far as I can tell, for many programmers, static types are burdensome. Are they a "straitjacket"? Maybe not; in Haskell, I rarely find myself in situations thinking "I don't know how to encode this in the type system!" (where by encode I hope to push some useful information into the types.) It's more of a problem when I write in, say, Java.
        
        When I was composing this post, I picked the name "Straitjacket Programming" near the end of the writing process as a pointed rhetorical device to catch people's eyes. I see how it might have backfired. :-)
    - id: 314
      author: Frank Atanassow
      date: "2010-04-15 08:31:42"
      content: |
        &gt; for many programmers, static types are burdensome.
        
        A motorcycle is a burden if you try to move it by pedalling because all you've ever seen is a bicycle. It's a burden for programmers who only know the type erasure translation: they keep trying to move by pedalling. Part of the reason they only know this translation is because everyone who writes about typing implicitly assumes this is the ONLY translation, and since it embeds statically typed programs in untyped languages rather than the other way around, they think the restrictiveness is a property of the languages rather than the translation. Teach them to stop pedalling.
        
        &gt; Both typed and untyped languages can express all computable functions.
        
        That is not the point. The point is that there are multiple ways to embed one in the other. The natural numbers are isomorphic to the integers, but I can also exhibit them as a proper subset of the integers: there are multiple embeddings.
---

The importance of constraint is one well known to those who embark on creative endeavors. Tell someone, "you can do anything you want: anything at all," and they will blank, paralyzed by the infinite possibility. Artists welcome constraint. Writers like the constraint of a sonnet because it imposes form and gives a place to start; roleplaying groups like the constraint of a campaign setting because it imposes rules and sets the scene for the story to be told; jazz musicians like the constraint of the chords underlying an improvisation because it keeps the soloist anchored to the source tune and suggests ideas for the melody.

However, many programmers don't the like the constraint of a type system. "The static type system doesn't let me do what I want to." "I needed to write four classes for what would have been two lines of Python!" "What? I can't do that? Why not?" For them, it's like a straightjacket. How does anyone ever get *anything* done when constraint ties you up?

![image](/wp-content/uploads/2010/03/Straitjacket-rear-300x151.jpg)

I beg to differ. *Accept* the straightjacket. The things it will let you do... are *surprising.*

------------------------------------------------------------------------

The straitjacket was historically used as an implement to prevent dangerous individuals from harming themselves and others. Programmers are not quite mental asylum inmates, though at a glance it may seem that we've been trying to reduce the ways for us to hurt ourselves. But such changes have often brought with them benefits, and many have eagerly traded away pointers and manual memory management for increased expressiveness.

Static types, however, are still a pain point for many people, and Haskell is an unusually constrained language due to its type system. An overenthusiastic user of Haskell's type system might exclaim, "after I made it typecheck, it just worked!" Of course, this statement is not actually true; there is a certain essential complexity to classes of algorithms that mean the type system won't catch the fact that you seeded your hash function with the wrong magic number.

But not all code is like this. A lot of code is just plain *boring*. It's the code that generates your website, or logs your errors; it's the code that serves as the glue for your build infrastructure, or it shuffles data from a file into an in-memory representation into a database. It's the code is foundational; it is the code that lets you express simple ideas simply. When you look at the development of this code, the errors being made are very simple mental typos, they're the ones that take a total of fifteen seconds to track down and fix once they manifest, but if rolled up in the time it takes to run your test suite or, dare I say it, *manually* test, quickly ticks to the minutes. A fast static type checker saves you so much pain, whether or not it is a Haskell compiler or `pylint -e`. The difference is that `pylint -e` is optional; there is no guarantee that any given Python project will play nicely with it, and it is frequently wrong. The Haskell compiler is not.

This is a specific manifestation of a more general phenomenon: types reduce the number of ways things can go wrong. This applies for complicated code too; `(a -> r) -> r` may not illuminate the meaning of the continuation to you, but it certainly puts a lot of restrictions on how you might go about implementing them. This makes it possible to look at the types without any understanding of what they mean, and mechanically derive half of the solution you're looking for.

This is precisely how types increase expressiveness: it's really hard for people to understand dense, highly abstracted code. Types prevent us from wading too far off into the weeds and make handling even more powerful forms of abstractions feasible. You wouldn't rely on this in Python (don't write Haskell in Python!), and in the few cases I've written higher-order functions in this language, I've been sure to also supply Haskell style type signatures. As Simon Peyton Jones has said, the type offers a "crisp" succinct definition of what a function does.

Even more striking is Haskell's solution to the null pointer problem. The exception that strikes terror in the hearts of the Java programmer is the `NullPointerException`: it's a [runtime exception](http://java.sun.com/j2se/1.4.2/docs/api/java/lang/RuntimeException.html), which means that it doesn't need to be explicitly declared in the `throws` specification of a method; a testament to the fact that basically any dereference could trigger this exception. Even in Java, a language of static typing, the type system fails to encode so basic a fact as "am I guaranteed to get a value here?"

Haskell's answer to this problem is the `Maybe` type, which explicitly states in the type of a function that the value could be `Nothing` (null) or `Just a` (the value). Programmers are forced to recognize that there might not be anything, and explicitly handle the failure case (with `maybe`) or ignore it (with `fromJust`, perhaps more appropriately named `unsafeFromJust`). There's nothing really special about the data type itself; I could have written a Java generic that had the same form. The key is the higher order functions that come along with the Functor, Applicative, Monad, MonadPlus, Monoid and other instances of this type. I'd run straight into a wall if I wanted to write this in Java:

    pureOperation <$> maybeVal

`<$>`, a higher order function also known as `fmap`, is critical to this piece of code. The equivalent Java would have to unpack the value from the generic, perform the operation on it, and the pack it up again (with conditionals for the case that it was empty). I could add a method that implements this to the Maybe interface, but then I wouldn't have an elegant way of passing `pureOperation` to these method without using anonymous classes... and you've quickly just exploded into several (long) lines of Java. It becomes dreadfully obvious why the designers didn't opt for this approach: an already verbose language would get even more verbose. Other languages aren't quite as bad, but they just don't get close to the conciseness that a language that celebrates higher order operators can give you.

In summary, while it may seem odd to say this about a language that has (perhaps undeservedly) earned a reputation for being hard to understand, but the constraint of Haskell's type system increases the tolerance of both writer and reader for abstraction that ultimately increases expressiveness. Problems that people just shrugged and claimed, "if you want to fix that, you'll have to add tons of boilerplate," suddenly become tractable. That's powerful.

One final note for the escape artists out there: if you need the dynamic typing (and I won't claim that there aren't times when it is necessary), you can [wriggle out of the static type system completely!](http://www.haskell.org/ghc/docs/6.12.1/html/libraries/base-4.2.0.0/Data-Dynamic.html) Just do it with caution, and not by default.
