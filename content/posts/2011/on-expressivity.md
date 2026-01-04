---
title: "On expressivity"
date: 2011-03-16 09:00:12
slug: on-expressivity
categories: [Programming]
comments:
    - id: 1930
      author: Toddeman
      date: "2011-03-16 13:46:51"
      content: |
        Great post! It makes me wonder if you work where I do though. Just yesterday I was having a conversation with some friends and coworkers about exactly this. 
        
        I must say though, you put it much nicer than I did. I come across as an evangelist sometimes.
    - id: 1933
      author: Don Stewart
      date: "2011-03-17 03:06:01"
      content: "Check out Felleisen's famous paper on what expressiveness means. \"On the expressive power of programming languages\""
    - id: 1935
      author: Mathnerd314
      date: "2011-03-17 10:00:49"
      content: |
        I would term some of these "compiler features" or "runtime features" rather than language features. After all, it is rather hard to do I/O or concurrency in Haskell with -XNoImplicitPrelude and no imports, whereas using Template Haskell without Language.Haskell.TH is at least possible. In assembly, on the other hand, I believe one can write a self-contained Hello World program without recourse to any outside libraries except the UNIX interface. Thus assembly should be in some way "more expressive" than Haskell in terms of I/O, while "less expressive" in terms of macros.
        
        The problem with your notion of "expressiveness" is that it depends on the entire compiler-library-program infrastructure. What is a library call in one might require hundreds of lines of code in another. You could collapse all of those components into the term "language", but then you've lost contact with compiler-writers.
---

<div class="container center">

*Wherein I make fun of functional programming advocates.*

</div>

In this essay, I’d like to discuss the ideologies of “imperative programming” and “functional programming” in terms of the language features they lean on: in particular, the mechanisms by which they allow developers to express themselves in less code. I propose that the set of features that make up imperative programming constitute a dominant programming monoculture that is partially incompatible with functional programming’s favored features, requiring functional programming advocates to do funny things to gain the attention of the programmers.

To first give a flavor of *expressiveness*, here are some frequently seen language features that increase expressiveness:

- Macros
- Concurrency
- Mutation
- Indirection
- Laziness
- Dynamic typing
- Polymorphism
- Higher-order functions
- Exceptions
- Eval
- Input/Output
- Continuations
- Anonymous functions

A few of those entries might make you laugh, because you might not understand how you could program without them. You may recognize a few that your favorite language supports well, a few that your language supports less well, and a few your language has no support for. The culture around the language will also have its folklore about what kinds of features are acceptable for use and which ones are not (think Pythonic or *JavaScript: The Good Parts*). The language you choose determines which features you get to know well.

Being expressive has a cost, which most developers figure out with a little experience in the field. There is a sort of natural selection going on here: language features that are well supported by languages, that other programmers know how to use, and that allow the job to get done are favored—in particular the community effect reinforces the winner. As a result, we have developer monoculture that is mostly comfortable with mutation, input/output, indirection, exceptions, polymorphism, etc. But even the bread-and-butter of current programming practice doesn’t come without cost: think about the famed division between “those who get pointers and those who don’t”, or the runtime costs of using exceptions in C++, or the representation complications of polymorphism (e.g. autoboxing in Java and Go’s lack thereof).

When someone does *functional programming advocacy*, what they’re really doing is asking you to look more closely at some of the other mechanisms we have for increasing expressiveness. You might feel like these are the only voices you hear, because there’s not much point advocating something that everyone uses already. And you might feel like the enthusiasm is unjustified, because the feature seems bizarrely complicated (continuations, anyone?) or you’ve tried using it in your favorite language, and there’s nothing more painful than seeing someone try to do functional programming in Python. Fact of the matter is, it’s not easy to add these extra language features to the existing monoculture. The new features interact in very complex and subtle ways.

This is why a functional programming advocate will often ask you to give up some of your old tools of expression. They will ask you to give up shared-state mutation, because otherwise handling concurrency is really damn hard. They will ask you to give up dynamic typing, because otherwise higher-order functions become much harder to reason about. The rhetoric will edge on the side of “stop doing that!” because it’s the common practice—they don’t actually mean “stop it entirely” but to the poor functional programming advocate it seems like a little bit of hyperbole is necessary to get the point across—and you can do some pretty amazing things with these extra features.

I encourage programmers to learn about as many ways to express themselves as possible, even if their language or workplace won’t necessarily allow them to use the method. The reasons are manifold:

1.  “Any complex enough program eventually contains a poorly written implementation of Lisp.” Like it or not, eventually you will be faced with a hard problem that is handily dispatched by one of these well studied language features, and if you’re going to have to implement it by hand, you might as well know how it’s going to look like from the start. As the Gang of Four once said, language features that are not actually supported by the language often show up as design patterns; knowing the pattern makes your code clearer and cleaner.
2.  Conversely, if you are reading someone else’s code and they resort to using one of these patterns, knowing how the feature should work will greatly aid comprehension.
3.  Libraries and frameworks are considered essential to the working developer’s toolbox, yet they seem to grow and be obsoleted at a dizzying rate. Language features are eternal: the anonymous functions of 1936 (when Alonzo Church invented the lambda calculus) are still the anonymous functions of today.
4.  Language features are fun to learn about! Unlike “yet another API to memorize”, a language feature will tickle your brain and make you think very hard about what is going on.

**tl;dr** Certain programming language features increase developer expressiveness, and the “imperative programming methodology” captures the dominant monoculture containing those language features in wide use. But there are other ways of expressing oneself, and programmers are encouraged to explore these methods even when practical use necessitates them to stop using some of their favorite expressive tools.
