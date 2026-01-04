---
title: "If you're using lift, you're doing it wrong (probably)"
date: 2013-09-26 15:03:42
slug: if-youre-using-lift-youre-doing-it-wrong-probably
categories: [Haskell]
comments:
    - id: 6252
      author: heatsink
      date: "2013-09-26 20:01:22"
      content: "I've settled on a development style that uses monad transformers to derive problem-specific monads, but wraps them in a newtype to hide their generic structure.  Each newtype'd monad is accompanied by meaningfully named wrappers around 'ask', 'get', 'set', etc., lifted as necessary.  Only these wrappers deal with the monad stack.  This way, monad transformers remove the work of writing boilerplate monad implementations, but they don't eliminate the need to write an interface, and I'm fine with that."
    - id: 6254
      author: Benjamin Barenblat
      date: "2013-09-27 12:13:57"
      content: |
        For those of us not at ICFP, what alternatives to monad transformers have people been discussing?
        
        Additionally, I’ve heard strong arguments in favor of heatsink’s approach – in particular, that exposing type class instances for your transformer stack is antimodular. (By exposing the constituents of your stack, the argument goes, you make it more difficult to modify that stack later on.) Do you think this is merely a problem with transformers? Do the alternatives solve this issue?
    - id: 6255
      author: Edward Z. Yang
      date: "2013-09-27 12:21:44"
      content: "There were a bunch (I think 2 in the main conference, and some more in the affiliated events) papers this year about algebraic effects and handlers. I don't have a good handle on the state of this research program."
    - id: 15126
      author: Muzietto
      date: "2015-07-17 13:46:31"
      content: "My view on his subject is that providing lifted functions for all transformers on all layers actually makes it harder to learn the subject. People believe they can use ask, put or tell regardless of the way the stack is implemented and become sloppy. Then you catch a subtle bug when you don't expect it and without a clear view of the theory it's hard to fix things. I'd rather have available raw Lego-blocks to build my monadic onion and then write lifters when I see those I need more often."
---

David Darais asked me to make this public service announcement: *If you're using lift, you're doing it wrong.* This request was prompted by several talks at ICFP about alternatives to monad transformers in Haskell, which all began their talk with the motivation, "Everyone hates lifting their operations up the monad stack; therefore, we need another way of organizing effects." This [StackOverflow question](http://stackoverflow.com/questions/9054731/avoiding-lift-with-monad-transformers) describes the standard technique that `mtl` uses to remove the use of lift in most monadic code.

Now, as most things go, the situation is a bit more nuanced than just "never use lift", and a technically incorrect quip at the beginning of a talk does not negate the motivation behind other effect systems. Here are some of the nuances:

- As everyone is well aware, when a monad transformer shows up multiple times in the monad stack, the automatic type class resolution mechanism doesn't work, and you need to explicitly say which monad transformer you want to interact with.
- This mechanism only works if the monadic operations you are interacting with are suitably generalized to begin with, e.g. `MonadReader a m => m a` rather than `Monad m => ReaderT m a` or `Reader a`. This is especially evident for the `IO` monad, where most people have not generalized their definitions to `MonadIO`. Fortunately, it is generally the case that only one `liftIO` is necessary.

And of course, there are still many reasons why you would want to ditch monad transformers:

- Type-class instances are inherently unordered, and thus a generalized `MonadCont m, MonadState m => m a` monadic value says nothing about what order the two relevant monads are composed. But the order of this composition has an important semantic effect on how the monad proceeds (does the state transfer or reset over continuation jumps). Thus, monad transformers can have subtle interactions with one another, when sometimes you want *non-interfering* effects that are truly commutative with one another. And indeed, when you are using the type class approach, you usually use only monads that commute with one another.
- The interference between different monad transformers makes it difficult to lift certain functions. For example, the type of `mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b`. If we think operationally what has to happen when IO is composed with State, the lifter has to some how arrange for the state to transfer all the way into the code that runs with exceptions restored. That's very tricky to do in a general way. It gets even worse when these callbacks are [invoked multiple times.](http://blog.ezyang.com/2012/01/monadbasecontrol-is-unsound/)
- At the end of the day, while the use of type classes makes the monad stack somewhat abstract and allows the elision of lifts, most of this code is written with some specific monad stack in mind. Thus, it is very rare for nontrivial programs to make use of multiple effects in a modular way, or for effects to be instantiated (i.e. a concrete monad selected) without concretizing the rest of the monad stack.

Monad transformers have problems, let's argue against them for the right reasons!
