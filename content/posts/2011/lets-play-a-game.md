---
title: "Let's play a game"
date: 2011-09-05 04:04:46
slug: lets-play-a-game
categories: [Haskell]
comments:
    - id: 2952
      author: Smarter
      date: "2011-09-05 08:47:55"
      content: "Great! But didn't you mean \"((a -&gt; r) -&gt; r)\" instead of \"((a -&gt; r) -&gt; a)\" in the third and fourth line of the transformation?"
    - id: 2953
      author: Edward Z. Yang
      date: "2011-09-05 12:05:59"
      content: "Thanks, fixed."
    - id: 2957
      author: Bryan
      date: "2011-09-05 19:41:21"
      content: |
        I have a hard time following this material.  I can follow the first proposal, but I don't understand the second or how it relates to the first.
        
        What exactly is the second proposal stating?  I think if you put it in words like you did the first one"(which says given a machine which turns As into Bs, and an A, it can create a B)" that would help.
        
        Also I am generally interested in this subject.  Do you have any suggested resources for logic and intuitionistic logic?
        
        Thanks,
        Bryan
    - id: 2960
      author: Anonymous
      date: "2011-09-05 22:14:37"
      content: "I for one found this extremely confusing. Not only are some of the sentences difficult to parse, but the terminology doesn't lend itself to intuition (inventer = proposer?). Also, the diagrams seem to have their own language going on (despite the cute monopoly character) but they're completely unexplained. Sorry."
    - id: 2961
      author: Luke Palmer
      date: "2011-09-05 23:22:02"
      content: You have just painted the picture of the proof system I have always wanted to use.
    - id: 2962
      author: Anonymous
      date: "2011-09-06 03:26:45"
      content: |
        It *is* confusing. The cloud is a sub-function, the orange boxes are parameters to it, "I can make a 'b-&gt;a' ".
        
        task :: ((b -&gt; a) -&gt; c) -&gt; a -&gt; c
        task f a = f cloud where cloud = \b -&gt; a
    - id: 2968
      author: Edward Z. Yang
      date: "2011-09-11 16:28:36"
      content: |
        For those of you who were confused, sorry about the long delay. I've been thinking hard about how to clarify things, and I'll probably do a follow-up post.
        
        > What exactly is the second proposal stating?
        
        It's complicated, which is why I don't say it out loud. But in English, it's "Given a machine which takes machines which turn As into Rs and gives you an R, a machine which turns Bs into Rs, and a machine which takes As and machines which turn Bs into Rs, gives you an R, give me an R." (Just look at the picture...)
        
        > Also I am generally interested in this subject. Do you have any suggested resources for logic and intuitionistic logic?
        
        I highly recommend the little yellow book, "Lectures on the Curry-Howard Isomorphism"
    - id: 2974
      author: Alexander Solla
      date: "2011-09-15 14:52:07"
      content: |
        Some more comments:
        
        Edward is describing the class of "derivable functions".  These are also called the "free functions" for the language.  The connection between freeness and derivability is very deep mathematically, and can basically be constructed a natural transformation from the elements of the Howard-Curry isomorphism theorem to the categorical models of constructive logic.  In any event, the point is that these derivable functions are derivable because their type admits only one interpretation.  In the language of the Howard-Curry isomorphism theorem, the types are /theorems/ of a constructive language.
        
        For contrast, consider the type f :: Real -&gt; Real.  There are obviously lots of functions of the Real numbers to the Real numbers.  Sine and cosine form a pair, and they  have different semantics.
        
        Now consider a function f :: () -&gt; ().  Obviously, since there is only one element in the type (), there can only be one function with that type: f () = ().
        
        I am eliding talk of "bottom" -- it is not a value in the language of Haskell -- it is a semantic artifact of the language's interpretation.  In a "real" first order logic with equality, the sentence forall x . (x = x) holds.  Haskell's closest equivalent to FOL's equality does not hold for things like functions, so we must perform a quotient construction to recover a first-order language with equality (the environment which provides the right tools for reasoning about Haskell most completely).  A bottom is any thing x which does not satisfy (x == x).  In other words, a bottom is the embodiment of logical contradiction.  This is why we can say things like "Bottom is an element of every type" and simultaneously say things like "Bottom is not a value".  It is already a contradictory notion, so the apparent contradiction is merely a consequence of bottom's semantics.  Things can be slippery here -- it can be useful to read "Fast and Loose Reasoning is Morally Correct" to get a better sense of what is going on here.
    - id: 2975
      author: Anonymous
      date: "2011-09-16 04:37:29"
      content: |
        The subject of the post is very interesting, but unfortunately after the sentence:
        
        "The verifier is obligated to furnish more assumptions for this new proposal, but these are placed inside the cloud of abstraction.",
        
        everything becomes very difficult to follow.
        
        I mean, the second example is very cryptic (for me).
        
        It would be really nice to have a follow up with a more detailed explanation, thanks.
---

Ever wondered how Haskellers are magically able to figure out the implementation of functions just by looking at their type signature? Well, now you can learn this ability too. Let’s play a game.

You are an inventor, world renowned for your ability to make machines that transform things into other things. You are a **proposer**.

![image](/img/cont-game/proposer.png)

But there are many who would doubt your ability to invent such things. They are the **verifiers**.

![image](/img/cont-game/verifier.png)

The game we play goes as follows. You, the proposer, make a **claim** as to some wondrous machine you know how to implement, e.g. `(a -> b) -> a -> b` (which says given a machine which turns As into Bs, and an A, it can create a B). The verifier doubts your ability to have created such a machine, but being a fair minded skeptic, furnishes you with the inputs to your machine (the **assumptions**), in hopes that you can produce the **goal**.

![image](/img/cont-game/basic.png)

As a proposer, you can take the inputs and machines the verifier gives you, and **apply** them to each other.

![image](/img/cont-game/apply.png)

But that's not very interesting. Sometimes, after the verifier gives you some machines, you want to make another proposal. Usually, this is because one of the machines takes a machine which you don’t have, but you *also* know how to make.

![image](/img/cont-game/lambda.png)

The verifier is obligated to furnish more assumptions for this new proposal, but these are placed inside the cloud of **abstraction**.

![image](/img/cont-game/levels.png)

You can use assumptions that the verifier furnished **previously** (below the cloud of abstraction),

![image](/img/cont-game/accessible.png)

but once you’ve finished the proposal, all of the new assumptions **go away**. All you’re left with is a shiny new machine (which you ostensibly want to pass to another machine) which can be used for the original goal.

![image](/img/cont-game/discharge.png)

These are all the rules we need for now. (They constitute the most useful subset of what you can do in constructive logic.)

Let’s play a game.

![image](/img/cont-game/cont.png)

Our verifier supplies the machines we need to play this game. Our goal is `r`.

![image](/img/cont-game/cont1.png)

That’s a lot of machines, and it doesn't look like we can run any of them. There's no way we can fabricate up an `a` from scratch to run the bottom one, so maybe we can make a `a -> r`. (It may seem like I’ve waved this proposal up for thin air, but if you look carefully it’s the only possible choice that will work in this circumstance.) Let’s make a new proposal for `a -> r`.

![image](/img/cont-game/cont2.png)

Our new goal for this sub-proposal is also `r`, but unlike in our original case, we can create an `r` with our extra ingredient: an `a`: just take two of the original machines and the newly furnished `a`. Voila, an `r`!

This discharges the cloud of abstraction, leaving us with a shiny new `a -> r` to pass to the remaining machine, and fulfill the original goal with.

![image](/img/cont-game/cont3.png)

Let's give these machines some names. I’ll pick some suggestive ones for you.

![image](/img/cont-game/named.png)

Oh hey, you just implemented **bind** for the **continuation monad**.

![image](/img/cont-game/bind.png)

Here is the transformation step by step:

    m a -> (a -> m b) -> m b
    Cont r a -> (a -> Cont r b) -> Cont r b
    ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
    ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r

The last step is perhaps the most subtle, but can be done because arrows right associate.

As an exercise, do `return :: a -> (a -> r) -> r` (wait, that looks kind of familiar...), `fmap :: (a -> b) -> ((a -> r) -> r) -> (b -> r) -> r` and `callCC :: ((a -> (b -> r) -> r) -> (a -> r) -> r) -> (a -> r) -> r` (important: that’s a `b` inside the first argument, not an `a` !).

This presentation is the **game semantic** account of intuitionistic logic, though I have elided treatment of **negation** and **quantifiers**, which are more advanced topics than the continuation monad, at least in this setting.
