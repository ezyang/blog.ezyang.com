---
title: "Intelligence is the ability to make finer distinctions: Another Haskell Advocacy Post"
date: 2010-10-29 09:00:13
slug: finer-distinctions
categories: [Haskell]
comments:
    - id: 1378
      author: Simon Michael
      date: "2010-10-29 14:13:04"
      content: Very well said.
    - id: 1379
      author: gasche
      date: "2010-10-29 15:04:25"
      content: |
        &gt; Implicit laziness has a number of notable benefits. It permits user-level
        &gt; control structures. It encodes streams and other infinite data
        &gt; structures. It is more general* than strict evaluation. It is critical in the
        &gt; construction of amortized persistent data structures. (Okasaki)
        
        None of the example above rely on *implicit* lazyness. Most of them are well with *explicit* lazyness (as you would have in a strict-by-default language with a thunk/force facility), and I think the last one (fine-grained algorithmic computations) are easier with explicit¹ lazyness.
        
        ¹: this is based upon the slightly biased consideration that evaluation control is easier with call-by-value by default, and explicit call-by-need, rather than the other way around, as it is generally easier (here lies the bias) to predict the evaluation behavior of call-by-value code, so it's better if the more difficult part are made explicit.
    - id: 1386
      author: Colin Adams
      date: "2010-11-01 07:26:05"
      content: |
        What about the finer distinction between non-strict and lazy?
        
        Are there any features of Haskell in practise that rely on lazy (as opposed to non-strict) evaluation? I know the standard doesn't mandate laziness, but is there anything in the libraries that do?
    - id: 1389
      author: Edward Z. Yang
      date: "2010-11-03 10:08:36"
      content: |
        gasche: Yeah, the view you just expressed is fairly common. I’m still not fully sure that non-strict semantics are a good idea, though I do lean in that direction. As Colin points out, there's a further distinction between non-strict and lazy, and when GHC performs strictness analysis and eliminates laziness, it is only allowed to do so because it doesn't change the semantics of the expression. But perhaps the performance of the expression is an essential part of its semantics!
        
        One possible way to think of it is that I should write my code in the most general way possible, and then add annotations to fix performance problems, which recommends non-strict semantics. Another possible line of thought is that robust, multithreaded thunks are actually kind of hard to implement, and so non-strict semantics force us to be honest with our implementation, and make it easier to use non-strict evaluation.
        
        Colin: I think the answer to your second question is no, because non-strict is by definition observationally equivalent to lazy evaluation: lazy evaluation is one way to implement non-strict evaluation, although not necessarily the fastest.
    - id: 1395
      author: Colin Adams
      date: "2010-11-04 04:34:58"
      content: "The reason I asked the second question was some time ago I read a paper (can't remember what it was called, otherwise I'd be re-reading it) that showed a hierarchy of eager -&gt; non-strict -&gt; lazy, and then showed their capabilities as limited by the hierarchy (I think it was something like you needed non-strict for circular lists, and lazy for infinite lists, but it may have been the other way round). Your answer is apparently contradicting my admittedly somewhat vague recollections of the paper."
---

<div class="container center">

*I apologize in advance for another* [Haskell advocacy piece](http://blog.ezyang.com/2010/01/why-haskell/)

</div>

My parents like foisting various self-help books on me, and while I sometimes groan to myself about it, I do read (or at least skim) them and extract useful bits of information out of them. This particular title quote is from Robert Kiyosaki’s “rich dad” in *Rich Dad, Poor Dad.*

“Intelligence is the ability to make finer distinctions” really spoke to me. I’ve since found it to be an extremely effective litmus test to determine if I’ve really understood something. A recent example comes from my concurrent systems class, where there are many extremely similar methods of mutual exclusion: mutexes, semaphores, critical regions, monitors, synchronized region, active objects, etc. True knowledge entails an understanding of the conceptual details differentiating these gadgets. What are the semantics of a *signal* on a condition variable with no waiting threads? Monitors and synchronized regions will silently ignore the signal, thus requiring an atomic release-and-wait, whereas a semaphore will pass it on to the next *wait*. Subtle.

------------------------------------------------------------------------

We can frequently get away with a little sloppiness of thinking, indeed, this is the mark of an expert: they know precisely how much sloppiness they can get away with. However, from a learning perspective, we’d like to be able to make as fine a distinction as possible, and hopefully derive some benefit (either in the form of deeper understanding or a new tool) from it.

Since this is, after all, an advocacy piece, how does learning Haskell help you make finer distinctions in software? You don’t have to look hard:

> Haskell is a standardized, general-purpose **purely** functional programming language, with **non-strict semantics** and strong static typing.

These two bolded terms are concepts that Haskell asks you to make a finer distinction on.

------------------------------------------------------------------------

*Purity.* Haskell requires you to make the distinction between pure code and input-output code. The very first time you get the error message “Couldn't match expected type `[a]` against inferred type `IO String`” you are well on your way to learning this distinction. Fundamentally, it is the difference between *computation* and *interaction with the outside world*, and no matter how imperative your task is, both of these elements will be present in a program, frequently lumped together with no indication which is which.

Pure code confers tremendous benefits. It is automatically thread safe and asynchronous exception safe. It has no hidden dependencies with the outside world. It is testable and deterministic. The system can speculatively evaluate pure code with no commitment to the outside world, and can cache the results without fear. Haskellers have an obsession with getting as much code outside of IO as possible: you don’t have to go to such lengths, but even in small doses Haskell will make you appreciate how what is considered good engineering practice can be made rigorous.

------------------------------------------------------------------------

*Non-strict semantics.* There are some things you take for granted, the little constants in life that you couldn’t possibly imagine be different. Perhaps if you stopped and thought about it, there was another way, but the possibility never occurred to you. Which side of the road you drive is one of these things; strict evaluation is another. Haskell asks you to distinguish between strict evaluation and lazy evaluation.

Haskell isn’t as in your face about this distinction as it is about purity and static typing, so it’s possible to happily hack along without understanding this distinction until you get your first stack overflow. At which point, if you don’t understand this distinction, the error will seem impenetrable (“but it worked in the other languages I know”), but if you are aware, a stack overflow is easily fixed—perhaps making the odd argument or data constructor explicitly strict.

Implicit laziness has a number of notable benefits. It permits user-level control structures. It encodes streams and other infinite data structures. It is more general\* than strict evaluation. It is critical in the construction of amortized persistent data structures. (Okasaki) It also is not always appropriate to use: Haskell fosters an appreciation of the strengths and weaknesses of strictness and laziness.

> \* Well, almost. It only fully generalizes strict evaluation if you have infinite memory, in which case any expression that strictly evaluates also lazy evaluates, while the converse is not true. But in practice, we have such pesky things as finite stack size.

------------------------------------------------------------------------

*The downside.* Your ability to make finer distinctions indicates your intelligence. But on the same token, if these distinctions don’t become second nature, they impose a cognitive overhead whenever you need invoke them. Furthermore, it makes it difficult for others who don’t understand the difference to effectively hack on your code. (Keep it simple!)

Managing purity is second-nature to experienced Haskellers: they’ve been drilled by the typechecker long enough to know what’s admissible and what’s not, and given the mystique of monads, it’s usually something people actively try to learn when starting off with Haskell. Managing strictness also comes easily to experienced Haskellers, but has what I perceive to be a higher learning curve: there is no strictness analyzer yelling at you when you make a suboptimal choice, and you can get away with not thinking about it most of the time. Some might say that lazy-by-default is not the right way to go, and are [exploring the strict design space](http://trac.haskell.org/ddc/). I remain optimistic that we Haskellers can build up a body of knowledge and teaching techniques to induct novices into the mysteries and wonders of non-strict evaluation.

------------------------------------------------------------------------

So there they are: purity and non-strictness, two distinctions that Haskell expects you to make. Even if you never plan on using Haskell for a serious project, getting a visceral feel for these two concepts will tremendously inform your other programming practice. Purity will inform you when you write thread-safe code, manage side-effects, handle interrupts, etc. Laziness will inform you when use generators, process streams, control structures, memoization, fancy tricks with function pointers, etc. These are seriously powerful engineering tools, and you owe it to yourself to check them out.
