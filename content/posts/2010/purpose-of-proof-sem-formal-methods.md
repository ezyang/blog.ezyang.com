---
title: "Purpose of proof: semi-formal methods"
date: 2010-10-20 09:00:42
slug: purpose-of-proof-sem-formal-methods
categories: [Compilers, Computer Science, Math, Programming]
comments:
    - id: 1318
      author: Thomas
      date: "2010-10-20 13:19:37"
      content: |
        Ask not whether a program is a proof, ask whether your proof is a program. Then you get correctness "for free." Having a program and a specification as separate entities leads to all sorts of problems. Having the logic in which you reason about your programs behaviour separate from the implementation of the program is also a bad thing if you do not have a trustworthy and autmated method of reconciling the two representations.
        Synthesizing the program from a proof seems to be the right way to go: You don't have separation of implementation and specification and there is a sound way of arriving at an implementation given its specification. If you've got the wrong properties or your specification is not strong enough you'll more or less immediately see it in the generated code. If both your intuition about how to write a piece of code and the generated code from your specification conincide you can be very sure that you've got it right.
    - id: 1319
      author: Edward Z. Yang
      date: "2010-10-20 13:28:59"
      content: "Don't get me wrong: I love types (the most widely used formal method used today) and I think formal methods have a lot to offer. However, I think we also have to look at how incredibly difficult computer acceptable levels of formality are to achieve (just ask the mathematicians), and I can't help but wonder if programmer's insistence on the computer doing all the work has set back our craft. But this is all speculation :-)"
    - id: 1321
      author: Jeffrey Bosboom
      date: "2010-10-20 20:29:26"
      content: "When writing concurrent code I often do semi-formal proofs by cases for my own sake: \"if thread B calls this function while no thread has lock X, then... / ... while thread A has lock X, then... / while thread B has lock X, then...\".  It's a lot easier to make sure all of the state transitions (if you will) are correct and lead to valid states then to say (and know) something like \"all accesses to this variable are guarded by lock X, so the code is correct\"."
    - id: 1322
      author: Edward Z. Yang
      date: "2010-10-21 07:47:25"
      content: |
        Hello Jeffrey; proof by enumeration is indeed a simple proof method, and a good first step to rigor. Unfortunately, my experience has been that due to the open nature of threads or lock accesses, this a fairly error prone approach. I think a case where you are doing a simple exhaustive proof is an opportunity for a better abstraction, or maybe formal methods. To extend you exmple, suppose I want to prove this lock never deadlocks: in that case, an exhaustive proof will require exponentially many steps.
        
        In this way, you could say that when we write code in a good style, we are attemptimg to suggest an obvious proof method. Of course, obvious is in the eye of the beholder.
    - id: 1323
      author: solrize
      date: "2010-10-21 22:20:37"
      content: "Another very interesting post and I'd like to see more in its vein.  Types are great but they only say things about values, and values are only half the computing story.  We also have to care about operational semantics, so we need suitable logic for that.  That's of course another reason to be interested in DDC, which incorporates some fragment of separation logic into its types, if I understand properly."
    - id: 1325
      author: Artyom Shalkhakov
      date: "2010-10-22 10:50:12"
      content: |
        A great post, Edward.
        
        If only could we persuade more programmers of the utility of all this...
    - id: 3371
      author: Mark
      date: "2012-01-26 05:37:48"
      content: |
        i have seen one example of semi-formal proof in testing experience magazine
        
        issue 15, first paper:
        
        http://www.testingexperience.com/testingexperience15_09_11.pdf
---

In which the author muses that “semi-formal methods” (that is, non computer-assisted proof writing) should take a more active role in allowing software engineers to communicate with one another.

------------------------------------------------------------------------

[C++0x](http://en.wikipedia.org/wiki/C%2B%2B0x) has a lot of new, whiz-bang features in it, one of which is the atomic operations library. This library has advanced features that enable compiler writers and concurrency library authors to take advantage of a relaxed memory model, resulting in blazingly fast concurrent code.

It’s also ridiculously bitchy to get right.

The [Mathematizing C++ Concurrency](http://www.cl.cam.ac.uk/~pes20/cpp/) project at Cambridge is an example of what happens when you throw formal methods at an exceedingly tricky specification: you find bugs. Lots of them, ranging from slight clarifications to substantive changes. As of [a talk Mark Batty gave on Monday](http://talks.cam.ac.uk/talk/index/26712) there are still open problems: for example, the sequential memory model isn’t *actually* sequential in all cases. You can consult the [Pre-Rapperswil paper §4](http://www.cl.cam.ac.uk/~pes20/cpp/test.pdf) for more details.

Which brings me to a piercing question:

> When software engineers want to convince one another that their software is correct, what do they do?

This particular question is not about proving software “correct”—skeptics rightly point out that in many cases the concept of “correctness” is ill-defined. Instead, I am asking about communication, along the lines of “I have just written an exceptionally tricky piece of code, and I would now like to convince my coworker that I did it properly.” How do we do this?

*We don’t.*

Certainly there are times when the expense of explaining some particular piece of code is not useful. Maybe the vast majority of code we write is like this. And certainly we have mechanisms for “code review.” But the mostly widely practiced form of code review revolves around the patch and frequently is only productive when the original programmer is still around and still remembers how the code works. Having a reviewer read an *entire* program has been determined to be a frustrating and inordinately difficult thing to do—so instead, we focus on style and local structure and hope no one writes immaculate evil code. Security researchers may review code and look for patterns of use that developers tend to “get wrong” and zero in on them. We do have holistic standards, but they tend towards “it seems to work,” or, if we’re lucky, "it doesn’t break any automated regression tests.”

What we have is a critical communication failure.

------------------------------------------------------------------------

One place to draw inspiration from is that of proof in mathematics. The proof has proven to be an useful tool at communicating mathematical ideas from one person to another, with a certain of rigor to avoid ambiguity and confusion, but not computer-level formality: unlike computer science, mathematicians have only recently begun to formalize proofs for computer consumption. Writing and reading proofs is tough business, but it is the key tool by which knowledge is passed down.

Is a program a proof? In short, yes. But it is a proof of the *wrong thing*: that is, it precisely specifies what the program will do, but subsequently fails to say anything beyond that (like correctness or performance or any number of other intangible qualities.) And furthermore, it is targeted at the computer, not another person. It is one of the reasons why “the specification of the language is the compiler itself” is such a highly unsatisfying answer.

Even worse, at some point in time you may have had in your head a mental model of how some dark magic worked, having meticulously worked it out and convinced yourself that it worked. And then you wrote `// Black magic: don't touch unless you understand all of this!` And then you moved on and the knowledge was lost forever, to be rediscovered by some intrepid soul who arduously reread your code and reconstructed your proof. Give them a bone! And if you haven’t *even* convinced yourself that the code for your critical section will do the right thing, *shame on you!* (If your code is simple, it should have been a simple proof. If your code is complicated, you probably got it wrong.)

------------------------------------------------------------------------

You might argue that this is just the age-old adage “we need more documentation!” But there is a difference: proofs play a fundamentally different role than just documentation. Like programs, they must also be maintained, but their maintenance is not another chore to be done, inessential to the working of your program—rather, it should be considered a critical design exercise for assuring you and your colleagues of that your new feature is theoretically sound. It is stated that good comments say “Why” not “What.” I want to demand rigor now.

Rigor does not mean that a proof needs to be in “Greek letters” (that is, written in formal notation)—after all, such language is frequently off putting to those who have not seen it before. But it’s often a good idea, because formal language can capture ideas much more precisely and succinctly than English can.

Because programs frequently evolve in their scope and requirements (unlike mathematical proofs), we need unusually good abstractions to make sure we can adjust our proofs. Our proofs about higher level protocols should be able to ignore the low level details of any operation. Instead, they should rely on whatever higher level representation each operation has (whether its pre and post-conditions, denotational semantics, predicative semantics, etc). We shouldn’t assume our abstractions work either (nor should we throw up our hands and say “all abstractions are leaky”): we should *prove* that they have the properties we think they should have (and also say what properties they don’t have too). Of course, they might end up being the *wrong* properties, as is often the case in evolutionary software, but often, proof can smoke these misconceptions out.
