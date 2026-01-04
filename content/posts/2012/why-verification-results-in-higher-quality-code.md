---
title: "Why verification results in higher quality code"
date: 2012-06-23 00:40:11
slug: why-verification-results-in-higher-quality-code
categories: [Computer Science]
comments:
    - id: 3832
      author: gasche
      date: "2012-06-23 05:34:30"
      content: |
        As a complementary (certainly not contradictory) comment, I would like to remark that unit testing provides similar organization benefits. To be able to reasonably test a program, you must split its functionality in several components that can be tested separately. Take a compiler for example: testing by feeding input programs won't allow you to test fine-grained-enough aspects of lower phases transformations. To test them correctly you have to allow direct input of the intermediate language where each lower phase happen, and therefore you have design pressure to think harder about the specification of those intermediate languages and phases.
        
        I think the effects are related because the actions are related : both testing and mechanized verification are, at their core, ways to *specify* a program. The main agent of quality improvement is the continuous feedback loop between implementation and specification, with testing and verifications being automated ways (of varying strenght) to gain confidence that they actually agree on the program.
        
        Probably an even lighter way to put similar design pressure is literate programming. By forcing yourself to write a program in a way that can be explained clearly but precisely (without glossing over details) to a human, you will also force yourself to design clean interfaces (otherwise the explanation is awkward), formulate invariants, etc.
        
        I am not convinced that, of these "specification" techniques, mechanized proof always makes economic sense (a claim you, wisely, have not made). It is certainly the stronger in the guarantee it brings, but it is also the costlier by a rather wide margin. In fact I'm quite sure that, with current tools, it doesn't make sense in general except for extremely widely used code (a standard library) or highly safety-critical domains. The bar may lower over time, as we found new way to present and automate such verifications.
        But different testing techniques (whitebox fuzzing looks very cool) will also improve in the confidence they bring. Hopefully we will end up with a continuum of tools where we can seamlessly move from "test-like" to "proof-like" specification techniques, with a quite fun intermediate world in the middle.
    - id: 3833
      author: yachris
      date: "2012-06-23 09:06:49"
      content: |
        First, interesting post.  When I was in school a million years ago, formal proof of program correctness was taught (in some class I never took) but recognized as not making economic sense.  Glad to hear it's still being worked on, and may become useful.
        
        Second, gasche is completely right -- unit testing has had an amazing effect on my programs.  Again, in school we studied "coupling and cohesion" (and I still can't tell you what cohesion is :-) but when a class has to be tested alone, by definition it's uncoupled.  And, gee, that crazy academical idea... it really means something!  My uncoupled classes work better, and are more amenable to refactoring to improve how they work, and more reusable, and...
        
        The other thing that's amazed me is informal code review.  We did formal code reviews on the project I'm on for a few months years ago.  You know the drill: print out the code for everyone who's going to be in the review, get everyone to read through it before the review, have someone assigned (who is NOT the author of the code!) to explain it in the review, collect everyone's comments, have the author rewrite to fix what was found.
        
        This worked, but was pretty quickly abandoned as just taking way too much time.  So a while back, we tried just having one person informally review the code after it's been checked in (and tested!) by the author.
        
        Again, wow, huge improvement in quality.  Yes, bugs slip through, but this is kind of a pareto principle thing -- the informal reviews catch 90% of the bugs with 10% of the effort.  Combine that with a strong unit testing culture, and we chase far fewer bugs than we used to, and the code is generally of a higher quality, since we get more eyeballs looking at it and suggesting better ways.
        
        I do have to say that I looked at literate programming briefly, and found it (at least the code I looked at) hopeless... I find I have to have an entire block of code to go up and down, to understand.  Having a function chopped up into little bits and scattered all over the place made it incomprehensible.
    - id: 3834
      author: Edward Z. Yang
      date: "2012-06-23 12:00:33"
      content: |
        While I agree unit testing gives similar specification benefits, I am making an even stronger statement about formal verification: not only do you have to specify what your code does, you have to make your code *really good*, lest you fail to actually show the specification goes. In unit testing, all you need to do is make the code executable. It truly is a black box/white box distinction. I think that literate programming is much more alike, but since there is no computer keeping you honest, it is substantially weaker. Code review is better, since people are reading the code, but you have to be careful: if someone doesn't understand something, they need to distinguish between the code being unclear, and them just being unfamiliar with the domain. This is certainly why CR is good at fixing local defects in code, but not global ones.
        
        I agree that the economic incentives are not here yet; but I think that developers are deluded if they think they can have consistently high quality code, even when they have been asked (and given the time) to do so.
    - id: 3836
      author: Tom Moertel
      date: "2012-06-25 17:00:26"
      content: |
        In other words, verification is an incentive scheme that motivates humans to put systems into their simplest forms. It's a motivation hack.
        
        Slightly longer: It's expensive to explain what a system is supposed to do, and even more expensive to explain it to an unthinking machine. So, forcing a human to explain to an unthinking machine what a system is supposed to do gives the human a strong incentive to lower the cost of the explanation, something done most easily by simplifying the structure of the system being explained.
        
        That the system can be shown to satisfy the explanation is a bonus :-)
    - id: 3839
      author: Sam
      date: "2012-06-26 10:13:00"
      content: |
        The very effort for rigor forces us to find out simpler methods of proof. 
        
        — David Hilbert
        ‘Mathematical Problems’, Bulletin of the American Mathematical Society (Jul 1902), 8, 441.
    - id: 3842
      author: Cody
      date: "2012-06-28 05:48:25"
      content: |
        This "formal methods are still to expensive to be of any practical use" myth has got to die. Verification technology has made bounds within the last decade, and while I wouldn't advise the use of formal methods in low-security settings, claiming that it is only useful in some "highly safety-critical domains" seems a bit disingenuous. Additionally, there is now a wider range of properties that one may wish to prove about their code, ranging from type-correctness and simple safety properties to full correctness.
        
        That the industry practice has changed little over the decade, however, is a case you can easily make.
    - id: 3863
      author: Neel Krishnaswami
      date: "2012-07-13 04:44:09"
      content: |
        <blockquote>When different components do interact, the principle of encapsulation says that I do not want to know all of the gory details of a component, just its high-level interface. In theorem prover land, “all of the gory details” means unmanageably large facts about many pointers, and a "high-level interface" is an abstract predicate which rolls up all of these facts into a single, cohesive logical fact (“this is a linked list.”) Developing these predicates is critical to keeping your theorem statements concise and understandable, and in higher-order provers like Bedrock, they can apply not only to data but also to code, i.e. function pointers.</blockquote>
        
        For modular reasoning about imperative programs, higher-order predicates in separation logic is necessary, but not sufficient. The basic issue is that in separation logic, separation means disjointness with respect to a <em>fixed</em> notion of resource -- namely, the machine heap. But that's a low level of abstraction, and the seams show quite badly when you try to build programs in a layered style. 
        
        This is because we often introduce highly-aliased data structures which nevertheless are disjoint over some conceptual or logical resource. For example, if you build a database library, you might use connection pooling to reuse database connections and reduce setup/teardown overhead. This results in (a) wild aliasing of connection objects, but (b) is invisible to clients of the library, who can act as if every connection they request is unique and separate. But expressing (b) in terms of abstract predicates (in the standard separation logic model) is quite painful. 
        
        As a result, I don't think it's quite right to say that verification results in uniformly-better code, since it may well be the case that you have a good design that is  beyond our ability to give clean specifications for. For this particular example, there's been a lot of recent progress: see our new ICFP paper <em><a href="http://www.mpi-sws.org/~neelk/icfp12-superficial-krishnaswami-turon-dreyer-garg.pdf" rel="nofollow">Superficially Substructural Types</a></em>, including the references. But the progress is quite recent.
---

Correctness is overrated. After all, no one knows what it means for any reasonably complicated system to be "correct", and even when we do, the mileposts move around on a daily basis. With the *raison d'être* of formal verification stripped away, can we still consider it a worthy goal?

Perhaps verification results in higher quality code. But this is not obviously true: correctness is not quality. We might hope that high quality code is readable and easily understood, that it should be as self-contained and independent from the rest of the system, that it is efficient and economical. There is no a priori reason to believe that verification would grant us any of these properties. No matter how horrible some code is, as long as it is correct, there exists a proof which vouches for its correctness.

But as anyone who has gone through the sweat and tears of verifying a program can tell you, formal verification really does make your code better. Here's the secret: proving theorems is really hard. If we want any hope to successfully prove something about a program, we *must* to make *reasoning* about the code as easy as possible. A program under verification irresistibly gravitates towards it’s most “reasonable” form, because otherwise the proofs are simply too arduous to carry out. And in this form, the tenets of high quality code follow.

Take for example [Bedrock](http://adam.chlipala.net/bedrock/), a system for building verified low-level programs which manipulate pointers and registers. These are programs that deal with mutable state, a feature which is well known to dramatically increase the difficulty of reasoning. Bedrock, and many systems like it, would be dead out of the water if not for the development of an important system called [separation logic](http://en.wikipedia.org/wiki/Separation_logic). The central idea behind it is so obvious to any experienced practitioner it is barely worth stating: private local state is easier to reason about than public global state—\*\*modularity\*\* is good. It enforces this through a clever formalism, the star operator, which combines two assertions about two regions of memory while assuring that the regions are *disjoint*. Regardless, the end result is this: if your components are independent, the theorem proving is easy; if your components are tangled together, the theorem proving is hard. You do the math.

But it doesn’t stop there. When different components do interact, the principle of **encapsulation** says that I do not want to know all of the gory details of a component, just its high-level interface. In theorem prover land, “all of the gory details” means unmanageably large facts about many pointers, and a "high-level interface" is an *abstract predicate* which rolls up all of these facts into a single, cohesive logical fact (“this is a linked list.”) Developing these predicates is critical to keeping your theorem statements concise and understandable, and in higher-order provers like Bedrock, they can apply not only to data but also to code, i.e. function pointers.

The tenets of high quality code speak for code that is written for humans to understand, and not just machines to execute. But code that is written for machines to understand have many of the same properties that are valued by humans, for if they do not, getting the machine to “understand” becomes an impossible task. Computers may be simple-minded, but all that means is code a computer can understand is code that you can understand too. And *that* is high quality code.
