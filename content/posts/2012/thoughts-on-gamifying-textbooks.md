---
title: "Thoughts on gamifying textbooks"
date: 2012-05-24 19:10:57
slug: thoughts-on-gamifying-textbooks
categories: [Teaching]
comments:
    - id: 3770
      author: Rick Fleischer
      date: "2012-05-25 05:31:08"
      content: "You have figured out a lot from one sample.  I would amend the 6 points of your conclusion only slightly: theorem provers will be part of a *family* of exercise engines; and there should be a 7th point, that multiple mental models/explanations need to be encouraged/supported.  Thank-you for noticing that terminology and notation can be a barrier to understanding.  I might go one step further than the context-sensitive help that you suggest and allow the student to switch notations/terminology back and forth throughout a document.  By the way, your last exercise (with the \"there exists\" implying the provability of \"for every\") doesn't look true."
    - id: 3771
      author: Thomas
      date: "2012-05-25 05:37:14"
      content: "It's certainly a noble experiment, but I for one think that students at university level are probably better off using the tools directly. For example, I like the course by Benjamin Pierce that uses Coq to introduce first constructive logic and then some important material from TAPL. In a similar vain, one could create a course on logic that introduces resolution first and then uses Prolog for all the other stuff including sequent calculus and natural deduction (although I'd start with natural deduction and then, optionally, get into sequent calculus). If I were a student both options, using Coq (or Isabelle/HOL for that matter) or Prolog would both appeal to me whereas being treated to a \"gamified textbook\" would certainly put me off because it hides some of the details and does not feel like I am learning about using the computer as a serious tool but that the programmer of this gamified textbook had all the fun. But I guess that my opinion is a minority one and I do not have very strong feelings either one provided that the majority of students learn the material."
    - id: 3773
      author: Edward Z. Yang
      date: "2012-05-25 12:21:12"
      content: |
        Rick: I agree with all of your comments. They are things I have thought about. (Re the last exercise, it is true classically! The statement is called the "drinker's paradox".)
        
        Thomas: If your end goal is teaching PL theory, and things which are very close to the original intent of Coq and other theorem provers, that is almost nearly acceptable. But in my experience, any such course will end up being about the *theorem prover*, and not the subject content. The other trouble with Coq is that its reasoning style does not look anything like the sequent calculus, so if you're trying to learn that notion it doesn't work well for that all.
    - id: 3774
      author: John Regehr
      date: "2012-05-25 15:20:43"
      content: |
        The other day I saw a talk by Pete Manolios from Northeastern. They're using ACL2 in low-level courses, first as a counterexample engine and second to really prove things about programs. It sounds pretty great.
        
        I find this whole gamification thing to be somewhat bullshit, at least if taken very far, but perhaps it's useful if it helps people think about ways to teach more effectively.
    - id: 3781
      author: Joseph Tassarotti
      date: "2012-05-26 20:18:28"
      content: |
        "But in my experience, any such course will end up being about the *theorem prover*, and not the subject content."
        
        This is true to some degree. But I think Pierce's book and associated course do get pretty far into the subject content. Of course, one could probably get further by teaching it the traditional way, and I think Pierce has written that this was his experience.
        
        However, isn't this because students come into such courses with some experience doing paper and pencil proofs but no experience using Coq? If all (or at least more) courses used theorem provers, then this wouldn't happen. The first class would have this large overhead, but in subsequent classes students would be prepared.
        
        Your point about mismatches between reasoning styles is still an issue though.
    - id: 3835
      author: contextfree
      date: "2012-06-23 13:49:29"
      content: "Re theorem provers and gamification, reading the \"Coq'Art\" book and doing the exercises has been one of the most enjoyably game-like textbook experiences I've had."
    - id: 3859
      author: Issam
      date: "2012-07-06 12:52:58"
      content: "Should programmers be rqeuired to implement the compiler/interpreter/runtime or whatever else applies in their particular situation, the tools with which they write and edit code or the operating systems whose calls are hidden away behind layers upon layers of libraries? And what about the computer hardware and microcode, the firmware of the devices. Or do you really have to understand the physical effects behind semiconductors, hard disks, and so on to even dare push the power button on a modern computer?Like in math you don't have to prove everything yourself. And most of the time the proofs that are done in math textbooks or in math classes are done quite sloppily. Most of the time formal proof is really not formal at all. It's a matter of convenience, a matter of concentrating on the things that really help you build your particular tower of abstractions. Why should software engineers be held to higher standards than most mathematicians?"
    - id: 27022
      author: Daniel Donnelly aka EnjoysMath
      date: "2021-11-26 21:07:45"
      content: |
        I really enjoyed this blog post and you hit many (most) points that I didn't even consider.
        
        Right now I have a Markdown Editor example up &amp; working using Qt / C++ / Marked.js / Markdown.css.  It works extremely quickly to update the previou - in fact instantaneously.  It's not my code - in fact it's an example on the Qt doc website.
        
        I'm usually a Python coder, but every time I get down to the nitty gritty of logic programming, I find I need something speedier.
        
        I don't know if I want to tie into Lean 4 or Coq right now.  If anything the textbook-maker software should be language-agnostic (when it comes to the code that checks problems), or in other words the user can select different options in the long-term, but for simplicity should start out supporting just one backend.
        
        It's nice to use markdown for this, because the textbook does not have to look like a rendered LaTeX document, of which there are a million different flavors anyhow.  Since the end-user views the book electronically - there is no need to type-set for print I don't think.  Also LaTeX document editing is extremely difficult - harder than C++ even.  So markdown it is (for my implementation at least).
        
        I also got an "Advanced Docking System" for Qt library to work (an example): think Visual Studio docking capabilities including tabbing.  This is required I think, because you don't want to make just one or two perspectives available.  The users would expect, that if they're book got digitalized, then they should be able to put pages together any way they want, even like notecards spread out on a table.
        
        I don't completely agree that the entire experience should seem like a usual printed textbook.   Think of visualizations in category theory and in this theory in particular things get rather visual.  Would the user rather memorize a strange list of equalities or a 3x3 grid making up a commutative diagram?   So diagram-editing is a must, especially because you're going to tie-into a backend (so you need to be able to decode the graph format, which is much harder if it's an image).  So anyway, you have 2D diagram editing say (perhaps via Quiver - everyone should know about quiver by now, if not, google Quiver CD editor).  Quiver exports a format that I have worked with before.  Anyway, the user is going to eventually expect a 3D-looking diagram to be manipulateable in 3D space using graphics code.  So now your textbook goes from textbook-style to 3D scene viewer.    And you can imagine maybe the user would maybe sometimes edit stuff using other widgets as part of a problem, so I would keep the UX open-ended.  However, one thing I can say about the book experience is that it does hit all those nice points you mentioned about it.  So that should be the default or home view mode.
        
        Also, when an advanced user starts back up a text for the second or more time, they usually skip past a bunch of stuff they already know and authors usually cover a bunch of stuff they don't really need to (but for printed books is a convenience).  Since everything is now accessible at the click of a button, a link to a well-known definition would suffice.   Anyway, the textbook should be modifiable so that it optimally satisfies the needs of each individual user.  So a user can permanently delete a page if they want, as long as there are links back to the original copy somewhere.
---

Earlier this year, Woodie Flowers wrote this [criticism of MITx](http://web.mit.edu/fnl/volume/243/flowers.html):

> We seem to have decided to offer “courses” rather than participate in the exciting new process of replacing textbooks with more effective training tools.

[Logitext](http://logitext.ezyang.scripts.mit.edu/logitext.fcgi/tutorial), true to its name, was intended to explore what a chapter from a next-generation textbook on formal logic might look like. But if you asked anyone what subjects the most important textbooks of this century would be about, I doubt logic would be particularly high on anyone’s list. In terms of relevance, Logitext misses the mark. But I do think there are some design principles that Logitext helps elucidate.

# On interactivity

It is a good thing that the quality of the Metropolitan Opera's productions is not correlated with the quality of [their website pages](http://ringcycle.metoperafamily.org/characters). Here, we have an egregious example of interactivity for the sake of interactivity, with no real benefit to the presentation of information.

There are many things that interactivity can bring the table. However, **interactive textbooks should still look like textbooks.** The conventional textbook is a masterwork of design for the static medium: it is skimmable, supports random access and easily searched. You *cannot* beat this format at its own game, no matter how well designed your [video game levels may be](http://tryruby.org/).

In order to apply interactivity tastefully, you must consider what the *limitations* of the static medium were: it is precisely here where interactivity can bring something to the table. Here are some examples:

- Every field of study has its jargon, which assists people versed with the language but impedes those who are not. In a static medium, you can only define jargon a limited number of times: it clutters up a text to redefine it a term every time it shows up in the text, even if you know your students frequently forget what a term means. In an a dynamic medium, the fix is trivial: tooltips. Logitext did not start off with tooltips, but I quickly learned readers were getting confused about the meanings of “conjunction”, “disjunction” and “turnstile”. Tooltips let us easily **extend the possible audience of a single work**.
- The written medium demands a linear narrative, only sparingly stopping for questions or elaborations. Too many waypoints, and you risk losing the reader. In an interactive text, **the system can give context-sensitive information only when it is relevant.** In Logitext, when a reader clicks on a clause which requires an instantiation of a variable, the system explains how this inference rule works. This explanation is given elsewhere in a more general explanation of how quantifiers work, but the system also knows how to offer this information a timely and useful manner.
- Sometimes, the information you are trying to convey should also be given in another form. It's the difference between describing a piece of music or actually hearing it, the difference between giving someone a map or letting them wander around for a few hours. Whenever possible, **show, don't tell.** And if possible, show in different ways—different intuitions work for different people. I can explain what the “no free occurrence” rule is until the chickens come home, but the unexpected renaming of variables when you click “forall right” immediately introduces the intuitive idea (even though it still needs to be explained for full understanding.)

It is telling that each of these enhancements have been abused by countless systems in the past. Many people have a dim view of tooltips and Clippy, myself included. I think one way to limit the damage of any such abuse is to demand that the textbook **gracefully degrade** without interactivity. (For technological reasons, Logitext doesn’t render correctly without JavaScript, but I consider this a bug.)

# On exercise design

In order to truly learn something, you must solve some problems with it. Gamification of exercises has done well at supplying extrinsic motivation, but up until recently, the state of the art in online exercise design has been something [like this](http://math.com/school/subject1/practice/S1U4L3/S1U4L3Pract.html). I find this depressing: there is no indication the student is really learned the underlying concepts, or has just constructed [an elaborate private system which happens also to be wrong](http://blog.mathed.net/2011/07/rysk-erlwangers-bennys-conception-of.html). Remember when you were asked to show your work? We should be demanding this online too.

This is easier said than done. It was no accident that I picked the *sequent calculus* for Logitext: while logic holds a very special place in my heart, I also knew that it would be easy to automate. The road to a system for something as simple as High School Algebra will be long and stony. Logitext sidesteps so many important questions, even ones as simple as "How do we get student's answers (with work) onto the computer?" let alone thorny issues such as one addressed by a [recent PhD thesis](http://www.marvin-schiller.de/): "How do we tell if the student needs to provide more work?"

I think I came away with two important ideas from Logitext. The first is a strong conviction that **theorem provers are the right foundational technology for interesting exercises in mathematics.** Building the Logitext system was a bit of work, but once the platform was designed, defining exercises was simple, literally a line of code per exercise. If every exercise had to be implemented from scratch, the cost would have been prohibitively expensive, and the tutorial would have looked a lot different. We know that, in principle, we can formalize all of mathematics; in the case of elementary mathematics, we may not even have to solve open research questions to get there. Theorem provers also know when you’ve gotten the answer *right*, and I suspect from a gamification perspective that is all you need.

The second important idea is that computers can **assist exploration of high-level concepts, even when the foundations are shaky**. For some people, copying down a string of mathematical symbols quickly and accurately is an ordeal: a system like Logitext abstracts that away and allows them to see the higher order structure of these proofs. It is true that it is better of students have a strong foundation, but if we had a system which could prevent them from getting left behind, I think the world would be strictly better for it. The solution to a curriculum which relies on [a freakish knack for manipulating abstract symbols](http://worrydream.com/KillMath/) should not be eliminating symbols, but making it easier to manipulate them. Educational systems should have what I call **adjustable affordance**: you should have the option to do the low level manipulations, or have the system do them for you.

# Conclusion

I have articulated the following design principles for the gamification of textbooks:

- An interactive textbook should still look like a textbook.
- Use interactivity to extend the possible audience of a textbook by assisting those with less starting knowledge.
- Use interactivity to offer context-sensitive information only when relevant.
- Use interactivity to show; but be sure to explain afterwards.
- (Heavily modified) theorem provers will be the fundamental technology that will lie beneath any nontrivial exercise engine.
- One of the most important contributions of computers to exercises will not be automated grading, but assisted exploration.

I’ve asserted these very confidently, but the truth is that they are all drawn from a sample size of one. While the Logitext project was a very informative exercise, I am struck by how little I know about K12 education. As an ace student, I have a rather unrepresentative set of memories, and they are probably all unreliable by now anyway. Education is hard, and while I think improved textbooks will help, I don’t really know if they will really change the game. I am hopeful, but I have the nagging suspicion that I may end up waiting a long time.
