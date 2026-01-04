---
title: "Being an expert considered harmful"
date: 2010-03-08 09:00:20
slug: expert-considered-harmful
categories: [Haskell, Teaching]
comments:
    - id: 143
      author: Edward Kmett
      date: "2010-03-08 10:28:58"
      content: |
        http://paste.lisp.org/display/86914 =)
        
        But seriously, you never truly can say you understand something until you are proficient enough with the domain to be able to teach it to walk the comparatively uninitiated through it in language they can understand. 
        
        That said, there are some things -- especially in mathematics -- that do just require a ton of base knowledge before they can be adequately explained and not misleadingly simplified.
        
        Trying to teach something is a great way to put your knowledge -- or lack thereof -- into perspective.
    - id: 144
      author: Edward Z. Yang
      date: "2010-03-08 10:47:14"
      content: "Yes, I absolutely agree! When I was reading this draft last night before going to bed, I realized the post might make it seem like I'm claiming to be an expert in... uh, something. I probably have a good working knowledge, but not much further than that. But even at this point of learning, it's still easy to assume too much base knowledge! I had an eye at mathematics when I was writing this post, but I also see the expert trap when teaching people about more mundane things like Unix system administration. :-)"
    - id: 160
      author: "Chung-chieh Shan"
      date: "2010-03-11 17:46:00"
      content: "In a bout of self-application, I wonder if it is futile to tell novices at teaching how to teach -- instead, just force them to teach how to teach?"
    - id: 161
      author: Edward Z. Yang
      date: "2010-03-11 17:50:15"
      content: "Unclear, although I always say meta-evaluation tends to reduce substance, so at some point there's nothing left!"
---

*It's a sunny day in your advanced symbolic programming class. Your teacher has just started going over monads—in Scheme, though—and you sit in the back of the classroom snarking about little tidbits of knowledge you know from Haskell. Suddenly, the teacher says (quite earnestly too), "Edward here seems to know a lot about monads. Why don't we have him come up and teach them to the class?" Suddenly, you're up expounding types to people who have never used Haskell before and failing utterly to explain to people how the continuation monad works. Only after several iterations do you manage to partially rewrite the presentation in a form that doesn't assume fluency in Haskell. You've fallen into the expert trap.*

You're an expert. You are in possession of in-depth knowledge, have accumulated wisdom and intuition, and all-in-all can work much more effectively than others within your domain. You might have an ego; you might get into hot arguments with other experts. Or you might be very unassuming and thoughtful; your expertise has little to do with your ego.

But unless you've been paying attention to the pre-requisite knowledge you assume, you will be terrible at teaching your area of expertise. Your expertise is getting in the way of teaching effectively, for *the expert assumes too much prerequisite knowledge.*

<div class="container center">

![image](/wp-content/uploads/2010/03/einstein-300x225.jpg)

</div>

What do I mean when I speak of prerequisite knowledge? I don't mean prerequisite "facts"—what is an iterative algorithm to solve linear equations, how does one reverse a list using a fold, how do X in my favorite framework. I do mean foundational knowledge: abstractions and higher-order primitives to think with—linear algebra, reducing higher-order operators and the architecture of said framework. One answers "how." Another answers "Why."

All of engineering and mathematics is perpetually in search of the right abstraction to tackle a problem. Perhaps the most striking change that occurs when you've put the problem in the right representation is that it becomes substantially shorter and easier to manipulate at a higher level. It's no surprise that Newton needed to [invent Calculus](http://en.wikipedia.org/wiki/Philosophi%C3%A6_Naturalis_Principia_Mathematica) in order to develop his ideas about physics. The high-level programming languages and systems we build today would have been inconceivable in [pure assembly language or silicon](http://6004.csail.mit.edu/).

Finding and understanding the right abstraction is *enlightenment*: it makes hard things easy and impossible things possible. Calculations that used to take a page now are succinctly described in a sentence. The structure of the verbose system is encoded into the abstraction, leaving behind the salient pieces of the problem. Much of the same could be said for programs: before the advent of high level languages, assembly programs could fit on a few pages and be understood by a single programmer. They *had to be.* Modern software has gone far beyond.

In both cases, an expert will look at this new formulation, and immediately understand. The beginner, perhaps familiar with but not proficient in this encoding, has to now work out the underlying foundation again (or risk stumbling around with a simpler but faulty set of premises).

You might say, "Well, that's not the problem of the expert; they just don't have the prerequisites! I will teach them this topic once they learn that foundation." *This is not acceptable.* It is true that formal education can grant them a familiarity with the basic primitives and relations of the abstraction; it is especially effective at weeding out false conceptions. But the facility that an expert has for an abstraction only comes when you spend some time "in the trenches", using and applying the abstraction to bigger problems.

You might say, "I am not that uncharitable; I'll teach the prerequisites too." You might even expect to be able to impart knowledge upon the listener! Undelude yourself. In all but the simple topics (the ones where the simple statement of the solution is enough to illuminate), they won't understand if you simply lecture to them. The teaching is just a roadmap for doing, the only way to truly get a visceral feel for any hard problem.

What you should say is, "I am just one lantern in the novice's arsenal of understanding. I seek to illuminate precisely what the novice doesn't think to look at." In fact, there is an easy way to fulfill this purpose: force the novice to teach! They will start off with a very limited and ill-defined mental model of the concept: of the many roads to understanding, there is only *one* that they know. They will explain it in brutal detail of all their missteps and your implicit knowledge. They will be asked questions, and those questions will force them to clarify their understanding of this path. Eventually they will feel confident in their knowledge of the path, and if they continue learning, that path will expand to encompass many paths, different routes to understanding. The novice has become an expert. But, as the Buddha might say, *they* are the ones who must discover enlightenment. The teacher merely shows them the path.
