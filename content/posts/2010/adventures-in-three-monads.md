---
title: "Adventures in Three Monads"
date: 2010-01-01 13:16:05
slug: adventures-in-three-monads
categories: [Haskell]
comments:
    - id: 5
      author: T_S_
      date: "2010-01-05 10:39:24"
      content: |
        In the first example perhaps define:
        
        <code>fromList = (msum . map return)</code>
        
        to make Listing 1 more readable
    - id: 6
      author: Ricardo Herrmann
      date: "2010-01-05 11:30:42"
      content: |
        Hi, just to let you know your evensList example will actually return odds ;-)
        
        I have just skimmed it, but will read it as soon as I can. The logict paper is very good but maybe it scares some people away (as there's only 1 package in hackage that depends on it (http://bifunctor.homelinux.net/~roel/cgi-bin/hackage-scripts/revdeps/logict-0.3#direct)), and maybe now more programmers will start using it after reading your article.
    - id: 7
      author: Anonymous
      date: "2010-01-05 12:14:18"
      content: |
        About a third of the way down on page 2:  "If the type signature is specied to be [a], the original list operations fall out again." Could you reword this?  What type signature?  What does "fall out again" mean?
        
        Bottom of page 10: "Note that they still are data constructors, so we can still use pattern matching, we can't return any old value (it has to be of type GADTExample a), ...",  but about a third of the way down on page 11:  "As mentioned before, the return type need not be GADTExample a."  Looks like a contradiction.
        
        Listing 14:  Where does "prompt" come from?
    - id: 8
      author: Edward Z. Yang
      date: "2010-01-05 12:57:06"
      content: |
        Thanks for all of your comments! I've updated the article accordingly. Ricardo, very good catch. :-)
        
        Re Anonymous "If the type signature is specied to be [a], the original list operations fall out again." The list monad is an instance of MonadPlus. So any operator on MonadPlus, say mplus or mzero, has a generic type, but if you (by inference or explicitly) narrow the type to the list, you'll find that it is equivalent to whatever type-specific function it corresponds to. I've tried to make this a little clearer in the article.
    - id: 9
      author: Ricardo Herrmann
      date: "2010-01-05 15:09:10"
      content: "Hi, I thought you had already submitted the final version to TMR ... in case not, how about using guards in Listing 1 to make it more idiomatic, since it mentions MonadPlus right after the example ? Just my 2 cents ..."
    - id: 10
      author: Edward Z. Yang
      date: "2010-01-05 15:18:06"
      content: "I was thinking about that, and I decided to explicitly write out the if...then...else, because it showed use of mzero/return, rather than a sort of magical (to those who don't quite understand its operation) guard operator. I guess it would be a good thing to mention, though."
    - id: 12
      author: roy_hu
      date: "2010-01-10 12:42:47"
      content: |
        On page 3, "A Turing machine a consists of a tape, ..."
                                                       ^
    - id: 13
      author: roy_hu
      date: "2010-01-10 16:18:47"
      content: |
        The state of the nondeterministic Turing Machine is defined as 
        
        data State = StateA | StateB
        
        I was like, huh?
    - id: 14
      author: roy_hu
      date: "2010-01-10 16:20:14"
      content: "You defined choices in Listing 7. It's a duplication of fromList from Listing 1."
    - id: 16
      author: roy_hu
      date: "2010-01-10 22:20:44"
      content: |
        On page 8, the sequence should be 0,1,0,2,0,1,0,3,0,1,0,2,(0,1,)0,4,0,...
        You missed the numbers between the parentheses.
    - id: 17
      author: roy_hu
      date: "2010-01-10 22:37:53"
      content: "The meaning of \"faux monads\" isn't clear to me."
    - id: 18
      author: Edward Z. Yang
      date: "2010-01-10 22:43:38"
      content: |
        Hey Roy,
        
        Thanks for the great comments! You should redownload the PDF, because I've made some updates (for example, I removed "faux monad" because both you and Brent Yorgey mentioned that it was confusing.)
    - id: 19
      author: roy_hu
      date: "2010-01-10 23:34:20"
      content: "On page 15, There *are* also Applicative and Functor versions, ..."
    - id: 22
      author: roy_hu
      date: "2010-01-11 00:14:07"
      content: "FYI, the page numbers in my comments correspond to the latest PDF. But I didn't check \"faux monad\" in it."
    - id: 23
      author: Edward Z. Yang
      date: "2010-01-11 00:20:15"
      content: "Excellent. I've updated the PDF accordingly with your comments."
    - id: 24
      author: roy_hu
      date: "2010-01-11 00:35:06"
      content: |
        Could you talk more about the Turing Machine's state? I didn't see any reference to the "StateA | StateB" thing.
        
        Also, I'm interested in "the ordering being sensitive to monad transformations", although you deleted it. Could you talk more about it on your blog?
    - id: 25
      author: roy_hu
      date: "2010-01-11 00:42:46"
      content: "I'm honored to be placed in the acknowledgements. Could you put in my real name, Wei Hu?"
---

I've been busy at work over this winter break working on an article for [The Monad Reader](http://themonadreader.wordpress.com/), entitled "Adventures in Three Monads." The material will overlap the second part of my IAP talk [Haskell Typeclasses](http://sipb.mit.edu/iap/#haskelltype) that I will be delivering under the auspices of SIPB IAP.

The article itself is a literate Haskell file, and contains sample code I've cribbed from the various Haskell applications I've written over my year long flirtations with the language: included is code and explanation for the probabilistic Turing machine I built in order to brute-force a [6.004](http://6004.csail.mit.edu/) assignment. (To the course staff: the code is incomplete enough that it's not equivalent to publishing all of the solutions; intrepid readers will still have to write a search function themselves.)

I'll be keeping a pre-print of the article available in my [public directory](http://web.mit.edu/~ezyang/Public/threemonads.pdf). Questions, comments and suggestions would be greatly appreciated!
