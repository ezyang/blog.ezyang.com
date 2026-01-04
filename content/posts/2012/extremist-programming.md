---
title: "Extremist Programming"
date: 2012-11-20 16:15:06
slug: extremist-programming
categories: [Computer Science]
comments:
    - id: 4822
      author: Franklin Chen
      date: "2012-11-20 16:30:10"
      content: "I agree that extremism in order to learn is a great strategy. Whether one is programming, cooking, running, or sleeping, testing the limits is the way to find out what is possible, and only then, step back and decide what is pragmatic based on what you find out. If you never find out for yourself, you never know whether you really were being pragmatic or just being a closed-minded sheep. That's my philosophy of life: extremism as a strategy, not as dogma."
    - id: 4823
      author: Norswap
      date: "2012-11-20 16:40:25"
      content: "So I guess the takeaway is to use Scala (I'm joking, am I?)."
    - id: 4824
      author: Norswap
      date: "2012-11-20 16:41:43"
      content: "Didn't read trough the article properly, and now my comment looks silly. Sigh."
    - id: 4825
      author: Danno
      date: "2012-11-20 17:59:34"
      content: "Could you elaborate on Footnote 6 a little? I'm a little confused by what you mean: Does Coq with automation enable straight-line programs without abstractions to be easily written? Does it do the opposite?"
    - id: 4827
      author: Edward Z. Yang
      date: "2012-11-20 18:17:58"
      content: "Danno: Coq's default tactic library makes it very easy to write proof scripts which look like this: 'tactic1. tactic2. tactic3. tactic4. ...' (and on and on, for pages and pages). It's one of the defining characteristics of a proof script of someone who is new to Coq. This chapter of CPDT shows the contrast pretty well: http://adam.chlipala.net/cpdt/html/StackMachine.html"
    - id: 4828
      author: BMeph
      date: "2012-11-20 18:28:49"
      content: "RE: Footnote 4 - what, no Self? Self is quite the extremist of object prototyping, eh?"
    - id: 4829
      author: Edward Z. Yang
      date: "2012-11-20 18:29:54"
      content: "There are quite a few languages in this space, so I apologize for any oversights from my lack of knowledge :^)"
    - id: 4836
      author: Jan Stolarek
      date: "2012-11-21 04:08:15"
      content: "When I decided to learn functional programming I started with Scala, following my friend's advice. I quickly realized that if I can create objects and use mutable variables I will be simply writing Java in Scala and will accomplish nothing. No surprise that Haskell turned out to be much better for learning FP as it just forced me to think differently."
    - id: 4837
      author: Anonymous
      date: "2012-11-21 04:43:33"
      content: Abstractions in programming languages must (ideally) be uniform and orthogonal.  Extremist programming is the simplest route to this goal.
    - id: 4839
      author: Johann
      date: "2012-11-21 04:54:37"
      content: |
        Thinking about your reasoning further I would come to the conclusion: Taking an extremist language and apply it's (extreme) concepts while learning is a good idea to identify those areas where the respective concepts make sense.
        
        Thus, for a real-life project, I would rather pick
        * a multi-paradigm language (Mozart Oz??)
        * in terms of concepts a rather mediocre language which gives me bit's and pieces from all and nothing (No example here to offend no one)
    - id: 4841
      author: agumonkey
      date: "2012-11-21 05:22:22"
      content: "This is reminiscent of Bret Victor 'inventing with principle'. Going full depth with one idea is a very important thing to do. Hit all the walls."
    - id: 4844
      author: Anonymous
      date: "2012-11-21 06:16:54"
      content: |
        I like the concept of extreme programming. I find it puts me into the zone faster than any other approach.  The best way I have found so far to introduce extremism is to eat a lot of gas producing sulfurous vegetables (broccoli, cauliflower, brussels sprouts). You need to eat enough to produce stomach bloating. 
        
        When the bloating is sufficiently advanced, release the gas into your programming chair and get to work.  If the mind starts to lose concentration, release more gas.  The advantage of sufurous vegetables is that the olfactory signature is notable and it clings to your programmers chair and keyboard long after the event.
        
        After only a couple of days of this regimen, one only has to think about gas (without releasing any) and you will find your mind is into the zone as clearly as if one has been using gas.
    - id: 4853
      author: Anonymous
      date: "2012-11-21 11:26:54"
      content: |
        consistency is a very attractive feature in a language, it gives you the impression that you won't have to learn as much.
        
        sometimes it's probably even true... although there's never a guarantee that you won't waste tons of time trying to fit all tasks into the consistent paradigm. if you're learning programming for the first time, however, consistency could at least in theory reduce the amount of confusion between "code? i can't write code!" and: "i can... at least a little."
        
        consistency might not provide the best tools, but the whole idea of using the best tool for the job is sometimes overrated. sometimes the easiest (or sadly, even the flashiest or most gimmicky) tool will sort of do.
    - id: 4854
      author: gary
      date: "2012-11-21 11:55:17"
      content: "I love this.  It's also good to be rigorous and thorough, but my best learning has come from backtracking after extremism.  Sometimes it's good to do things that you don't know how to do."
    - id: 4855
      author: Daniel
      date: "2012-11-21 12:01:13"
      content: |
        I like it. I've considered this as an approach for genetic algorithms in an attempt to create an emergent AI. 
        
        Generate functions, apply functions to data mass, score, modify and repeat.
    - id: 4856
      author: Zac
      date: "2012-11-21 13:01:17"
      content: "I did an exercise like this using only functions and classes in Ruby. http://vimeo.com/53360559"
    - id: 4857
      author: Stacy
      date: "2012-11-21 13:04:49"
      content: |
        I've done this too, specifically I've tried using 'Tell Don't Ask' (http://pragprog.com/articles/tell-dont-ask) for all Java code (i.e. all methods return void). I was motivated to give it a try after reading (why getters and setters are evil: http://www.javaworld.com/javaworld/jw-09-2003/jw-0905-toolbox.html and other similar articles). 
        
        I guess the Haskell equivalent would be: only use side effects, never return a value, i.e. make all functions have type: Something -&gt; IO ().
    - id: 4858
      author: LarryA
      date: "2012-11-21 13:16:38"
      content: |
        Heh, I did a bunch of extremist programming with FoxBase+/Mac in the day, Of the Mac languages it was easy to approach and had great graphic output capability.  One memorable project was writing an ASCII art printer - using it to print out some scaled down ascii art files from the 70s, worked great.
        
        There were some features on it that made creating things way much easier than languages today.  *sigh*
    - id: 4859
      author: k0nsl
      date: "2012-11-21 13:26:20"
      content: |
        Extremism and radicalism can be very worthwhile.
        
        Good read.
        
        -k0nsl
    - id: 4865
      author: David
      date: "2012-11-21 18:43:50"
      content: |
        Regarding your statements "x is awesome. What if we made a y for which always-x?", at first I just thought these were suggestions. But I think the allusions you're making are:
        
        Files - Unix
        
        Cons cells - Lisp
        
        Mathematics - Coq? 
        
        Arrays - APL/J?
    - id: 4866
      author: julian
      date: "2012-11-21 19:07:55"
      content: "smalltalke *USED* this to good effect? Really? past tense?"
    - id: 4869
      author: Anonymous
      date: "2012-11-21 21:29:55"
      content: "Turing machines are awesome. What if we made a PL that was turing complete?"
    - id: 4870
      author: rdm
      date: "2012-11-21 21:37:36"
      content: "Not only are \"extreme\" languages good for learning new concepts, but also applying extreme tactics in languages which were not designed for them can also be extremely instructive."
    - id: 4873
      author: jcd
      date: "2012-11-22 01:58:47"
      content: "Solutions looking for problems, basically?"
    - id: 4875
      author: Anonymous
      date: "2012-11-22 02:57:20"
      content: |
        see also:
        
        https://plus.google.com/101021359296728801638/posts/HB7tkcHgBxC
    - id: 4876
      author: "Lewis Cowles (@LewisCowles1)"
      date: "2012-11-22 03:59:56"
      content: "Not sure I wholeheartedly agree with this, \"we must do things extremely to understand\" idea! It seems like a very resource-intensive process to me, in order to confirm what others have surely found or deduced. All programming languages offer their own solutions, but it's not like I can write method B of class A in Java and method C in the same class in Pascal, at least not efficiently..."
    - id: 4879
      author: look
      date: "2012-11-22 05:28:18"
      content: |
        Can't agree more.
        Stupid people always make things  unnecessarily complex.
    - id: 4880
      author: Nei
      date: "2012-11-22 07:33:03"
      content: |
        I remember once, maybe 10 years ago, when I was working on a fairly generic large database application, one that had evolved far past the ability to keep the database schema documented properly.
        
        For a long time I experimented with turning our 100+ tables into just 2 tables - one called 'things' and one called 'properties'. After all every record in every table is just a thing with some properties.
        
        To me as a developer, I saw that I could replace 95% of our custom-written code to manipulate each table, with generic code that could do the same to *any* entity in my two tables.
        
        I even did enough proof-of-concept to show that performance was the same or even better in certain cases.
        
        No matter, the powers-that-be refused to entertain it even as a joke.
        
        Recently I read that this is exactly how Twitter manages it's data, and it is the only workable option when you need the scalability they do.
        
        The important part, of course, is knowing when an extreme approach really is a waste of time. 
        
        The danger of articles like this is that 50% of readers will walk away convinced that extreme logical purity in design is now the optimum solution for all software problems.
        
        Never mind, it's a price worth paying for the rest of us.
    - id: 4882
      author: Huru
      date: "2012-11-22 11:10:51"
      content: "This interesting approach sounds similar to what I read a long time ago in book called, \"Against Method\", by a guy named Feyerabend."
    - id: 4884
      author: Anonymous
      date: "2012-11-22 14:48:48"
      content: |
        Everything is ...
        
        Files – Unix? Hardly, think networking for an example where Unix falls short of the "everything is a file" paradigm. Plan 9 is probably more "Unixy" if you think "Unix == files all the way down."
        
        Cons cells – Lisp. In theory, maybe, but in practise? Hmm ...
        
        Mathematics – Coq? No way. In Coq you don't even get something basic like sets or extensionality. That's not very pratical if you want to develop the kind of mathematics that most people do at the end of the day. 
        
        Arrays – APL/J? More like "everything is a matrix," but not quite. I'm not sure I would call the objects in APL arrays or matrices in their purest form but maybe I am being extremist here ...
        
        How about:
        
        Category Theory – Charity. But that's been dead for quite a long time now. Maybe there are newer developments in this direction?
    - id: 4890
      author: Philip Oakley
      date: "2012-11-23 05:29:20"
      content: |
        go FORTH, and multiply.... 
        
        FORTH is functions all the way down to the machine code. It is simply a list of function addresses, who executable is [a pointer to]: `execute this list of addresses in sequence and then return`. At the very very bottom the pointer is to the next byte/word where it actually executes that machine code, then returns [back up to that list of function addresses].
        
        Oh the good old days..
        
        Philip
    - id: 4893
      author: Anonymous
      date: "2012-11-23 08:47:08"
      content: I assure you my coq is fully automated.
    - id: 4895
      author: Nissim
      date: "2012-11-23 13:11:58"
      content: "eXtreme Programming is EXACTLY taking a principle all the way - why would you state otherwise?"
    - id: 4990
      author: Edward Z. Yang
      date: "2012-11-27 00:29:54"
      content: "Eugene Wallingford points to an earlier essay \"The Three Bears\" here: http://www.cs.uni.edu/~wallingf/blog/archives/monthly/2012-11.html#e2012-11-26T15_24_10.htm"
    - id: 5079
      author: "Extremist Programming | JR Tashjian"
      date: "2012-12-01 19:40:36"
      content: "[...] Extremist Programming Share this:   December 1, 2012 [...]"
    - id: 6108
      author: "编程极端主义 | 尚乐网志_孙财神"
      date: "2013-05-24 04:58:35"
      content: "[...] [英文原文：Extremist Programming ] [...]"
    - id: 6197
      author: "Multi Stack Environment in Awelon | Awelon Blue"
      date: "2013-08-14 21:57:55"
      content: "[&#8230;] post, I took things to an extreme. Which is a good thing. Extremism tells us where the limits are. After we know the limits, we can seek [&#8230;]"
    - id: 15061
      author: earthengine
      date: "2015-07-13 19:08:22"
      content: "Continuations are awesome. What if we made a PL where everything was an continuation?"
    - id: 22004
      author: "Writing a Forth | Artificia Intelligence"
      date: "2017-06-08 21:49:02"
      content: "[&#8230;] like working on ‘extreme’ languages, because applying a principle everywhere is the best way to learn it’s possibilities and [&#8230;]"
    - id: 24863
      author: "=== popurls.com === popular today"
      date: "2020-12-19 09:05:31"
      content: "[&#8230;] Extremist Programming [&#8230;]"
    - id: 24864
      author: "Extremist Programming (2012) - JellyEnt"
      date: "2020-12-19 10:46:11"
      content: "[&#8230;] Read More [&#8230;]"
    - id: 24866
      author: Anonymous
      date: "2020-12-19 19:15:26"
      content: |
        The elements of programming languages which you mentioned seem arbitrarily chosen in order to strengthen your argument. It's clear that if you went a step further and chose to make examples of PL constructs which are foundational in the field, you wouldn't have an argument. Choosing type systems, for example:
        
        Types are awesome. What if we made a PL that mandated each object to be typed?
        
        This proposition doesn't seem ridiculous in the least. In fact, a basic type system is required for any degree of rigor in your PL. A type system is one of those things which separates a PL from an assembly language. Actually, if we were being pedantic, we could even say that ISAs incorporate a sort of type system which has one defined type; the fixed-size word. Operations on words are well defined. To have /some/ form of type system is unavoidable; /some/ knowledge of the attributes of an object is required to apply any operation to that object.
        
        Going even further, it's clear that the absolute foundation of programming languages is Logic. Why not attempt to deconstruct that too?
        
        Logic is awesome. What if we made a PL that was only logically coherent?
        
        Who would say otherwise? To take the contrary position is to be a proponent of incoherent gibberish. But, you wouldn't want to be an extremist, would you now, goy? You've gotta include nonsensical constructs in your programming language to placate the fence-sitting non-extremists.
    - id: 24880
      author: nuclight
      date: "2020-12-20 17:20:00"
      content: "And Tcl, where \"every thing is a string\"."
---

<div class="container center">

*Functions are awesome. What if we made a PL that only had functions?*

*Objects are awesome. What if we made a PL where everything was an object?*

*Lazy evaluation is awesome. What if we made a PL where every data type was lazy?*

</div>

**Extremist programming** (no relation to extreme programming) is the act of taking some principle, elevating it above everything else and applying it everywhere. After the dust settles, people often look at this extremism and think, “Well, that was kind of interesting, but using X in Y was clearly inappropriate. You need to use the right tool for the job!”

Here’s the catch: sometimes you *should* use the *wrong* tool for the job—because it might be the right tool, and you just don’t know it yet. If you aren’t trying to use functions everywhere, you might not realize the utility of functions that take functions as arguments \[1\] or cheap lambdas \[2\]. If you aren’t trying to use objects everywhere, you might not realize that both integers \[3\] and the class of an object \[4\] are also objects. If you aren’t trying to use laziness everywhere, you might not realize that purity is an even more important language feature \[5\].

This leads to two recommendations:

1.  *When learning a new principle, try to apply it everywhere.* That way, you’ll learn more quickly where it does and doesn’t work well, even if your initial intuitions about it are wrong. (The right tool for the job, on the other hand, will lead you to missed opportunities, if you don’t realize that the principle is applicable in some situation).
2.  *When trying to articulate the essence of some principle, an extremist system is clearest.* If you want to know what it is like to program with lazy evaluation, you want to use Haskell, not a language with optional laziness. Even if the extremist system is less practical, it really gets to the core of the issue much more quickly.

There are a lot of situations where extremism is inappropriate, but for fun projects, small projects and research, it can really teach you a lot. One of the most memorable interactions I had in the last year was while working with Adam Chlipala. We were working on some proofs in Coq, and I had been taking the moderate route of doing proofs step-by-step first, and then with Ltac automation once I knew the shape of the proof. Adam told me: “You should automate the proofs from the very beginning, don’t bother with the manual exploration.” \[6\] It was sage advice that made my life a lot better: I guess I just wasn’t extremist enough!

<div class="container center">

*Files are awesome. What if we made an OS where everything was a file?*

*Cons cells are awesome. What if we made a PL where everything was made of cons cells?*

*Mathematics is awesome. What if we made a PL where everything came from math?*

*Arrays are awesome. What if we made a PL where everything was an array?*

</div>

------------------------------------------------------------------------

\[1\] Higher-order functions and combinators: these tend to not see very much airplay because they might be very verbose to write, or because the language doesn't have a very good vocabulary for saying what the interface of a higher-order function is. (Types help a bit here.)

\[2\] Cheap lambdas are necessary for the convenient use of many features, including: monads, scoped allocation (and contexts in general), callbacks, higher-order functions.

\[3\] Consider early versions of Java prior to the autoboxing of integer and other primitive types.

\[4\] Smalltalk used this to good effect, as does JavaScript.

\[5\] This is one of my favorite narratives about Haskell, it comes from Simon Peyton Jones’ presentation [Wearing the hair shirt](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-retrospective/) (in this case, laziness).

\[6\] This is the essence of the Chlipala school of Coq proving, in recognition of how astonishingly easy it is to trick experienced computer scientists into writing the equivalents of straight-line programs by hand, without any abstractions.
