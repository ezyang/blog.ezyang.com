---
title: "Is Haskell liberal or conservative?"
date: 2012-08-10 09:12:50
slug: is-haskell-liberal-or-conservative
categories: [Haskell]
comments:
    - id: 3921
      author: Ketil
      date: "2012-08-10 10:44:56"
      content: |
        I suspect higher order functions / combinators replace a lot of the need for metaprogramming.  Further, I think this would be much more difficult to get right without static typing.
        
        I also think ostensibly "liberal" values like succinctness and simplicity definitely belong in the "conservative" camp.
    - id: 3922
      author: smitty
      date: "2012-08-10 10:59:25"
      content: |
        The 'politicization of everything' is absolutely not a conservative approach, and I bitterly object to applying such terms to programming.
        IT is typically a refuge from the coarseness of the political brawl.
        That said, I will admit to the suspicion that PHP is a Commie plot.
    - id: 3923
      author: Nicolas Frisby
      date: "2012-08-10 11:18:38"
      content: |
        This is a great take on it! Another fun and interesting post.
        
        Quasiquoters can also do pretty great for extensible syntax; eg Duregård &amp; Jansson's "Embedded Parser Generators" at Haskell'11, BNFC-meta on hackage. I for one would like the particularly brutal abuses of infix to be a thing of the past.
        
        I was also hoping you could elaborate on this a bit: "Why use macros when you can do it properly in Template Haskell!"? Perhaps I'm misreading some tongue-in-cheekiness here?
    - id: 3924
      author: Alex Lang
      date: "2012-08-10 11:31:56"
      content: "I think you meant to write {-# UNPACK #-} and not {-# UNBOXED #-}."
    - id: 3925
      author: Edward Z. Yang
      date: "2012-08-10 11:55:52"
      content: |
        Alex: Thanks, fixed.
        
        Ketil: They do. But one must think that there are some areas that they do quite poorly at, looking at all of the different metaprogramming approaches that have been developed in Haskell! (I think one area they do particularly poorly at is when you have very large data types with lots of fields or variants. You can write a higher order function to deal with it, but you still need a lot of boiler plate for that function, which is compounded when you need lots of different "versions" of the function for different use-cases.)
        
        Nicolas: The cheekiness is taking a jab at unhygienic macros, which no one actually uses anymore, but are part of the reason why macros have a bad rep.
    - id: 3927
      author: lukewarm
      date: "2012-08-10 12:32:12"
      content: "Haskell must be libertarian :^)"
    - id: 3928
      author: Anonymous
      date: "2012-08-10 12:57:51"
      content: |
        I am not familiar with Template Haskell, but I am doubtful that the full extent of Lisp macros is possible in Haskell. I would not say that Lisp is liberal or that Haskell is conservative, but both languages certainly feel very, very different and I find myself thinking differently about the problem at hand depending on whether I write Haskell (usually starting with appropriate data types, then coming up with operations that make expressing the solution easy) or I write in Lisp (everything is a list initially, try to  solve the problem interactively on the REPL and then factor the solution into little functions and macros, rinse and repeat several times). An analogy for this comes to mind: Writing in Haskell is more like designing a circuit by careful calculation and research into what other people would do, looking for the right ICs (i.e. data structure), going to great lengths to make everything as neat as possible where writing in Lisp is more like starting with a problem, a breadboard, a handful or wires and some components and through methodical experimentaiton building a circuit that does the job.
        
        But I guess it's a cultural divide. Some people believe in hygiene and others believe that only the dosage makes a poison whereas too much of a good thing can kill you. ;-)
    - id: 3929
      author: Johan Tibell
      date: "2012-08-10 12:58:56"
      content: I think applying applying a dumb political spectrum to programming languages is dumb. It illuminates nothing.
    - id: 3930
      author: "Alain O'Dea"
      date: "2012-08-10 15:13:31"
      content: |
        The joke at the close is brilliant :)
        
        I like your response to Steve's post.  It makes a very sensible analysis of the liberal possibilities enabled by a conservative core.  There is an irony in the presence of such a solid core allowing radical freedom to the programmer because they can trust the core.  My feeling is that it is difficult to trust a liberally designed core, but I am glad to use liberal features in my own edge code.
    - id: 3932
      author: Dan
      date: "2012-08-10 19:19:28"
      content: |
        "Auto-casting: Numeric literals, anyone?"
        
        Totally does not count: (length [])/2
    - id: 3936
      author: ceii
      date: "2012-08-11 13:18:19"
      content: |
        I don't see anything wrong with Haskell being called conservative, since it's built squarely on the belief that safety and good comfy certitudes are enablers rather than obstacles. That's basically what we mean when we say "we do crazy shit that no one in their right mind would do without Haskell’s safety features".
        
        Sure Yegge's list of criteria is flawed, but that's because it's written by a hardcore software liberal. Since good=liberal and everyone agrees succint=good, obviously succintness ends up on the liberal list (same with ease of change, reliance on programmer skill, etc.). Still the divide makes sense: Haskellers believe every minute spent fixing a bug is time wasted, and putting a solid, tangible structure over their problem domain helps them think creatively. Clojurers (say) believe every minute spent making the compiler happy is time wasted, and having only a big, shapeless pile of clay-do in front of them helps them think creatively.
    - id: 3940
      author: arpunk
      date: "2012-08-12 13:10:24"
      content: "lukewarm said it best. Haskell is nor conservative or liberal, it's libertarian."
    - id: 3941
      author: Tony Morris
      date: "2012-08-12 17:15:13"
      content: |
        Yegge's typically dishonest drivel does not deserve a response from thinking people. He has absolutely no clue on the subject and insists on staying that way. Sure, the nonsense is so extensive that it regularly misleads curious minds, but we can alter that with solid and honest guidance, rather than paying attention to this inane crap.
        
        Please can we just dismiss his garbage and move on, just like last time and the before that?
    - id: 3942
      author: noname
      date: "2012-08-12 18:31:22"
      content: |
        I'm with Tony Morris here.  
        
        And we can drop the political spectrum comparisons.  Haskell is a language with strong mathematical foundations.  Computer science's history is traced directly to Russell and Whitehead, Godel, Turing, Bernays, von Neumann, and /finally/ to electrical engineers who adopted the von Neumann architecture to physically realize a model of computation. Haskell goes back to the roots of computer science by adopting a "mathematical" computational model.  Of course, the EE-types resist this sort of shift, since it exposes the inherent lack of safety in the underlying electronic implementation and simultaneously reveals their lack of a solution with the required generality.
        
        And please, don't call Haskell "Libertarian".  Haskell is mostly coherent.
    - id: 3943
      author: Harald K
      date: "2012-08-13 07:33:21"
      content: |
        "It doesn’t disallow language features, just make them more wordy (unsafePerformIO) or harder to use. Haskell has a healthy belief in escape hatches."
        
        The same can be said of Ada, which will cheerfully let you define a variable to a fixed memory location and coerce it into a function pointer - it's just takes a little more work and disabling of safety checks than in C.
    - id: 3945
      author: Anonymous
      date: "2012-08-14 06:30:11"
      content: "None of them, it is a lazy functional programming language."
    - id: 3976
      author: Anonymous
      date: "2012-08-17 10:17:50"
      content: |
        I don't know if political labels can be attached to programming languages, but I think we can say something about the programmers of certain languages. 
        Haskell, like most other functional programming languages is the brainchild of a bunch of mathematicians in academia - which is pretty much communist territory. 
        The said mathematicians, like most people who (try to) use functional languages today (they don't really manage to use them in any practical and meaningful way), have a very poor understanding of software engineering in general and tend to struggle hard with OOP languages. It's almost like a commie "class" struggle. Commie programmers who have a hard time understanding why OOP languages dominate the programming field, Even worse, it's not very rare to hear FP people talking about corporate conspiracy &amp; other ridiculous stufff in relation to OOP's clear dominance. Just like classical commies, they fail to understand and accept the REALITY, how OOP came into being and how practical it is, and how FP is a useless attempt at creating languages that serve only one purpose: to accomodate  mathematicians' inability to learn real world programming (not surpsingly, the code FP languages resemble math notation, including its clumsiness, cryptical and confusing style - code which is often only clear to those who originally wrote the code - or in other words - horrible, unintelligible, unmaintainable code which ironically is called "expressive" and "easy to read" by the said mathematicians... however, all they need to do is ask other people to read they code - they'd be in for a huge surprise)
    - id: 4022
      author: Anonymous
      date: "2012-08-27 12:22:55"
      content: |
        "Of course, the reason for this is not syntax for its own sake, but syntax for the sake of closely modeling existing syntax that mathematicians and other practitioners already use."
        
        One problem with this is that mathematicians and other practitioners don't come up with the clearest syntax because they tend to assume a large body of associated knowledge that isn't necessarily present in readers. http://video.google.com/videoplay?docid=-2726904509434151616#
    - id: 4023
      author: Edward Z. Yang
      date: "2012-08-27 12:26:41"
      content: "Right. But on the flip side, the assumption of a body of associated knowledge is exactly what makes the notation work, and speeds up transmission of knowledge. It's analogous to the backwards compatibility problem in software engineering: yes, the protocol is suboptimal in a large number of ways, but everyone knows what it is and speaks it, and this network effect outweighs the purported benefits of any improvement to it. Eventually, the scale tips and we see new technologies come on board, but until then it's an uphill battle."
    - id: 6586
      author: Anonymous
      date: "2014-04-12 12:41:02"
      content: |
        The post is good. But its finny that Tony Morris says move on I am quite sure he posted on Yegge's blog under another name. And kept posting, until Steve shut him up by asking him to name atleast some product that the masses could not leave without.
        Haskell is good language , one of my favs...but I think the Haskell community needs to grow up and understand that the industry is a far practical place. Java, C++ and Objective C might not be a great functional language and might be actually very poor language in the eyes of academics arguing right and left fold and Hindley Miler or category theory...it hasnt stopped the masses from using the brilliant softwares built with it.
        
        The industry accepts languages with which practical interesting products can be built easily, without understanding a mass of mathematics or other theory. Has teh Haskell community built a website like Linkedlin (built mostly with java), Google(c++, java) or github(ruby ) or itunes? I guess not...not remotely close
    - id: 26089
      author: Anonymous
      date: "2021-05-28 09:37:24"
      content: |
        things like C, assembly language, unix shell scripts, sed/awk/perl, are more libertarian, giving the user freedom to do whatever he likes and leaving him with the responsibility to do something sensibly on his own...
        whereas things like rust, haskell, lisp, ML, etc, are authoritarian/totalitarian.. obsessed with ideology and conceptual purity bordering on theocratic.. form is more important than function and one has to do contortions to reshape one's work to fit the programming model, instead of the other way around..
        whereas things like C++, java, most of the 'business' languages, are bureacratic, suffocating any attempt at programming with endless boilerplate and process just to get anywhere, and you cant even bribe some official to just get your work done..
    - id: 26867
      author: Marcus
      date: "2021-10-12 10:12:26"
      content: "When liberalism reaches its natural conclusion, socialism or communism, it is extremely risk-averse. There is simply no more money to throw away. It is difficult to flirt with political categories and programming languages."
---

Steve Yegge has posted a [fun article](https://plus.google.com/u/0/110981030061712822816/posts/KaSKeg4vQtz) attempting to apply the liberal and conservative labels to software engineering. It is, of course, a gross oversimplification (which Yegge admits). For example, he concludes that Haskell must be “extreme conservative”, mostly pointing at its extreme emphasis on safety. This completely misses one of the best things about Haskell, which is that *we do crazy shit that no one in their right mind would do without Haskell’s safety features.*

So I thought I’d channel some Yegge and take a walk through the criteria proposed for assessing how conservative a user of a language is, and try to answer them to the best of my ability with my ”Haskell hat” on:

1.  *Software should aim to be bug free before it launches.* Yes. Though, “Beware of bugs in the above code; I have only proved it correct, not tried it.”
2.  *Programmers should be protected from errors.* Yes. **But**, Yegge then adds the sentence: “Many language features are inherently error-prone and dangerous, and should be disallowed for all the code we write.” This is not the approach that Haskell takes: if you want continuations with mutable state, Haskell will give them to you. (Try doing that in Python.) It doesn’t *disallow* language features, just make them more wordy (`unsafePerformIO`) or harder to use. Haskell has a healthy belief in escape hatches.
3.  *Programmers have difficulty learning new syntax.* **No.** Haskell is completely on the wrong side of the fence here, with arbitrary infix operators; and even more extremist languages (e.g. Coq) go even further with arbitrary grammar productions. Of course, the reason for this is not syntax for its own sake, but syntax for the sake of closely modeling existing syntax that mathematicians and other practitioners already use. So we allow operator overloading, but only when it is backed up by algebraic laws. We allow metaprogramming, though I suspect it’s currently used sparingly only because it’s so unwieldy (but *culturally*, I think the Haskell community is very open to the idea of metaprogramming).
4.  *Production code must be safety-checked by a compiler.* Yes. **But,** anyone who has used a dependently typed language has a much higher standard of what “safety-checked” means, and we regularly play fast and loose with invariants that we decided would be too annoying to statically encode. Note that Yegge claims the opposite of compiler safety-checking is *succinctness*, which is a completely false myth perpetuated by non-Hindley Milner type systems with their lack of type inference.
5.  *Data stores must adhere to a well-defined, published schema.* Well-defined? Yes. Published? No. The emphasis that Haskell has on static checking mean that people writing data types are a lot more willing to update them as the needs of the application change, and don’t really mind global refactoring of the database because it’s so damn easy to get right.
6.  *Public interfaces should be rigorously modeled.* Yes. (though *cough* “ideally object oriented” *cough*)
7.  *Production systems should never have dangerous or risky back-doors.* **Accidental.** The lack of tooling here means that it’s pretty difficult to snoop into a running compiled executable and fiddle around with internal data: this is a big sore point for the current Haskell ecosystem. But in the abstract, we’re pretty flexible: XMonad, for example, can be restarted to run arbitrary new code *while preserving the entirety of your working state*.
8.  *If there is ANY doubt as to the safety of a component, it cannot be allowed in production.* This is something of a personal question, and really depends on your project, and not so much on the language itself. Haskell is great for safety critical projects, but I also use it for one-off scripts.
9.  *Fast is better than slow.* **No.** Haskell code has the opportunity to be really fast, and it tends to be quite zippy from the get go. But we’ve emphasized features (laziness and abstraction) which are known to cause performance problems, and most Haskellers take the approach of only optimizing when our (very awesome) profiler yells at us. Some Haskellers reflexively add `! {-# UNPACK #-}` to their data types, but I don’t—at least, not until I decide my code is too slow.

Haskell has a lot of features which show up in Yegge’s “Liberal Stuff”. Here are some of them:

- Eval: We love coding up interpreters, which are like type-safe evals.
- Metaprogramming: Template Haskell.
- Dynamic scoping: Reader monad.
- all-errors-are-warnings: We can [delay type errors to runtime!](http://hackage.haskell.org/trac/ghc/ticket/5624).
- Reflection and dynamic invocation: `class Data`.
- RTTI: I hear it’s called a “dictionary”.
- The C preprocessor: Indispensable, begrudgingly.
- Lisp macros: Why use macros when you can do it properly in Template Haskell!
- Domain-specific languages: Haskell eats EDSLs for lunch.
- Optional parameters: It’s called combinator libraries.
- Extensible syntax: Fuck yeah infix!
- Auto-casting: Numeric literals, anyone?
- Automatic stringification: `class Show` and deriving.
- Sixty-pass compilers: GHC does *a lot* of passes.
- Whole-namespace imports: Yep (and it's both convenient and kind of annoying).

The feeling I get from this conversation is that most people think “Haskell” and “static typing” and while thinking about how horrible it is to write traditional dynamically typed code in Haskell, forget that Haskell is actually a surprisingly liberal language prizing understandability, succinctness and risk-taking. Is Haskell liberal or conservative? I think of it as an interesting point in the design space which treats some conservative viewpoints as foundational, and then sees how far it can run from there. *It’s folded so far right, it came around left again.*
