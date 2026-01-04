---
title: "Obviously Correct"
date: 2011-10-24 09:00:34
slug: obviously-correct
categories: [Computer Science]
comments:
    - id: 3056
      author: "Sam Tobin-Hochstadt"
      date: "2011-10-24 11:52:53"
      content: |
        Pattern matching, of course, is present in lots of languages without static types.  Racket [1], Clojure [2], Common Lisp [3]. And QuickCheck is these days being actively developed in Erlang.
        
        [1] http://docs.racket-lang.org/reference/match.html
        [2] https://github.com/clojure/core.match
        [3] http://common-lisp.net/project/cl-match/doc/clmatch.htm
    - id: 3057
      author: Edward Z. Yang
      date: "2011-10-24 12:00:07"
      content: "Yes, though arguably these two features work a lot better when you have types."
    - id: 3058
      author: nominolo
      date: "2011-10-24 13:37:09"
      content: |
        Your argument of static compilation being better than JIT is also not very strong.  I think dynamic compilation can be very effective independent of the typing discipline of your source language.  LuaJIT 2 is a good example.  Whether a (tracing) JIT can be effective or not depends more on the language semantics itself.  Lua, being a small and orthogonal language can be made very fast with a JIT, but I think it's much, much harder to do the same for languages like Javascript, Ruby, or Python.
        
        The problem is in the degree of observability and the associated room for optimisation.  As an example, the Javascript spec mandates that traversing the fields of an object must occur in the same order in which they were added to the object prototype.  Maintaining these semantics requires memory overhead or performance overhead (or both).  Languages that evolved around an interpreter-based implementation often get features which were easy to implement in the interpreter, but may cause severe problems for future optimisers (e.g., the ability to inspect call stack frames).
        
        I think a better argument for static types is the ability to use your types to enforce invariants.  E.g., "newtype NonEmptyList a = NEList a [a]"  I.e., types are no longer just documentation.  In my experience these kinds of things are easier in a statically typed language.
    - id: 3061
      author: gasche
      date: "2011-10-25 04:30:57"
      content: |
        &gt; With static types you also get pattern matching,
        &gt; QuickCheck style property testing,
        &gt; and performance benefits. 
        
        As noted by Sam Tobin-Hochstadt, this point is really weak. I don't see how pattern matching would "work better" with static types; ok, you get typed pattern matching (and, if you want to be able to do convenient things, GADTs), but that was to be expected. Similarly, I don't understand what you see in Quickcheck that specifically needs static typing. I agree that it combine well with Haskell type-classes, but in the end it's just a convenient DSL language for random generation and testing, none of it relying on static typing. Type-classes are used to convey the per-type random generation operations, but that could equally be done by adding methods to some objects or what not, and static type information is used to derive default implementations for those operations, but then again objects and introspection would probably work just as well. I've been thinking about doing something Quickcheck-like for OCaml; it is statically typed, but I don't see how that would help me.
        
        The performance argumentation is better, but still not "killer", as shown by the relatively good performance of systems with no static type system, or unsound type systems (eg. Java).
        
        All in all, I'm not so much convinced by your argumentation. In the end, a static type system is some form of static analysis on programs. It stands to reason that having some form of static analysis available is better than having no static analysis available; for pre-execution static analysis is still the most efficient defect-detection technique. Now I would play the devil advocate and argue that :
        - There is no fundamental difference between type systems and external static analysis tools such as Erlang's Dialyzer : http://www.it.uu.se/research/group/hipe/dialyzer .
        - We should not take it for granted that type system will stop us from writing and running desirable programs. We should evolve the type systems to accept more programs, and we should be able to conveniently move from static verification to dynamic verification, being able to say locally "ok you don't know how to prove that correct, well, just insert a check and leave me alone"
        
        Static verification is good. Dynamic checks are also good (see eg. Racket Contract that are, for most applications, a much more cost-effective way than proof assistants to enforce invariants that the usual ML type systems can't express conveniently). You want to have both and to be able to go smoothly from one to another. Dynamic-only languages lose from that point of view. But current static languages are not resilient enough to type checking failures, or do not expose their dynamic capacities in a convenient enough way. It's telling that the most advanced language in this direction nowadays (read: that has good idiomatic contract-programming features)  is probably partially-Typed Racket, not Haskell. Fortunately, things are moving in the right direction; contracts are very much a 21st century development.
    - id: 3062
      author: gasche
      date: "2011-10-25 04:35:31"
      content: "PS: I think what we should loudly ask for nowaday is not a type system, but a clear and precise language semantics. We can go a long way with a well-specified language, be it designed with a static type system from scratch or not. \"Good semantics\" has always been the design pressure to keep things honest; and I'd argue we have more problems nowadays with unclear semantics than with dynamically typed language -- of course, there is a cultural correlation between ill-specified and dynamic languages, and researchy-static and well-specified language. Once you have good semantics, you can come up with reasonable static analyses."
    - id: 3065
      author: Edward Z. Yang
      date: "2011-10-25 14:36:29"
      content: |
        Wow, really good opinions.
        
        Re Observability: I agree, this can be a nontrivial part of making something fast. It's just not clear to me to what degree. I will have to admit ignorance with LuaJit; I would love it if you described more things that Lua has underspecified that let it JIT fast. Is it lack of introspection? Restricted dynamic dispatch?
        
        Re QuickCheck: I think you're essentially correct, but I think you ignore the degree to which being in a statically typed language makes this procedure feasible. One point of dynamic types is that you can expand the set of valid values for some value: but in this universe you can't get any sensible guarantee of how much you need to put into the function to get any sort of reasonable assurance. You can tell the program explicitly... but isn't that just static types (just without the checking, granted!)
        
        Perhaps I shouldn't have *mentioned* static types at all: the overall argument should carry without citing them. The question here is *restriction*. (And there's not really any reason restriction can't help dynamic analyses.)
    - id: 3092
      author: Anonymous
      date: "2011-11-07 03:08:40"
      content: "The way that I see this is that the more disciplined the compiler is, the less disciplined that I have to be!"
    - id: 3093
      author: kikito
      date: "2011-11-07 04:28:28"
      content: "I just want to say that I don't want type safety the same way I don't want memory management. Thinking about types takes too much of my time for too little benefit: I rarely get type errors - maybe once every 10000 LOC, and those are usually picked by tests - and I don't need that extra bit of performance anyway. But I have to appease the type god on every method definition."
    - id: 3095
      author: Josh Bloch
      date: "2011-11-08 15:21:41"
      content: Another huge benefit of static typing is that it enables high quality autocompletion in IDEs.
    - id: 3263
      author: oelewapperke
      date: "2011-12-26 23:47:24"
      content: |
        Oh and don't forget that the idea of "obviously correct" is fundamentally incompatible with dynamic typing. Take the matching example again :
        
        static typing : you're 100% sure you're not forgetting a corner case if you don't get a compiler error
        dynamic typing : well ... uhm ... yeah ... how exactly is this correct again ?
        
        Same goes for several other correctness properties. Dynamic typing destroys them. In the case of python, it's not even guaranteed to be correct for memory leaks. 
        
        @gasche
        "we should be able to conveniently move from static verification to dynamic verification, being able to say locally “ok you don’t know how to prove that correct, well, just insert a check and leave me alone”"
        
        This is exactly what static typing + match gets you. Just insert an assert in one of the cases. What static typing gets you is a guarantee of no surprises. The type of the variables will not magically change without warning, as it so often does in python.
---

What do automatic memory management, static types and purity have in common? They are methods which take advantage of the fact that we can make programs *obviously correct* (for some partial definition of correctness) upon visual inspection. Code using automatic memory management is *obviously correct* for a class of memory bugs. Code using static types is *obviously correct* for a class of type bugs. Code using purity (no mutable references or side effects) is *obviously correct* for a class of concurrency bugs. When I take advantage of any of these techniques, I don’t have to *prove* my code has no bugs: it just is, automatically!

Unfortunately, there's a catch. What all of these “obviously correct” methodologies ask you do is to sacrifice varying degrees of expressiveness at their altar. No more pointer tricks. No more playing fast and loose with data representation. No more mutation. If this expressiveness was something most people really didn’t want anyway (e.g. memory management), it is happily traded away. But if it’s something they *want*, well, as language designers, we’re making it harder for people to do things that they want to do, and it shouldn’t surprise us when they grab their torches and pitchforks and storm the ivory tower, assertions about correctness and maintainability be damned.

It seems to me that we must fight fire with fire: if we’re going to take away features, we better be giving them compelling new features. With static types you also get pattern matching, QuickCheck style property testing, and performance benefits. With purity, you get software transactional memory and speculative evaluation. Discovering and implementing more of these “killer apps” is the key to adoption. (Some research that I’m currently doing with Adam Chlipala is leveraging purity to offer automatic caching for web applications. It’s not much, but I think it’s in the right direction.)

I still have a fanatical devotion to correctness. But these days, I suspect that for most people, it’s something bitter, like medicine, to be taken with some better tasting features. That’s fine. Our challenge, as programming language researchers, is to exploit correctness to bring tangible benefits now, rather than nebulous maintainability benefits later.

*Thanks Nelson Elhage and Keegan McAllister for their comments.*

------------------------------------------------------------------------

*Postscript: Performance of static types versus dynamic types.* An earlier draft of this post pointed at [Quora’s decision to move to Scala from Python](http://www.quora.com/Is-the-Quora-team-considering-adopting-Scala-Why) as a clear indicator of this fact. Unfortunately, as several pre-readers pointed out, there are too many confounding factors to make this claim definitive: CPython was never explicitly engineered for performance, whereas the JVM had decades of work poured into it. So I’ll have to leave you with a more theoretical argument for the performance of static types: the optimization techniques of runtime just-in-time compilers for dynamic compilers involves identifying sections of code which are actually statically typed, and compiling them into the form a static compiler will. So, if you know this information ahead of time, you will always do better than if you know this information later: it's only a question of degree. (Of course, this doesn't address the possibility that JIT can identify information that would have been difficult to determine statically.)

*Postscript: Shared transactional memory.* Joe Duffy had a great [retrospective on transactional memory](http://www.bluebytesoftware.com/blog/2010/01/03/ABriefRetrospectiveOnTransactionalMemory.aspx) and the experience he had attempting to implement it for Microsoft’s stack. And despite a great enthusiasm for this idea, it’s interesting to note this quote:

> Throughout all of this, we searched and searched for the killer TM app. It’s unfair to pin this on TM, because the industry as a whole still searches for a killer concurrency app. But as we uncovered more successes in the latter, I became less and less convinced that the killer concurrency apps we will see broadly deployed in the next 5 years needed TM. Most enjoyed natural isolation, like embarrassingly parallel image processing apps. If you had sharing, you were doing something wrong.

Richard Tibbetts points out that concurrency is often addressed at an architectural level *lower* than what most working programmers want to deal with, and so while STM is a killer application for those platforms, most developers don't want to think about concurrency at all.
