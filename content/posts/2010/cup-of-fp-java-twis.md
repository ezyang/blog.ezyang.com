---
title: "Cup of FP with a Java twist"
date: 2010-04-05 09:00:26
slug: cup-of-fp-java-twis
categories: [Hack, Haskell]
comments:
    - id: 268
      author: Heinrich Apfelmus
      date: "2010-04-06 08:28:21"
      content: |
        <i>Our second problem set asks us to write some code in this pseudolanguage. Unfortunately, being a pseudolanguage, you can't actually run it..</i>
        
        Dijkstra argues that this is actually desirable: <a href="http://userweb.cs.utexas.edu/users/EWD/ewd10xx/EWD1036.PDF" rel="nofollow">On the cruelty of really teaching computing sciences</a>. :)
    - id: 273
      author: Edward Z. Yang
      date: "2010-04-07 12:00:56"
      content: |
        I believe Dijkstra's argument is slightly different, specifically: <i>[The programmer's] main task is to give a formal proof that the program he proposes meets the equally formal functional specification...in order to drive home the message that this introductory programming course is primarily a course in formal mathematics, we see to it that the programming language in question has <u>not</u> been implemented on campus so that students are protected from the temptation to test their programs.</i> Unfortunately, this course is doing a pretty crummy job at encouraging students to use formal methods either.
        
        I am reminded of Knuth's quote: <i>Beware of bugs in the above code; I have only proved it correct, not tried it.</i>
---

    zip: List<A>, List<B> -> List<(A, B)>
    zip(Nil, Nil) = Nil
    zip(_, Nil) = Nil
    zip(Nil, _) = Nil
    zip(Cons(a, as), Cons(b, bs)) = Cons((a, b), zip(as, bs))

    fst: (A, B) -> A
    fst((a, _)) = a

    last: List<A> -> A
    last(Cons(a, Nil)) = a
    last(Cons(a, as)) = last(as)

    foldl: (B, A -> B), B, List<A> -> B
    foldl(_, z, Nil) = z
    foldl(f, z, Cons(x, xs)) = foldl(f, f(z, x), xs)

Good grief Edward, what do you have there? It's almost as if it were some bastardized hybrid of Haskell, Java and ML.

It actually is a psuedolanguage inspired by ML that was invented by Daniel Jackson. It is used by [MIT course 6.005](http://ocw.mit.edu/OcwWeb/Electrical-Engineering-and-Computer-Science/6-005Fall-2008/CourseHome/index.htm) to teach its students functional programming concepts. It doesn't have a compiler or a formal specification (although I hear the TAs are frantically working on one as a type this), though the most salient points of its syntax are introduced [in lecture 10 (PDF)](http://ocw.mit.edu/NR/rdonlyres/Electrical-Engineering-and-Computer-Science/6-005Fall-2008/5FC036C0-0505-49AE-BCA2-455E89B1AB18/0/MIT6_005f08_lec10.pdf) when they start discussing how to build a SAT solver.

Our second problem set asks us to write some code in this pseudolanguage. Unfortunately, being a pseudolanguage, you can't actually run it... and I hate writing code that I can't run. But it certainly looks a lot like Haskell... just a bit more verbose, that's all. I asked the course staff if I could submit the problem set in Haskell, and they told me, "No, since the course staff doesn't know it. But if it's as close to this language as you claim, you could always write it in Haskell and then translate it to this language when you're done."

So I did [just that](http://github.com/ezyang/haskell-mit6005).

The plan wouldn't really have been possible without the existence of an existing [pretty printer for Haskell](http://hackage.haskell.org/packages/archive/haskell-src/1.0.1.3/doc/html/Language-Haskell-Pretty.html) to do most of the scaffolding for me. From there, it was mucking about with `<>`, `lparen` and `comma` and friends in the appropriate functions for rendering data-types differently. [Pretty printing combinators rock!](http://hackage.haskell.org/packages/archive/pretty/1.0.1.1/doc/html/Text-PrettyPrint-HughesPJ.html)
