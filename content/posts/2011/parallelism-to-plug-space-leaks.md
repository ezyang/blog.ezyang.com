---
title: "Parallelism to plug space leaks"
date: 2011-07-15 09:00:26
slug: parallelism-to-plug-space-leaks
categories: [Haskell]
comments:
    - id: 2797
      author: Wu Xingbo
      date: "2011-07-15 10:59:56"
      content: "for many other languages, being lazy is so hard to put into practice. In Haskell I realy find operating with lists being effective and more 'possibility'. But sometime I had to make effort to put 'seq's or '!'s into the 'it just works' code, translate these finite operations into strict version.   Laziness mainly ease the hardness of 'infinite'/'recursive' data-structures.  But with trivial structures it may be not always that necessary :)"
    - id: 2798
      author: Edward Z. Yang
      date: "2011-07-15 11:02:04"
      content: "I'll note that this transformation is of practical importance to imperative languages as well. Suppose you have a very large database which you would like to calculate stats on: you will likely get a streaming interface to access the rows. The obvious thing to do is make a giant loop to calculate everything you want at once. If you have parallelism, you can write it more naturally and not have to load everything into application memory."
    - id: 2799
      author: elaforge
      date: "2011-07-15 12:59:24"
      content: |
        At the Bay Area HUG the other night we were talking about iteratees.  The presenters showed a 'fanout' operator that splits an enumeratee's output to multiple iteratees.  It doesn't have to be explicitly parallel, but the effect is the same.  And it seems less likely to lead to space leaks.
        
        Now that I think of it, this is another thing you can do with iteratees that I don't think you can do with iterators.  You can fork an iterator, but since you have no control over how the iterator is used it can easily lead to a space leak.  And in fact it probably will since the forked iterators leave the responsibility for interleaving in the caller's hands.
    - id: 2800
      author: Paul Liu
      date: "2011-07-15 14:56:59"
      content: |
        This example was just bad code. Rewrite it in C or Java or any language, it would perform equally bad if not worse. It has nothing to do with laziness or Haskell or compiler or garbage collection.
        
        The problem is that the two traversals were not shared, and thus it was not only a space leak but also a time leak where efforts are duplicated. Running in parallel will only appear to hide the former (with a LOT of love from the runtime and garbage collector and other criteria as you listed here) but never the latter. 
        
        A proper fix is to combine the two traversals into one. If you argue that such a rewrite hurts modularity, maybe some kind of fusion (that can be expressed in GHC rules) can help to automatically derive it. But the first and foremost important thing is that the programmer still must express some kind of sharing for this to happen, e.g., defining length in terms of fold'. 
        
        It doesn't matter whether you use hand written recursion, or combinators such as foldl, or higher level concepts like stream programming or interatee, if sharing is not expressed in the original program, no compiler can do for you (except perhaps a best effort CSE and we all know how (in)effective it could be). 
        
        Also, even call-by-need would still fail to preserve some sharing during evaluation. Here is a rewrite of the original program, where sharing is properly expressed, but call-by-need would still leak. 
        
            import Data.List 
        
            big = [1..10000000] 
        
            average xs = fromIntegral sum / fromIntegral len 
        
               where fold' op z = foldl' op z xs 
        
                     sum = fold' (+) 0 
        
                     len = fold' (\l _ -&gt; l + 1) 0  
        
            main = print (average big)
        
        Naively, the above code uses fold' to indicate that we want to share the traversal of xs. However, this is sharing of a function not a value, and call-by-need would fail to recognize it. We either need complete laziness [1] or optimal evaluation or online specialization or some kind of fusion done at compile time.
        
        [1] Complete laziness: a natural semantics. FR Sinot - Electronic Notes in Theoretical Computer Science, 2008
    - id: 2801
      author: Paul Liu
      date: "2011-07-15 16:11:03"
      content: |
        I'd like to add that one situation that par can really help is with parser combinators, namely the choice operator. It's perhaps a better example than the superfluous "average" function.
        
        When we goes deep in one matching branch, we are almost always certain that all other branch would have failed even though the entire input has to be kept around until other branches are tried. Making this process parallel would plug space leaks in this situation.  It also looks a lot easiler than other solutions such as implementing lookahead, or re-writing grammar, or the parsec way of commiting by default but use "try" to backtrack.
    - id: 2803
      author: ketil
      date: "2011-07-15 16:35:30"
      content: "Is -N really necessary?  I think that as long as you are using the threaded runtime, the RTS will context-switch between the two sparks.  (And typically, the one who is behind is working on already generated data hot in the cache, so it will tend to catch up)"
    - id: 2804
      author: Edward Z. Yang
      date: "2011-07-15 16:40:40"
      content: |
        elaforge: Yes, I think the situation with iteratees is analogous, though iteratees must be written with a very specific style which makes this sort of optimization always possible.
        
        Paul Liu: I think we are in agreement (esp. with your second comment.) You don't gain particularly much in the case of average, where both stream consumers can easily be operated in lock step and have very simple behavior: the foldl' transformation is not too difficult to do. But when you have complicated consumers (like Parsec) which may skip ahead, or backtrack, up to some constant factor, getting the two consumers to actually work together is pretty complicated, whereas you can just write them separately and then parallelize them, and as long as they are sufficiently well behaved you won't leak too much. I'm not aiming for the moon; I'm just trying to think of a way that eliminates a space leak.
        
        I have to object to the notion that two traversals are always worse than one. Theoretically speaking, this is only a constant factor. Practically speaking, there is a well-known class of cases in C where splitting a loop into two parts improves performance, because the loop body now fully fits into the instruction cache and you are not constantly evicting the other half of the loop. This is probably not true for the code that GHC currently produces, but it could become true in the near future (as our optimizations get better.)
        
        Fusion (which eliminates allocations) is an even better optimization, but it's not always possible.
        
        ketil: I don't know! If it works without -N that would be delightful. I'll check when I get home. I'm not optimistic it will work, though, because par generates sparks and sparks are only run if there is an idle CPU. You could probably make a heavier hammer that always gets run, and I intend on trying my hand at it.
    - id: 2810
      author: Anonymous
      date: "2011-07-18 14:08:42"
      content: |
        This is just a textbook case of (horizontal) fusion (well-known to C/Fortran compilers for at least 15+ years):
        --------------------------------------------------------------
        for(i=M;i&lt;N; i+=K){as[..] = F(xs[i]);}
        for(i=M;i
        for(i=M;i&lt;N; i+=K){
          as[..] = F(xs[i]);
          bs[..] = G(xs[i]);
        }
        --------------------------------------------------------------
        (NOTE: this is still possible if the ranges/strides are different in the two loops, just (sometimes much) trickier).
        
        In general, the conditions for fusing two such loops are
          1) the &quot;input&quot; to the second loop is not data-dependent upon the output of the first
          2) the &quot;iteration-spaces&quot; of the two loops are &quot;sufficiently compatible&quot;
        
        This has a clear mapping to list traversals (or in general (ie with added trickiness), &quot;well-beheved&quot; traversals of an arbitrary datastructure). It&#039;s just that currently GHC lacks the required sophistication/machinery (in this particular area) needed to apply this to the list (or any other) case.
        
        For instance, consider double foldl loops:
        
        blah xs = foo xs + bar xs
        where foo xs = foldl F A xs
                 bar xs = foldl G B xs
        
        So in &#039;blah&#039;, since &#039;foo&#039; and &#039;bar&#039; both consume their input lists in identical ways (their &quot;\&quot;iteration spaces\&quot;&quot; are &quot;\&quot;sufficiently compatible\&quot;&quot;), AND since the input to each loop are not data-dependent upon the output of the other, they can be (horizontally) fused.
    - id: 2811
      author: Anonymous
      date: "2011-07-18 14:10:27"
      content: |
        My for-loops got butchered:
        
        ————————————————————–
        for(i=M;i&lt;N; i+=K){as[..] = F(xs[i]);}
        for(i=M;i&lt;M; i+=K{bs[..] = G(xs[i]);}
        --------------------------------------------------------------------
        to
        --------------------------------------------------------------------
        for(i=M;i&lt;N; i+=K){
        as[..] = F(xs[i]);
        bs[..] = G(xs[i]);
        }
        ————————————————————–
    - id: 2812
      author: Edward Z. Yang
      date: "2011-07-18 19:01:07"
      content: "Condition (1) seems a bit too aggressive: I can still avoid space leak in the case that the second loop depends on the output of the first, so long as the second loop is a good consumer of the first loop."
    - id: 2816
      author: Anonymous
      date: "2011-07-19 15:14:14"
      content: "Oh absolutely, in that case it would be s/horizontal/vertical/ fusion."
    - id: 2817
      author: Anonymous
      date: "2011-07-19 15:16:42"
      content: "(\"horizontal\"/\"vertical\" being motivated by the spatial directions of fusion if we're considering circuit elements)"
    - id: 2818
      author: Anonymous
      date: "2011-07-19 15:19:07"
      content: "well, i suppose too that that depends on how the circuits are drawn, but you get the idea."
---

It is [not too difficult (scroll to “Non sequitur”)](http://blog.ezyang.com/2010/11/is-multiply-carry-strongly-universal/) to create a combinator which combines two folds into a single fold that operates on a single input list in one pass. This is pretty important if your input list is pretty big, since doing the folds separately could result in a space leak, as might be seen in the famous “average” space leak:

    import Data.List
    big = [1..10000000]
    sum' = foldl' (+) 0
    average xs = fromIntegral (sum' xs) / fromIntegral (length xs)
    main = print (average big)

(I’ve redefined `sum` so we don’t stack overflow.) I used to think combining functions for folds were pretty modular, since they had a fairly regular interface, could be combined together, and really represented the core notion of when it was possible to eliminate such a space leak: obviously, if you have two functions that require random access to elements of the list, they’ll retain the entirety of it all the way through.

Of course, a coworker of mine complained, “No! That’s not actually modular!” He wanted to write the nice version of the code, not some horrible gigantic fold function. This got me thinking: is it actually true that the compiler can’t figure out when two computations on a streaming data structure can be run in parallel?

But wait! We can tell the compiler to run these in parallel:

    import Data.List
    import Control.Parallel
    big = [1..10000000]
    sum' = foldl' (+) 0
    average' xs =
        let s = sum' xs
            l = length xs
        in s `par` l `par` fromIntegral s / fromIntegral l
    main = print (average big)

And lo and behold, the space leak goes away (don’t forget to compile with `-threaded` and run with at least `-N2`. With the power of multiple threads, both operations can run at the same time, and thus there is no unnecessary retention.

It is perhaps not too surprising that `par` can plug space leaks, given that `seq` can do so too. But `seq` has denotational content; `par` does not, and indeed, does nothing when you are single-threaded. This makes this solution very fragile: at runtime, we may or may not decide to evaluate the other thunk in parallel depending on core availability. But we can still profitably use `par` in a single-threaded context, if it can manage pre-emptive switching between two consumers of a stream. This would be a pretty interesting primitive to have, and it would also be interesting to see some sort of semantics which makes clear the beneficial space effects of such a function. Another unbaked idea is that we already have a notion of good producers and consumers for stream fusion. It doesn’t seem like too far a stretch that we could use this analysis to determine when consumers could be merged together, improving space usage.
