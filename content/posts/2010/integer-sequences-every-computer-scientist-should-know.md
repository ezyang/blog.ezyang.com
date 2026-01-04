---
title: "Integer sequences every computer scientist should know?"
date: 2010-11-24 09:00:09
slug: integer-sequences-every-computer-scientist-should-know
categories: [Computer Science]
comments:
    - id: 1551
      author: Brent Yorgey
      date: "2010-11-24 12:38:53"
      content: "One of my favorite integer sequences is <a href=\"http://www.mathlesstraveled.com/?p=94\" rel=\"nofollow\">1,1,2,1,3,2,3,1,4,3,5...</a>, although I wouldn't say it's something every computer scientist ought to know."
    - id: 1552
      author: WhoeverIAm
      date: "2010-11-24 12:58:36"
      content: |
        1) 2,3,5,7,11,13,17,19,23,29,... (A000040)
        2) Binomial coefficients e.g. a) 1,2,1 b) 1,3,3,1 b) 1,5,10,10,5,1
        4) 0, 1, 3,  2,  6,  7, 5, 4, ... (A003188)
        
        :P
    - id: 1553
      author: Anonymous
      date: "2010-11-24 16:04:48"
      content: "That's simple. 2 4 8 16 32 64... (and of course the resulting maximum boundaries). How many times did I look at an error message or log and think, \"hmm, that number 65535 sure looks familiar\", and whoops, solved the problem. Now imagine that would just be a random number for me, I'd be debugging forever. :)"
    - id: 1554
      author: Anonymous
      date: "2010-11-24 16:10:25"
      content: "0, 1, 3, 2, 6, 7, 5, 4..."
    - id: 1555
      author: Edward Z. Yang
      date: "2010-11-24 16:59:52"
      content: "Of course! I don't know how I forgot that one :-) Relatedly, 1, 3, 7, 15, 31, 63... (since we're zero-indexed bastards.)"
    - id: 1556
      author: Derek Elkins
      date: "2010-11-24 18:49:56"
      content: 0 1 3 2 6 7 5 4
    - id: 1557
      author: Edward Z. Yang
      date: "2010-11-24 18:57:03"
      content: Yum Gray codes.
    - id: 1560
      author: Ashley Yakeley
      date: "2010-11-25 06:47:53"
      content: |
        I've got my very own entry:
        
        5, 6, 8, 16, 65536, ....
    - id: 1561
      author: Edward Z. Yang
      date: "2010-11-25 07:40:32"
      content: "Apologies to users who got their comments trapped by the spam filter. I guess Akismet thinks that numbers are spammy. :o)"
    - id: 1562
      author: jeff putnam
      date: "2010-11-25 10:42:56"
      content: "1, 1, 2, 6, 24, 120..."
    - id: 1567
      author: Leon P Smith
      date: "2010-11-26 05:23:53"
      content: |
        I was slightly embarrassed that I had to look this one up the other day:
        
        2, 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, 1, 1, 10, ...
        
        I dunno how important it really is to computer scientists though.
    - id: 1568
      author: alex
      date: "2010-11-26 07:18:02"
      content: |
        I would have said the Catalan numbers if you hadn't already.
        
        A000112 is one that's been important for me: 1, 1, 2, 5, 16, 63, 318, 2045, 16999, ...
        It tells you the number of ways of putting a partial order on a set of n (otherwise indistinguishable) things. eg: with two things you could have them being unordered, or one being above the other, making two possibilities in total.
        
        This also allows you, via Birkhoff's representation theorem, to enumerate finite distributive lattices in terms of how many (join|meet)-prime elements they have (eg: there are 16 distributive lattices that have exactly four join-prime elements).
        
        A003087: 1, 1, 2, 6, 31, 302, 5984, ...
        is the number of ways n otherwise indistinguishable things can be put into a DAG. As OEIS says, this is "also the number of equivalence classes of n x n real (0,1)-matrices with all eigenvalues positive, up to conjugation by permutations."
        
        Should every computer scientist know these? No, but at least some computer scientists should. Binary trees, posets and DAGs come up all the time and it's nice to know something about the combinatorics, even if it's only "there are a lot of them". For each of these, it's also good to know other ways that the same sequence arises - there are so many examples with the Catalan numbers in particular - because the isomorphism provides you with other possibilities for perceiving and analyzing your original objects.
---

The [On-Line Encyclopedia of Integer Sequences](http://oeis.org/Seis.html) is quite a nifty website. Suppose that you’re solving a problem, and you come up with the following sequence of integers: `0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2...` and you wonder to yourself: “huh, what’s that sequence?” Well, just [type it in](http://oeis.org/search?q=0%2C+1%2C+0%2C+2%2C+0%2C+1%2C+0%2C+3%2C+0%2C+1%2C+0%2C+2&sort=&language=english&go=Search) and the answer comes back: [A007814](http://oeis.org/A007814), along with all sorts of tasty tidbits like constructions, closed forms, mathematical properties, and more. Even simple sequences like [powers of two](http://oeis.org/A000079) have a bazillion alternate interpretations and generators.

This got me wondering: what are integer sequences that every computer scientist should know? That is, the ones they should be able to see the first few terms of and think, “Oh, I know that sequence!” and then rack their brains for a little bit trying to remember the construction, closed form or some crucial property. For example, almost anyone with basic math background will recognize the sequences [1, 2, 3, 4, 5](http://oeis.org/A000027); [0, 1, 4, 9, 16](http://oeis.org/A000290); or [1, 1, 2, 3, 5](http://oeis.org/A000045). The very first sequence I cited in this article holds a special place in my heart because I accidentally derived it while working on my article [Adventures in Three Monads](http://blog.ezyang.com/2010/01/adventures-in-three-monads/) for The Monad.Reader. Maybe a little less familiar might be [1, 1, 2, 5, 14, 42](http://oeis.org/A000108) or [3, 7, 31, 127, 8191, 131071, 524287, 2147483647](http://oeis.org/A000668), but they are still quite important for computer scientists.

So, what integer sequences every computer scientist should know? (Alternatively, what’s your favorite integer sequence?)
