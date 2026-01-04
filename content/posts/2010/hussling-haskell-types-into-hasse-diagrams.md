---
title: "Hussling Haskell types into Hasse diagrams"
date: 2010-12-06 09:00:24
slug: hussling-haskell-types-into-hasse-diagrams
categories: [Denotational Semantics, Haskell]
comments:
    - id: 1604
      author: Anonymous
      date: "2010-12-06 09:36:54"
      content: "The ordering for lazy lists should also include _|_ : _|_, _|_ : *, _|_ : * : _|_, and other such creatures, should it not?"
    - id: 1606
      author: Felipe Lessa
      date: "2010-12-06 10:25:39"
      content: "It does include.  Look at the stars!"
    - id: 1608
      author: Max
      date: "2010-12-06 14:25:24"
      content: "Shouldn't Nat and [a] have infinite values (i.e., a top?) -- Haskell data definitions define the *greatest* fixed points of equations."
    - id: 1610
      author: Robert Massaioli
      date: "2010-12-06 16:01:58"
      content: "All I can say is that I was drawn into the article by all the nice colours and diagrams. Made it interesting; nice article, as usual I might add."
    - id: 1611
      author: Marcus
      date: "2010-12-06 16:07:08"
      content: "This brings me back to my masters thesis, about a few static analyses on C, in Haskell. Quite a few painfully TeXed has diagrams. Thanks for the trip down memory lane :)"
    - id: 1612
      author: Marcus
      date: "2010-12-06 16:07:56"
      content: "On == of, has == hasse. Damn iPad."
    - id: 1613
      author: ivan
      date: "2010-12-06 16:41:57"
      content: "Thanks. I grasped how values form a partial order. What I failed to understand is what is this denotational semantics thing :). Could you put it in a sentence please?"
    - id: 1615
      author: Edward Z. Yang
      date: "2010-12-06 17:18:07"
      content: |
        Max: You're right; in particular, 'fix S' and 'fix (★:)' should do the trick.
        
        ivan: Really, denotational semantics is just “let’s use math to describe programs.” The subtleties are in figuring out what the right maths are to use, and partial orders are one essential tool in the kit.
    - id: 1617
      author: Mathnerd314
      date: "2010-12-06 17:32:22"
      content: "How do you draw the pretty pictures?"
    - id: 1620
      author: Mathnerd314
      date: "2010-12-06 17:36:10"
      content: "Sorry, I should read your blog more. There was a post only 8 months ago..."
    - id: 1621
      author: Edward Z. Yang
      date: "2010-12-06 17:39:14"
      content: "Mathnerd314: Haha, glad you found it. :-)"
    - id: 1623
      author: Edward Z. Yang
      date: "2010-12-06 20:50:38"
      content: "Actually, Anonymous (1st comment) makes a good point; for consistency I should explicitly write out the bottoms on List."
    - id: 1624
      author: Edward Z. Yang
      date: "2010-12-06 21:49:21"
      content: "Max: I've decided I'll talk about fixpoints in another post, since they tie in nicely with functions too. :-)"
    - id: 1625
      author: notreallyahaskeller
      date: "2010-12-07 11:42:38"
      content: "I'm not too familiar with the Haskell aspects of the post, but could you tell me how your got those diagrams (a software, or a scan from a notebook?) I'd like to use something like that in my blog posts on networks - hand-drawn, but colourful, neat and legible."
    - id: 1627
      author: Edward Z. Yang
      date: "2010-12-07 20:34:06"
      content: "notreallyahaskeller: See here http://blog.ezyang.com/2010/04/diagramming-in-xournal-and-gimp/"
    - id: 1640
      author: Toby
      date: "2010-12-09 17:47:17"
      content: |
        The first word in the headline should, presumably, be "Hustling".  Or was the phonetic spelling intentionally intended to mirror "Hasse"?
        
        On the subject matter of the article, the point about bottom being part of every type is definitely something that I tend to forget in practice even though I "know" it from having read TAPL.  I think the reason for that is that we write code trying to avoid encountering bottom, and most of the time, we're pretty sure that we've succeeded.  To a programmer, rather than a type theorist, bottom seems like something that's "outside" the type, being an exceptional condition.
    - id: 1648
      author: Edward Z. Yang
      date: "2010-12-13 15:30:09"
      content: |
        Uh, let’s just say it was intentional. :-)
        
        You bring up a good point, in that most developers think strictly about their code, and exceptions and nontermination are an after thought. ("Me? Write infinite loops? Never!") Control structures are compartmentalized into the section of the language that must be built in: user-defined control structures are rarely considered. But this is precisely when we purposely introduce bottom: the road not traveled may frequently result in bottom.
        
        I kind of wonder if this sort of pedagogy is necessary in order to truly understand how a lazy language works. Many experienced Haskellers claim that they rarely are stumped by laziness related space leaks, and I’ve not been able to put my finger on what separates them from others.
    - id: 2915
      author: Rafael
      date: "2011-08-24 09:29:07"
      content: "Edward, pictures are indeed nice but... am I missing something, or you never defined what do the lines mean i.e. what is the ordering?"
    - id: 2916
      author: Edward Z. Yang
      date: "2011-08-24 09:46:19"
      content: "Yes, the vertical lines are the partial ordering, greater elements on top."
    - id: 2934
      author: Rafael
      date: "2011-08-29 14:21:15"
      content: "Yes, I inferred that \"top--bottom\" means top &gt;= bottom but what is \"&gt;=\"? E.g., in the Nat case, it seems the partial order is defined as  \"includes\" (sort of). But I don't know about the other types."
    - id: 2935
      author: Edward Z. Yang
      date: "2011-08-29 16:08:38"
      content: "You can think of the partial order as \"definedness\", where the top is most defined, and the bottom is least defined. When I have bottom, I haven't made any commitment to what the shape of the data is; when I have a list whose cdr cell is bottom, I have made a commitment to what the first element looks like, but not any beyond. And so on."
    - id: 3119
      author: Sanjoy Das
      date: "2011-11-15 02:06:42"
      content: |
        Very interesting post.  This is the kind of stuff that keeps me interested in Haskell.
        
        And I love the diagrams -- very cute.
    - id: 30820
      author: lu
      date: "2023-10-28 12:40:04"
      content: "I'm 12 years late but this is very cool, love the diagrams :P"
---

Values of Haskell types form a partial order. We can illustrate this partial order using what is called a [Hasse diagram](http://en.wikipedia.org/wiki/Hasse_diagram). These diagrams are quite good for forcing yourself to explicitly see the bottoms lurking in every type. Since my [last post about denotational semantics](http://blog.ezyang.com/2010/12/how-i-learned-to-stop-worrying-and-love-the-bottom/) failed to elicit much of a response at all, I decided that I would have better luck with some more pictures. After all, everyone loves pictures!

------------------------------------------------------------------------

We’ll start off with something simple: `()` or unit.

![image](/img/haskell-hasse/unit.png)

Immediately there are a few interesting things to notice. While we normally think of unit as only having one possible value, `()`, but in fact they have two: `()` and bottom (frequently written as `undefined` in Haskell, but `fix id` will do just as well.) We’ve omitted the arrows from the lines connecting our partial order, so take as a convention that higher up values are “greater than” their lower counterparts.

A few more of our types work similarly, for example, `Int` and `Bool` look quite similar:

![image](/img/haskell-hasse/int.png)

![image](/img/haskell-hasse/bool.png)

Note that `Int` without bottom has a total ordering independent of our formulation (the usual -3 is less than 5 affair, alluded to by the `Ord` instance for `Int`). However, *this is not the ordering you’re looking for!* In particular, it doesn’t work if bottom is the game: is two less than or greater than bottom? In this partial ordering, it is “greater”.

It is no coincidence that these diagrams look similar: their unlifted sets (that is, the types with bottom excluded) are discrete partial orders: no element is less than or greater than another.

------------------------------------------------------------------------

What happens if we introduce data types that include other data types? Here is one for the natural numbers, Peano style (a natural number is either zero or the successor of a natural number.)

![image](/img/haskell-hasse/nat.png)

We no longer have a flat diagram! If we were in a strict language, this would have collapsed back into the boring partial orders we had before, but because Haskell is lazy, inside every successor constructor is a thunk for a natural number, which could be any number of exciting things (bottom, zero, or another successor constructor.)

We’ll see a structure that looks like this again when we look at lists.

------------------------------------------------------------------------

I’d like to discuss polymorphic data types now. In [Haskell Denotational semantics wikibook](http://en.wikibooks.org/wiki/Haskell/Denotational_semantics), in order to illustrate these data types, they have to explicitly instantiate all of the types. We’ll adopt the following shorthand: where I need to show a value of some polymorphic type, I’ll draw a star instead. Furthermore, I’ll draw wedges to these values, suggestive of the fact that there may be *more than one constructor* for that type (as was the case for Int, Bool and Nat). At the end of this section I’ll show you how to fill in the type variables.

Here is Maybe:

![image](/img/haskell-hasse/maybe.png)

If Haskell allowed us to construct infinite types, we could recover Nat by defining Maybe (Maybe (Maybe ...)).

Either looks quite similar, but instead of Nothing we have Right:

![image](/img/haskell-hasse/either.png)

Is Left ⊥ greater than or less than Right () in this partial order? It’s a trick question: since they are different constructors they’re not comparable anymore.

Here’s a more interesting diagram for a 2-tuple:

![image](/img/haskell-hasse/tuple.png)

The values merge back at the very top! This is because while ((), ⊥) is incomparable to (⊥, ()), both of them are less than ((), ()) (just imagine filling in () where the ⊥ is in both cases.)

------------------------------------------------------------------------

If we admit lazy data structures, we get a lot richer space of possible values than if we’re forced to use strict data structures. If these constructors were strict, our Hasse diagrams would still be looking like the first few. In fact, we can see this explicitly in the difference between a lazy constructor and a strict constructor:

![image](/img/haskell-hasse/data.png)

![image](/img/haskell-hasse/data-strict.png)

The strict constructor squashes ⊥ and C ⊥ to be the same thing.

It may also be useful to look at newtype, which merely constructs an isomorphism between two types:

![image](/img/haskell-hasse/newtype.png)

It looks a bit like the strict constructor, but it’s actually not at all the same. More on this in the next blog post.

------------------------------------------------------------------------

How do we expand stars? Here’s a diagram showing how:

![image](/img/haskell-hasse/star.png)

Graft in the diagram for the star type (excluding bottom, since we’ve already drawn that into the diagram), and duplicate any of the incoming and outgoing arrows as necessary (thus the wedge.) This can result in an exponential explosion in the number of possible values, which is why I’ll prefer the star notation.

------------------------------------------------------------------------

And now, the *tour de force*, lazy lists:

![image](/img/haskell-hasse/list.png)

*Update.* There’s one bit of extra notation: the stars with subscript ⊥ mean that you’ll need to graft in bottom as well (thanks Anonymous for pointing this out.) Tomorrow we’ll see list expanded in its full, exponential glory.

We almost recover Nat if we set a to be `()`, but they’re not quite isomorphic: every `()` might actually be a bottom, so while `[()]` and `[⊥]` are equivalent to one, they are different. In fact, we actually want to set a to the empty type. Then we would write 5 as \[⊥, ⊥, ⊥, ⊥, ⊥\].

Next time, we’ll [draw pictures of the partial ordering of functions and illustrate monotonicity.](http://blog.ezyang.com/2010/12/gin-and-monotonic/)
