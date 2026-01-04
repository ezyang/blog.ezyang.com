---
title: "Tomatoes are a subtype of vegetables"
date: 2014-11-14 21:00:36
slug: tomatoes-are-a-subtype-of-vegetables
categories: [Computer Science]
comments:
    - id: 10233
      author: ddiazc
      date: "2014-11-14 21:57:12"
      content: |
        What a coincidence, I too wrote today about the composition of contravariant functors: http://productivedetour.blogspot.com.es/2014/11/pawn-shops-refrigerators-and.html
        
        But I like your analogy better (and it has prettier pictures!)
    - id: 10234
      author: Ashley Yakeley
      date: "2014-11-14 22:35:34"
      content: |
        I notice that in English, the word "of" indicates covariance, while the word "for" indicates contravariance. For example, "Soup" is covariant: tomato soup is soup <em>of</em> tomatoes. "Recipe" is contravariant: tomato soup recipe is recipe <em>for</em> tomato soup.
        
        Also I've come to the conclusion that "subtype" means exactly that there is an injective map in all possible worlds. Obviously subtypes imply injective maps, but surprisingly the converse seems to be true too.
    - id: 10235
      author: UberAlex
      date: "2014-11-15 05:18:14"
      content: "It's interesting that you chose tomatoes for this example. There is a well-known quote that \"\"Knowledge is knowing that a tomato is a fruit. Wisdom is knowing that a tomato doesn't belong in a fruit salad.\" (http://en.wikipedia.org/wiki/Miles_Kington)"
    - id: 10236
      author: Pete
      date: "2014-11-15 06:37:37"
      content: "The eggplant and tomato are both fruit?"
    - id: 10237
      author: Yawar
      date: "2014-11-15 17:23:30"
      content: |
        Very cool. This feels like something out of _Learn You a Haskell._ I have one confusion: you say
        
            ... vegetable soup recipes are a subtype of tomato soup recipes.
        
        But you also say
        
            (Tomato -&gt; Soup) -&gt; Soup is a subtype of (Vegetable -&gt; Soup) -&gt; Soup.
        
        And I can't seem to reconcile the former with the latter; specifically, if tomatoes are a subtype of vegetables, which seems to be implied by the latter, then how can vegetable soup recipes be a subtype of tomato soup recipes?
        
        Maybe I'm missing a point here....
    - id: 10238
      author: Edward Z. Yang
      date: "2014-11-15 17:26:04"
      content: "Yawar: Tomato -> Soup is a tomato soup recipe; (Tomato -> Soup) -> Soup is Alice calling you because she has tomatoes and she needs a soup recipe. The inversion of control causes the subtyping relationship to flip."
    - id: 10239
      author: Yawar
      date: "2014-11-15 17:52:08"
      content: |
        So if I'm understanding right,
        
        Tomato  Soup  Soup
        
        (Tomato -&gt; Soup) -&gt; Soup  Soup) -&gt; Soup
        
        So every time we add an arrow to take our type to some other type (invert control), the subtyping relationship flips around?
    - id: 10240
      author: Yawar
      date: "2014-11-15 17:56:16"
      content: "Argh. Looks like some HTML sanitiser ate my angle brackets. I've posted this as a question on Reddit: http://www.reddit.com/r/haskell/comments/2meyxf/tomatoes_are_a_subtype_of_vegetables/cm3kn2b"
    - id: 10242
      author: Derek Elkins
      date: "2014-11-15 23:03:04"
      content: |
        I feel that this provides an example, but it doesn't (explicitly) point out any intuition, though the intuitions are "hidden in plain sight."  The key intuition, that certainly can be faster than formal reasoning (though that's pretty quick and easy too) is whether you have/can provide a value of some type, or whether you need/must receive a value of some type.   The former behaves covariantly, the latter contravariantly.  (More correctly, would be to say the former precludes contravariant behavior, and the latter precludes covariant behavior.)  This intuition is useful for understanding polymorphic, and particularly higher rank types too.  This is no accident as polymorphic instantiation induces subtyping relationships, where the polymorphic type is a subtype of its instances.
        
        If you have a Tomato, you have a Vegetable.  If you need a Tomato, you don't need an arbitrary Vegetable but a particular one, namely a Tomato.  For Alice to make use of a recipe turning Tomatoes into Soup, she'd actually have to have some Tomatoes.
    - id: 10258
      author: Barend Venter
      date: "2014-11-16 16:03:38"
      content: "Although I think this is an exemplary piece of writing, it might be worth it to make a version that omits or explains the Haskell style notation, as this example is going to be useful for people coming from C# and other systems that exhibit this kind of contravariance wrt subtyping relations."
    - id: 10449
      author: TuukkaH
      date: "2014-11-23 08:51:55"
      content: "You refer to \"the rules\" both in the beginning and in the end. I think a link to these rules would be beneficial to readers. Also, a summary table of the three cases would be nice in the end (to see how the relationship flips around twice)."
    - id: 10690
      author: Paolo G. Giarrusso
      date: "2014-12-05 17:50:18"
      content: |
        &gt; Also I’ve come to the conclusion that “subtype” means exactly that there is an injective map in all possible worlds. Obviously subtypes imply injective maps, but surprisingly the converse seems to be true too.
        
        1. Technically, injections for sum types also create injective maps (and I guess in all possible worlds).
        2. However, your intuition is correct for "subsets": when some mathematicians generalize "A is a subset of B" in a certain sense, they come up with something like "there is an injective function between A and B". But since you talk about generic map this means that lambda x: [-1 .. 1]. -x : real is also an injection map (which negates x while upcasting it). However, this is isomorphic to the usual lambda x: [-1 .. 1]. x : real (the two directions of the isomorphism are both lambda x: [-1 .. 1]. -x : [-1 .. 1]) under the appropriate notion of isomorphism.
        
        More technically and concisely, in category theory a subobject of B is a monic arrow between an object A and B (where monic arrows generalize injective functions), or an "equivalence class" of such arrows with the appropriate notion of isomorphism. See Wikipedia. https://en.wikipedia.org/wiki/Subobject
    - id: 10886
      author: Andrew M. Farrell
      date: "2014-12-09 20:33:38"
      content: "We know that in the domains of cooking and taxes (see Nix v. Hedden), tomatoes are a subtype of vegetables but in the domain of botany, tomatoes are not a subtype of vegetables. Out of curiosity, what is the right word for \"domain\" here?"
    - id: 12130
      author: Lindsey Kuper
      date: "2015-01-16 04:14:35"
      content: "This is a great explanation!  jcreed also used a food metaphor to explain contravariance: http://jcreed.livejournal.com/1611909.html"
    - id: 12566
      author: Danno
      date: "2015-02-04 00:32:01"
      content: |
        I wonder if calling them subtypes and implying a linear order (just because sub would seem to imply that) is part of the reason this is unintuitive. 
        
        Is there another way of describing this concept in English that *just* works?
        
        b &lt;: a
        
        (a -> r) &lt;: (b -> r)
        
        (b -> r) -> r &lt;: (a ->r ) -> r
        
        Hmm... what about saying: b is an example of a? Yeah. That works. a -&gt; r is an example of b -&gt; r, (b -&gt; r) -&gt; r is an example of (a -&gt; r) -&gt; r. I kinda like that!
        
        Does saying "example" instead of "subtype" make sense in all cases?
        
        I think Ashley might be on to something too!
        
        (<strong>Editor:</strong> Updated formatting)
    - id: 12567
      author: Danno
      date: "2015-02-04 00:33:34"
      content: "d'oh, my subtype notation got eaten.  I had written out the relationships between the three algebraic examples provided. b sub a, (a -&gt; r) sub (b -&gt; r), and (b -&gt; r) -&gt; r  sub (a -&gt;r ) -&gt; r"
---

![image](/img/vegetables/st1.png)

Subtyping is one of those concepts that seems to makes sense when you first learn it (“Sure, convertibles are a subtype of vehicles, because all convertibles are vehicles but not all vehicles are convertibles”) but can quickly become confusing when function types are thrown into the mix. For example, if `a` is a subtype of `b`, is `(a -> r) -> r` a subtype of `(b -> r) -> r`? (If you know the answer to this question, this blog post is not for you!) When we asked our students this question, invariably some were lead astray. True, you can mechanically work it out using the rules, but what’s the intuition?

Maybe this example will help. Let `a` be tomatoes, and `b` be vegetables. `a` is a subtype of `b` if we can use an `a` in any context where we were expecting a `b`: since tomatoes are (culinary) vegetables, tomatoes are a subtype of vegetables.

What about `a -> r`? Let `r` be soup: then we can think of `Tomato -> Soup` as recipes for tomato soup (taking tomatoes and turning them into soup) and `Vegetable -> Soup` as recipes for vegetable soup (taking vegetables—any kind of vegetable—and turning them into soup). As a simplifying assumption, let's assume all we care about the result is that it’s soup, and not what type of soup it is.

![image](/img/vegetables/recipes.png)

What is the subtype relationship between these two types of recipes? A vegetable soup recipe is more flexible: you can use it as a recipe to make soup from tomatoes, since tomatoes are just vegetables. But you can’t use a tomato soup recipe on an eggplant. Thus, vegetable soup recipes are a subtype of tomato soup recipes.

![image](/img/vegetables/st2.png)

![image](/img/vegetables/st2a.png)

This brings us to the final type: `(a -> r) -> r`. What is `(Vegetable -> Soup) -> Soup`? Well, imagine the following situation...

------------------------------------------------------------------------

One night, Bob calls you up on the phone. He says, “Hey, I’ve got some vegetables left in the fridge, and I know your Dad was a genius when it came to inventing recipes. Do you know if he had a good soup recipe?”

![image](/img/vegetables/phone.png)

“I don’t know...” you say slowly, “What kind of vegetables?”

“Oh, it’s just vegetables. Look, I’ll pay you back with some soup, just come over with the recipe!” You hear a click on the receiver.

You pore over your Dad’s cookbook and find a tomato soup recipe. Argh! You can’t bring this recipe, because Bob might not actually have tomatoes. As if on cue, the phone rings again. Alice is on the line: “The beef casserole recipe was lovely; I’ve got some tomatoes and was thinking of making some soup with them, do you have a recipe for that too?” Apparently, this happens to you a lot.

“In fact I do!” you turn back to your cookbook, but to your astonishment, you can’t find your tomato soup recipe any more. But you do find a vegetable soup recipe. “Will a vegetable soup recipe work?”

“Sure—I’m not a botanist: to me, tomatoes are vegetables too. Thanks a lot!”

You feel relieved too, because you now have a recipe for Bob as well.

------------------------------------------------------------------------

Bob is a person who takes vegetable soup recipes and turns them into soup: he’s `(Vegetable -> Soup) -> Soup`. Alice, on the other hand, is a person who takes tomato soup recipes and turns them into soup: she’s `(Tomato -> Soup) -> Soup`. You could give Alice either a tomato soup recipe or a vegetable soup recipe, since you knew she had tomatoes, but Bob’s vague description of the ingredients he had on hand meant you could only bring a recipe that worked on all vegetables. Callers like Alice are easier to accommodate: `(Tomato -> Soup) -> Soup` is a subtype of `(Vegetable -> Soup) -> Soup`.

![image](/img/vegetables/st3.png)

In practice, it is probably faster to formally reason out the subtyping relationship than it is to *intuit* it out; however, hopefully this scenario has painted a picture of *why* the rules look the way they do.
