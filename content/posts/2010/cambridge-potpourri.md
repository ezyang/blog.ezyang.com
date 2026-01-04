---
title: "Cambridge potpourri"
date: 2010-11-19 09:00:39
slug: cambridge-potpourri
categories: [Personal]
comments:
    - id: 1529
      author: treblih
      date: "2010-11-19 09:37:15"
      content: "last boy is you?"
    - id: 1531
      author: gasche
      date: "2010-11-19 12:13:58"
      content: |
        Church encoding represents all data as they destructor (or catamorphism). maybe, foldr and such transform a data into its destructor.
        
        This relation is helpful when you're unsure what the correct form for a fold is, for a more exotic data structure you just defined. You just have to copy the algebraic constructors, with the data type turned into a generic result type. For example, (Nil : list a) | (Cons : a -&gt; list a -&gt; list a) gives (forall b . b -&gt; (a -&gt; b -&gt; b) -&gt; b).
    - id: 1534
      author: Edward Z. Yang
      date: "2010-11-19 17:08:18"
      content: |
        treblih: Yeah, that’s me.
        
        gasche: Yup! But I mention currying because it means we can immediately convert something from a data structure to a function and then pass around the latter instead. Though I can't think of any good reason why you'd want to do that. :-)
    - id: 1541
      author: Brent Yorgey
      date: "2010-11-21 11:50:01"
      content: "Nice pictures.  I rode on that cycle path every day this summer. =)  And actually, the market has tons of local produce during the summer, but I guess not so much at this time of year."
    - id: 1542
      author: Edward Z. Yang
      date: "2010-11-21 11:59:00"
      content: "Brent: Awesome. It's quite a nice cycle path... once you get up the hill that it's on. :-) The comment about summer makes sense: I guess I've been spoiled by summer farmer's markets :-)"
---

<div class="container center">

*In which Edward tells some stories about Cambridge and posts lots of pictures.*

</div>

Apparently, [Alyssa B. Hacker](http://projects.csail.mit.edu/gsb/archives/gsb-msg00111.html) (sic) went on the Cambridge-MIT exchange.

![image](/img/cambridge/alyssa-b-hacker.jpeg)

------------------------------------------------------------------------

This is perhaps a phenomenon of having been around MIT for too long, a campus which has a reputation for not being too picturesque (I can probably count the actually pretty spots on campus with one hand), but it’s real easy to stop appreciating how nice the buildings and architecture are around here. Look!

![image](/img/cambridge/pretty-buildings.jpeg)

![image](/img/cambridge/pretty-buildings-2.jpeg)

![image](/img/cambridge/pretty-buildings3.jpeg)

Granted, I don’t live in any of these places; I just pass by them on my way to lecture in central Cambridge. My college, Fitzwilliam, is not so pretty from the outside (pictures omitted), but absolutely gorgeous inside:

![image](/img/cambridge/morning-in-college.jpeg)

Erm, never mind the bags of shredded paper awaiting collection.

![image](/img/cambridge/tree.jpeg)

A gorgeously illustrated map of Fitzwilliam college (sorry about the glare):

![image](/img/cambridge/fitz-map.jpeg)

If you squint, you can make out a location on the map called *The Grove*. There is actually a quite story behind this part of the college: it was actually owned by the Darwin family (as in, *the* Charles Darwin), and the college was built around the grove, which was not part of the college until Emma Darwin died and the house became incorporated as part of the college.

------------------------------------------------------------------------

![image](/img/cambridge/hoare.jpeg)

It is so tremendously easy to wander into a room and see someone famous. Like Tony Hoare, inventor of Quicksort and Hoare logic. I told people I came to Cambridge University in part to get a taste of its theoretical flavour, and I have not been disappointed. I tremendously enjoyed [Marcelo Fiore’s](http://www.cl.cam.ac.uk/~mpf23/) lectures on denotational semantics (maybe I’ll get a blog post or two out of what I learned in the class). Other lectures have been a somewhat mixed bag (but then again, when are they not?), but they’ve managed to fill out areas of my education that I didn’t know I didn’t know about. The section about skolemization from my Logic and Proof course reminds me of [this blog post](http://genericlanguage.wordpress.com/2010/08/23/who-ordered-skolem-constants/) from a while back, bemoaning the fact that no one actually tells you that Skolem constants is how you actually implement a typechecker. Well, *apparently*, skolemization is a classic technique in the logic world, and acts precisely the way you might expect it to with a type system (after all, Curry-Howard isomorphism).

Also, our logic and proof supervisor gives us tea. `:-)` His room at Trinity college (where we hold our supervisions) is the same room that the brilliant mathematician Ramanujan stayed in while he was a fellow at Trinity. Speaking of which, it’s the [hundredth anniversary of Russell and Whitehead’s Principia](http://www.srcf.ucam.org/principia/).

I’ve utterly failed (thus far) at actually doing any research, but I’m enjoying the theoretical talks and soaking in all of this foundational knowledge.

![image](/img/cambridge/computer-lab.jpeg)

Speaking of which, the computer lab is *not* in classical style. I guess you’d have a hard time convincing architects these days of constructing old-style buildings. You’ll find oddities like libraries on stilts inside hardcore traditional colleges:

![image](/img/cambridge/library-on-stilts.jpeg)

Ah, the march of modernity. If I wasn’t taking History and Philosophy of Science, I might have been stuck in the modern buildings of West Cambridge, but fortunately there is a wonderful little bike path between the lab and Fitzwiliam college (which Nidhi mentioned to me, and which I managed to miss and end up biking down the grass at the back of Churchill college and then along side the road in the wrong direction because of a fence blocking my way):

![image](/img/cambridge/cycle-path-to-storeys.jpeg)

![image](/img/cambridge/cycle-path.jpeg)

------------------------------------------------------------------------

The food might be bad in Britain, but you really can’t beat its sweets:

![image](/img/cambridge/olde-sweet-shoppe.jpeg)

Alas, I have banned myself from purchasing any sweets, lest they mysteriously disappear before I return to my college. Nutella for breakfast has also proved a stunningly bad idea and I have canned the practice in favor for a pastry and two pieces of fruit. Cambridge has a market square which is open every day (good!) but does not have very much local produce (understandable but boo!)

![image](/img/cambridge/market-square.jpeg)

------------------------------------------------------------------------

Actually, I discovered the library on stilts because I was attempting to determine the location of an ICUSU event and, unlike at MIT, I don’t carry around my laptop everywhere with me while in Cambridge (gasp!) I eventually found the location:

![image](/img/cambridge/punting-entrance.jpeg)

If you say you are *punting*, it means a quite different thing in Cambridge then at MIT. Though I [consulted Wikipedia](http://en.wikipedia.org/wiki/Punt_(boat)), which agrees with the Cambridge sense of the term. Oh, did I mention, dogs punt too!

![image](/img/cambridge/punting-doggy.jpeg)

Punting is a bit different from sailing; in particular, you push the punt pole in the direction you want to go (whereas the tiller goes in the opposite direction you want to go.)

![image](/img/cambridge/punting-me.jpg)

------------------------------------------------------------------------

This post is getting a bit long, so maybe I’ll save more for later.

*Postscript.* I noticed a (not particularly insightful) relationship between currying and church encoding today: namely, if you curry the destructor function (`either`, `maybe`, `foldr`, etc) with the data, you get the function with the same type as the Church encoding of that data type.
