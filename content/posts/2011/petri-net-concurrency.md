---
title: "Petri net concurrency"
date: 2011-03-04 09:00:50
slug: petri-net-concurrency
categories: [Concurrency, Haskell]
comments:
    - id: 1858
      author: Thomas
      date: "2011-03-05 09:39:31"
      content: "There are variants of Petri nets (note the capital P because it's a name) in which tokens have some kind of identity (coloured PN) so that you could talk about data flow."
    - id: 1860
      author: Anonymous
      date: "2011-03-06 09:00:22"
      content: |
        Interesting fact : Petri nets can be encoded by linear logic. For example, <a>this</a> is, in linear logic :
        
          P1 ⊸ (P2 ⊗ P3)
          (P2 ⊗ P3) ⊸ (P4 ⊗ P1)
    - id: 1861
      author: Anonymous
      date: "2011-03-06 09:01:36"
      content: "Oops, I wanted \"this\" to link to http://upload.wikimedia.org/wikipedia/commons/f/fe/Detailed_petri_net.png"
---

A [petri net](http://en.wikipedia.org/wiki/Petri_net) is a curious little graphical modeling language for control flow in concurrency. They came up in this talk a few weeks ago: [Petri-nets as an Intermediate Representation for Heterogeneous Architectures](http://talks.cam.ac.uk/talk/index/29894), but what I found interesting was how I could describe some common concurrency structures using this modeling language.

Here is, for example, the well venerated lock:

![image](/img/petri/lock.png)

The way to interpret the graph is thus: each circle is a “petri dish” (place) that may contain some number of tokens. The square boxes (transitions) are actions that would like to fire, but in order to do so all of the petri dishes feeding into them must have tokens. It’s the sort of representation that you could make into a board game of sorts!

If multiple transitions can fire off, we pick one of them and only that one succeeds; the ability for a token to flow down one or another arrow encodes nondeterminism in this model. In the lock diagram, only one branch can grab the lock token in the middle, but they return it once they exit the critical area (unlock).

Here is a semaphore:

![image](/img/petri/semaphore.png)

It’s exactly the same, except that the middle place may contain more than one token. Of course, no one said that separate processes must wait before signalling. We can implement a simple producer-consumer chain like this:

![image](/img/petri/produce-consume.png)

Note that petri net places are analogous to `MVar ()`, though it takes a little care to ensure we are not manufacturing tokens out of thin air in Haskell, due to the lack of linear types. You may also notice that petri nets say little about *data flow*; we can imagine the tokens as data, but the formalism doesn’t say much about what the tokens actually represent.
