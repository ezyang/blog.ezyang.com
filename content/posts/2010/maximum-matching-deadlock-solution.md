---
title: "Maximum matching deadlock solution"
date: 2010-07-16 09:00:13
slug: maximum-matching-deadlock-solution
categories: [Galois Tech Talk]
comments:
    - id: 693
      author: Phil
      date: "2010-07-16 12:04:11"
      content: "ahhh I had played with an idea like this (but hadn't gotten the crucial bit about storing the additional bit, which led me to discard the idea).. it seemed a bit like preemption, but if the node is voluntarily yielding the thread, I guess it's cooperative instead :)"
    - id: 694
      author: Phil
      date: "2010-07-16 12:05:08"
      content: "I enjoyed the puzzle btw; I know yours isn't a systems puzzle blog, but you're in my rss feeds now so I'll know about any further ones you post. thanks!"
---

[Last Monday](http://blog.ezyang.com/2010/07/graphs-not-grids/), I presented a parallel algorithm for computing maximum weighted matching, and noted that on real hardware, a naive implementation would deadlock.

Several readers correctly identified that sorting the nodes on their most weighted vertex only once was insufficient: when a node becomes paired as is removed from the pool of unpaired nodes, it could drastically affect the sort. Keeping the nodes in a priority queue was suggested as an answer, which is certainly a good answer, though not the one that Feo ended up using.

*Feo’s solution.* Assign every node an “is being processed bit.” When a node attempts to read its neighbor’s full/empty bit and finds the bit empty, check if the node is being processed. If it is not, atomically check and set the “is being processed bit” to 1 and process the node recursively. Fizzle threads that are scheduled but whose nodes are already being processed. The overhead is one bit per node.

I think this is a particularly elegant solution, because it shows how recursion lets work easily allocate itself to threads that would otherwise be idle.
