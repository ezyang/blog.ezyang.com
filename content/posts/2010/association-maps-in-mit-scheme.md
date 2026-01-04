---
title: "Association maps in mit-scheme"
date: 2010-04-21 09:00:08
slug: association-maps-in-mit-scheme
categories: [Scheme]
comments:
    - id: 2246
      author: Robb
      date: "2011-04-29 10:48:54"
      content: "Quite enjoyed this.  Looking at the Bagwell paper.  My question is what do you mean by \"persistent\" in persistent data structures here?"
    - id: 2247
      author: Edward Z. Yang
      date: "2011-04-29 10:50:07"
      content: "Persistent means old versions of the structure continue to be accessible, as opposed to ephemeral structures whose old versions are destroyed on update."
---

I recently some did some benchmarking of persistent data structures in mit-scheme for my UROP. There were a few questions we were interested in:

1.  For what association sizes does a fancier data structure beat out your plain old association list?
2.  What is the price of persistence? That is, how many times slower are persistent data structures as compared to your plain old hash table?
3.  What is the best persistent data structure?

These are by no means authoritative results; I still need to carefully comb through the harness and code for correctness. But they already have some interesting implications, so I thought I'd share. The implementations tested are:

- *assoc*, [association lists](http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Association-Lists.html#Association-Lists)
- *hamt*, hash array mapped tries (hand-implemented, using 26-bit fixnums)
- *hash-table*, [hash tables](http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Hash-Tables.html#Hash-Tables)
- *prb-tree*, persistent red-black trees (hand-implemented)
- *wt-tree*, [weight-balanced trees](http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Weight_002dBalanced-Trees.html#Weight_002dBalanced-Trees)

All implementations use `eq?` for key comparison.

![image](/img/scheme-hamt/insert.png)

Unsurprisingly, assoc beats out everyone else, since all it has to do is a simple cons. However, there are some strange spikes at regular intervals, which I am not sure of the origin; it might be the garbage collector kicking in.

![image](/img/scheme-hamt/lookup-hit.png)

Of course, you pay back the cheap updates in assoc with a linear lookup time; the story also holds true for weight-balanced trees, which have fast inserts but the slowest lookups.

![image](/img/scheme-hamt/lookup-miss.png)

The hamt really flies when the key isn't present, even beating out hash-tables until 15 elements or so.

Source code for running the benchmarks, our home-grown implementations, and graphing can be found at the [scheme-hamt repository](http://github.com/ezyang/scheme-hamt).
