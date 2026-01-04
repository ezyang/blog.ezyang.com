---
title: "Cost semantics for STG in modern GHC"
date: 2013-09-07 16:54:51
slug: cost-semantics-for-stg-in-modern-ghc
categories: [GHC]
comments:
    - id: 6216
      author: Luca Bruno
      date: "2013-09-08 19:29:07"
      content: |
        I was lately reading about STG and turned out that some of the papers are out of date. This is certainly not that problematic, ghc code is clean and papers very helpful, still updating them is a nice thing do.
        I'm trying to get involved in ghc/ghcjs and that helps me a lot, thanks.
    - id: 6217
      author: Luca Bruno
      date: "2013-09-08 19:33:38"
      content: "Also, what's the syntax at page 4? It's something logic but it's very complex to read."
    - id: 6218
      author: Edward Z. Yang
      date: "2013-09-08 22:05:57"
      content: |
        Luca: Great! Hope they are of use.
        
        Re the syntax on page 4, those are the cost semantics. They are big(-ish) step operational semantics with some extra bits (namely the costs of operations). So if you've never looked at a big-step operational semantics before, it might be a little difficult to understand, since this particular one is a little hairy. If you have any specific questions I can probably answer them, but a full answer would probably take another blog post...
    - id: 6219
      author: Luca Bruno
      date: "2013-09-09 03:42:30"
      content: "Ok thanks, couldn't find a name for that syntax :-) It would be helpful to tell that in the paper. Now reading what it operational semantics is, do you have any suggested paper about big-step operational semantics at hand?"
    - id: 6220
      author: Edward Z. Yang
      date: "2013-09-09 04:32:15"
      content: "I'm not really sure how I picked it up (I guess by osmosis). Essentially, it says \"this expression in this environments evaluates to this other expression in some new environment\" + some side conditions (i.e. there is something particular in the heap, or some other evaluation goes. If you stare at it a while it should make some sense."
    - id: 6221
      author: Luca Bruno
      date: "2013-09-09 04:49:12"
      content: "The problem is with the symbols, have to find some paper describing them :-)"
    - id: 6222
      author: Edward Z. Yang
      date: "2013-09-09 05:06:13"
      content: |
        Well, they vary from presentation to presentation, so you're unlikely to find one which exactly matches the presentation here :)
        
        But point taken, I will add some more explanatory text for the notational conventions here.
---

One of the problems with academic publishing is that it’s hard to keep old papers up-to-date. This is the certainly case for this [1995 Sansom paper on profiling non-strict, higher-order functional languages](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.43.6277). While the basic ideas of the paper still hold, the actual implementation of cost centers in GHC has changed quite a bit, perhaps the most dramatic change being the introduction of cost center stacks. So while the old paper is good for giving you the basic idea of how profiling in GHC works, if you really want to know the details, the paper offers little guidance.

So what do you do when your cost semantics are out-of-date? Why, update them of course! I present an [updated cost-semantics for STG in modern GHC (PDF)](https://github.com/ezyang/stg-spec/raw/master/stg-spec.pdf) ([GitHub](https://github.com/ezyang/stg-spec)). Eventually these will go in the GHC repository proper, alongside [core-spec](http://typesandkinds.wordpress.com/2012/12/03/a-formalization-of-ghcs-core-language/) which is a similar document for Core. However, I haven't done any proofs with these semantics yet, so they are probably a little buggy.

Despite the lack of proofs, the formalization has been helpful already: I’ve already spotted one bug in the current implementation (remarked upon in the document). I’ve also identified a potential refactoring based on the way the rules are currently setup. Please let me know about any other bugs you find!
