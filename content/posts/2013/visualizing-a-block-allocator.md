---
title: "Visualizing a block allocator"
date: 2013-10-30 21:48:02
slug: visualizing-a-block-allocator
categories: [GHC]
comments:
    - id: 6281
      author: Steve M
      date: "2013-10-31 06:12:20"
      content: The text in the pdf is garbled on my iOS 7 platform.
    - id: 6282
      author: Angel Alvarez
      date: "2013-10-31 07:26:55"
      content: |
        It also seems to have trouble displaying in Mac OS…
        
        Even that, thats a good article, thanks so much
    - id: 6283
      author: Edward Z. Yang
      date: "2013-10-31 13:44:36"
      content: "Dammit, I even tried to pick a universal font. I'll try to fix it."
    - id: 6286
      author: Edward Z. Yang
      date: "2013-10-31 17:16:54"
      content: "OK, apparently this bug doesn't show up if I use a Type 1 font. So reuploaded, with a different font."
    - id: 6287
      author: Anonymous
      date: "2013-11-04 15:01:47"
      content: "One question: is there a significance to the 2D shape of the diagrams? I.e., is the problem to allocate 2D squares of memory, or are we ultimately allocating contiguous sections? Thanks."
    - id: 6288
      author: Edward Z. Yang
      date: "2013-11-04 16:15:36"
      content: "No, the memory is contiguous. I decided to use squares, because it makes it easier to show the fractal nature of the block descriptor table and the blocks themselves."
---

GHC’s [block allocator](http://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/BlockAlloc) is a pretty nifty piece of low-level infrastructure. It offers a much more flexible way of managing a heap, rather than trying to jam it all in one contiguous block of memory, and is probably something that should be of general interest to anyone who is implementing low-level code like a runtime. The core idea behind it is quite old (BIBOP: Big Bag of Pages), and is useful for any situation where you have a number of objects that are tagged with the same descriptor, and you don’t want to pay the cost of the tag on each object.

Managing objects larger than pages is a bit tricky, however, and so I wrote a document visualizing the situation to help explain it to myself. I figured it might be of general interest, so you can get it here: <http://web.mit.edu/~ezyang/Public/blocks.pdf>

Some day I’ll convert it into wikiable form, but I don’t feel like Gimp'ing the images today...
