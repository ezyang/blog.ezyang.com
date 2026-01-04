---
title: "Haskell Implementor's Workshop '14"
date: 2014-09-07 09:05:02
slug: haskell-implementors-workshop-14
categories: [GHC, Haskell]
comments:
    - id: 8215
      author: Anonymous
      date: "2014-09-26 05:13:20"
      content: "Which software did you use to create your slides (perhaps xournal or ipe?)?"
    - id: 8220
      author: Edward Z. Yang
      date: "2014-09-26 12:34:45"
      content: "Xournal, yep. http://blog.ezyang.com/2010/04/diagramming-in-xournal-and-gimp/"
    - id: 8221
      author: Anonymous
      date: "2014-09-26 13:42:14"
      content: Thanks very much.
---

This year at ICFP, we had some blockbuster attendance to the [Haskell Implementor's Workshop](http://www.haskell.org/haskellwiki/HaskellImplementorsWorkshop/2014) (at times, it was standing room only). I had the pleasure of presenting the work I had done over the summer on Backpack.

![image](/img/backpack-ufo.png)

You can [grab the slides](http://web.mit.edu/~ezyang/Public/hiw14-backpack-slides.pdf) or [view the presentation itself](https://www.youtube.com/watch?v=0dF9zuwTSTc) (thank you ICFP organizers for being incredibly on-the-ball with videos this year!) The talk intersects a little bit with my blog post [A taste of Cabalized Backpack](http://blog.ezyang.com/2014/08/a-taste-of-cabalized-backpack/), but there are more pictures, and I also emphasize (perhaps a little too much) the long term direction we are headed in.

![image](/img/backpack-schema.png)

There were a lot of really nice talks at HiW. Here are some of my personal highlights:

- [Richard Eisenberg: Dependent Haskell](https://www.youtube.com/watch?v=O805YjOsQjI)
- [Alejandro Serrano: Interactive features in ghc-mod (lightning talk)](https://www.youtube.com/watch?v=SqCZRtfmfdM) (Heavily based off of Idris-mode)
- [Lennart Augustsson: Better type-error messages (lightning talk)](https://www.youtube.com/watch?v=rdVqQUOvxSU) (Why isn’t this in GHC? The big problem is that the data type representing types is the *same* as the internal data type for types in Core. Tracking locations should be strictly at the surface syntax, so we’d have to refactor the frontend first.)
- [Michael Adams: Extending "Optimize your SYB" (lightning talk)](https://www.youtube.com/watch?v=iRq0LDFSP24&list=PL4UWOFngo5DW6nKDjK0UB5Oy9zmdWdo7K&index=22) (In the original paper, they suggest speedups can be gained by aggressively evaluating expressions of type `TypeRep`, `TyCon`, `Data` and `Typeable` at compile time. Michael was wondering if there were any other types which should receive similar treatment. One audience-member suggested `Int` (i.e. to get rid of boxing), but I don’t find that very convincing.)
