---
title: "Evaluation on the Haskell Heap"
date: 2011-04-20 16:41:00
slug: evaluation-on-the-haskell-heap
categories: [Haskell Heap]
comments:
    - id: 2095
      author: gasche
      date: "2011-04-23 13:13:56"
      content: "I'm finding your post a bit light on the technical content, but the drawings are so enjoyable that you are forgiven."
---

<div class="container center">

New to the series? Go [to the beginning.](http://blog.ezyang.com/2011/04/the-haskell-heap/)

</div>

![image](/img/heap/ghost.png)

<div class="container center">

*The Ghost of the Christmas Present*

</div>

In today’s post, we’ll do a brief survey of the various things that can happen when you open haunted presents in the Haskell heap. Asides from constants and things that have already been evaluated, mostly everything on the Haskell heap is haunted. The real question is what the ghost haunting the present does.

In the simplest case, almost nothing!

![image](/img/heap/indirection.png)

Unlike gift-cards, you have to open the next present (Haskell doesn’t let you evaluate a thunk, and then decide not to follow the indirection...)

![image](/img/heap/tail-call.png)

More commonly, the ghost was *lazy* and, when woken up, has to open other presents to figure out what was in your present in the first place!

![image](/img/heap/stack-bump.png)

Simple primitive operations need to open all of the presents involved.

![image](/img/heap/evaluation.png)

But the ghost may also open another present for no particular reason...

![image](/img/heap/seq.png)

or execute some IO...

![image](/img/heap/unsafe-perform-io.png)

Note that any presents he opens may trigger more ghosts:

![image](/img/heap/ghost-buddies.png)

Resulting in a veritable ghost jamboree, all to open one present!

![image](/img/heap/ghost-party.png)

The fact that opening a present (thunk) can cause such a cascading effect is precisely what makes the timing of lazy evaluation surprising to people who are used to all of the objects in their heap being unwrapped (evaluated) already. So the key to getting rid of this surprise is understanding when a ghost will decide it needs to unwrap a present (strictness analysis) and whether or not your presents are unwrapped already (amortized analysis).

Last time: [The Haskell Heap](http://blog.ezyang.com/2011/04/the-haskell-heap/)

Next time: [IO evaluates the Haskell Heap](http://blog.ezyang.com/2011/04/io-evaluates-the-haskell-heap/)

![image](http://i.creativecommons.org/l/by-sa/3.0/88x31.png)

This work is licensed under a [Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).
