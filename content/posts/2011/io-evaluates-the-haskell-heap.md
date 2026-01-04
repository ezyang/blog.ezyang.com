---
title: "IO evaluates the Haskell Heap"
date: 2011-04-24 12:36:46
slug: io-evaluates-the-haskell-heap
categories: [Haskell Heap]
comments:
    - id: 2119
      author: Don Stewart
      date: "2011-04-24 13:04:59"
      content: "Would you consider explaining speculative thunk evaluation via `par`?"
    - id: 2120
      author: Edward Z. Yang
      date: "2011-04-24 13:21:03"
      content: "Yes, can do!"
    - id: 2130
      author: Andrew Pennebaker
      date: "2011-04-24 19:38:44"
      content: |
        That's a creative explanation of hunks. Could you include example code for evaluate, bottom, and most importantly, trace.
        
        I love Haskell's purity, but until you mentioned Debug.Trace, I had no idea how to show values without turning everything into IOs and using print statements.
    - id: 2133
      author: Edward Z. Yang
      date: "2011-04-24 20:15:10"
      content: |
        Ah yes. 'evaluate' lives in Control.Exception, 'bottom' is also known by the name 'undefined' and 'error "Foo"', and 'trace' lives in Debug.Trace.
        
        unsafePerformIO is evil, but for debugging purposes traces are really useful. For example, the GHC codebase uses them a lot. Don't be afraid of tracing your code! It's like printf debugging, but even better.
    - id: 2134
      author: Fred
      date: "2011-04-24 21:00:03"
      content: "This is great!  It did take me a while to realize that was a french ghost, however.  Excellent stuff."
---

<div class="container center">

New to the series? Go [to the beginning.](http://blog.ezyang.com/2011/04/the-haskell-heap/)

</div>

In today’s post, we focus on *you*, the unwitting person rooting around the Haskell heap to open a present. After all, presents in the Haskell heap do not spontaneously unwrap themselves.

![image](/img/heap/sleeping-heap.png)

Someone has to open the first present.

![image](/img/heap/woken-heap.png)

If the Haskell heap doesn’t interact with the outside world, no presents need to be opened: thus IO functions are the ones that will open presents. What presents they will open is not necessarily obvious for many functions, so we’ll focus on one function that makes it particularly obvious: `evaluate`. Which tells you to...

![image](/img/heap/evaluate-steps.png)

...open a present.

If you get a primitive value, you’re done. But, of course, you might get a gift card (constructor):

![image](/img/heap/evaluate-result.png)

Will you open the rest of the presents? Despite that deep, dissatisfaction inside you, the answer is no. `evaluate` only asks you to open one present. If it’s already opened, there’s nothing for you to do.

![image](/img/heap/already-evaluated.png)

> Advanced tip: If you want to evaluate more things, make a present containing a ghost who will open those things for you! A frequently used example of this when lazy IO is involved was `evaluate (length xs)`, but don’t worry too much if you don’t understand that yet: I haven’t actually said how we make presents yet!

Even though we’re only opening one present, many things can happen, as described in the last post. It could execute some IO...

![image](/img/heap/unsafe-perform-io-tired.png)

This is our direct window into evaluation as it evolves: when we run programs normally, we can’t see the presents being opened up; but if we ask the ghost to also shout out when it is disturbed, we get back this information. And in fact, this is precisely what `Debug.Trace` does!

![image](/img/heap/mr-trace.png)

There are other ways to see what evaluation is going on. A present could blow up: this is the exploding booby-trapped present, also known as “bottom”.

![image](/img/heap/booby-trapped.png)

Perhaps the explosion was caused by an `undefined` or `error "Foobar"`.

![image](/img/heap/seq-booby-trapped.png)

Boom.

------------------------------------------------------------------------

We’ll end on a practical note. As we’ve mentioned, you can only be sure that a present has been opened if you’ve explicitly asked for it to be opened from IO. Otherwise, ghosts might play tricks on you. After all, you can’t actually *see* the Haskell heap, so there’s no way to directly tell if a present has been opened or not.

![image](/img/heap/blindfolded-evaluate.png)

If you’re unsure when a thunk is being evaluated, add a trace statement to it. If ghosts are being lazy behind your back, the trace statement will never show up.

![image](/img/heap/trace-comic.png)

More frequently, however, the trace statement will show up; it’ll just be later than you expect (the ghosts may be lazy, but they’ll eventually get the job done.) So it’s useful to prematurely terminate your program or add extra print statements demarcating various stages of your program.

Last time: [Evaluation on the Haskell Heap](http://blog.ezyang.com/2011/04/evaluation-on-the-haskell-heap/)

Next time: [Implementing the Haskell Heap in Python, v1](http://blog.ezyang.com/2011/04/implementing-the-haskell-heap-in-python-v1/)

*Technical notes.* Contrary to what I’ve said earlier, there’s no theoretical reason why we couldn’t spontaneously evaluate thunks on the heap: this evaluation approach is called *speculative evaluation.* Somewhat confusingly, IO actions themselves can be thunks as well: this corresponds to passing around values of `IO a` without actually “running” them. But since I’m not here to talk about monads, I’ll simply ignore the existence of presents that contain `IO` actions—they work the same way, but you have to keep the levels of indirection straight. And finally, of course infinite loops also count as bottom, but the image of opening one present for the rest of eternity is not as flashy as an exploding present.

![image](http://i.creativecommons.org/l/by-sa/3.0/88x31.png)

This work is licensed under a [Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).
