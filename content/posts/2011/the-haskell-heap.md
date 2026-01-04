---
title: "The Haskell Heap"
date: 2011-04-18 09:00:46
slug: the-haskell-heap
categories: [Haskell Heap]
comments:
    - id: 2067
      author: gasche
      date: "2011-04-18 10:47:11"
      content: "Wait, you mean opening a present is a *mutation* of the heap؟"
    - id: 2070
      author: Matthias
      date: "2011-04-18 11:06:06"
      content: |
        &gt; Wait, you mean opening a present is a *mutation* of the heap
        
        Yes, it is.  Haskell does lots of mutation behind the scenes.
        
        Okasaki said something like "Lazyness can be seen as a very disciplined version of mutation."
    - id: 2071
      author: fuzxxl
      date: "2011-04-18 12:12:19"
      content: |
        And... what is an MVar or a mutable array in your concept of presents?
        
        Keep on! This is the best introduction to thunks I've ever seen.
    - id: 2073
      author: rian
      date: "2011-04-18 15:04:00"
      content: "this is amazing. i definitely feel like there is a dearth of information about and attention to the time and space behavior of haskell programs. this is on purpose of course but i worry about using haskell for my microcontroller, i never know if that one badly programmed evaluation will start crunching and stalling."
    - id: 2075
      author: Mitch S
      date: "2011-04-18 17:14:47"
      content: |
        Great metaphor!  Love the bomb present.  The ghost drawing cracks me up.
        
        Hooray for engaging the monkey-brain to understand abstract stuff!
    - id: 2084
      author: Edward Z. Yang
      date: "2011-04-19 17:44:40"
      content: "fuzxxl: Mutation won’t be treated in this series. Fortunately, there’s already a lot of good work about mutation, so I think people can use their existing intuition to figure this out."
    - id: 2225
      author: Schrödinger
      date: "2011-04-28 10:08:27"
      content: "In reality a thunk is a box for Schrödinger's cat. As long as you do not look into the box, the content is in a meta-state. Only if you open the box in order to observe the content, then its content turns into a final state."
    - id: 3622
      author: Ywen
      date: "2012-04-04 09:33:48"
      content: "Unboxed types are stored on the stack, right?"
    - id: 3624
      author: Edward Z. Yang
      date: "2012-04-04 12:38:19"
      content: Yep.
    - id: 6426
      author: "Some Lazy Fun with Streams - Alex Bowe"
      date: "2014-03-05 00:50:15"
      content: "[&#8230;] in the context of formal languages is that evaluation is postponed until absolutely necessary (Here is a cute (illustrated) blog post describing this lazy evaluation stuff). Take this code for [&#8230;]"
    - id: 22162
      author: "Чтение на выходных: 17 независимых блогов по математике, алгоритмам и языкам программирования &#8212; APSYHEA.TK"
      date: "2017-12-04 13:47:36"
      content: "[&#8230;] блоггер интересуется Haskell (например, в блоге есть иллюстрированное введение в работу на [&#8230;]"
    - id: 22404
      author: Manuel
      date: "2018-06-26 09:47:07"
      content: "You should really provide an index for the series, I entered the series in a post that linked to the beginning, but what's next?"
---

![image](/img/heap/title.png)

The Haskell heap is a rather strange place. It’s not like the heap of a traditional, strictly evaluated language...

![image](/img/heap/traditional-heap.png)

...which contains a lot of junk! (Plain old data.)

In the Haskell heap, every item is wrapped up nicely in a box: the Haskell heap is a heap of *presents* (thunks).

![image](/img/heap/present.png)

When you actually want what’s inside the present, you *open it up* (evaluate it).

![image](/img/heap/evaluated.png)

Presents tend to have names, and sometimes when you open a present, you get a *gift card* (data constructor). Gift cards have two traits: they have a name (the `Just` gift card or the `Right` gift card), and they tell you where the rest of your presents are. There might be more than one (the tuple gift card), if you’re a lucky duck!

![image](/img/heap/constructor.png)

But just as gift cards can lie around unused (that’s how the gift card companies make money!), you don’t have to redeem those presents.

Presents on the Haskell heap are rather mischievous. Some presents explode when you open them, others are haunted by ghosts that open other presents when disturbed.

![image](/img/heap/tricksters.png)

Understanding what happens when you open a present is key to understanding the time and space behavior of Haskell programs.

![image](/img/heap/evaluate.png)

In this series, Edward makes a foray into the webcomic world in order to illustrate the key operational concepts of evaluation in a lazily evaluated language. I hope you enjoy it!

Next time: [Evaluation on the Haskell Heap](http://blog.ezyang.com/2011/04/evaluation-on-the-haskell-heap/)

> *Technical notes.* Technically speaking, this series should be “The GHC Heap.” However, I’ll try to avoid as many GHC-isms as possible, and simply offer a metaphor for operationally reasoning about any kind of lazy language. Originally, the series was titled “Bomberman Teaches Lazy Evaluation,” but while I’ve preserved the bomb metaphor for thunks that error or don’t terminate, I like the present metaphor better: it in particular captures several critical aspects of laziness: it captures the evaluated/non-evaluated distinction and the fact that once a present is opened, it’s opened for everyone. The use of the term “boxed” is a little suggestive: indeed, boxed or *lifted* values in GHC are precisely the ones that can be nonterminating, whereas unboxed values are more akin to what you’d see in C’s heap. However, languages like Java also use the term boxed to refer to primitive values that look like objects. For clarity’s sake, we won’t be using the term boxed from now on (indeed, we won’t mention unboxed types).

![image](http://i.creativecommons.org/l/by-sa/3.0/88x31.png)

This work is licensed under a [Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).
