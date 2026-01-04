---
title: "You could have invented fractional cascading"
date: 2012-03-05 01:30:22
slug: you-could-have-invented-fractional-cascading
categories: [Computer Science]
math: true
comments:
    - id: 3494
      author: dankane
      date: "2012-03-05 02:07:06"
      content: "How is this any better than just merging all of the arrays and for each element of the big thing having a lookup table for the k answers?  This seems like it would take basically the same amount of space and time (perhaps up to constants), require only log(nk) queries rather than log(n) + k queries, and be easier to implement."
    - id: 3495
      author: Edward Z. Yang
      date: "2012-03-05 02:22:42"
      content: "That requires an extra O(nk^2) space for the lookup tables, if I understand correctly."
    - id: 3499
      author: Andrew Pennebaker
      date: "2012-03-05 13:12:12"
      content: "If you have k sorted arrays, you could pretend they're working arrays from a mergesort call upstream. Merging them together is O(kn), then binary searching the merged arrays is O(ln kn)."
    - id: 3500
      author: Edward Z. Yang
      date: "2012-03-05 13:14:03"
      content: "Yes, but one crucial difference is that you need to turn that single search into k elements, which as remarked above may require you to store lots of extra info!"
    - id: 3501
      author: dankane
      date: "2012-03-05 13:41:21"
      content: "OK.  Fine.  I can save on space by storing my lookup table as an array of pointers into my original array.  Then I get by with O(nk log(nk)) space which is comparable unless k &gt;&gt; n."
    - id: 3502
      author: dankane
      date: "2012-03-05 13:42:14"
      content: Wait.  Nevermind.
    - id: 3503
      author: Kannan
      date: "2012-03-05 22:44:59"
      content: |
        What about storing one big array of (key,value) pairs sorted by the key, including duplicate keys.  Once you binary search and find a matching key, just slide left/right to find all the other matching elements.
        
        Sorry to bring this up again, but I still don't understand why the single array technique doesn't work as well (for the first use case, at least).
    - id: 3504
      author: Edward Z. Yang
      date: "2012-03-05 23:26:42"
      content: "Kannan, the \"slides\" can take up to O(kn) time; just consider the case where you have arrays [1, 1, 1...], [2, 2, 2...], [3, 3, 3...], ..."
    - id: 3505
      author: Kannan
      date: "2012-03-05 23:40:21"
      content: "In your example, what's the exact result if we're trying to search for the element \"2\"?"
    - id: 3506
      author: Edward Z. Yang
      date: "2012-03-05 23:47:23"
      content: "In the first list, the predecessor of 2 is 1, so we return 1. The second list is as you might expect. For all the other lists, we say no predecessor exists."
    - id: 3507
      author: Kannan
      date: "2012-03-06 01:06:06"
      content: "Ah.  I understand now.  I forgot about having to return the closest predecessor element.  Thanks!"
    - id: 3508
      author: Dave
      date: "2012-03-06 02:11:18"
      content: "Just read the comments.  I think the author is wrong--you couldn't have invented fractional cascading."
    - id: 3509
      author: Edward Z. Yang
      date: "2012-03-06 08:03:04"
      content: "Certainly it would have had to have been a good day!"
    - id: 3511
      author: Dhruv
      date: "2012-03-06 11:56:39"
      content: "Any idea how I can update (edit, insert, delete) values to/from the individual arrays in &lt; O(n) time?"
    - id: 3515
      author: Dhruv
      date: "2012-03-07 00:05:37"
      content: |
        It would be super if you could post some problems (puzzles??) that require fractional cascading to solve them since an application of the technique would really nail the idea.
        
        Thanks again!
    - id: 3516
      author: Edward Z. Yang
      date: "2012-03-07 00:08:18"
      content: |
        Dhruv: This might be a reasonable starting point for dynamic updates: http://en.wikipedia.org/wiki/Fractional_cascading#Dynamic_fractional_cascading (you should also chase up the original paper).
        
        As for application, I highly recommend you try out the first exercise. Check Demaine's notes if you get stuck.
    - id: 3522
      author: Dhruv
      date: "2012-03-07 09:49:22"
      content: |
        &gt; As for application, I highly recommend you try out the first exercise. Check Demaine’s notes if you get stuck.
        
        Thanks Edward! That's a great idea!
    - id: 3530
      author: Adito
      date: "2012-03-07 19:56:51"
      content: |
        I like your explanation.
        
        Especially the using of the image.
        Are you draw all of this manually?
    - id: 3533
      author: Edward Z. Yang
      date: "2012-03-07 23:47:05"
      content: "I use Xournal. See also http://blog.ezyang.com/2010/04/diagramming-in-xournal-and-gimp/"
    - id: 6249
      author: "Miscellaneous Project / Research Worthy Ideas [WIP] : Gaurav&#039;s Log"
      date: "2013-09-22 09:41:46"
      content: "[&#8230;] Fractional Cascading This is a really cool idea. Read about it here. It helps speed up bounded-box queries by a log [&#8230;]"
    - id: 22002
      author: Anonymous
      date: "2017-06-07 00:38:47"
      content: "Nice explanation, thanks!"
    - id: 26891
      author: Ari
      date: "2021-10-21 00:20:39"
      content: |
        Hey man, thank you so much for this overview.
        
        Nearly ten years after this was posted it is still one of the friendliest, most intuitive explanations of Fractional Cascading on the internet. It was invaluable for my independent study on advanced data structures, which ended up focused on Fractional Cascading (I linked the paper below - may have borrowed some of your graphics but this page is a source!)
        
        Anyways, that whole experience inspired me to take a formal computational geometry course, and for the final project I'm focusing on Layered Range Trees. If not for your blog, I think I would've written a page or two on Fractional Cascading and moved on but instead it has become one of the most important topics in my CS career.
        
        So thanks again, these posts really do have impacts.
        
        https://github.com/AriBernstein/FractionalCascading/blob/master/Independent%20Study%20Final%20Report.pdf
---

Suppose that you have *k* sorted arrays, each of size *n*. You would like to search for single element in each of the *k* arrays (or its predecessor, if it doesn't exist).

![image](/img/fractional-cascading/intro.png)

Obviously you can binary search each array individually, resulting in a \$O(k\lg n)\$ runtime. But we might think we can do better that: after all, we're doing the same search *k* times, and maybe we can "reuse" the results of the first search for later searches.

Here's another obvious thing we can do: for every element in the first array, let's give it a pointer to the element with the same value in the second array (or if the value doesn't exist, the predecessor.) Then once we've found the item in the first array, we can just follow these pointers down in order to figure out where the item is in all the other arrays.

![image](/img/fractional-cascading/pointers.png)

But there's a problem: sometimes, these pointers won't help us at all. In particular, if a later lists is completely "in between" two elements of the first list, we have to redo the entire search, since the pointer gave us no information that we didn't already know.

![image](/img/fractional-cascading/pointer-fail.png)

So what do we do? Consider the case where *k = 2*; everything would be better if only we could guarantee that the first list contained the right elements to give you useful information about the second array. We could just merge the arrays, but if we did this in the general case we'd end up with a totally merged array of size \$kn\$, which is not so good if *k* is large.

But we don't need all of the elements of the second array; every other item will do!

![image](/img/fractional-cascading/one-level.png)

Let's repeatedly do this. Take the last array, take every other element and merge it into the second to last array. Now, with the new second to last array, do this to the next array. Rinse and repeat. How big does the first array end up being? You can solve the recurrence: \$T(k) = n + T(k-1)/2\$, which is the geometric series \$n + n/2 + n/4 + n/8 + \ldots = 2n\$. Amazingly, the new first list is only twice as large, which is only one extra step in the binary search!

![image](/img/fractional-cascading/multi-level.png)

What we have just implemented is **fractional cascading**! A fraction of any array cascades up the rest of the arrays.

There is one more detail which has to be attended to. When I follow a pointer down, I might end up on an element which is not actually a member of the current array (it was one that was cascaded up). I need to be able to efficiently find the next element which is a member of the current array (and there might be many cascaded elements jammed between it and the next member element, so doing a left-scan could take a long time); so for every cascaded element I store a pointer to the predecessor member element.

![image](/img/fractional-cascading/extra-pointers.png)

Fractional cascading is a very useful transformation, used in a variety of contexts including *layered range trees* and *3D orthogonal range searching*. In fact, it can be generalized in several ways. The first is that we can cascade some fixed fraction α of elements, rather than the 1/2 we did here. Additionally, we don't have to limit ourselves to cascading up a list of arrays; we can cascade up an arbitrary graph, merging many lists together as long as we pick α to be less than *1/d*, where *d* is the in-degree of the node.

![image](/img/fractional-cascading/graph.png)

*Exercise.* Previously, we described [range trees](http://blog.ezyang.com/2012/02/visualizing-range-trees/). How can fractional cascading be used to reduce the query complexity by a factor of \$O(\lg n)\$?

*Exercise.* There is actually another way we can setup the pointers in a fractionally cascaded data structure. Rather than have downward pointers for every element, we only maintain pointers between elements which are identical (that is to say, they were cascaded up.) This turns out to be more convenient when you are constructing the data structure. However, you now need to maintain another set of pointers. What are they? (Hint: Consider the case where a search lands on a non-cascaded, member element.)
