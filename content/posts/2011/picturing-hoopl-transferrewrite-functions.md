---
title: "Picturing Hoopl transfer/rewrite functions"
date: 2011-02-16 09:00:44
slug: picturing-hoopl-transferrewrite-functions
categories: [Hoopl]
comments:
    - id: 1784
      author: Sam Martin
      date: "2011-02-20 06:29:30"
      content: "Nice pic, but I'm still finding Hoopl a bit incomprehensible. I think I need some worked examples. I'm hoping GHC HQ will provide adequate material for this soonish, but the original papers were a bit thin on examples."
    - id: 1785
      author: Edward Z. Yang
      date: "2011-02-20 09:12:12"
      content: "Yeah, concrete examples are definitely good. I’ll see if I can code one up :-)"
    - id: 1848
      author: yaqian
      date: "2011-03-02 03:20:15"
      content: "Hi Edward, what kind of tool you are using to draw the pic? Looks great."
    - id: 1970
      author: Jian
      date: "2011-03-28 09:52:08"
      content: "Hi, I am trying to do something with hoopl now. However, beside those two papers, i can find nothing very helpful online. I am wondering if you know some useful links or anything else."
    - id: 1971
      author: Edward Z. Yang
      date: "2011-03-28 09:53:09"
      content: "Not that I know of. I'm planning on doing a tutorial-ish series using the sample application in the Hoopl repository; it's probably a good idea to look at that if you need more examples."
    - id: 1972
      author: Jian
      date: "2011-03-28 10:02:48"
      content: "That would be great!!! I just did the installation today, then I found I cannot really write a single simple example. Really frustrated~"
---

[Hoopl](http://hackage.haskell.org/package/hoopl) is a “higher order optimization library.” Why is it called “higher order?” Because all a user of Hoopl needs to do is write the various bits and pieces of an optimization, and Hoopl will glue it all together, the same way someone using a fold only needs to write the action of the function on one element, and the fold will glue it all together.

Unfortunately, if you’re not familiar with the structure of the problem that your higher order functions fit into, code written in this style can be a little incomprehensible. Fortunately, Hoopl’s two primary higher-order ingredients: transfer functions (which collect data about the program) and rewrite functions (which use the data to rewrite the program) are fairly easy to visualize.

![image](/img/hoopl.png)
