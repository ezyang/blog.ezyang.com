---
title: "You could have invented zippers"
date: 2010-04-07 09:00:54
slug: you-could-have-invented-zippers
categories: [C, Haskell]
comments:
    - id: 271
      author: Conor
      date: "2010-04-07 11:36:41"
      content: "Closer to the source, it might be worth mentioning Gérard Huet's 1997 JFP Functional Pearl \"The Zipper\" which kicked the topic off and introduced the name. James McKinna shoved it into my hands, and it burrowed to my bones..."
    - id: 272
      author: Edward Z. Yang
      date: "2010-04-07 11:42:42"
      content: "I don't know how I forgot to mention that paper! Added."
    - id: 275
      author: falcon
      date: "2010-04-07 12:54:21"
      content: |
        I wonder if you could describe the context a bit further, perhaps a data definition in C?
        This is the closest I've come to understanding zippers, so I hope you explains a few things further.
        
        "...or go up the red arrow pointed away from the green node; we'll call the structure pointed to by this arrow a context."
        The red arrow is pointing to the parent node, so the context is just another name for the relevant node?
    - id: 276
      author: Edward Z. Yang
      date: "2010-04-07 13:05:19"
      content: |
        I added a C data definition for context; I'll see if I can add some more explanatory text.
        
        <em>The red arrow is pointing to the parent node, so the context is just another name for the relevant node?</em>
        
        Yup! But it's not a tree node in the usual sense, because it lacks one of its two child pointers and may have another pointer going up. That's a good clarification I'll add.
    - id: 278
      author: Dan Farmer
      date: "2010-04-07 15:45:40"
      content: "Great post Edward. Excellent content as usual, and the presentation is top notch."
    - id: 279
      author: Aria Haghighi
      date: "2010-04-07 16:01:12"
      content: The clojure core has a nice implementation that is used by the XML processing libraries
    - id: 280
      author: Edward Z. Yang
      date: "2010-04-07 19:42:04"
      content: |
        Aria, for the curious:
        
        http://clojure.org/other_libraries
        http://en.wikibooks.org/wiki/Clojure_Programming/Examples/API_Examples/Advanced_Data_Structures#zipper
        http://github.com/richhickey/clojure/blob/master/src/clj/clojure/zip.clj
    - id: 281
      author: Ron de Bruijn
      date: "2010-04-08 04:06:58"
      content: "Zippers are a trivial application of a technique called \"pointer reversal\", which apparently is not referred to by Huet, invented in or before the 1970s."
    - id: 282
      author: Tony Sloane
      date: "2010-04-08 05:40:32"
      content: |
        As Ron says, this pointer reversal idea has been around since the 1960s.  I think the following paper has the original description and it's often referred to as the Schorr-Waite algorithm.
        
        Schorr, H. and W. Waite, ‘‘An Efficient Machine-Independent Procedure for Garbage Collection in Various List Structures’’, Communications of the ACM 10, 8 (Aug. 1967), pp. 481-492.
    - id: 283
      author: Conor
      date: "2010-04-08 06:56:57"
      content: |
        Huet may not mention "pointer reversal" explicitly, but he does say that he's merely recording folklore. He's also specifically addressing the issue in the context of applicative programming with persistent, immutable data. Moreover, he does so in a typed way, distinguishing forward pointers from backward, thus statically maintaining the invariant that the backward pointers form a path. An offstage discipline becomes an onstage type. Meanwhile, the entire pattern becomes a program, given a bit of differential calculus at the type level.
        
        So, while there is a clear connection with pointer reversal, this is what purely functional programmers do instead. As to whether or not the business of applying a type discipline and a sharing strategy to produce a functional analogue of pointer reversal is "trivial", I couldn't possibly comment. It's certainly rather useful.
    - id: 284
      author: Johan Tibell
      date: "2010-04-08 10:03:59"
      content: "What did you use to make the diagrams? (I'm serious. I want to know)."
    - id: 287
      author: Edward Z. Yang
      date: "2010-04-08 11:53:22"
      content: |
        Conor has said it well. I'll also add, in my opinion, the pointer reversal idea is the easy bit (the part "you could have invented"), whereas the actual shoehorning of the idea into an algebraic data structure, where we tend not to really think about pointers, is the conceptual leap. It's not very far, but as some have said, "a 'trivial' concept gets invented over and over again until someone writes it down and gets credit for it."
        
        Johan, I use Xournal (for drawing and shading) and Gimp (for cropping and PDF to PNG conversion), on my X61 tablet.
    - id: 288
      author: Tony Sloane
      date: "2010-04-08 19:34:06"
      content: "Conor and Edward, I certainly don't mean to downplay Huet's achievement, but we should also give credit to the earlier work, whether or not we believe it to be the \"easy bit\"."
    - id: 293
      author: paurullan
      date: "2010-04-10 13:09:39"
      content: "Gorgeous introduction! Thank you!"
    - id: 3543
      author: "Immutable Data Structures | Loominate"
      date: "2012-03-12 22:32:44"
      content: "[...] from Edward Z. Yang another perspective on zippers. I especially like the template &#8220;You could have invented &#8230;&#8221; that he employs. [...]"
    - id: 6185
      author: "Rethink a basic tree algorithm with purely functional data structures: the &#8220;zippers&#8221;. | Benoît Patra&#039;s blog"
      date: "2013-07-24 17:34:48"
      content: "[...] is on our left, above and on our right. If you want to visualize some zipper I suggest you to read this post. You can see that the difference with Huet&#8217;s implementation on leaf-labeled trees is that the [...]"
    - id: 19500
      author: "Rewriting a tree algorithm with purely functional data structures: the &#8220;zippers&#8221;. | Benoit Patra personal blog"
      date: "2015-12-03 10:08:17"
      content: "[&#8230;] is on our left, above and on our right. If you want to visualize some zipper I suggest you to read this post. You can see that the difference with Huet&#8217;s implementation on leaf-labeled trees is that the [&#8230;]"
    - id: 20525
      author: "Elixir Zipper, a Love Story | (Un)Boxing Code"
      date: "2016-02-26 08:05:57"
      content: "[&#8230;] You could have invented zippers Haskell/Zippers + Theseus and the Zipper Story Gérard Huet. The Zipper. Journal of Functional Programming Getting Acquainted With Clojure Zippers Clojure Zippers: Structure Editing With Your Mind Clojure Zipper Roll Your Own Window Manager: Tracking Focus with a Zipper ZipperTree &#8211; Elixir [&#8230;]"
    - id: 21348
      author: Iaroslav Karkunov
      date: "2016-09-27 03:36:00"
      content: "Thank you for the great article!"
    - id: 22109
      author: Turn
      date: "2017-09-08 02:38:24"
      content: "Finally out of labyrinth! Thank you!"
    - id: 22880
      author: Kofi
      date: "2019-06-20 22:07:32"
      content: |
        This is the best Zipper (Tree) explanation that I have seen since trying to figure it out.
        I read your post and solved it in 30 minutes.
        
        Thanks.
    - id: 30992
      author: "你本可以发明拉链（2010年） - 偏执的码农"
      date: "2023-11-26 15:33:24"
      content: "[&#8230;] 详情参考 [&#8230;]"
---

In the beginning, there was a binary tree:

    struct ctree { // c for children
      int val;
      struct ctree *left;
      struct ctree *right;
    }

The flow of pointers ran naturally from the root of the tree to the leaves, and it was easy as blueberry pie to walk to the children of a node.

![image](/img/zipper/ctree.png)

Unfortunately, given a node, there was no good way to find out its parent! If you only needed efficient parent access, though, you could just use a single pointer in the other direction:

    struct ptree { // p for parent
      int val;
      struct ptree *parent;
    }

The flow of pointers then ran from the leaves of the tree to the root:

![image](/img/zipper/ptree.png)

And of course, put together, you could have the best of both worlds:

    struct btree {
      int val;
      struct btree *parent;
      struct btree *left;
      struct btree *right;
    }

![image](/img/zipper/btree.png)

Our data structure had become circular, but as a result we had extremely efficient ways to walk up and down the tree, as well as insert, delete and move nodes, simply by mutating the relevant pointers on our node, its children and its parent.

*Trouble in paradise.* Pointer tricks are fine and good for the mutable story, but we want immutable nodes. We want nodes that won't change under our nose because someone else decided to muck around the pointer.

In the case of `ctree`, we can use a standard practice called *path copying*, where we only need to change the nodes in the path to the node that changed.

![image](/img/zipper/ctree-mod.png)

In fact, path copying is just a specific manifestation of the rule of immutable updates: if you replace (i.e. update) something, you have to replace anything that points to it, recursively. In a `ptree`, we'd need to know the subtree of the updated node and change all of them.

![image](/img/zipper/ptree-mod.png)

But `btree` fails pretty spectacularly:

![image](/img/zipper/btree-mod.png)

Our web of pointers has meant we need to replace *every* single node in the tree! The extra circular pointers work to our detriment when looking for a persistent update.

What we'd like to do is somehow combine the `ptree` and the `ctree` more intelligently, so we don't end up with a boat of extra pointers, but we still can find the children and the parent of a node.

Here, we make the critical simplifying assumption: we only care about efficient access of parents and children as well as updates *of a single node.* This is not actually a big deal in a world of immutable data structures: the only reason to have efficient updates on distinct nodes is to have a modification made by one code segment show up in another, and the point of immutability is to stop that spooky action at a distance.

So, on a single node, we want fast access to the parent and children and fast updates. Fast access means we need pointers going away from this node, fast updates means we need to eliminate pointers going into this node.

Easy! Just flip some pointers (shown in red.)

![image](/img/zipper/pointers.png)

Congratulations, the data structure you see here is what we call a zipper! The only task left for us now is to figure out how we might actually encode this in a `struct` definition. In the process, we'll assign some names to the various features inside this diagram.

Let's consider a slightly more complicated example:

![image](/img/zipper/zipper.png)

We've introduced a few more notational conveniences: triangles represent the tree attached to a given node when we don't care about any of its subnodes. The squares are the values attached to any given node (we've shown them explicitly because the distinction between the node and its data is important.) The red node is the node we want to focus around, and we've already gone and flipped the necessary pointers (in red) to make everything else accessible.

When we're at this location, we can either traverse the tree, or go up the red arrow pointed away from the green node; we'll call the structure pointed to by this arrow a context. The combination of a tree and a context gives us a location in the zipper.

![image](/img/zipper/loc.png)

    struct loc {
      struct ctree *tree;
      struct context *context;
    }

The context, much like the tree, is a recursive data-structure. In the diagram below, it is precisely the node shaded in black. It's not a normal node, though, since it's missing one of its child pointers, and may contain a pointer to its own parent.

The particular one that this location contains is a "right context", that is, the arrow leading to the context points to the right (shown in black in the following diagram).

![image](/img/zipper/right-context.png)

As you can see, for our tree structure, a context contains another context, a tree, and a value.

![image](/img/zipper/left-context.png)

Similarly, a "left context" corresponds to an arrow pointing to the left. It contains the same components, although it may not be quite obvious from the diagram here: where's the recursive subcontext? Well, since we're at the top of the tree, instead we have a "top context", which doesn't contain any values. It's the moral equivalent of `Nothing`.

    enum context_type {LEFT, RIGHT, TOP}
    struct context {
      enum context_type type;
      // below only filled for LEFT and RIGHT
      int val;
      struct context *context;
      struct ctree *tree;
    }

And there we have it! All the pieces you need to make a zipper:

    > data Tree a = Nil | Node a (Tree a) (Tree a)
    > data Loc a = Loc (Tree a) (Context a)
    > data Context a = Top
    >                | Left a (Tree a) (Context a)
    >                | Right a (Tree a) (Context a)

*Exercises:*

1.  Write functions to move up, down-left and down-right our definition of `Tree`.
2.  If we had the alternative tree definition `data Tree a = Leaf a | Branch Tree a) (Tree a)`, how would our context definition change?
3.  Write the data and context types for a linked list.

*Further reading:* The original crystallization of this pattern can be found in [Huet's paper (PDF)](http://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf), and two canonical sources of introductory material are at [Wikibooks](http://en.wikibooks.org/wiki/Haskell/Zippers) and [Haskell Wiki](http://www.haskell.org/haskellwiki/Zipper). From there, there is a fascinating discussion about how the differentiation of a type results in a zipper! See [Conor's paper (PDF)](http://www.cs.nott.ac.uk/~ctm/diff.pdf), the Wikibooks article, and also Edward Kmett's post on using [generating functions](http://comonad.com/reader/2008/generatingfunctorology/) to introduce more exotic datatypes to the discussion.
