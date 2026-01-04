---
title: "Tail recursion makes your loops cleaner"
date: 2011-05-23 09:00:04
slug: tail-recursion-makes-your-loops-cleaner
categories: [Haskell]
comments:
    - id: 2487
      author: Dan Doel
      date: "2011-05-23 12:22:17"
      content: |
        To be fair...
        
        int x1 = ceil(r/sqrt(2));
        
        for(int x = 0, int k = 5 - 4*r, int y = -r ; x &lt; x1 ; x++) { ...
        
        is an option (assuming I haven&#039;t messed up the syntax).
    - id: 2488
      author: Edward Z. Yang
      date: "2011-05-23 13:02:11"
      content: "Yes, that's true. Though, you still fail to update the \"loop variables\" in the third segment (you could probably hack it in there too, but then the code is getting a bit dodgy...)"
    - id: 2489
      author: MarkusQ
      date: "2011-05-23 13:12:36"
      content: |
        :)  Two bugs, actually (assuming the haskell version is canonical).
        
        -- MarkusQ
    - id: 2490
      author: Edward Z. Yang
      date: "2011-05-23 13:17:41"
      content: "Oh yes, that second bug was inadvertent, I've (hopefully) fixed it now."
    - id: 2491
      author: MarkusQ
      date: "2011-05-23 13:27:57"
      content: |
        Yep.  :)
        
        Nice post, BTW.
        
        -- MarkusQ
    - id: 2495
      author: Anonymous
      date: "2011-05-23 19:34:00"
      content: |
        for (int x = 0, int k = 5 - 4*r, int y = -r;
             x 0?(++y,8*y+20):12), ++x) 
          draw(x, y);
    - id: 2496
      author: Anonymous
      date: "2011-05-23 19:35:32"
      content: |
        for (int x = 0, int k = 5 - 4*r, int y = -r;
             x &lt;= ceil(4/sqrt(2));
             k += 8*x + (k&gt;0?(++y,8*y+20):12), ++x) 
          draw(x, y);
    - id: 2497
      author: augustss
      date: "2011-05-23 19:49:23"
      content: "A pity you didn't use a tail recursive function to illustrate tail recursion."
    - id: 2521
      author: "re: augustss"
      date: "2011-05-28 21:07:47"
      content: |
        Truth is beauty; beauty, truth*.
        
        * Except in lame "go functional programming!" posts.
---

Recursion is one of those things that functional programming languages shine at—but it seems a bit disappointing that in many cases, you have to convert your beautiful recursive function back into iterative form. After all, iteration is what imperative languages do best, right?

Actually, explicitly tail-recursive functions in functional programming languages can be fairly beautiful: in fact, in the cases of complicated loops, they can be even prettier than their imperative counterparts. Take this midpoint line-drawing algorithm as an example:

    circleMidpoint d r = go 0 (-r) k0
        where k0 = 5 - 4 * r
              x1 = ceiling (fromIntegral r / sqrt 2)
              go x y k | x > x1    = return ()
                       | k > 0     = d (x,y) >> go (x+1) (y+1) (k+8*x+8*y+20)
                       | otherwise = d (x,y) >> go (x+1)  y    (k+8*x+12)

There are three loop variables: `x`, `y` and `k`, and depending on various conditions, some of them get updated in different ways. `x` is a bog-standard loop variable; ye old C-style `for` loop could handle it just fine. But `y` and `k` are updated differently depending on some loop conditions. But since they’re parameters to the `go` helper function, it’s always clear what the frequently changing variables are. You lose that nice structure in the imperative translation:

    // global variables and loop variables are all mixed together
    int k = 5 - 4 * r;
    int y = -r;
    int x1 = ceil(r/sqrt(2));
    for (int x = 0; x <= x1; x++) { // only x is obviously an index var
      draw(x, y);
      if (k > 0) {
        y++;
        k += 8*x + 8*y + 20;
      } else {
        k += 8*x + 12;
      }
      // does it ever make sense for any code to live here?
    }

I’ve also managed to introduce a bug in the process...
