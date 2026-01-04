---
title: "Blame Trees"
date: 2013-08-12 14:55:28
slug: blame-trees
categories: [Academia]
comments:
    - id: 6229
      author: K. J.
      date: "2013-09-11 03:27:11"
      content: |
        Interesting reading. I also found another recent paper on three way merges with similar performance: They use treaps instead of rb-trees to get history independent balancing.
        http://arxiv.org/abs/1301.3388
    - id: 6230
      author: Edward Z. Yang
      date: "2013-09-11 03:32:41"
      content: "Nice find! I'll have to take a look."
    - id: 21105
      author: S.N.M
      date: "2016-08-11 16:13:19"
      content: It is fantastic I understand lot of things about Blame
---

I just presented *Blame Trees* at the [13th Algorithms and Data Structures Symposium](http://www.wads.org/). Blame trees are a functional data structure which support an efficient merge operation by incorporating information about the “blame” (think `git blame`) of any given part of the structure. It’s a theory paper, so the constant factors are not so good, but the asymptotics are much better than traditional merge algorithms used by modern VCSes.

This was joint work with [David A. Wilson](http://web.mit.edu/dwilson/www/), [Pavel Panchekha](http://pavpanchekha.com/) and [Erik D. Demaine](http://erikdemaine.org/). You can view the [paper](http://ezyang.com/papers/demaine13-blametrees.pdf) or check out the [slides.](http://ezyang.com/slides/ezyang13-blametrees-slides.pdf) I also have a slightly older version of the talk recorded on [YouTube (20 minutes)](http://youtu.be/f8e-QE6Gus8) which I had used to help get feedback from my out-of-town collaborators before actually giving the talk. Thanks also to David Mazières for giving useful comments on the presentation in person.
