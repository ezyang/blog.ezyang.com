---
title: "To the right! Autocompletable names"
date: 2010-01-25 12:00:00
slug: to-the-right-autocompletable-names
categories: [Miscellaneous]
comments:
    - id: 64
      author: dave glasser
      date: "2010-02-02 00:17:09"
      content: "or use ido!"
    - id: 66
      author: Edward Z. Yang
      date: "2010-02-02 03:39:44"
      content: "Damn emacs users! :-)"
    - id: 3775
      author: anonymous
      date: "2012-05-26 00:16:30"
      content: |
        I wonder if this pattern is actually a sign that a subdirectory should be used--that way it seems you win on both fronts: the hierarchy imposed by the filesystem and the tab-completion because you can start in the subdirectory.
        
        The only downside is that now files that were once "siblings" are now "cousins", and can't be used together as easily.
    - id: 3776
      author: Edward Z. Yang
      date: "2012-05-26 00:18:13"
      content: "If I need rapid access to files in different subdirectories, I end up in the same situation. The point is, I want the first set of characters I type to have as much information as possible."
---

In my younger days, the stylistic convention of MM/DD/YYYY confused me; why on earth would people opt for such an illogical system that placed months, days and years in non-hierarchical order? Surely something on order of YYYY-MM-DD would make far more sense: this format is sortable and, all-in-all, quite logical.

Eventually, though, I grudgingly accepted that MM/DD/YYYY, trades machine-friendliness for human-friendliness; after all, the year entry rarely changes, and for humans the month and date are the most important pieces of information. Context is usually more than enough to implicity specify what the year is.

But as a auto-complete user, I've come to appreciate that this sort of ordering can come in handy even when computers are involved. Consider the hierarchically named and non-hierarchally named list of files:

    # hierarchally named
    test-algorithm.sh
    test-bottles.sh
    test-capistrano.sh
    utils.sh

    # non-hierarchally named
    algorithm-test.sh
    bottles-test.sh
    capistrano-test.sh
    utils.sh

In the hierarchal case, to auto-complete `test-algorithms.sh`, I need to type `t<tab>a<tab>`; a total of four keystrokes. In the non-hierarchal case, however, I only need to type `a<tab>`. If I'm frequently accessing these files, the extra keystrokes add up.

So here's my plea: the next time you're coming up with a naming convention for files you're sticking in a directory, consider both moving the "category" component to the end, and thinking of autocomplete friendly names. Your fingers will thank you for it.

(Hat-tip to GameTeX for showing me the light.)
