---
title: "On cargo culting and hacking"
date: 2011-02-28 09:00:46
slug: on-cargo-culting-and-hacking
categories: [Software Engineering]
comments:
    - id: 1842
      author: ketil
      date: "2011-03-01 01:20:19"
      content: "Of course, cargo culting in science means that you might attribute an effect to the entirely wrong factor, because you mindlessly copy everything else.  And, yes, this happens, when an unsubstantiated claim gets copied around enough in the literature, it becomes indistinguishable from substantiated facts."
    - id: 1854
      author: ben
      date: "2011-03-03 10:52:02"
      content: |
        That's a pretty good definition of hacking, but while generalizability is important, it's more of a symptom. Hacking is mostly about simply doing something without all the planning and formalism that people usually go through.
        
        It demands a high level of expertise. To take the classic analogy, if you know where you're going and how to drive, you don't carefully plan your route, you don't think about getting the tach to 2000 before releasing the clutch, you just get in and go. You know where you are, where you're going, you can feel the vehicle and it becomes an extension of you.
        
        A competent driver has internalized all the elements of driving, and that's also the case with a competent hacker. That internalized knowledge is precisely why the hacker can break the rules and get away with it: the rules might not specifically allow it, but the hacker knows it's right because it feels right.
---

<div class="container center">

*two inflammatory vignettes*

</div>

The term *to cargo cult* is one with derogatory connotations: it indicates the act of imitating the superficial exterior without actually understanding the underlying causal structure of the situation. The implication is that one should try to understand what one is doing, before doing it. There is, however, an ounce of truth in the practice of cargo culting: when you are in a situation in which you legitimately do not know what’s going on (e.g. the context of an experiment), it is safest to preserve as many superficial traits as possible, in case a “superficial” trait in fact has a deep, non-obvious connection to the system being studied. But in this regard, beneficial “cargo culting” is nothing like the islanders throwing up airstrips in hopes of attracting planes—understanding what conditions are applicable for this treatment is often the mark of experience: the novice ignores conditions that should be preserved and does not know how to probe deeper.

------------------------------------------------------------------------

*Hacking* is the art of accidental generalization. It is developing a program under a single set of conditions (a hard-coded test input, a particular directory structure, a single URL) and (perhaps) hoping it will work in the more general case. Anything that gets in the way of specificity—proofs, types, security, verbosity, edge-cases, thinking—is the enemy for pure creation. It is the art of the present, and much profit and pleasure can be derived from it. It is the art of laser precision, each problem dealt with as it comes. It is an art that becomes more acceptable engineering practice with experience: one develops little internal censors that continually pipe up with mental flags where you need to give a little extra to make the generalization work. Novices are recommended to bring their check-lists along.
