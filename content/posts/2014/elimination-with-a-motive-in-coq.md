---
title: "Elimination with a Motive (in Coq)"
date: 2014-05-07 23:13:15
slug: elimination-with-a-motive-in-coq
categories: [Coq]
comments:
    - id: 6593
      author: Pseudonym
      date: "2014-05-08 04:06:19"
      content: |
        For obvious (or perhaps not so obvious) reasons, and_elim should read:
        
        Lemma and_elim : forall { A B C : Prop }, A /\ B -&gt; (A -&gt; B -&gt; C) -&gt; C.
        
        You used parentheses rather than braces. You want the propositions to be implicit for maximal usefulness.
    - id: 6594
      author: Edward Z. Yang
      date: "2014-05-08 04:18:49"
      content: "Oops, good catch."
    - id: 6595
      author: ja
      date: "2014-05-08 21:13:15"
      content: "One other problem with dependent induction in coq is how often it changes. This makes it brittle, and proofs using it may not survive coq version changes."
    - id: 6607
      author: tbelaire
      date: "2014-05-20 16:56:26"
      content: "Could you explain `Vect_elim` in more detail?  What's going on with the {} and the @P? I'm still just learning Coq.  I tried googling, but it's surprisingly hard to google about syntax for Coq."
    - id: 6608
      author: Edward Z. Yang
      date: "2014-05-20 17:00:57"
      content: "tbelaire: Curly braces indicate that an argument is implicit, and prefixing a function/constructor with @ is how you explicitly pass implicit arguments."
    - id: 6685
      author: tbelaire
      date: "2014-05-29 01:57:56"
      content: "Thanks!"
---

Elimination rules play an important role in computations over datatypes in proof assistants like Coq. In his paper "Elimination with a Motive", Conor McBride argued that "we should exploit a hypothesis not in terms of its immediate consequences, but in terms of the leverage it exerts on an arbitrary goal: we should give elimination a motive." In other words, proofs in a refinement setting (backwards reasoning) should use their goals to guide elimination.

I recently had the opportunity to reread this historical paper, and in the process, I thought it would be nice to port the examples to Coq. Here is the result:

> <http://web.mit.edu/~ezyang/Public/motive/motive.html>

It's basically a short tutorial motivating John Major equality (also known as heterogenous equality.) The linked text is essentially an annotated version of the first part of the paper—I reused most of the text, adding comments here and there as necessary. The source is also available at:

> <http://web.mit.edu/~ezyang/Public/motive/motive.v>
