---
title: "Practical Foundations for Programming Languages (first impressions)"
date: 2012-08-15 01:00:31
slug: practical-foundations-for-programming-languages
categories: [Programming Languages]
comments:
    - id: 3952
      author: Anonymous
      date: "2012-08-15 20:48:59"
      content: "Any tips how you skim through a large book like PFPL and take away the most important concepts? :)"
    - id: 3955
      author: Edward Z. Yang
      date: "2012-08-15 23:23:25"
      content: |
        That’s a little bit generous; I’ve only closely read a handful of chapters, and make no claims about having gotten all of the important concepts...
        
        That being said, it does help a bit to go into a book like PFPL with some idea of what you're looking for, rather than just reading without any direction.
    - id: 5299
      author: John Salvatier
      date: "2012-12-10 22:04:40"
      content: You might consider posting your review to Amazon.
    - id: 5300
      author: Edward Z. Yang
      date: "2012-12-10 22:06:34"
      content: "According to Amazon, the item is not released yet and thus not eligible to be reviewed ;-)"
    - id: 6083
      author: Paolo G. Giarrusso
      date: "2013-05-10 11:43:16"
      content: |
        The item can now be reviewed on Amazon. And none of the current 4 reviews come from PL researchers - maybe people were swayed by the use of "practical" in the title? So your review might help.
        
        (Reviews at:
        http://www.amazon.com/Practical-Foundations-Programming-Languages-Professor/product-reviews/1107029570/ref=cm_cr_dp_see_all_btm?ie=UTF8&amp;showViewpoints=1&amp;sortBy=bySubmissionDateDescending)
    - id: 6084
      author: Edward Z. Yang
      date: "2013-05-10 16:46:30"
      content: "Mmm... I'm pretty sure Lars is a PL researcher http://www.lars.com/ :) (On a side note, I don't know anyone who would buy a textbook after reading a positive Amazon review for it... seems like the wrong way to do textbook acquisition.)"
    - id: 20523
      author: Agam
      date: "2016-02-23 01:02:26"
      content: |
        Excellent article, inspired me to read PFPL!
        
        Since I have a habit of spotting and mentioning typos, hope you don't mind if I point out that you probably meant s/the occur/that occurs, in "... shows how the usual concrete syntax masks the tagging the occur ...".
        
        <b>Editor:</b> Thanks, fixed!
---

[Robert Harper](http://www.cs.cmu.edu/~rwh/) has (somewhat) recently released a [pre-print of a book (PDF)](http://www.cs.cmu.edu/~rwh/plbook/book.pdf) that he has been working on, *Practical Foundations for Programming Languages*. I downloaded a copy when it initially came out, but I was guilty of putting off actually digging into the book’s 590-some pages. It was only until Harper successfully baited me with [one of his most recent blog posts](http://existentialtype.wordpress.com/2012/08/14/haskell-is-exceptionally-unsafe/) that I finally sat down and skimmed it a bit more thoroughly.

The immediate temptation is to compare PFPL to Benjamin Pierce’s seminal *Types and Programming Languages.* At first glance, there would seem to be quite a bit of overlap, both in terms of content and in terms of presentation. Both books starting with a very simple programming language and successively add features to explain successively more complex topics in programming languages design. But PFPL consciously differs from TAPL in many aspects. For ideological reasons, Harper completely skips the untyped language, jumping straight to a typed language with variable let-bindings, in order to immediately introduce types, contexts and safety. This presentation is substantially more terse, and newcomers with no programming languages experience may perceive that PFPL feels more like a reference manual than a textbook—\`one commenter \<http://existentialtype.wordpress.com/2012/08/06/there-and-back-again/#comment-949\>\`\_ likened it to a math textbook. (Apropos of nothing, Harper’s introductory class [15-312 Principles of Programming Languages](http://www.cs.cmu.edu/~rwh/courses/ppl/schedule.html), which uses PFPL, *does* start with the untyped lambda calculus.)

Nevertheless, this terseness is an asset for PFPL; for one thing, it permits Harper to cover a lot of ground, covering topics that TAPL did not handle at all. Nor does the terseness mean that Harper has “left anything out”, each chapter is self-contained and comprehensive on the topics it chooses to cover. It also makes it a fun read for people like me who do have familiarity with the topics discussed but benefit from seeing and thinking about a different treatment.

Harper has been blogging about his book, and I think his blog posts are a good indication of what parts of the book are particularly worth looking at. Harper has taken the style of going “all intuition” in his blog posts, and relegating most of the formalism to his book. I think this is a shame, since the formalisms he defines are quite accessible and would make things a lot clearer for many in his audience (judging from the comments section, at least!) Here are some of the pairings:

- [Dynamic Languages are Static Languages](http://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/) is a companion to Chapter 18, “Dynamic Typing”. There, he develops Dynamic PCF (essentially the core of Lisp) and shows how the usual concrete syntax masks the tagging that occurs, and the usual dynamics masks the wasteful and repetitive checks that are endemic to dynamically typed languages. There is always a temptation, in these holy wars, to expand the scope of the argument, but if you accept Dynamic PCF as a valid way of framing one aspect of the debate, it is *extremely* precise.
- [Referential transparency](http://existentialtype.wordpress.com/2012/02/09/referential-transparency/) is a companion to Chapter 32, “Symbols”. Symbols are a little weird, because most languages don’t even have a way of even *acknowledging* this concept exists. You might think of it as an identifier for a “generalized mutable cell” apart from how you actually access it, but really you should just read the formal treatment, since it is very simple.
- [Words matter](http://existentialtype.wordpress.com/2012/02/01/words-matter/) is a companion to Chapter 36, “Assignable References”. It’s a simple terminology split, motivated by how Harper defines the term “variable”, way in Chapter 1 of his book.
- [Haskell is Exceptionally Unsafe](http://existentialtype.wordpress.com/2012/08/14/haskell-is-exceptionally-unsafe/) is a companion to Chapter 34, “Dynamic Classification”. It argues that it is important to be able to generate exception classes at *runtime* (the term “class” here has a very precise meaning, namely, it is an index of a finite sum, in this case the exception type; this is discussed in Chapter 12). At least in the Haskell community, this is not a particularly common usage of the term “dynamic” (though I agree with Harper that it is a correct usage), and PFPL spells exactly what it means, no more, no less.

All-in-all, *Practical Foundations for Programming Languages* is well worth checking out. It is a not too widely kept secret that no one really reads textbooks from tip to tail, but if you found yourself reading one of Harper’s blog posts and being puzzled, do give the companion chapter a chance. Even with just the small bits of the book I have read, PFPL has taught me new things and clarified my thinking.
