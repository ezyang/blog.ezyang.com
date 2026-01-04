---
title: "Are Haskell data types positive or negative?"
date: 2015-01-19 20:32:53
slug: 
draft: true
categories: [Haskell, Programming Languages]
---

At ICFP, I had the pleasure of chatting with [Paul Downen](http://ix.cs.uoregon.edu/~pdownen/index.html), and in particular, asking him an age old question: “Are Haskell data types positive or negative?”

OK, maybe most people don’t think of this as an age old question, but it certainly kicked up some dust when I wrote a post [comparing Haskell to Coq](http://blog.ezyang.com/2014/03/haskell-for-coq-programmers/). When I originally wrote this post, I included a discussion of the product type in Haskell versus the product type in Coq, and got a pile of interesting comments from Bob Harper (who said that products were negative) and Paul’s officemate Philip Johnson-Freyd (who said that Haskell’s products were positive).

So, who is right? And why should you care?

In this post, I'll argue that pairs are positive, in the sense that they are *data*, not *codata*.

Our conclusion we came to is that Bob misunderstood how Haskell is operationally implemented, which is a combination of positive and negative.

Our conclusion is that the terms *positive* and *negative* have been overloaded in no less than three ways, and in the case of Haskell, its classification along these axes is to the left, to the right, and spinning off into a generally recursive doom. Let’s take a look.

The "Smalltalk" analogy: if you message pass that's negative, if you directly look at data it's positive.

# Inductive versus coinductive
