---
title: "Errors in the academic record"
date: 2017-03-19 19:31:18
slug: 
draft: true
categories: [Miscellaneous]
---

As humans, we are all fallible. But as academics, we like to believe that if a paper passes the peer review

**Translating dependency into parametricity.** Suppose that you are designing a system that needs to process both high security (top secret) and low security (public) data. To ensure this system is secure, you would like to make it so that high security computations can make use of low security data, but *not* vice versa.

A way of formalizing what it means for a system to have this property is *noninterference.* Intuitively, what noninterference says is that if you have a low-security observer, they should not be able to distinguish between systems that vary only in their high-security data: this data must always remain "hidden".

In the ICFP'04 paper [Translating Dependency into Parametricity](http://www.cis.upenn.edu/~stevez/papers/TZ04b.pdf), Stephen Tse and Steve Zdancewic noticed that this "information hiding" beared a strong resemblance to the information hiding that occurs when you have a parametric function: when you write a function with the polymorphic type `a -> a`, this function cannot distinguish between different arguments. In their paper, they gave a translation of a DCC, a calculus for information flow control into System F, showing that parametricity in System F implied noninterference in DCC.

Unfortunately, their proof was not correct!
