---
title: "Views and Viewtypes in ATS"
date: 2011-12-19 18:57:34
slug: 
draft: true
categories: [Computer Science]
---

*This seminar was given as part of the MIT PL seminars series.*

\[\[ ATS is a programming language that while having a reputation for being [blazingly fast](http://shootout.alioth.debian.org/u64/ats.php), has also got the reputation of being somewhat difficult to understand. \]\]

One particular idea behind ATS is that you want to imitate the C program: see how close you can get. If you’re feeling lazy, you can just copy the C code, and not bother with any proofs. This is pretty useful, since initialization in ATS can be painful. But what they wanted to do was build imperative programming on top of theorem proving, while encouraging as close a correspondence between it and C (for example, ATS and C have the same data representation for the simple data types.) (Arvind comments that ATS also gives advanced features, which need to make implementation choices.) Another way of putting it is that ATS is just a *C frontend.* Fancy type systems for classical style programming! (Worth elaborating, since it's easy to get distracted by all of its other features.)

The goal, however, is that you should be able to think of what you want to do, in terms of safety, and then simply write it down. So we want to unify the syntax that lets you write down the implementation, as well as the type system: if I write code to manipulate data, I also write code to manipulate proofs. This is probably the most important aspect of ATS: the programming paradigm that it espouses. (To contrast, other dependently typed systems may let you write down the programs you want, and then require you to separately prove that they type-check, since type-checking is undecidable in those systems.) There is a separation: you could take a highlighter and highlight the “dynamic” parts of the program, which concern traditional programs, and the ”static” parts of the program, which is a simpler programming language operating on types.

Fancy type system just for classic programming.

ATS covers STLC, Dependent ML, System F, F_c, F_ω ATS does not cover dependent lambda calculus, or the calculus of constructions

The statics language is a simply typed language with some constants (in practice, we have integers and booleans, since integers are pretty useful for representing addresses.) For example, you have the unit type, `1 : () => type` (note that none of these are curried, so if you don't need arguments to ), or `true : () => bool` for the type level `true`, or `-> : (type, type) => type` for construction function types.

Now, here is where ATS gives you a little more rope than you would normally get, however. There is

Structure

EXAMPLE

Discourse on what is going on with linear types.

Plug for ATS.

- ATS is a statically typed general-purpose programming language. (Call-by-value, supports lazy evaluation)
- The signatory feature of ATS is a programming paradigm named programming-with-theorem-proving in which code for (run-time) computation and code for proof construction can be written in a syntactically intertwined manner.
- In ATS, there is direct support for both dependent types and linear types.
- While the dependent types in ATS, which are of a restricted form originated from the development of Dependent ML (DML), are well-studied, the linear types in ATS are much less well-known.
- In this talk, I will give an introduction to a notion of linear types referred to as viewtypes in ATS, which combine views (i.e., linear types for proofs) and types (for run-time values).
- In addition, I will present several concrete examples to illustrate certain practical uses of views and viewtypes.
