---
title: "Specifying finite state machines"
date: 2012-10-24 14:21:48
slug: specifying-finite-state-machines
draft: true
categories: [Miscellaneous]
---

In any complexity theory textbook, the finite state machine is defined as follows: “a deterministic finite state machine is a quintuple (Σ, S, s0, δ, F) such that...” This is definition has a number of benefits: it is precise and easily implemented and interpreted. It also is a bitch to actually write programs in; you’d much rather draw a graph or write a regular expression, than actually ever specify this quintuple explicitly.

The quintuple rears its ugly head in many places; for example, a Turing Machine can be thought of as a finite state machine with access to an auxiliary data structure, the tape. Complexity theorists have a long history of programming Turing Machines with an understandably large amount of handwaving, but even in their constructions, they don’t even bother specifying the set of possible states. There’s a proof obligation here: that their construction actually only has a finite set of possible states; but this proof obligation is (rightly) held to be self-evident.

I want a programming language for expressing finite state machine which has a natural correspondence to
