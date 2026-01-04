---
title: "Sleep considered harmful"
date: 2012-07-16 13:18:09
slug: sleep-considered-harmful
draft: true
categories: [Miscellaneous]
---

*No, not that kind of sleep.*

It is a function of many names. Some call it `sleep`, while others speak of `setTimeout` and `delay`. But all agree it is a function of both immeasurable utility for software engineers and inimitable terror for software testers.

The difficulty of testing functions with the power to influence time begins with time’s inextricable relationship with concurrency in programs. Single-threaded programs can sleep, as `SLEEP(3)` has demonstrated for many decades, but many a software developer has had the niggling feeling that their software ought to be doing something *else* while it sleeps. The ascetics of pure Java and C reach for their thread libraries (which secretly contain a hundred angry rabid weasels); the rest place their futures in the hands of a single-threaded min priority queue. This priority queue is, of course, invariably tucked away in a dark corner of their JavaScript engine or their Android user interface library. So the first thing to do is beg and plead with your runtime team to make the queue extensible (don’t forget to also overload `TIME(3)`) or reimplement the priority queue in userspace.

Once you’ve done that, we can begin talk about testing.

The first problem one encounters is the fact that most people think of time as a global thing, so there’s usually only one global priority queue in the sky. This is not modular in any sense, and rather embarrassing when you attempt to parallelize your test harness and they shoot each other dueling for the singleton queue. It seems essential to have multiple notions of time coexisting together, which means multiple priority queues

The second problem one encounters is the fact that actually threading which queue to use through all of the code involved is fairly annoying: after all, most of the code couldn’t care less if it were being executed in the Mesozoic era. In many languages, your only choice is to try to implicitly thread this state using some sort of dynamic scoping (or set a global variable when you enter the region and unset it when you exit); higher-order languages can use abstractions like the reader monad to do all of the necessary threading and lifting.

The third problem is getting all of these separate regions to talk to each other.
