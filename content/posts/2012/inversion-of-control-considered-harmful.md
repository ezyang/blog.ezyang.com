---
title: "Inversion of control considered harmful"
date: 2012-01-06 09:00:01
slug: inversion-of-control-considered-harmful
draft: true
categories: [Haskell]
---

<div class="container center">

*Don’t call me, I’ll call you.*

</div>

In [Oleg’s presentation on Iteratees](http://okmij.org/ftp/Haskell/Iteratee/IterateeIO-talk-notes.pdf), he dismisses handle based IO with the following remarks:

1.  It is not that simple,
2.  Handle IO puts the file descriptor in the non-blocking mode,
3.  Cannot do our own input multiplexing with select/epoll,
4.  Resource leaks, closed handles errors, and
5.  Cannot do handle IO over nested/embedded streams.

I’d like to focus on issues two and three, which are very traditional reasons for imposing inversion of control on input-output code. There are very good reasons for preferring inverted, push-style programming on the operational side; indeed, this fact has lead to the proliferation of callback-based asynchronous event libraries in many languages.

However, the Haskell community has had a lot of experience designing APIs that look direct but are actually inverted under the hood. We have monads which can make continuation-passing style code look frighteningly normal, cheap and ubiquitous lambdas, and a modern GHC which has a kickass runtime scheduler that lets us write blocking code on file handles and concurrency channels without worrying about tying up operating system threads.

It’s almost as if Iteratees are the last hold-out from the inversion of control camp still in Haskell. It took me a while to notice it, but it seems something is *wrong* here.

Declarative style should be considered the gold standard of API design (this was a lesson John Launchbury taught me while I was at Galois). And indeed, in theory, iteratees should do well here. But if your users believe they need to routinely write inverted code, some API have gone terribly awry. Yes iteratees, I’m looking at you.
