---
title: "Notation for commutative monads"
date: 2012-08-16 02:57:57
slug: notation-for-commutative-monads
draft: true
categories: [Haskell]
---

Two years ago, I paid a visit to [Chung-Chieh (Ken) Shan](http://www.cs.rutgers.edu/~ccshan/), back when he was faculty at Rutgers University. He was giving a talk about [probabilistic inference and embedding DSLs](http://blog.ezyang.com/2010/09/data-is-code/), and we had a good conversation about cultural differences between programming languages researchers and machine learning researchers. I wondered why Ken had used OCaml to implement their system, and not Haskell, and Ken replied that the big reason for this was the fact that when writing out a model, a machine learning researcher wants to say, `if flip 0.5 then heads else tails`, rather than the verbose monadic version `flip 0.5 >>= \b -> if b then heads else tails`.

Later that year, I would come across Simon Peyton-Jones remark in [Wearing the hair shirt: a retrospective on Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-retrospective/index.htm) that do-notation over-sequentialized so-called *commutative monads*, which are monads for which the order of operation of monadic actions doesn’t matter. Simon posed the question: is there good notation for these commutative monads?

There is a close relationship between these two questions, though I didn’t realize it at the time. (Partially because I didn’t realize at the time that the random numbers monad was commutative.)

XXX

Idempotent monads

So it's really a *substitution problem*
