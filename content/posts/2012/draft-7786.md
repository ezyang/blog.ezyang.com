---
title: "Why doesn't Safe ML exist?"
date: 2012-11-15 21:35:30
slug: 
draft: true
categories: [Miscellaneous]
---

Safe ML, patterned after [Safe Haskell](http://www.haskell.org/ghc/docs/latest/html/users_guide/safe-haskell.html), is a hypothetical variant of ML which provides features to allow the safe execution of untrusted third-party code. \[1\] Why doesn’t Safe ML exist? There are a few obvious reasons why not: most variants of ML are not pure, making the tracking of side-effects with monads optional, as opposed to Haskell’s compulsory tracking. \[2\]

But there is also a strong reason why Safe ML *should* exist: ML’s module system, and in general, ML’s strong emphasis on encapsulation. Once you’ve stopped

XXX encapsulation of modules, inability to violate boundaries

XXX exceptions

------------------------------------------------------------------------

\[1\] Actually, it’s not completely hypothetical: Andi Scharfstein built a [sandboxing infrastructure](http://www.ps.uni-saarland.de/theses/scharfstein/) for Alice ML. This design was heavily influenced by Java-style sandboxing and focuses on dynamic access-control on resources, as opposed to type-enforced usage of “safe APIs.” These approaches are not opposed to one another; indeed, most practical systems will want to combine the two, but since types are going to play a very important role in this essay, we won’t consider this system further.

\[2\] Modulo `unsafePerformIO` and its ilk.
