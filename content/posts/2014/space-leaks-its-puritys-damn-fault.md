---
title: "Space leaks: it's purity's damn fault"
date: 2014-04-09 19:28:58
slug: space-leaks-its-puritys-damn-fault
draft: true
categories: [Haskell]
---

Much ink has been spilled on the subject of laziness versus strict evaluation, and its relationship to the time and space usage of programs. The progression of thoughts for someone whose introduction to functional programming is Haskell tends to go something like this:

1.  You start having programmed in traditional, garbage-collected languages like Java and Python using the local idioms; usually you don't ever see a space leak unless you are doing something complicated. (But you tend not to write code with very complicated control flow or higher-order behavior.)
2.  You learn Haskell, which is your first introduction to lazy, purely functional programming. You learn to write code in a mathematical style; some idioms which you’d expect to be fine actually turn out to have space leaks. You think, “Well, laziness causes space leaks.”
3.  You quite like purity, so you try your hand at doing pure programming in your favorite, strictly evaluated language (maybe OCaml or Rust.) You discover some lazy idioms that were efficient in Haskell use up too much stack space. You think, “Well, I guess strictness can cause space leaks in some situations too; so there’s a tradeoff between the two.” (Or perhaps you read Okasaki’s book on *Purely Functional Data Structures*, where Okasaki notes how laziness is critical to recover efficiency in some circumstances in a pure setting.)
