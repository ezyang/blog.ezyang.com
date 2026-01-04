---
title: "N-ary fusion"
date: 2017-07-14 21:39:30
slug: 
draft: true
categories: [Haskell]
---

Fusion in the functional programming tradition primarily concerns itself with achieving transformations like `map f . map g ==> map (f . g)`. The *mechanism* by which this is achieved may vary (e.g., the [foldr/build rule](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/deforestation-short-cut.pdf) `foldr c n (build g) → g c n`, or the [stream fusion rule](https://pdfs.semanticscholar.org/64d2/a65a7d559f9b05570fb0fea8bb4cccd83ae2.pdf) `stream (unstream s) → s`), but the *pattern* is always the same: the composition of two functions can be eliminated. And this is great in a language like Haskell, where we love constructing long pipelines of function calls.
