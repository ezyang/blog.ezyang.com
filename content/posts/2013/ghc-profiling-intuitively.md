---
title: "GHC profiling"
date: 2013-03-02 17:00:24
slug: ghc-profiling-intuitively
draft: true
categories: [GHC]
---

Patrick M. Sansom’s [Time and space profiling for non-strict, higher-order functional languages](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.43.6277) is the original

GHC has excellent profiling capabilities. For much of this, we have Patrick M. Sansom to thank; the details of the shindig were in the paper , which was later expanded into a PhD thesis. ([Here](http://stackoverflow.com/a/9068514/23845) is an excellent summary of the relevant work.)

So does this mean one has to acquire a PhD to understand how GHC profiling work? No! While there is a lot of subtlety in the formulation of the semantics of cost centers, the basic idea is quite simple. Whenever code is evaluated, there is a **current cost center**, to which the costs of the evaluation are attributed. By default, the current cost center is `MAIN` (the top-level of the program); the cost-center is set by a **set-cost-center** expression, i.e. an SCC.
