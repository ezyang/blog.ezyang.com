---
title: "Hoopl: Dataflow transformations"
date: 2011-06-17 08:16:54
slug: 
draft: true
categories: [Hoopl]
---

In the long awaited finale to my series on Hoopl, I finally describe how Hoopl rewrite functions work. As you might expect, these functions use the facts produced by the transfer functions in order to determine if certain transformations are safe. However, the innovation of Hoopl is that these transformations can, themselves, affect the analysis on the program. We’ll walk through a few transformations to get a feel for what is going on under the hood.

------------------------------------------------------------------------

Hoopl encourages
