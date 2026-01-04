---
title: "Persistence, not laziness, is the problem"
date: 2011-06-21 10:40:11
slug: persistence-not-laziness
draft: true
categories: [Haskell]
---

Haskell is a pretty remarkable language. Though lazy evaluation and purely functional programming have been found in a variety of other contexts, Haskell is in many ways the first widely used language to take both of these concepts to the very limit. In this light, it might not be surprising that we haven’t fully distilled and publicized all of the academic folklore about these programming models in a more industrial context. Writing efficient code in these settings requires a different mindset than in a strict, mutable context—not necessarily harder, just different. In this essay, I argue that the mindset change for *persistence* is greater and more pervasive than the mindset change for *laziness*.
