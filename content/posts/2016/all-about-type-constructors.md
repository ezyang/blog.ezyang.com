---
title: "All about type constructors"
date: 2016-10-14 17:01:50
slug: all-about-type-constructors
draft: true
categories: [Miscellaneous]
---

\-*- mode: rst -*-

Fact: You can't define type lambdas in Haskell. This restriction is also referred to as generativity, namely the property that given `t a ~ s b`, we have `t ~ s` and `a ~ b`. To see why this restriction is important, consider this code: `return True :: Maybe []`
