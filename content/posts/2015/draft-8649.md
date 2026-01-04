---
title: "Iteration/recursion Rust semantic properties"
date: 2015-03-02 17:26:54
slug: 
draft: true
categories: [Miscellaneous]
---

It’s well known that iteration can be converted into a tail-recursive form, and vice versa, by observing the correspondence between *loop variables* and *stack arguments*. Ordinarily, this kind of transformation is only of interest to functional programmers, who tend to use languages that don’t have any built-in iteration construct. However,
