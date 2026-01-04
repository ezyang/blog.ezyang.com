---
title: "A thought on low latency interpreters"
date: 2017-04-09 21:58:33
slug: a-thought-on-low-latency-interpreters
draft: true
categories: [Miscellaneous]
---

A [low latency interpreter](http://people.csail.mit.edu/jaffer/CNS/interpreter-latency) is an interpreter for which you can make changes to the source program and very quickly restart your application. A low latency interpreter doesn't have to be particularly fast at executing code, but it should be fast at executing the *first* line of code.

An interesting
