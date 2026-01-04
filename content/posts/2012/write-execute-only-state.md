---
title: "Write/execute-only state"
date: 2012-09-19 16:37:52
slug: write-execute-only-state
draft: true
categories: [Ur/Web]
---

Ur/Web has a somewhat unusual property in that inside a `transaction`, you can write to mutable cells called `source`s, but not read from them
