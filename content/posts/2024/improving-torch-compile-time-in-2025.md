---
title: "Improving torch.compile time in 2025"
date: 2024-11-30 23:38:20
slug: improving-torch-compile-time-in-2025
draft: true
categories: [Miscellaneous]
---

It is a truth universally acknowledged that any compiler in possession of optimizations, must be in want of better compile time performance. Compile times with torch.compile are no exception. There aren't very many extant, consolidated descriptions about what we have been doing to improve torch.compile time, so in this post I want to lay out how we are thinking about the problem, what we've done so far, and where we are going in the short-mid term future.

# The user stories

Training job is restarting.

Inference job deployment.

Local co-development with compiler.

Eager mode use-case.

Online training.

Extremely large training job.

Compiler is load bearing.

# Where are we today?

On the one hand, the trade you are making with torch.compile is simple: in exchange for spending T time for compiling, you get some X potential speedup in subsequent execution. On the other hand, this means that even when the compiler runs extremely slowly, as long as you spend enough time running the compiled code you may end up being positive ROI. This means we don't *actually* have to

# Putting it together
