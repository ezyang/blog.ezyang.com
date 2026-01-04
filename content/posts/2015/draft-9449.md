---
title: "Rethinking the compiler/build system distinction"
date: 2015-11-30 04:01:53
slug: 
draft: true
categories: [Miscellaneous]
---

What is a compiler? Naively,

, a compiler is a program which transforms source code into object code, no more.

This conventional definition also means that a compiler is not very useful by itself: the object code must be linked together to form an actual executable, you must determine what order to compile your source files in, etc.

Not eto self: check if Go loads the transitive closure of packages something depends on. Can check using strace. If not, check if Go builds up the interface with stuff it depends on.
