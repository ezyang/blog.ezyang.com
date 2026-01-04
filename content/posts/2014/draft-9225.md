---
title: "Tracing the compilation of Hello Factorial! (Cabal edition)"
date: 2014-09-18 16:16:19
slug: 
draft: true
categories: [Miscellaneous]
---

A while ago, I wrote an article on [tracing the compilation of factorial](http://blog.ezyang.com/2011/04/tracing-the-compilation-of-hello-factorial/), as a way to give readers a taste of how the internals of GHC worked. Now, while GHC is a very important and interesting part of the compilation pipeline, it’s by no means the end of the story: in all likelihood, you are not calling `ghc` to compile your project, but rather `cabal`. Cabal actually does quite a bit of work under the hood to figure out how to compile your program. Shall we take a look?

# The package

Let us start with the
