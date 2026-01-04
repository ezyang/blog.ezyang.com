---
title: "Backpack: the Haddock problem"
date: 2016-09-07 03:19:43
slug: 
draft: true
categories: [Miscellaneous]
---

As the GHC 8.2 merge window moves closer, I've been looking for issues that would block people from using Backpack seriously in the real world; in particular, reasons why people wouldn't be keen on libraries like base taking on Backpack to solve the string problem. Some of the modest problems include Backpack being backwards incompatible with old versions of GHC (painful, but one can always wait or maintain two branches) and needing to adjust the GHC build system to handle Backpack (ew, but it probably can be done with enough ceiling wax, and Hadrian should improve things more.)

But there is one big problem which I do not know what to do about: Haddock documentation. Pull up a chair; this is going to be a long story.

# Architecture of Haddock today
