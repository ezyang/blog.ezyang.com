---
title: "Technical challenges of Inventing on Principle"
date: 2012-02-20 16:41:41
slug: technical-challenges-of-inventing-on-principle
draft: true
categories: [Miscellaneous]
---

In [Inventing on Principle](http://vimeo.com/36579366), Bret Victor describes his guiding principle, “creators need an immediate connection to what they create.” To illustrate this, he provides several demos ranging from video games to program execution visualization. In this analysis, I would like to elucidate some of the technical challenges of realizing this vision and suggest a number of ways to overcome these problems. We'll draw from programming languages (e.g. eliminating mutable state), algorithms and data structures (e.g. retroactive data structures), systems (e.g. inotify) and other fields. One reason Bret's demos are so impressive is that many of the features demonstrated are *hard to implement in generality*. Using his principle to tell us what features are important, we roll up our sleeves and consider what engineering tools are needed to make these features happen.

# Cherry blossom tree

<http://worrydream.com/DynamicPicturesMotivation/> Dependencies, dealing with sparse parameter space Compilation must be fast: play as many stupid tricks here as possible There is an emphasis on exploration of the space based on permutation of source code; in some sense, the source code must support the entirety of the space being explored Demonstrates average case behavior, not worst case behavior

# Video game

<http://worrydream.com/LadderOfAbstraction/> Going backwards in time is kind of hard (persistent data structures) Making changes and then efficiently recalculating forward is hard (retroactive time; actually we're just looking at persistent)
