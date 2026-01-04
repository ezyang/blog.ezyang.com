---
title: "Efficient reactive programming"
date: 2012-11-18 03:17:48
slug: efficient-reactive-programming
draft: true
categories: [Miscellaneous]
---

Reactive programming \[1\] is founded on the proposition that *data changes*, and thus anything which depends on this data should automatically reflect changes to the source data. The benefits of this kind of paradigm can be immediately appreciated anyone who has had to perform any client-side programming involving multiple UI elements that have to be synchronized with one another in some fashion. Naïvely, event handlers for each element need to update the display of all of the other elements, obliterating all modularity in the process.

[D3.js](http://d3js.org/)

------------------------------------------------------------------------

\[1\] I am, alas, not appealing to Conal Elliot’s excellent manifesto on [the meaning of functional reactive programming.](http://stackoverflow.com/questions/1028250/what-is-functional-reactive-programming)
