---
title: "You could have invented dependent pattern match"
date: 2013-08-22 20:01:15
slug: dependent-pattern-match
draft: true
categories: [Miscellaneous]
---

Perhaps one of the most mysterious language constructs in Coq is the dependent pattern match, of the form:

    match E as y in (T x1 ... xn) return U with
      | C z1 ... zm => B
      | ...
    end

What is all of this `as y in (T x1 ... xn) return U` nonsense about? You could consult the [Coq manual](http://coq.inria.fr/distrib/current/refman/Reference-Manual003.html#sec38) about it, but while the explanation it gives is technically complete, I find that a lot of people find it very unen
