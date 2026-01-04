---
title: "POPL'07: Operational Semantics for Multi-Language Programs"
date: 2014-01-21 17:08:28
slug: popl07-operational-semantics-for-multi-language-programs
draft: true
categories: [Miscellaneous]
---

Lump embedding. Requires notion of expressions that yield values. Values of other language can be passed around, but you have to cross the barrier to actually manipulate them.

Q: Why doesn't L show up in the typing rules for the extension? It does in the MS rule, since tau can take on anything. It should take on L if it's going to be opaque, but if cancellation occurs you want another type tau.

Interesting footnote: "We could also have introduced two different notations for Scheme and ML application, but we find it inelegant; it suggests that a multi-language implementation would decide how to evaluate each term by inspecting it, when real systems decide how to evaluate a term based on the language in which the term is being evaluated — i.e.. its context. Also, this is the same extension we made in earlier work to model Scheme's multiple values.

Principle: matching boundaries cancellation.

Natural embedding. Higher order function translation worth discussion.

Interesting result: natural embedding is macro-expressible in lump embedding

As Amal puts it: this formalism *controls* the inter-language interactions
