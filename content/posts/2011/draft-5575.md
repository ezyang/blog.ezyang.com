---
title: "Moving pattern matching up a PEG"
date: 2011-07-02 23:42:11
slug: 
draft: true
categories: [Miscellaneous]
---

I’m a huge fan of pattern matching on algebraic data types: the clarity and safety benefits are absolutely astounding. (A friend of mine is [hacking up pattern matching for C++](https://github.com/blackhole89/algebraiccpp), his programming lineage being C++ and Standard ML.) So when I ran across [OMeta](http://tinlizzie.org/ometa/) which promised *better* pattern matching, I thought, “Huh, how can pattern matching get better?”

One motivating example for OMeta is the fact that, while lexing and parsing are *like* pattern matching, most people don’t actually use classic ML-style pattern matching to implement them. As Alessandro Warth’s original paper notes, “it is not expressive enough on its own to support more complex pattern matching tasks such as lexical analysis and parsing.” This claim, which did not have a nice little footnote attached to it, is what I’d like to dissect. What features of parsing tend to be problematic for traditional pattern matching?

- Syntax ambiguity. In normal function application, a successful pattern match has the final word: any further patterns never get matched. In a context-free grammar, an ambiguous input string may have multiple valid parse trees.
- Backtracking.

Backtracking is the big one. Can we use Conal's unamb to do this? Problem: we can’t properly prefer a left non-bottom, since being strict on the left side means we go to bottom if it is bottom. So no simple semantics.

<http://tinlizzie.org/ometa/> <http://www.moserware.com/2008/06/ometa-who-what-when-where-why.html>

CFG versus PEG, oh hey, it's packrat parsers!

*By the way.* I think calling OMeta an object-oriented pattern matching language is a bit unfortunate. The key feature that OMeta draws from its object oriented lineage is *extensibility.*
