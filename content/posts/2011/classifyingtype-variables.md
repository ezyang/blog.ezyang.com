---
title: "Classifying type variables"
date: 2011-10-01 19:34:23
slug: classifyingtype-variables
draft: true
categories: [Haskell]
---

A **type variable** is a variable ranging over types, e.g. the `a` in the type signature `id :: a -> a`. The simplest way we can classify type variables is by **kind**. Primitive types like `Int` and `Bool`, which can be used to specify the type of an expression, have the kind `*` (pronounced star); more complex type constructors like `Maybe` have kinds like `* -> *`.

That’s all very well and fine, but GHC rarely sticks to just the term “type variable” when generating error messages about these beasts. You may see such exotic terms like “flattening type variable” or “interactive-debugger skolem”. What the heck are those? In this post, I’ll try to explain all of these terms and more.

# Skolem type variable

# Ambiguous type variable

# Escaping type variable

# Untouchable type variable

# Flattening type variable

# Interactive-debugger type variable

# Rigid type variable
