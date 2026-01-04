---
title: "Designing a bytecode representation"
date: 2021-08-23 21:45:49
slug: designing-a-bytecode-representation
draft: true
categories: [Miscellaneous]
---

Suppose I have some sort of programming language, e.g., :

    e ::= l
        | e + e
        | e * e

and I want to serialize a representation of programs in this language so that I can execute it later. Furthermore, let's assume that we have these values:

- **Performance over human readability.** I could serialize a human readable format in ASCII (e.g., the original source code, or something like JSON or YAML), but instead I will sacrifice this readability if it means that my language can perform better.
- **Portability over performance.** I will not just compile this program to x86 and ship the binaries directly, because this is not portable to other platforms. Instead, I want to define a representation for which I can write an interpreter in any target architecture.
- **Customizability over ecosystem.** We're going actually to define our own bytecode, instead of reusing one of the (many) preexisting bytecodes, because we think we know something about our domain and can come up with a design that better suits our requirements than a preexisting bytecode.

Even with these ground rules, we still have a large amount of latitude in how we could define such a serialization. Here are some things that we might find desirable in our IR, which are often in tension with other desiderata:

- **No parsing.** Ideally, we'd like our representation to be directly interpretable after being mapped to memory, rather than requiring some sort of parsing phase. This trades off **ease of manipulation**, since the instructions will be represented as one giant block of bytecode so you won't be able to easily insert/modify instructions; not a big deal in most interpreters, but a bigger problem if you want to feed it to another compiler! Furthermore, it may still be necessary to validate the bytecode in contexts where the input bytecode is **untrusted**.
- **Compact representation.** We'd like our representation to be as small as possible, so it has good memory locality; maybe in extremely memory limited environments having small code also helps you stay within your memory budget or reduce network traffic. Getting a really small representation, however, might tradeoff with **complexity of the representation**; for example, in ISAs, it has been found that CISC programs tend to be 25% smaller than RISC programs [(Weaver 2009)](http://web.eece.maine.edu/~vweaver/papers/iccd09/iccd09_density.pdf).
- **Easy to generate.** If we're designing a
