---
title: "Domain specific languages for cryptography"
date: 2014-04-09 22:58:13
slug: 
draft: true
categories: [Programming Languages]
---

With the recent [Heartbleed](http://heartbleed.com/) OpenSSL vulnerability making the waves around the Internet, PL wonks have been smugly thinking, “If only OpenSSL were implemented in a *real* programming language, such a silly memory disclosure bug would have been avoided.” Today, I want to explore this assertion in more detail: let’s suppose that OpenSSL, and other cryptographic software like it, should be implemented in a programming language that is not C. What are desirable characteristics of such a language? In particular, should cryptographic software be written in a domain specific language specialized for cryptography, or will a general purpose language do?

Here is a standard list of objections which might be trotted out for a prospective reimplementation of OpenSSL:

- You’re implementing your own crypto, so you just lose
- Your implementation is vulnerable to timing attacks (or perhaps it is not possible to eliminate timing attacks from your programming language)
- Your implementation does not zero sensitive data after it is done being used
- Your implementation is not fast enough/does not have enough throughput
- Your implementation is not a drop-in replacement for OpenSSL
- Your choice of PL is not mature enough to support a mission-critical software development
- Your PL is not supported by the FFI of most languages
- Your PL requires garbage collection
- Your PL is vulnerable to memory safety vulnerabilities
- Your PL is not fast enough

Some of these objections are just unsatisfiable (cryptography implementation: it's turtles all the way down), while other individually reasonable objections, together, are very difficult to fulfill. For example, statically ensuring memory safety without garbage collection in a general purpose language is a research level problem—and the most realistic contender in this space, Rust, would be excluded if you required the language to be “mature.” Alternately, if you opted for dynamic checks, you might find that such implementations were too slow or your language didn’t give you enough rope to make these checks mandatory. An orthogonal issue is the question of FFIs and interfaces: if your library is not written in C, it is a fact of life that it is going to be considered a second-class library, as far as the ecosystem is concerned.

Here’s one interesting observation, however:

I have a lot of cryptographer friends,

(formal method wonks) (memory management, lack of complicated pointer stuff) (thinking about cryptographic software in general) (why is C the standard? it's the lingua franca of programming languages. it seems you want to compile to C) (Cryptol: tradition of compiling to hardware as well) (we shouldn't reimplement things; API backwards-compatibility) (reducing the size of code) (Ada, Rust, ATS)
