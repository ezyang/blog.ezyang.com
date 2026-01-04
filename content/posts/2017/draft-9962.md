---
title: "ASTs in C++"
date: 2017-06-26 22:42:35
slug: 
draft: true
categories: [Miscellaneous]
---

I have recently been put into a position where I am writing an abstract syntax tree in C++ (cough cough). It is a very strange experience: whereas in Haskell or ML there is one obvious way to go about defining the AST ([well, maybe there is some nuance to it...](http://blog.ezyang.com/2013/05/the-ast-typing-problem/)), in C++ there are a dizzying number of options for how to go about setting up the definition of your AST. To try to get a sense for what people commonly do, I looked at the AST structures of [LLVM](https://github.com/llvm-mirror/llvm/tree/master/include/llvm/IR), [Swift](https://github.com/apple/swift/tree/master/lib/AST) and [Web Assembly](https://github.com/WebAssembly/binaryen/blob/master/src/wasm.h), three reasonably modern compilers written in C++. To assist in comparing the IR styles, I wrote [Hutton's Razor](https://stackoverflow.com/questions/17870864/where-is-huttons-razor-first-defined) in the respective style of the compiler.

# Web Assembly

[Web Assembly's IR](https://github.com/WebAssembly/binaryen/blob/master/src/wasm.h) is perhaps the simplest of all three.
