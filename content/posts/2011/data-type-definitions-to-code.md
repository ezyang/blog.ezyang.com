---
title: "From data type definitions to code"
date: 2011-07-25 09:00:48
slug: data-type-definitions-to-code
categories: [OCaml]
comments:
    - id: 2854
      author: Sebastian
      date: "2011-07-25 15:40:34"
      content: |
        Manipulating the AST of a core language instead may be more difficult for a non-language-expert than manupilating the AST of the whole language.
        He already knows how to program in the whole language, he doesn't for the core language.
        
        Maybe it would be most convenient if the "input" to the transformations was the core language (so you don't need to handle too much cases) but the "output" could be the whole language.
    - id: 2855
      author: Edward Z. Yang
      date: "2011-07-25 15:51:18"
      content: "Ah yes, you put your finger on a good point. The core language is easy to do analysis over (with 100% certainty if it's a strict subset of the original language, which is a design goal, I'd hope), but the whole language is more convenient to do output for. The suggestion seems reasonable, although the compiler would have to reconvert it back to core if another pass wants to run over this code."
    - id: 2857
      author: Sean
      date: "2011-07-26 00:18:12"
      content: I came to a similar conclusion in my paper Optimising Embedded DSLs using Template Haskell
---

What do these problems have in common: recursive equality/ordering checks, printing string representations, serializing/unserializing binary protocols, hashing, generating getters/setters? They are repetitive boilerplate pieces of code that have a strong dependence on the structure of the data they operate over. Since programmers love automating things away, various schools of thought have emerged on how to do this:

1.  Let your IDE generate this boilerplate for you. You right-click on the context menu, click “Generate `hashCode()`”, and your IDE does the necessary program analysis for you;
2.  Create a custom metadata format (usually XML), which you then run another program on which converts this description into code;
3.  Add sufficiently strong macro/higher-order capabilities to your language, so you can write programs which generate implementations in-program;
4.  Add sufficiently strong reflective capabilities to your language, so you can write a fully generic dynamic implementation for this functionality;
5.  Be a compiler and do static analysis over abstract syntax trees in order to figure out how to implement the relevant operations.

It hadn’t struck me how prevalent the fifth option was until I ran into wide scale use of one particular facet of the [camlp4](http://caml.inria.fr/pub/old_caml_site/camlp4/index.html) system. While it describes itself as a “macro system,” in [sexplib](http://caml.inria.fr/cgi-bin/hump.en.cgi?contrib=474) and [bin-prot](http://caml.inria.fr/cgi-bin/hump.en.cgi?contrib=642), the macros being used are not those of the C tradition (which would be good for implementing 3), rather, they are in the Lisp tradition, including access to the full syntax tree of OCaml and the ability to modify OCaml’s grammar. Unlike most Lisps, however, camlp4 has access to the abstract syntax tree of *data type definitions* (in untyped languages these are usually implicit), which it can use to transform into code.

One question that I’m interested in is whether or not this sort of metaprogramming can be made popular with casual users of languages. If I write code to convert a data structure into a Lisp-like version, is a logical next step to generalize this code into metaprogramming code, or is that a really large leap only to be done by extreme power users? At least from a user standpoint, camlp4 is extremely unobtrusive. In fact, I didn’t even realize I was using it until a month later! Using sexplib, for example, is a simple matter of writing:

    type t = bar | baz of int * int
      with sexp

Almost magically, `sexp_of_t` and `sexp_to_t` spring into existence.

But defining new transformations is considerably more involved. Part of the problem is the fact that the abstract-syntax tree you are operating over is quite complex, the unavoidable side-effect of making a language nice to program in. I could theoretically define all of the types I cared about using sums and products, but real OCaml programs use labeled constructors, records, anonymous types, anonymous variants, mutable fields, etc. So I have to write cases for all of these, and that’s difficult if I’m not already a language expert.

A possible solution for this is to define a simpler, core language on which to operate over, much the same way GHC Haskell compiles down to Core prior to code generation. You can then make the extra information available through an annotations system (which is desirable even when you have access to the full AST.) If the idea is fundamentally simple, don’t force the end-user to have to handle all of the incidental complexity that comes along with making a nice programming language. Unless, of course, they want to.

*Postscript.* One of the things I’m absolutely terrible at is literature search. As with most ideas, it’s a pretty safe bet to assume someone else has done this already. But I couldn’t find any prior art here. Maybe I need a better search query than “intermediate language for metaprogramming.”
