---
title: "The single export pattern"
date: 2013-03-31 20:39:41
slug: the-single-export-pattern
categories: [Programming Languages]
comments:
    - id: 5988
      author: "Sam Tobin-Hochstadt"
      date: "2013-03-31 23:44:19"
      content: "The name \"single export\" is used, at a minimum, in the node community, which is where the people on TC39 that you heard get it from."
    - id: 5990
      author: Edward Z. Yang
      date: "2013-04-01 04:45:44"
      content: "That makes sense. I drew the inference of not very wide usage from the fact that Google searches for \"single import\" bring up TC39 discussions as the top results (and even then, not very top)."
---

<div class="container center">

*From the files of the ECMAScript TC39 proceedings*

</div>

**Single export** refers to a design pattern where a module identifier is overloaded to also represent a function or type inside the module. As far as I can tell, the term “single export” is not particularly widely used outside the ECMAScript TC39 committee; however, the idea shows up in other contexts, so I’m hoping to popularize this particular name (since names are powerful).

The basic idea is very simple. In JavaScript, a module is frequently represented as an object:

    var sayHello = require('./sayhello.js');
    sayHello.run();

The methods of `sayHello` are the functions exported by the module. But what about `sayHello` itself? Because functions are objects too, we could imagine that `sayHello` was a function as well, and thus:

    sayHello()

would be a valid fragment of code, perhaps equivalent to `sayHello.run()`. Only one symbol can be exported this way, but in many modules, there is an obvious choice (think of jQuery’s `$` object, etc).

This pattern is also commonly employed in Haskell, by taking advantage of the fact that types and modules live in different namespaces:

    import qualified Data.Map as Map
    import Data.Map (Map)

`Map` is now overloaded to be both a type and a module.
