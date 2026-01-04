---
title: "OCaml gotchas"
date: 2011-02-04 09:00:04
slug: ocaml-gotchas
categories: [Haskell, OCaml]
comments:
    - id: 1752
      author: gasche
      date: "2011-02-04 11:51:13"
      content: |
        If you want to use different generators in parallel, avoiding interferences, you may use the Random.State module which is intended for this purpose: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.State.html
        
        Random.State provides a reification of the generator state that can be used independently -- instead of a global generator state. Of course, you then have to pass those local states around, either by hand or using a monadic style.
    - id: 1753
      author: Luke Palmer
      date: "2011-02-04 12:14:24"
      content: "I stopped being annoyed by having to use a monad for random numbers when I stopped thinking of \"Rand a\" as mutable state and started thinking of it as \"a probability distribution of a's\".  It flipped my annoyance upside down -- I don't want something of type Int to be a probability distribution of Ints. If I wanted that, I would have said that :-)"
    - id: 1754
      author: "Jean-Denis Koeck"
      date: "2011-02-04 13:31:44"
      content: "Regarding the begin/end syntax, you can use parentheses as well."
    - id: 1756
      author: Edward Z. Yang
      date: "2011-02-04 17:21:56"
      content: |
        Luke, that’s quite a good way of thinking about it. It makes me wonder now if we can design a random monad that supports symbolic evaluation and simulation.
        
        gasche and Jean-Denis, good points.
    - id: 1757
      author: Arlen
      date: "2011-02-05 06:20:57"
      content: |
        Two alternate ways of putting 'repeat'. I'm not sure if I prefer mine, though.
        
        let rec repeat thunk = function
          | 0 -&gt; ()
          | n -&gt;
            thunk ();
            repeat thunk (pred n)
        
        let rec repeat thunk n =
          if n &gt; 0 then begin
            thunk ();
            repeat thunk (pred n)
          end
            if n = 0 then ()
            else thunk (); repeat (n-1) thunk
    - id: 1758
      author: Thomas M. DuBuisosn
      date: "2011-02-05 13:35:55"
      content: "It's probably worth rephrasing #1 to \"Unlike GHC's Int, which is 32/64 bits\" seeing as Haskell only promises us 30 bits.  This also means that the Haskell \"RandomGen\" class which gives random Ints potentially only give 30 bits of entropy."
    - id: 1762
      author: solrize
      date: "2011-02-08 22:34:07"
      content: |
        My big hassle about Haskell's random monad is that it can't guarantee that you don't run it twice with the same initial seed, an easy error to make that could be fatal to some crypto applications.  I had a chat about this on irc and the conclusion seems to be that doing it the right way would require linear types in the language.  
        
        May I ask what possessed you to start using Ocaml?  How do you like it?  I'm annoyed and disillusioned with Haskell in several ways, but Ocaml seems pretty crude by comparison.  I haven't yet felt pioneerish enough to try DDC.
---

I spent some time fleshing out my [count min sketch](https://github.com/ezyang/ocaml-cminsketch) implementation for OCaml (to be the subject of another blog post), and along the way, I noticed a few more quirks about the OCaml language (from a Haskell viewpoint).

- Unlike Haskell’s `Int`, which is 32-bit/64-bit, the built-in OCaml `int` type is only 31-bit/63-bit. Bit twiddlers beware! (There is a `nativeint` type which gives full machine precision, but it less efficient than the `int` type).

- Semicolons have quite different precedence from the “programmable semicolon” of a Haskell do-block. In particular:

      let rec repeat n thunk =
          if n == 0 then ()
          else thunk (); repeat (n-1) thunk

  doesn't do what you'd expect similarly phrased Haskell. (I hear I'm supposed to use `begin` and `end`.)

- You can only get 30-bits of randomness from the Random module (an positive integer using Random.bits), even when you're on a 64-bit platform, so you have to manually stitch multiple invocations to the generator together.

- I don't like a marching staircase of indentation, so I hang my “in”s after their statements—however, when they’re placed there, they’re easy to forget (since a `let` in a do-block does not require an `in` in Haskell).

- Keyword arguments are quite useful, but they gunk up the type system a little and make it a little more difficult to interop keyword functions and non-keyword functions in a higher-order context. (This is especially evident when you're using keyword arguments for documentation purposes, not because your function takes two ints and you really do need to disambiguate them.)

One observation about purity and randomness: I think one of the things people frequently find annoying in Haskell is the fact that randomness involves mutation of state, and thus be wrapped in a monad. This makes building probabilistic data structures a little clunkier, since you can no longer expose pure interfaces. OCaml is not pure, and as such you can query the random number generator whenever you want.

However, I think Haskell may get the last laugh in certain circumstances. In particular, if you are using a random number generator in order to generate random test cases for your code, you need to be able to reproduce a particular set of random tests. Usually, this is done by providing a seed which you can then feed back to the testing script, for deterministic behavior. But because OCaml's random number generator manipulates global state, it's very easy to accidentally break determinism by asking for a random number for something unrelated. You can work around it by manually bracketing the global state, but explicitly handling the randomness state means providing determinism is much more natural.
