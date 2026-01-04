---
title: "OCaml for Haskellers"
date: 2010-10-25 09:00:54
slug: ocaml-for-haskellers
categories: [Haskell, Programming]
comments:
    - id: 1342
      author: Etienne Millon
      date: "2010-10-25 10:13:27"
      content: |
        &gt; note that in OCaml the value actually a tuple, so you'd need Node (v,l,r) to match
        
        Not quite. `A of (t1 * t2 * t3)` and `A of t1 * t2 * t2` are different things. The first one is indeed a unary constructor (built from a tuple) whereas the second one is a ternary constructor (taking three arguments).
        
        For pattern matching, both will work with `A (x, y, z)` but the latter cannot be matched with something like `A xyz` (xyz having type `t1 * t2 * t3`). The runtime representation will also be different (the tuple version will have an extra indirection level).
    - id: 1343
      author: Edward Z. Yang
      date: "2010-10-25 10:17:57"
      content: "Thanks for the correction Etienne! I've fixed it."
    - id: 1344
      author: solrize
      date: "2010-10-25 10:45:19"
      content: |
        I'd be interested to know why you've decided to learn Ocaml (or any other ML), and how the programming experience (rather than just the language features) compares with Haskell.
        
        http://adam.chlipala.net/mlcomp/ (Ocaml vs SML comparison) might also interest you if you haven't seen it.
    - id: 1347
      author: Anonymous
      date: "2010-10-25 16:13:01"
      content: "I believe OCaml's records use a single : instead of :: for its fields."
    - id: 1348
      author: p4bl0
      date: "2010-10-25 16:15:26"
      content: |
        &gt; OCaml has native objects (not covered in this post).
        
        For everyone's sake and for OCaml's good (which I do not necessarily cant), it's better to ignore OCaml object layer. Actually there's no such thing.
        
        For OCaml records you typed
        
        &gt; type rec = { x :: int; y :: int };;
        
        but I believe it's "type rec = { x : int; y : int };;" (with only one colon, plus the ";;" part is optional as usual).
        
        Otherwise, interesting post :-).
    - id: 1349
      author: Edward Z. Yang
      date: "2010-10-25 16:19:15"
      content: |
        Yep, that's right. Fixed. (I hear Jane Street doesn't use OCaml's object layer for their work, so I can believe you.)
        
        solrize: I'll have to get back to you on that... later.
    - id: 1350
      author: Jason Dusek
      date: "2010-10-26 00:02:58"
      content: |
        Are these two really the same?
        
        case f x of
            y | x == 1 || x == 2 -&gt; y
        
        match f x with
            | (1 | 2) as y -&gt; y
    - id: 1351
      author: Ivan
      date: "2010-10-26 06:25:55"
      content: |
        Very nice to have them both compared in such a clear and concise way.
        
        I'm also looking forward for your answer to solrize's question.
        
        
        Thank you.
    - id: 1353
      author: Jon Harrop
      date: "2010-10-26 09:32:54"
      content: |
        You probably want = and  for (structural) equality and inequality in OCaml. The == and != operators are reference equality for more advanced uses (e.g. optimization).
        
        The main benefit of polymorphic variants in practice is that their types are inferred and not that they can be open.
        
        Regarding tail recursion, some of Haskell's standard library functions (e.g. getElems) stack overflow because they are not tail recursive. So I would not say that "you do not have to worry about tail calls in Haskell".
        
        Some major OCaml libraries rely heavily upon the object system. For example, LablGTK and PXP.
        
        Otherwise, excellent article!
    - id: 1354
      author: Edward Z. Yang
      date: "2010-10-26 12:26:57"
      content: |
        Jason, yes, unless there is a subtle semantic difference in an edge case that I don't know about.
        
        <pre># match 1 with (1 | 2) -> 4;;
        Warning P: this pattern-matching is not exhaustive.
        Here is an example of a value that is not matched:
        0
        - : int = 4
        # match 2 with (1 | 2) -> 4;;
        Warning P: this pattern-matching is not exhaustive.
        Here is an example of a value that is not matched:
        0
        - : int = 4
        # match 3 with (1 | 2) -> 4;;
        Warning P: this pattern-matching is not exhaustive.
        Here is an example of a value that is not matched:
        0
        Exception: Match_failure ("", 5, -26).</pre>
        
        Jon, thanks for the corrections, I've updated the article. Note that getElems is actually strict. :-)
    - id: 1367
      author: roy_hu
      date: "2010-10-28 02:19:26"
      content: |
        Looks like you wanted to say
        
        case f x of
        y | y == 1 || y == 2 -&gt; y
        
        instead of
        
        case f x of
        y | x == 1 || x == 2 -&gt; y
    - id: 1368
      author: roy_hu
      date: "2010-10-28 02:20:16"
      content: "Yes, Jane Street is not using objects, but mylife.com is using objects heavily."
    - id: 1369
      author: "Russell O'Connor"
      date: "2010-10-28 15:02:35"
      content: "I just learned that record updates using \"with\" require braces around the expression.  You may wish to update your post to reflect this."
    - id: 1370
      author: Edward Z. Yang
      date: "2010-10-28 17:16:33"
      content: "Fixed and fixed. Thanks!"
    - id: 1373
      author: "Russell O'Connor"
      date: "2010-10-28 22:22:21"
      content: |
        I believe the braces go as {r with  x = 2}
        
        At least that is what I wrote in my code and it compiled.
    - id: 1374
      author: Edward Z. Yang
      date: "2010-10-28 22:29:38"
      content: "Fixed again. :-)"
    - id: 2441
      author: Xuan Luo
      date: "2011-05-17 04:19:28"
      content: "The most direct equivalents for == and /= in Haskell are = and  in OCaml. These test for value equality. The operators == and != in OCaml are almost never used; they test for physical equality, for values that are boxed; for values that are not boxed (int/char/bool) it is the same as =/. (Physical equality does not exist in Haskell, probably because it doesn't make sense with laziness and referential transparency.)"
    - id: 2442
      author: Xuan Luo
      date: "2011-05-17 04:23:01"
      content: "Crap the inequality operator \"less-than greater-than\" got lost in the previous post."
    - id: 2443
      author: Xuan Luo
      date: "2011-05-17 04:37:19"
      content: "The equivalent of float from OCaml in Haskell is probably \"Double\". OCaml's library documentation says that \"float\" is double-precision: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#6_Floatingpointarithmetic"
    - id: 2444
      author: Xuan Luo
      date: "2011-05-17 04:41:47"
      content: "The best equivalent for \"int_of_float\" from OCaml in Haskell is probably \"truncate\" not \"floor\", since int_of_float rounds towards 0"
    - id: 2445
      author: Xuan Luo
      date: "2011-05-17 04:51:34"
      content: |
        Some more suggestions:
        
        initially opened module:
        Prelude
        Pervasives
        (both weird names)
        
        documentation comment:
        {-| ... -} (for Haddock)
        (** ... *) (for OCamldoc)
        
        list operations:
        x : xs
        x :: xs
        
        a ++ b
        a @ b
        
        a !! n
        List.nth a n
        
        length a
        List.length a
        
        head a
        List.hd a
        
        tail a
        List.tl a
        
        reverse a
        List.rev a
        
        concat a
        List.concat a
        
        map f a
        List.map f a
        
        zipWith f a b
        List.map2 f a b
        
        foldl f z a
        List.fold_left f z a
        
        foldr f z a
        List.fold_right f a z (note the different ordering of arguments)
        
        filter f a
        List.filter f a
        
        any f a
        List.exists f a
        
        all f a
        List.for_all f a
        
        x `elem` a
        List.mem x a
        
        zip a b
        List.combine a b
        
        unzip a
        List.split a
        
        sortBy cmp a
        List.sort cmp a
        (whereas cmp in Haskell returns an Ordering type, cmp in OCaml returns an int, where negative, zero, positive means less than, equal, greater than, à la strcmp in C)
    - id: 2446
      author: Xuan Luo
      date: "2011-05-17 05:22:37"
      content: |
        A note about equality and comparison operators:
        
        In Haskell, the == and /= operators are defined in the type class Eq, and the , =, compare, are defined in the type class Ord. To make these operators apply to a type, you must either make an instance declaration providing the implementation of these operators for your type, or you can make a "deriving" declaration when the type is declared, in which case a default, language-provided, recursive comparator is used. If you don't do either of these, these operators cannot be used on your type.
        
        In OCaml, the situation is different. It is as if ALL types automatically and irreversibly use the "deriving" declaration for the type classes Eq and Ord. In other words, 1) the operators, ==, &lt;, etc. can be used on any type in OCaml, automatically, even for types for which it doesn&#039;t seem to make sense. And 2) you cannot customize the ordering of these operators for specific types. However, places in the library which use orderings, such as List.sort, or the Map and Set ordered tree structures, allow you to specify the comparator used.
    - id: 6081
      author: Anonymous
      date: "2013-05-08 23:13:27"
      content: |
        I know this is an old post, but the following example you give is not grammatically valid because "rec" is a keyword and hence not a valid name for a type:
        
        type rec = { x : int; y : int };;
    - id: 6082
      author: Edward Z. Yang
      date: "2013-05-09 00:28:10"
      content: "Thanks, fixed."
    - id: 7275
      author: Andrew Pennebaker
      date: "2014-07-31 17:09:06"
      content: |
        &gt; By default all modules are automatically “imported” like import qualified Foo (no import list necessary).
        
        And how do we actually do this? This is surprisingly hard to Google, and Yang forgot to give a code snippet.
        
        If you DO want to import a library qualified, here's the syntax:
        
        module ShortForm = LibraryName
        
        where "ShortForm" is up to you, and "LibraryName" is what you want to import qualified.
        
        Here's a fully fleshed out example:
        
        https://github.com/mcandre/ios7crypt/tree/master/ocaml
    - id: 7639
      author: "関数型プログラマのためのRust | POSTD"
      date: "2014-08-27 03:35:12"
      content: "[&#8230;] Z. Yangが2010年に書いたOCaml for Haskellers、私自身が今年頭に書いたHaskell for OCaml [&#8230;]"
    - id: 20741
      author: "Bucket List &#8211; hiogawa&#039;s wordpress"
      date: "2016-04-18 08:51:59"
      content: "[&#8230;] Ocaml for Haskellers [&#8230;]"
    - id: 21993
      author: "Coq 05/21/2017 &#8211; EEM"
      date: "2017-05-21 17:01:45"
      content: "[&#8230;] OCaml for Haskellers : Inside 214-1E [&#8230;]"
    - id: 32908
      author: runeks
      date: "2024-07-09 04:31:50"
      content: "Would be nice for your list to also have the OCaml-equivalent of Haskell's `undefined` and/or `error` (if they exist). Very useful when writing new code and wanting to temporarily make the type system happy."
    - id: 33680
      author: Edward Z. Yang
      date: "2024-10-04 16:48:25"
      content: "Raise an exception in OCaml!"
---

I’ve started formally learning OCaml (I’ve been reading ML since Okasaki, but I’ve never written any of it), and here are some notes about differences from Haskell from Jason Hickey's *Introduction to Objective Caml*. The two most notable differences are that OCaml is *impure* and *strict.*

------------------------------------------------------------------------

*Features.* Here are some features OCaml has that Haskell does not:

- OCaml has named parameters (`~x:i` binds to `i` the value of named parameter `x`, `~x` is a shorthand for `~x:x`).
- OCaml has optional parameters (`?(x:i = default)` binds `i` to an optional named parameter `x` with default `default`).
- OCaml has open union types (`[> 'Integer of int | 'Real of float]` where the type holds the implementation; you can assign it to a type with `type 'a number = [> 'Integer of int | 'Real of float] as a`). Anonymous closed unions are also allowed (`[< 'Integer of int | 'Real of float]`).
- OCaml has mutable records (preface record field in definition with `mutable`, and then use the `<-` operator to assign values).
- OCaml has a module system (only briefly mentioned today).
- OCaml has native objects (not covered in this post).

------------------------------------------------------------------------

*Syntax.* Omission means the relevant language feature works the same way (for example, let `f x y = x + y` is the same)

Organization:

    {- Haskell -}
    (* OCaml *)

Types:

    ()   Int Float Char String Bool (capitalized)
    unit int float char string bool (lower case)

Operators:

    == /= .&.  .|. xor  shiftL shiftR complement
    = == != land lor lxor [la]sl [la]sr lnot

(arithmetic versus logical shift in Haskell depends on the type of the Bits.)

Float operators in OCaml: affix period (i.e. `+.`)

Float casting:

    floor fromIntegral
    int_of_float float_of_int

String operators:

    ++ !!i
    ^  .[i] (note, string != char list)

Composite types:

    (Int, Int)  [Bool]
    int * int   bool list

Lists:

    x :  [1, 2, 3]
    x :: [1; 2; 3]

Data types:

    data Tree a = Node a (Tree a) (Tree a) | Leaf
    type 'a tree = Node of 'a * 'a tree * 'a tree | Leaf;;

(note that in OCaml you'd need `Node (v,l,r)` to match, despite there not actually being a tuple)

Records:

    data MyRecord = MyRecord { x :: Int, y :: Int }
    type myrecord = { x : int; y : int };;
    Field access:
        x r
        r.x
    Functional update:
        r { x = 2 }
        { r with x = 2 }

(OCaml records also have destructive update.)

Maybe:

    data Maybe a = Just a | Nothing
    type 'a option = None | Some of 'a;;

Array:

    readArray a i  writeArray a i v
    [|1; 3|] a.(i)          a.(i) <- v

References:

    newIORef writeIORef readIORef
    ref      :=         !

Top level definition:

    x = 1
    let x = 1;;

Lambda:

    \x y -> f y x
    fun x y -> f y x

Recursion:

    let     f x = if x == 0 then 1 else x * f (x-1)
    let rec f x = if x == 0 then 1 else x * f (x-1)

Mutual recursion (note that Haskell let is always recursive):

    let f x = g x
        g x = f x
    let rec f x = g x
    and     g x = f x

Function pattern matching:

    let f 0 = 1
        f 1 = 2
    let f = function
        | 0 -> 1
        | 1 -> 2

(note: you can put pattern matches in the arguments for OCaml, but lack of an equational function definition style makes this not useful)

Case:

    case f x of
        0 -> 1
        y | y > 5 -> 2
        y | y == 1 || y == 2 -> y
        _ -> -1
    match f x with
        | 0 -> 1
        | y when y > 5 -> 2
        | (1 | 2) as y -> y
        | _ -> -1

Exceptions:

    Definition
        data MyException = MyException String
        exception MyException of string;;
    Throw exception
        throw (MyException "error")
        raise (MyException "error")
    Catch exception
        catch expr $ \e -> case e of
            x -> result
        try expr with
            | x -> result
    Assertion
        assert (f == 1) expr
        assert (f == 1); expr

Undefined/Error:

    error "NYI"
    failwith "NYI"

Build:

    ghc --make file.hs
    ocamlopt -o file file.ml

Run:

    runghc file.hs
    ocaml file.ml

------------------------------------------------------------------------

*Type signatures.* Haskell supports specifying a type signature for an expression using the double colon. OCaml has two ways of specifying types, they can be done inline:

    let intEq (x : int) (y : int) : bool = ...

or they can be placed in an interface file (extension `mli`):

    val intEq : int -> int -> bool

The latter method is preferred, and is analogous to an `hs-boot` file as [supported by GHC](http://www.haskell.org/ghc/docs/6.10.2/html/users_guide/separate-compilation.html#mutual-recursion).

------------------------------------------------------------------------

*Eta expansion.* Polymorphic types in the form of `'_a` can be thought to behave like Haskell’s monomorphism restriction: they can only be instantiated to one concrete type. However, in Haskell the monomorphism restriction was intended to avoid extra recomputation for values that a user didn’t expect; in OCaml the value restriction is required to preserve the soundness of the type system in the face of side effects, and applies to functions too (just look for the tell-tale `'_a` in a signature). More fundamentally, `'a` indicates a generalized type, while `'_a` indicates a concrete type which, at this point, is unknown—in Haskell, all type variables are implicitly universally quantified, so the former is always the case (except when the monomorphism restriction kicks in, and even then no type variables are ever shown to you. But OCaml requires monomorphic type variables to not escape from compilation units, so there is a bit of similarity. Did this make no sense? Don’t panic.)

In Haskell, we’d make our monomorphic value polymorphic again by specifying an explicit type signature. In OCaml, we generalize the type by eta expanding. The canonical example is the `id` function, which when applied to itself (`id id`) results in a function of type `'_a -> '_a` (that is, restricted.) We can recover `'a -> 'a` by writing `fun x -> id id x`.

There is one more subtlety to deal with OCaml’s impurity and strictness: eta expansion acts like a thunk, so if the expression you eta expand has side effects, they will be delayed. You can of course write `fun () -> expr` to simulate a classic thunk.

------------------------------------------------------------------------

*Tail recursion.* In Haskell, you do not have to worry about tail recursion when the computation is lazy; instead you work on putting the computation in a data structure so that the user doesn't force more of it than they need (guarded recursion), and “stack frames” are happily discarded as you pattern match deeper into the structure. However, if you are implementing something like `foldl'`, which is strict, you’d want to pay attention to this (and not build up a really big thunk.)

Well, OCaml is strict by default, so you always should pay attention to making sure you have tail calls. One interesting place this comes up is in the [implementation of map](http://ocaml.janestreet.com/?q=node/71), the naive version of which cannot be tail-call optimized. In Haskell, this is not a problem because our map is lazy and the recursion is hidden away in our cons constructor; in OCaml, there is a trade off between copying the entire list to get TCO, or not copying and potentially exhausting stack space when you get big lists. (Note that a strict map function in Haskell would have the same problem; this is a difference between laziness and strictness, and not Haskell and OCaml.)

------------------------------------------------------------------------

*File organization.* A single file OCaml script contains a list of statements which are executed in order. (There is no `main` function).

The moral equivalent of Haskell modules are called *compilation units* in OCaml, with the naming convention of `foo.ml` (lower case!) corresponding to the `Foo` module, or `Foo.foo` referring to the `foo` function in `Foo`.

It is considered good practice to write interface files, `mli`, as described above; these are like export lists. The interface file will also contain data definitions (with the constructors omitted to implement hiding).

By default all modules are automatically “imported” like `import qualified Foo` (no import list necessary). Traditional `import Foo` style imports (so that you can use names unqualified) can be done with `open Foo` in OCaml.

------------------------------------------------------------------------

*Module system.* OCaml does not have type classes but it does have modules and you can [achieve fairly similar effects with them](http://okmij.org/ftp/ML/ML.html#typeclass). (Another classic way of getting type class style effects is to use objects, but I’m not covering them today.) I was going to talk about this today but this post is getting long so maybe I’ll save it for another day.

------------------------------------------------------------------------

*Open question.* I’m not sure how much of this is OCaml specific, and how much generalizes to all ML languages.

*Update.* ocamlrun is not the same as runghc; I've updated the article accordingly.

*Update 2.* Raphael Poss has written a nice article in reverse: [Haskell for OCaml programmers](http://staff.science.uva.nl/~poss/haskell-for-ocaml-programmers.html)
