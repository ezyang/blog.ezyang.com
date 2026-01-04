---
title: "Why can't I just be a little lazy?"
date: 2012-11-26 09:00:28
slug: why-cant-i-just-be-a-little-lazy
categories: [Haskell]
comments:
    - id: 4951
      author: Daniel Yokomizo
      date: "2012-11-26 11:34:52"
      content: "Another under-appreciated point is having eager data and lazy codata."
    - id: 4952
      author: Vladimir
      date: "2012-11-26 12:15:10"
      content: "Interesting.  I am all in favor of having choices (provided one is knowledgeable and responsible enough).  Do you see Haskell development adhering to either direction?"
    - id: 4955
      author: Edward Z. Yang
      date: "2012-11-26 13:17:44"
      content: "Vladimir: Haskell makes it possible to make some of these choices (think strict and lazy versions of data structures), but not for everything (call by value); and the choices you make aren't reflected in the type system.  Adding them to the type system would be a very interesting project, and possibly very hard."
    - id: 4956
      author: Thiago Negri
      date: "2012-11-26 13:24:55"
      content: |
        You can use `seq` in Haskell to be more strict. The only problem that I see is that it do not change the type.
        
        You still may document it doing something like this: `type Strict a = a`; `f :: Strict Int -&gt; Int -&gt; Int` to tell that the function is strict on it's first argument. Yet not guaranteed by the typechecker.
    - id: 4966
      author: Tom
      date: "2012-11-26 14:47:34"
      content: "I think this is a fascinating line of potential research.  I don't have any informed comments to make except to wonder, as I always do, whether Call by Push Value can help (http://www.cs.bham.ac.uk/~pbl/cbpv.html)."
    - id: 4967
      author: Edward Z. Yang
      date: "2012-11-26 14:48:42"
      content: "In the comments of http://existentialtype.wordpress.com/2012/08/25/polarity-in-type-theory/ , Harper remarks that CBPV and polarity are essentially the same thing."
    - id: 4971
      author: Luke Palmer
      date: "2012-11-26 15:27:03"
      content: "Thiago, or `data Strict a = Strict !a`, then by convention your `f` would be defined as `f (Strict x) y z = ..., and then it would be true."
    - id: 4975
      author: Thiago Negri
      date: "2012-11-26 16:24:32"
      content: "Luke, sure. But I expect a feature like this to alleviate laziness overhead. Using a `data` type will introduce a new runtime unnecessary overhead."
    - id: 4978
      author: "Kim-Ee Yeoh"
      date: "2012-11-26 16:50:12"
      content: "Luke, Thiago: Replacing Strict with WeaklyStrict or WHNFStrict gives you the nice advantage that the name actually describes what's going on, preventing beginners from tripping over the confusion."
    - id: 5026
      author: Derek Elkins
      date: "2012-11-28 20:51:21"
      content: |
        data StrictPair a b = SP !a !b
        
        foldl' :: StrictPair (a -&gt; b -&gt; a) a -&gt; [b] -&gt; a
        foldl' (SP c n) [] = n
        foldl' (SP c n) (x:xs) = foldl' (SP c (c n x)) xs
    - id: 5027
      author: Edward Z. Yang
      date: "2012-11-28 20:56:48"
      content: "Oh oh, that is clever; when you reveal the true structure of the catamorphism, the pair gets forced because we need to get the function. I suppose this also says something about the difference between (a, b) -> c and a -> b -> c"
---

You can. Imagine a version of Haskell where every constructor was strict, e.g. every field had a `!` prefix. The semantics of this language are well defined; and in fact, the fine folks at CMU have known about this for some time:

> Up to this point we have frequently encountered arbitrary choices in the dynamics of various language constructs. For example, when specifying the dynamics of pairs, we must choose, rather arbitrarily, between the lazy dynamics, in which all pairs are values regardless of the value status of their components, and the eager dynamics, in which a pair is a value only if its components are both values. We could even consider a half-eager (or, equivalently, half-lazy) dynamics, in which a pair is a value only if, say, the first component is a value, but without regard to the second.
>
> Similar questions arise with sums (all injections are values, or only injections of values are values), recursive types (all folds are values, or only folds of values are values), and function types (functions should be called by-name or by-value). Whole languages are built around adherence to one policy or another. For example, Haskell decrees that products, sums, and recursive types are to be lazy, and functions are to be called by name, whereas ML decrees the exact opposite policy. Not only are these choices arbitrary, but it is also unclear why they should be linked. For example, we could very sensibly decree that products, sums, and recursive types are lazy, yet impose a call-by-value discipline on functions. Or **we could have eager products, sums, and recursive types, yet insist on call-by-name.** It is not at all clear which of these points in the space of choices is right; each has its adherents, and each has its detractors.
>
> Are we therefore stuck in a tarpit of subjectivity? No! The way out is to recognize that these distinctions should not be imposed by the language designer, but rather are choices that are to be made by the programmer. This may be achieved by recognizing that differences in dynamics reflect fundamental type distinctions that are being obscured by languages that impose one policy or another. We can have both eager and lazy pairs in the same language by simply distinguishing them as two distinct types, and similarly we can have both eager and lazy sums in the same language, and both by-name and by-value function spaces, by providing sufficient type distinctions as to make the choice available to the programmer.

This is from the Polarization chapter of Harper’s [Practical Foundations for Programming Languages](http://www.cs.cmu.edu/~rwh/plbook/book.pdf). Personally, I think call-by-name with (by default) eager data types by default is an under-appreciated point in the design space: with this combination, you still get the ability to implement your own control-flow structures like `if` (just not on data structures) and have lazy bindings, but you no longer have to worry about a large class of space leak. Of course, this regime doesn't eliminate all problems: for example, if you `foldl` instead of `foldl'`, you will still end up with a long line of function applications and stack overflow. It’s not clear to me if there is an alternative form of `fix` which dodges this bullet.
