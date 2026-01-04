---
title: "Idiomatic algebraic data types in Python with dataclasses and Union"
date: 2020-10-14 14:08:54
slug: idiomatic-algebraic-data-types-in-python-with-dataclasses-and-union
categories: [Haskell, Python]
comments:
    - id: 24580
      author: dlax
      date: "2020-10-15 03:16:45"
      content: |
        An alternative to explicit pattern matching is to use functools.singledispatch, e.g.:
        
        @singledispatch
        def showResult(r: NoReturn) -&gt; NoReturn:
            raise AssertionError("Unhandled type: {}".format(type(x).__name__))
        
        
        @showResult.register
        def show_ok(r: OK) -&gt; str:
            return str(r.result)
        
        
        @showResult.register
        def show_failure(r: Failure) -&gt; str:
            return "Failure: " + r.msg
    - id: 24710
      author: Dimi
      date: "2020-11-18 18:42:01"
      content: |
        Except that you want Result to be polymorphic. If you define Ok for int, what will you do for all other types? Maybe some kind of magic with a TypeVar is possible? Moreover you cannot do isinstance(OK(5), Result), try it.
        
        Now about this sentence: "The result is just as good as an ADT (or better, perhaps; their structural nature bears more similarity to OCaml's polymorphic variants)." You need to study.
    - id: 24711
      author: Dimi
      date: "2020-11-18 18:49:37"
      content: "PS. Good tips though. I'll use your way :)"
    - id: 24852
      author: Edward Z. Yang
      date: "2020-12-16 23:37:23"
      content: |
        >  Moreover you cannot do isinstance(OK(5), Result), try it.
        
        Yes, but in a fully statically typed program, you shouldn't need to do so, since you should statically know from context that something is a Result, without having to refine it manually with an isinstance test. (So for example, don't do something like `Union[Result, Result2]`, you want a tagged union for this case.)
        
        One thing that mypy-style type checking can't do for you is type-driven metaprogramming, ala type classes, which is honestly pretty useful. But there are other ways (e.g., object oriented programming) to get what you want in a language like Python.
    - id: 24949
      author: Franklin Chen
      date: "2020-12-30 20:40:22"
      content: "Is there a reason you choose to use dataclass rather than NamedTuple?"
    - id: 24973
      author: Edward Z. Yang
      date: "2021-01-06 10:18:50"
      content: "Yes: I typically don't want positional access to work :)"
    - id: 25478
      author: Andreas Abel
      date: "2021-02-14 02:24:28"
      content: |
        A bit like you do in Java: use an abstract class for the data type and one subclass for each of its constructors.
        Alternative to case with `instanceof` is the so-called visitor pattern.
    - id: 25813
      author: Felipe Gusmao
      date: "2021-04-08 07:24:00"
      content: This would now work even better with pattern matching that is comming to python 3.10
    - id: 25901
      author: Jeroen
      date: "2021-04-28 12:13:40"
      content: |
        &gt; Moreover you cannot do isinstance(OK(5), Result), try it.
        
        In Python 3.10, you will be able to write the Union using a new syntax with '|' which will also be supported by isinstance:
        
        Result = OK | Failure
        
        isinstance(OK(3), Result)
        =&gt; True
        
        See PEP 604 for more on this.
    - id: 26223
      author: David Froger
      date: "2021-06-15 13:50:59"
      content: |
        Very interesting, assert_never is what I was looking for!
        
        What about serialization, let's say in JSON? There is not "tag"
        to include, and including the class name seems not clean (as
        the class name may be an implementation detail).
        
        thanks
    - id: 26613
      author: Edward Z. Yang
      date: "2021-08-23 20:50:05"
      content: "David: Yeah, this doesn't work so great. Usually I try to convert into some sort of strongly-typed dataclass format ASAP, but that involves a bunch of parsing code."
    - id: 26448
      author: Daniel Anderson
      date: "2021-07-14 09:10:47"
      content: Thank you a lot for your advices. Agree with you that ADT is a great function in Haskell.
    - id: 26972
      author: Franklin Chen
      date: "2021-11-13 10:32:36"
      content: "Woohoo, Python 3.10 has pattern matching now! You might want to update your blog post to reflect this."
    - id: 29479
      author: ShalokShalom
      date: "2023-03-18 04:34:02"
      content: "Is it still compatible with the actually accepted PEP for pattern matching?"
    - id: 33679
      author: Edward Z. Yang
      date: "2024-10-04 16:46:45"
      content: "I did a quick read through https://peps.python.org/pep-0636/ and yes, I think it is. Specifically, the final PEP is still doing isinstance tests when matching against objects, which is the important thing for compatibility."
---

**Greetings from 2024!** An official pattern matching PEP has been accepted <https://peps.python.org/pep-0636/> and is available in Python 3.10. Class patterns are tested using isinstance, with no inheritance structure necessary, making the pattern described in this post 100% forward compatible to real pattern matching.

------------------------------------------------------------------------

One of the features I miss most in non-Haskell programming languages is algebraic data types (ADT). ADTs fulfill a similar role to objects in other languages, but with more restrictions: objects are an open universe, where clients can implement new subclasses that were not known at definition time; ADTs are a closed universe, where the definition of an ADT specifies precisely all the cases that are possible. We often think of restrictions of a bad thing, but in the case of ADTs, the restriction of being a closed universe makes programs easier to understand (a fixed set of cases to understand, as opposed to a potentially infinite set of cases) and allows for new modes of expression (pattern matching). ADTs make it really easy to accurately model your data structures; they encourage you to go for precise types that make illegal states unrepresentable. Still, it is generally not a good idea to try to manually reimplement your favorite Haskell language feature in every other programming language you use, and so for years I've suffered in Python under the impression that ADTs were a no go.

Recently, however, I have noticed that a number of new features in Python 3 have made it possible to use objects in the same style of ADTs, in idiomatic Python with virtually no boilerplate. The key features:

- A structural static type checking system with mypy; in particular, the ability to declare `Union` types, which let you represent values that could be one of a fixed set of other types, and the ability to refine the type of a variable by performing an `isinstance` check on it.
- The dataclasses library, which allows you to conveniently define (possibly immutable) structures of data without having to write boilerplate for the constructor.

The key idea: define each constructor as a dataclass, put the constructors together into an ADT using a Union type, and use `isinstance` tests to do pattern matching on the result. The result is just as good as an ADT (or better, perhaps; their structural nature bears more similarity to OCaml's polymorphic variants).

Here's how it works. Let's suppose that you want to define an algebraic data type with two results:

    data Result
       = OK Int
       | Failure String

    showResult :: Result -> String
    showResult (OK result) = show result
    showResult (Failure msg) = "Failure: " ++ msg

First, we define each constructor as a dataclass:

    from dataclasses import dataclass

    @dataclass(frozen=True)
    class OK:
        result: int

    @dataclass(frozen=True)
    class Failure:
        msg: str

Using the automatically generated constructors from dataclasses, we can construct values of these dataclasses using `OK(2)` or `Failure("something wrong")`. Next, we define a type synonym for the union of these two classes:

    Result = Union[OK, Failure]

Finally, we can do pattern matching on Result by doing `isinstance` tests:

    def assert_never(x: NoReturn) -> NoReturn:
        raise AssertionError("Unhandled type: {}".format(type(x).__name__))

    def showResult(r: Result) -> str:
        if isinstance(r, OK):
            return str(r.result)
        elif isinstance(r, Failure):
            return "Failure: " + r.msg
        else:
            assert_never(r)

`assert_never` is a [well known trick](https://github.com/python/typing/issues/735) for doing exhaustiveness checking in mypy. If we haven't covered all cases with enough `isinstance` checks, mypy will complain that `assert_never` was given a type like `UnhandledCtor` when it expected `NoReturn` (which is the uninhabited type in Python).

That's all there is to it. As an extra bonus, this style of writing unions is compatible with the [structured pattern matching PEP](https://www.python.org/dev/peps/pep-0634/), if it actually gets accepted. I've been using this pattern to good effect in our recent rewrite of PyTorch's code generator. If you have the opportunity to work in a statically typed Python codebase, give this style of code a try!
