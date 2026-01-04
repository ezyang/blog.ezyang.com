---
title: "On type synonyms"
date: 2011-06-15 12:22:05
slug: on-type-synonyms
categories: [Haskell]
comments:
    - id: 2634
      author: elaforge
      date: "2011-06-15 16:01:35"
      content: |
        I have a few more uses for type synonyms:
        
        Functions that simply pass a value to another function and don't depend on its type.  When that type changes, a synonym allows you to make the update in one place.
        
        A place to attach documentation to certain uses of a type.  Rather than copy and paste the doc on each function's arg docs, I can make a single type synonym.  Of course, the type synonym itself is documentation.  For instance, 'type Success = ...; type Failure = ...; newtype Continuation = Cont { runContinuation :: Success -&gt; Failure -&gt; ... }' is much more readable with the aliases, even if they are only ever used in that one place.  I will often give the value the same name as the type to imply the connection in the absence of explicit type annotations.
        
        I use quite a few type synonyms, and export most of them.  They're just simple functions from type to type, and expressing and naming structure by creating lots of little functions is one of those things haskell is good at.  Sure you can make trivial ones that aren't worth it, but you can do that with functions too.
    - id: 2635
      author: yachris
      date: "2011-06-15 16:12:39"
      content: |
        I used to think that "saving keystrokes" with abbreviations or (worse) acronyms was a good idea.  Then, at some point, I realized that (A) modern editors and IDEs allow intelligent automated expansion and (B) it's just way way way more comprehensible to read something... readable.
        
        Oh yeah, being a fast typist helps too :-)
    - id: 2636
      author: Anonymous
      date: "2011-06-15 19:26:45"
      content: |
        I stress to my co-workers to make aliases; an id is an integer, and you use them in sets? Make sure you make it an IdSet. Like you said, it's implicit documentation.
        
        Types cannot speak for themselves sometimes; you need to help them a bit with an alias.
    - id: 2637
      author: Twan van Laarhoven
      date: "2011-06-16 05:39:10"
      content: |
        Another good use for type synonyms is for filling in common parameters: `type Foo a = GenFoo Bells Whistles Bar a`. Most users would stick to Foo, but if you need it a more generic type is available. This is used for instance by parsec.
        
        I would also like to add one more condition to "Synonyms for function compound types are mostly OK": you should never declare a function with more arguments than its type signature apparently allows, i.e.
            foo :: TcSigFun
            foo x = ...
        To me as a reader the x argument is very confusing, what is its type?
        
        By the way: does `String` give any semantic information beyond `[Char]`?
    - id: 2641
      author: Josef Svenningsson
      date: "2011-06-16 12:00:45"
      content: "I really liked the restricted type synonyms in Gofer/Hugs. This feature made it possible to say that the type synonym should only be expanded in a particular set of functions. Outside of that set, the type synonym would not be expanded, thus effectively introducing a new type just like newtype. It had the advantages of ordinary type synonyms that it allowed to programmer to write very nice looking code without having to insert constructors yet at the same time providing encapsulation."
    - id: 2649
      author: Ivan Miljenovic
      date: "2011-06-18 01:20:07"
      content: "Another use for type synonyms that I've found: even more than providing extra semantic information, it allows you to create a more \"user-friendly\" version of another module with different documentation (since Haddock doesn't let you re-export a type with new documentation).  This is admittedly a little hacky, but it works for top-level \"overviews\" of large complex modules."
    - id: 2674
      author: Etienne Laurin
      date: "2011-06-18 22:09:11"
      content: |
        Type synonyms can be used to remove parenthesis:
        
        type a :$ b = a b
        infixr 0 :$
---

I recently had to remove a number of type synonyms from the GHC code base which were along the lines of `type CmmActuals = [CmmActual]`. The process made me wonder a little about *when* type synonyms are appropriate for Haskell code. The [Wikibooks article](http://en.wikibooks.org/wiki/Haskell/Type_declarations) says type synonyms are “for making the roles of types clearer or providing an alias to, for instance, a complicated list or tuple type” and [Learn You a Haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses) says they “make more sense to someone reading our code and documentation.” But under what circumstances is this actually true?

Let's try dividing the following use-cases of type synonyms:

- They can give extra semantic content, for example `DateString` is more informative than `String` about its contents, though they are the same.
- They can abbreviate long constructed types, for example `TcSigFun` might abbreviate `Name -> Maybe TcSigInfo`.

The first is an example of code reader benefit: types with extra semantic information make it easier to see what a function is doing; the second is example of coder writer benefit: abbreviations of long types make writing type signatures more pleasurable. Sometimes a type synonym can give both benefits.

The downside of type signatures is their opacity of implementation. Seeing a value with type `Address`, I do not know if this is an algebraic data type or a type synonym, where as if it were a `String` I would know immediately what functions I could use on it. The type synonym adds an extra layer of indirection to figuring out how to manipulate the value: thus, it is a downside for the writer. It is true that algebraic data types and newtypes also add a layer of indirection, but they also bring to the table extra type safety that type synonyms don’t. (Furthermore, an algebraic data type is usually marvelously self documenting, as each of its constructors gets its own name).

I think my taste in the matter is as follows:

- Don’t use type synonyms if are not going to give any extra semantic information beyond the structure of the type.
- Synonyms for atomic types can be used freely, if the correspondence is unique. If you have many synonyms referring to the same atomic type, consider newtypes.
- Synonyms for non-function compound types should be used sparingly. They should not leak out of module boundaries, and are candidates for promotion into algebraic data-types.
- Synonyms for function compound types are mostly OK (since conversion into an ADT doesn’t buy you much, and they are unlikely to get mixed up), but make sure they are documented properly.
- Prefer to keep type synonyms inside module boundaries, un-exported. (Though, I know a few cases where I”ve broken this rule.)

How do you feel about type synonyms?
