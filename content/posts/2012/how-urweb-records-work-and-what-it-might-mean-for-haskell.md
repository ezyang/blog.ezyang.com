---
title: "How Ur/Web records work and what it might mean for Haskell"
date: 2012-04-20 01:24:41
slug: how-urweb-records-work-and-what-it-might-mean-for-haskell
categories: [Haskell, Programming Languages]
comments:
    - id: 3666
      author: Daniel Yokomizo
      date: "2012-04-20 06:04:05"
      content: |
        I didn't knew Ur had records and type functions over them. It's (for quite some time) in my to study list.
        
        The expressiveness seems equivalent to the one in Morrow, described in "First-class labels for extensible rows" (http://research.microsoft.com/apps/mobile/showpage.aspx?page=/en-us/people/daan/pubs.aspx) which is my standard for record system.
    - id: 3669
      author: Greg Weber
      date: "2012-04-20 09:58:40"
      content: "Hi Edward, thanks for the distillation. The main concern of Haskell Records is their lack of name-spacing. Any thoughts along those lines? Can we avoid some of the overhead of the Ur implementation and avoid implementing meta-pr"
    - id: 3673
      author: Edward Z. Yang
      date: "2012-04-20 13:12:06"
      content: "Greg: The Ur system sidesteps this issue with first-class labels. Essentially, it says, \"Let there be namespace clashes!\" The reason I feel like this is not a bad idea is because tuples work this way already, and so all we're doing is adding named tuples. But I don't know how to avoid bringing in all of the Ur heavy machinery."
    - id: 3674
      author: Adam Chlipala
      date: "2012-04-21 10:06:25"
      content: "Daniel: Ur's record typing is substantially more expressive than Morrow's, thanks to non-trivial type-level computation features like record concatenation and mapping."
    - id: 3695
      author: AntC
      date: "2012-04-26 21:00:17"
      content: |
        Thank you Edward for the explanations.
        
        So Ur has a polymorphic/extensible record system, including anonymous records. This covers roughly the same capabilities as the HList models. (You can get permutable/order-independent records with HList, using the type-indexed products approach.) But of course there's many suggestions out there for various flavours of polymorphic records (including the old TREX system).
        
        I would be interested how you do record merge (as opposed to concatenation). For example: { Bar : int, Baz : bool } |&gt;&lt;| { Bar : int, quux : string } yields { Bar : Int, Baz : bool, quux : string } -- as needed to implement the equivalent of Relational join.
        
        WRT the Records in Haskell &#039;intense debate&#039;, as the wiki page you point to says quite clearly:
        •The narrow issue: namespacing for record field names. Currently in Haskell two records in the same module can&#039;t share a field name. This is sometimes extremely painful. This page is about the narrow issue. 
        
        I don&#039;t think what you&#039;ve explained about Ur helps with that narrow issue(?) Specifically how to pass round a polymorphic selector function that works on any record type with a baz field.
    - id: 3696
      author: Edward Z. Yang
      date: "2012-04-26 21:16:08"
      content: |
        I don't understand what the semantics of record merge would be, but I don't think Ur has anything like that. What would the use-case be?
        
        The polymorphic selector function issue is neatly resolved in the Ur model, however: the signature of such a function would be something like:
        
        t ::: Type -> ts ::: {Type} -> [[FieldName] ~ ts] => $([FieldName = t] ++ ts) -> t
        
        (Here using a combination of permutation and concatenation to encode "records that have some field").
        
        (I want to add that I'm not familiar with the HList model, and my intuition is dealing with permutations would be kind of annoying.)
    - id: 3697
      author: AntC
      date: "2012-04-26 23:06:38"
      content: |
        The use-case of record merge is Relational Join. http://en.wikipedia.org/wiki/Join_(relational_algebra)#Joins_and_join-like_operators  
        
        Strewth! that type signature looks like hard work.  What the various 'Records in Haskell' proposals are coming up with is along the lines of:
            baz :: (Has r Baz) =&gt; r -&gt; FieldTy r Baz     -- Type-level proposal
            baz :: (Has r "baz" t) =&gt; r -&gt; t                     -- Stringy Kinds proposal
        
        To explain:
         - the type level proposals are using H98 types to index the overloading,
                 punning on the label name baz  type Baz
                     (and a type level function to get the type of baz's field in record r)
        - the Stringy Kinds proposals are using the new (GHC 7.4) User-defined Kinds
                 (and type equality constraints/coercion to get the field's type)
        
        (Permutations in HList's type-indexed products are not atall annoying to use: it's just field selection. They are a bit more awkward to support behind the scenes: you need overlapping instances, and therefore you can't use type families, so you have to use functional dependencies.)
    - id: 3698
      author: Edward Z. Yang
      date: "2012-04-26 23:20:15"
      content: |
        I'm well aware of join in that context, but in my opinion it doesn't make sense as much when you only have singular records. Though, from a pragmatic standpoint, I see why they're attractive: they make it a lot more convenient to handle joins from the database. (Ur/Web takes a different approach: SQL queries are also strongly typed, so it can synthesize up the proper polymorphic records as necessary.)
        
        Yes, Ur type signatures are a bit of work, but about the same as any ole' dependently typed language :-) As I said in my post, the Ur system is much more invasive.
        
        I don't have a good enough feel for how the Records in Haskell approaches work in practice to really judge the day-to-day differences. Certainly I'm not a fan of overlapping instances and functional dependencies, and I don't have a laundry list of use-cases to do a point-by-point comparison.
    - id: 3700
      author: AntC
      date: "2012-04-27 06:24:57"
      content: |
        Use-case 1 for record merge (join): SQL
        So the problem with using SQL from a static strongly-typed language (Haskell) is that you have to abandon "well-typed programs can't go wrong."
        
        There's an impedence mismatch: at some point you have to pass a string to SQL, which (if you're lucky) might throw an error that it's ill-typed, or (worse) might be interpreted and dynamically type-checked OK, but return values that don't match the types in your program.
        
        Having a type-safe record algebra in Haskell that mirrors SQL's behaviour is a way of reducing (not eliminating) type mis-matches.
        
        Use-case 2 for record merge (join): avoiding redundancy in your data model
        You have an in-memory data model, no external database, no SQL. You want to make your data model as clean to manage as possible: no redundancy, no risk of anomalies.
        
        So you normalise (industry-standard approach) -- that is, split into semantically minimal clusters of fields/tuples.
        
        And to separate concerns/achieve data independence/avoid mixing up data retrieval with application logic, you make your access via joins (merges from the normalised tuples -- industry-standard approach). Typically presenting a screen with data meaningful to the user is all about joins: show the description as well as the Id.
        
        
        So explain to me the use-case for 'singular records'.
    - id: 3703
      author: Edward Z. Yang
      date: "2012-04-27 15:51:18"
      content: "When I said singular records, I meant that in the algebraic setting, records only refer to single values; we don't really have any first class notion of a collection of results (i.e. there is no language support for that; you build language support on top of it.) So while the algebraic record system may want to support a relational style modeling of your data, there are many other, valid ways of storing data. (I personally have no great love of the relational model as an end-all-be-all; I think there are many domains in which that style of modeling doesn't make sense.)"
    - id: 3705
      author: Adam Chlipala
      date: "2012-04-28 09:49:46"
      content: |
        As Edward has mentioned, Ur/Web supports all the standard features of SQL, encoded with strongly typed combinators that enforce SQL's typing rules.  No record merge operator is involved.  Despite such ideas being standard in database theory, I don't find them to be very useful.  The idea that sharing of a column name implies a semantic relationship between two records is off-putting to me.
        
        Also, the Ur primitives make it easy to define library functionality (and it is defined for Ur/Web, in the Mem module of the Meta library) that presents the simpler interface you mention, for passing around record field selectors.  In contrast, starting from that interface, I don't think it's possible to derive all of Ur's expressivity for type-level records.  Also, I tend to think that passing record field selectors around is a sign of bad design.  I more or less never feel the need to do it in Ur.
        
        Finally, Haskell is hopeless for reasoning about metaprogramming examples like you can find for Ur.  E.g., try to do this with any Haskell approach, without writing much uglier code:
          http://www.impredicative.com/ur/demo/crud1.html
    - id: 6212
      author: jamie
      date: "2013-09-07 12:08:15"
      content: |
        Thanks for sharing this content with us.
        I can have earned a grate knowledge from the content.
        Hopefully you will continue to sharing with us this kind of helpful content.
        Thanks again. Have a great day.
---

[Ur](http://www.impredicative.com/ur/) is a programming language, which among other things, has a rather interesting record system. Record systems are a topic of rather [intense debate](http://hackage.haskell.org/trac/ghc/wiki/Records) in the Haskell community, and I noticed that someone had remarked “\[Ur/Web has a <http://www.impredicative.com/ur/tutorial/tlc.html> very advanced records system\]. If someone could look at the UR implementation paper and attempt to distill a records explanation to a Haskell point of view that would be very helpful!” This post attempts to perform that distillation, based off my experiences interacting with the Ur record system and one of its primary reasons for existence: metaprogramming. (Minor nomenclature note: Ur is the base language, while Ur/Web is a specialization of the base language for web programming, that also happens to actually have a compiler. For the sake of technical precision, I will refer to the language as Ur throughout this article.)

# Records and algebraic data types are not the same thing

In Haskell, if you want to define a record, you have to go and write out a `data` declaration:

    data Foo = Foo { bar :: Int, baz :: Bool }

In Ur, these two concepts are separate: you can define an algebraic data type (the `Foo` constructor) and you can write types which describe a record (the `{ foo :: Int, bar :: Bool}` bit of the type). To emphasize this point, there are actually a lot of ways I can spell this record in Ur/Web. I can define a type synonym:

    type foo = { Bar : int, Baz : bool }

which offers me no protection from mixing it up with a structurally similar but semantically different `type qux = { Bar : int, Baz : bool }`, or I can define:

    datatype foo = Foo of { Bar : int, Baz : bool }

which desugars into:

    type foo' = { Bar : int, Baz : bool }
    datatype foo = Foo of foo'

that is to say, the datatype has a single constructor, which takes only one argument, which is a record! This definition is closer to the spirit of the original Haskell definition. (ML users might be familiar with this style; Ur definitely comes from that lineage.)

This design of separating algebraic data types from records means we now have obvious facilities for record construction (`let val x = { Bar = 2, Baz = true }`) and record projection (`x.Bar`); though if I have a datatype I have to unwrap it before I can project from it. These record types are unique up to permutation (order doesn't matter), which makes them a bit more interesting than `HList`. They are also nicely parsimonious: unit is just the empty record type `{}`, and tuples are just records with special field names: `1`, `2`, etc.

# Types and kinds of records

Now, if this was all there was to the Ur record system, it wouldn't be very interesting. But actually, the field `#Bar` is a first class expression in the language, and the curly brace record type syntax is actually syntax sugar! Unpacking this will require us to define quite a few new kinds, as well as a lot of type level computation.

In vanilla Haskell, we have only one kind: `*`, which in Ur parlance is a `Type`. Values inhabit types which inhabit this kind. Ur's record system, however, demands more exotic kinds: one such kind is the `Name` kind, which represents a record field name (`#Foo` is one). However, GHC has this already: it is the [recently added](http://hackage.haskell.org/trac/ghc/wiki/TypeNats/Basics) `Symbol` kind. What GHC doesn't have, however, is the kind constructor `{k}`, which is the kind of a “type-level record.” If value-level records are things that contain data, type-level records are the things that *describe* value-level records. They are not, however, the *type* of the value-level records (because if they were, their kind would be `Type`). Let’s look at a concrete example.

When I write:

    type foo = { Bar : int, Baz : bool }

What I’m really writing is:

    type foo = $[ Bar = int, Baz = bool ]

The `$` is a type level operator, being applied to the expression `[ Bar = int, Baz = bool ]`, which is a type level record, specifically of kind `{Type}` (the “values” of the record are types). The dollar sign takes type level records, and transforms them into `Type` (so that they can actually be inhabited by values).

This may seem like a meaningless distinction, until you realize that Ur has type level operators which work only on type level records, and not types in general. The two most important primitive type level operations are concatenation and map. They both do what you might expect: concatenation takes two records and puts them together, and map takes a type level function and applies it to every member of the record: so I can easily transform `[ Bar = int, Baz = bool ]` into `[ Bar = list int, Baz = list bool ]` by mapping the list type constructor. Extensible records and metaprogramming dispatched in one swoop!

Now, recall that field names all live in a global namespace. So what happens if I attempt to do `[ Bar = bool ] ++ [ Bar = int ]`? The Ur type checker will reject this statement as ill-typed, because I have not provided the (unsatisfiable) proof obligation that these records are *disjoint*. In general, if I have two record types `t1` and `t2` which I would like to concatenate, I need a disjointness proof `[t1 ~ t2]`. Handling disjointness proofs feels rather unusual to users coming from traditional functional programming languages, but not all that odd for users of dependently typed languages. In fact, the Ur/Web compiler makes handling disjointness obligations very easy, automatically inferring them for you if possible and knowing some basic facts about about concatenate and map.

# Type level computation

The Ur record system crucially relies on type level computation for its expressiveness: we can expand, shrink and map over records, and we can also take advantage of “folders”, which are functions which use the type level records as structure to allow generic folding over records. For more information about these, I suggest consulting the [type level computation tutorial](http://www.impredicative.com/ur/tutorial/tlc.html). But in order to offer these features in a user friendly way, Ur crucially relies on a compiler which has some level of knowledge of how these operators work, in order to avoid making users discharge lots of trivial proof obligations.

Unfortunately, here I must admit ignorance as to how the rest of the Haskell record proposals work, as well as how a record system like this would interact with Haskell (Ur does have typeclasses, so this interaction is at least reasonably well studied.) While this proposal has the benefit of having a well specified system in an existing language, it is complex, and definitely shooting for the moon. But I think it says a bit about what might have to be added, beyond type-level strings, to fulfill [Gershom Bazerman's vision here](http://www.haskell.org/pipermail/glasgow-haskell-users/2011-December/021410.html):

> It seems to me that there's only one essential missing language feature, which is appropriately-kinded type-level strings (and, ideally, the ability to reflect these strings back down to the value level). Given that, template haskell, and the HList bag of tricks, I'm confident that a fair number of elegant records packages can be crafted. Based on that experience, we can then decide what syntactic sugar would be useful to elide the TH layer altogether.
