---
title: "Inessential Guide to data-accessor"
date: 2010-04-14 09:00:23
slug: inessential-guide-to-data-accessor
categories: [Haskell]
comments:
    - id: 305
      author: anonymous
      date: "2010-04-14 10:12:45"
      content: |
        Hi,
        i somehow don't think, that naming the modifying functions 'semantic editor combinators' is really "correct". 
        Semantic Editors Combinators are more like functions you can combine to somehow deeply modify a given structure (not just records...) using another function... and this in a very lightweight way. Typical semantic editor combinators are functions like "fmap", "map", "(.)", "const", "liftM" and others.
        
        Maybe you may want to check out the packages "fclabels" and "sec".
        I haven't really used them by now, but from reading about them, i find "fclabels" slightly superior to data-accessor, whereas "sec" is very lightweight Semantic Editor Combinators package with TH support and optional "if"-Statement (see examples).
    - id: 306
      author: Edward Z. Yang
      date: "2010-04-14 10:51:58"
      content: |
        I'll definitely agree that they're on the light-weight side, but the fact that I can compose (it's the "deep" modification you mention), in my opinion, puts them in the semantic editor combinators category, if only as a very weak version.
        
        I didn't realize there were multiple labeled records packages vying for mindshare! This could get messy. :-)
    - id: 309
      author: Chris Eidhof
      date: "2010-04-14 13:03:44"
      content: |
        I'm one of the contributors to the fclabels package. What I like about our package is that it's very minimal, and provides support to compose labels in more than one way.
        
        Using fclabels, you can compose labels vertically, which means they will point deeper into a structure. Alternatively, you can also compose them horizontally, which gives us updatable views. This is interesting if you're doing web programming. For an example, have a look at the projectedForm documentation in the regular-web package. It uses fclabels's applicative instance to build updatable views:
        
        http://hackage.haskell.org/packages/archive/regular-web/0.1.1/doc/html/Generics-Regular-Formlets.html#v%3AprojectedForm
    - id: 310
      author: lpsmith
      date: "2010-04-14 13:09:19"
      content: "Calling a type \"T\" is a Modula-3-ism.   Once in a while I've adopted the same convention,  but it's not something that really stuck with me in Haskell."
    - id: 320
      author: Edward Kmett
      date: "2010-04-17 11:42:44"
      content: |
        Just thought I'd join the chorus recommending fclabels. =)
        
        You get a nice advantage there in that their labels can be combined using the machinery from Control.Category rather than requiring another arbitrary set of combinators to be used.
        
        If I had to complain about fclabels at all, it would be their choice of method names for getting and setting clashes with the use of the state monad, which offers one of the best opportunities to actually use their combinators!
    - id: 364
      author: Martijn van Steenbergen
      date: "2010-04-29 07:04:16"
      content: "data-accessor (and fclabels) generalize semantic editor combinators: SEC are the modify-only 'restriction' of labels (where modify :: (a :-&gt; b) -&gt; (a -&gt; a) -&gt; (b -&gt; b))."
    - id: 2227
      author: Lemming
      date: "2011-04-28 10:44:52"
      content: "data-accessor was made before Control.Category and thus defined its own operators. Feel free to not import them and use the Category dot, that we support since Control.Category exists."
---

[data-accessor](http://hackage.haskell.org/package/data-accessor-0.2.1.2) is a package that makes records *not suck.* Instead of this code:

    newRecord = record {field = newVal}

You can write this:

    newRecord = field ^= newVal
              $ record

In particular, `(field ^= newVal)` is now a value, *not* a bit of extra syntax, that you can treat as a first-class citizen.

I came across this module while attempting to use [Chart](http://hackage.haskell.org/package/Chart) (of criterion fame) to graph some data. I didn't recognize it at first, though; it was only after playing around with code samples did I realize that `^=` was not a combinator that Chart had invented for its own use (as opposed to the potpourri of `-->`, `<+>`, `|||` and friends you might see in an *xmonad.hs*). When utilized with Template Haskell, Data.Accessor represents something of a *replacement* for the normal record system, and so it's useful to know when a module speaks this other language. Signs that you're in a module using Data.Accessor:

- Use of the `^=` operator in code samples
- All of the records have underscores suffixed, such as `plot_lines_title_`
- Template Haskell gobbledygook (including type variables that look like `x[acGI]`, especially in the "real" accessors that Template Haskell generated).
- Unqualified `T` data types floating around. (As Brent Yorgey tells me, this is a Henning-ism in which he will define a type T or typeclass C intended to be used only with a qualified import, but Haddock throws away this information. You can use `:t` in GHC to get back this information if you're not sure.)

Once you've identified that a module is indeed using Data.Accessor, you've won most of the battle. Here is a whirlwind tutorial on how to use records that use data-accessor.

*Interpreting types.* An *accessor* (represented by the type `Data.Accessor.T r a`) is defined to be a getter (`r -> a`) and setter (`a -> r -> r`). `r` is the type of the record, and `a` is the type of the value that can be retrieved or set. If Template Haskell was used to generate the definitions, polymorphic types inside of `a` and `r` will frequently be universally quantified with type variables that `x[acGI]`, don't worry too much about them; you can pretend they're normal type variables. For the curious, these are generated by the quotation monad in Template Haskell).

*Accessing record fields.* The old way:

    fieldValue = fieldName record

You can do things several ways with Data.Accessor:

    fieldValue = getVal fieldname record
    fieldValue = record ^. fieldname

*Setting record fields.* The old way:

    newRecord = record {fieldName = newValue}

The new ways:

    newRecord = setVal fieldName newValue record
    newRecord = fieldName ^= newValue $ record

*Accessing and setting sub-record fields.* The old ways:

    innerValue = innerField (outerField record)
    newRecord = record {
      outerField = (outerField record) {
        innerField = newValue
      }
    }

The new ways (this is bit reminiscent of [semantic editor combinators](http://conal.net/blog/posts/semantic-editor-combinators/)):

    innerValue = getVal (outerField .> innerField) record
    newRecord = setVal (outerField .> innerField) newValue record

There are also functions for modifying records inside the state monad, but I'll leave those explanations for [the Haddock documentation](http://hackage.haskell.org/packages/archive/data-accessor/0.2.1.2/doc/html/Data-Accessor.html). Now go forth and, erm, access your data *in style*!
