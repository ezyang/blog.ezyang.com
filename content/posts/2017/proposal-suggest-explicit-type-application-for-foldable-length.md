---
title: "Proposal: Suggest explicit type application for Foldable length and friends"
date: 2017-03-21 19:50:13
slug: proposal-suggest-explicit-type-application-for-foldable-length
categories: [Haskell]
comments:
    - id: 21877
      author: David Feuer
      date: "2017-03-21 23:31:56"
      content: "`fold`, `toList`, and even `Functor` actually have this problem too, in a slightly different circumstance. Suppose you start with `[Int]` and \"upgrade\" it to `([Int], Int)`. Originally, `fmap (+1)` would add 1 to each element of the list; now it adds one to the second component of the tuple. Or suppose you start with `[[Int]]` and upgrade it to `([[Int]], [Int])`. Previously, `fold` would concatenate the list of lists. Now it just produces the second list."
    - id: 21878
      author: gasche
      date: "2017-03-22 03:44:58"
      content: |
        Marking functions as having some explicit (rather than implicit) parameters: I think that this is good language design.
        
        The idea of giving a "default" type that does not need an annotation is interesting but not fully fledged. I think you would rather want to have a list of "likely" instances that do not need to be explicitated (there is no reason to prefer lists over arrays for length, both should be accepted implicitly or neither), and users should be able to extend this base. This starts to remind us of Coq's "hint databases" for example, and it sounds like heavy and potentially-modularity-damaging design, so I would thread fairly carefully.
    - id: 21879
      author: kja
      date: "2017-03-22 07:23:03"
      content: "@David Feuer: I Disagree with you [Int] to ([Int],Int) upgrade example - the result of that call to fmap would, all other things being equal, still expect an [Int], and so even though the expression locally is fine, it won't be from its context."
    - id: 21880
      author: AntC
      date: "2017-03-22 16:40:56"
      content: |
        Haskell 2010 Report -- i.e. language standard, gives `length :: [a] -&gt; Int`. So the obvious fix is to make the language comply to standard. If you want a length-alike function to work for Foldables, name the function differently.
        
        "Lists are a commonly used data structure in functional languages" says the Haskell Gentle Introduction, and 'Why Functional Programming Matters' demonstrates that perfectly. (A little before that quote in the Gentle Intro, says "All type errors are detected at compile time" and "Haskell's type system is powerful enough to allow us to avoid writing any type signatures at all.") It then goes on to introduce Strings as Lists, and List Comprehensions. And of course `length` of a list is part of that.
        
        So Lists (and their associated functions, esp `map` and `fold`, 'reduce') are ubiquitious in learning materials. Nowadays from the cognoscenti I see Lists deprecated as almost always the wrong technique. The FTP change, whatever its merits for intellectual purity, has put Haskell further out of reach for beginners (and for those trying to teach them).
        
        Now it turns out beginners have to be nervous about using `length`. And you think the 'fix' is to introduce explicit type application? (The other suggested 'fix' I've seen is to avoid importing the now-standard Prelude, use a hobbled version of Prelude, with those generic functions specialised to Lists. Again a good way to burden nervous beginners.)
        
        This is presumably part of Haskell's strategy to "avoid success at all costs". Stop being a language that entices beginners into Functional Programming. (Which was its attraction for me.) Instead make it as recondite and burdensome to learn as any other.
        
        I see a smaller and smaller circle of people on ghc-devs and the Libraries lists making decisions about Haskell's direction. The effect is there's a steeper and steeper learning curve to get to use Haskell, and to get to understand the issues in language design. I find nowadays it more likely my code will get obscure type errors. It feels like my learning is going backwards.
        
        Sorry this isn't answering your question. I don't like any of the suggested answers. Because I don't like the question. I don't like that Haskell has somewhere gone sour, and such questions are necessary.
    - id: 21881
      author: max630
      date: "2017-03-22 17:20:36"
      content: "The \"solution\" of explicit type annotation breaks polimorphism. If you say \"but isn't it the polymorphism you don't like\" the no, I'm perfectly OK with using length for Vector or Set. But it does not mean that I'm OK if somebody decides that a structure which is not really a sequence could implement same interface I could ever willingly use Either to hold a sequence."
    - id: 21882
      author: max630
      date: "2017-03-22 19:12:42"
      content: "actually, it could be a solution: introduce list of \"unexpected\" instances and print a warning if they happen to be used in the module"
    - id: 21883
      author: AntC
      date: "2017-03-22 19:50:09"
      content: |
        @max630 "not really a sequence" is a good intuition. I can see Vector-as-sequence. I can't see Set-as-sequence. (The whole idea of a set, as a mathematical abstraction, is that it's unordered/has no internal structure.)
        
        I can't really see Arrays being a sequence as @gosche discusses. You could argue a Vector is just a one-dimensional Array. You could argue an Array is a Vector-of-Vectors. I have no strong intuition whether to count all the cells as equal, or just count the range of the first subscript (Foldable probably counts the last subscript, knowing how non-intuitive it is ;-). "length" (as an English word) doesn't seem right for either.
        
        We have a precedent that `map` is specialised for Lists, and there's a generic `fmap` and `foldMap`. And the FTP change didn't stuff around with that [**]. `genericLength` is already taken, and anyway "xxxLength" isn't the right abstraction for structures that aren't sequences. So let's use `cardinality` for the genericisation to Foldable.
        
        Given we are where we are. I think the answer to Edward's question is, at the top of every program:
        
        length :: [a] -&gt; Int;
        length = Prelude.length;
        
        cardinality = Prelude.length;
        
        
        
        [**] And there wasn't a `length` in Data.Foldable. I said "didn't stuff around" because of the shackle of backwards compatibility. We usually interpret that shackle to mean 'don't break existing code'. Usually when a new feature's being introduced, there's some extra syntax so you can explicitly invoke it. The trouble with the FTP change was deliberately no change in syntax. So existing code didn't break; good in general except ....
        
        There's a corollary to 'don't break existing code' (which only matters when you're not introducing new syntax): 'don't make code that breaks suddenly non-breaking'.
    - id: 21907
      author: fromagxo
      date: "2017-04-03 00:56:34"
      content: |
        instance Sized f where
            size :: f a -&gt; Natural
            default size :: Foldable f =&gt;
            size = size . toList
---

**tl;dr** *If you use a Foldable function like length or null, where instance selection is solely determined by the input argument, you should make your code more robust by introducing an explicit type application specifying which instance you want. This isn't necessary for a function like fold, where the return type can cross-check if you've gotten it right or not. If you don't provide this type application, GHC should give a warning suggesting you annotate it explicitly, in much the same way it suggests adding explicit type signatures to top-level functions.*

Recently, there has been some dust kicked up about [Foldable instances causing "bad" code to compile](https://mail.haskell.org/pipermail/libraries/2017-March/027716.html). The prototypical example is this: you've written `length (f x)`, where `f` is a function that returns a list `[Int]`. At some future point in time, a colleague refactors `f` to return `(Warnings, [Int])`. After the refactoring, will `length (f x)` continue to type check? Yes: `length (f x)` will always return 1, no matter how long the inner list is, because it is using the `Foldable` instance for `(,) Warnings`.

The solution proposed in the mailing list was to remove `Foldable` for `Either`, a cure which is, quite arguably, worse than the disease. But I think there is definitely merit to the complaint that the `Foldable` instances for tuples and `Either` enable you to write code that typechecks, but is totally wrong.

[Richard Eisenberg](https://mail.haskell.org/pipermail/libraries/2017-March/027743.html) described this problem as the tension between the goals of "if it compiles, it works!" (Haskell must *exclude* programs which don't work) and general, polymorphic code, which should be applicable in as many situations as possible. I think there is some more nuance here, however. Why is it that `Functor` polymorphic code never causes problems for being "too general", but `Foldable` does? We can construct an analogous situation: I've written `fmap (+2) (f x)`, where `f` once again returns `[Int]`. When my colleague refactors `f` to return `(Warnings, [Int])`, `fmap` now makes use of the `Functor` instance `(,) Warnings`, but the code fails to compile anyway, because the type of `(+1)` doesn't line up with `[Int]`. Yes, we can still construct situations with `fmap` where code continues to work after a type change, but these cases are far more rare.

There is a clear difference between these two programs: the `fmap` program is *redundant*, in the sense that the type is constrained by both the input container, the function mapping over it, and the context which uses the result. Just as with error-correcting codes, redundancy allows us to detect when an error has occurred; when you reduce redundancy, errors become harder to detect. With `length`, the *only* constraint on the selected instance is the input argument; if you get it wrong, we have no way to tell.

Thus, the right thing to do is *reintroduce* redundancy where it is needed. Functions like `fold` and `toList` don't need extra redundancy, because they are cross-checked by the use of their return arguments. But functions like `length` and `null` (and arguably `maximum`, which only weakly constrains its argument to have an `Ord` instance) don't have any redundancy: we should introduce redundancy in these places!

Fortunately, with GHC 8.0 provides a very easy way of introducing this redundancy: an **explicit type application.** (This was also independently [suggested by Faucelme](https://www.reddit.com/r/haskell/comments/5x4yka/deprecate_foldable_for_either/def96j4/).) In this regime, rather than write `length (f x)`, you write `length @[] (f x)`, saying that you wanted length for lists. If you wanted length for maps, you write `length @(Map _) (f x)`. Now, if someone changes the type of `f`, you will get a type error since the explicit type application no longer matches.

Now, you can write this with your FTP code today. So there is just one more small change I propose we add to GHC: let users specify the type parameter of a function as "suggested to be explicit". At the call-site, if this function is used without giving a type application, GHC will emit a warning (which can be disabled with the usual mechanism) saying, "Hey, I'm using the function at this type, maybe you should add a type application." If you really want to suppress the warning, you could just type apply a type hole, e.g., `length @_ (f x)`. As a minor refinement, you could also specify a "default" type argument, so that if we infer this argument, no warning gets emitted (this would let you use the list functions on lists without needing to explicitly specify type arguments).

That's it! No BC-breaking flag days, no poisoning functions, no getting rid of FTP, no dropping instances: just a new pragma, and an opt-in warning that will let people who want to avoid these bugs. It won't solve all `Foldable` bugs, but it should squash the most flagrant ones.

What do people think?
