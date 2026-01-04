---
title: "Open type families are not modular"
date: 2014-09-04 18:12:23
slug: open-type-families-are-not-modular
categories: [Backpack, Haskell]
comments:
    - id: 7791
      author: jberryman
      date: "2014-09-04 21:46:32"
      content: "This is a bit OT, and I'm sorry I skimmed your post: is there a use case for open type families that aren't (couldn't be) associated with a type class? And if that were a constraint placed on them would that get you anywhere in terms of modularity?"
    - id: 7793
      author: Edward Z. Yang
      date: "2014-09-05 02:40:56"
      content: "Don't be, it's a long post/ :)  If you have a type class with an associated type http://www.haskell.org/haskellwiki/GHC/Type_families#Associated_type_instances_2 it's equivalent to having defined a type family. So they really are one and the same."
    - id: 7794
      author: Jesper Nordenberg
      date: "2014-09-05 05:11:39"
      content: "TBH, the module system of Haskell is a mess compared to first class modules for example found in the Scala DOT calculus (modules and objects really are the same thing). All the additions to Haskell feels tucked on and really not integrated well in the language (why have records AND type classes AND modules?). But in fairness, this goes for pretty much all FPL's including SML, OCaml, Rust (if you can call it a FPL) etc. IMHO the module system needs to be one of the basic concerns when designing a type system."
    - id: 7795
      author: Chris Warburton
      date: "2014-09-05 05:26:53"
      content: |
        Have you considered providing a Haskell interface for all of this, so that signature modules can specify their own edge-case handling programatically rather than relying on BackPack to get everything right?
        
        For example:
        
        <pre>module A where
        import BackPack as BP
        
        app = BP.module "MyApp"
        
        -- Prefer Show Foo from MyApp, if available
        otherApp = if BP.hasInstance app ("Show", "Foo")
                      then BP.withoutInstance otherApp' ("Show", "Foo")
                      else otherApp'
                     where otherApp' = BP.module "MyOtherApp"
        
        -- Special name, similar to "main"
        module :: BP.Module
        module = BP.mkMod { BP.imports [app, otherApp] }</pre>
        
        Here, the "module" variable is being treated as the module's definition, similar to how "main" is treated as the program entry-point. The value of "module" is defined using a reflection-like API provided by BackPack, which we're using to define our import list programatically as [app, otherApp].
        
        "app" is just the "MyApp" module, whilst "otherApp" is either the "MyOtherApp" module, or a modified version with its 'Show Foo' instance hidden.
        
        This would be a "power-user" escape-hatch for the basic import/export functionality, which allows that mechanism to be kept simple.
    - id: 7828
      author: Edward Z. Yang
      date: "2014-09-06 04:43:55"
      content: "Chris: In principle, I don't think there is anything that is excluding us from building an alternative DSL for managing the module language, in the same way MirageOS has its own DSL for managing functor application in their operating system. I do tend to think about Cabal as the baseline, however, because it's always good to support the non-power users. :)"
    - id: 7834
      author: Robert Harper
      date: "2014-09-06 05:56:20"
      content: "We would never sacrifice modularity, so no worries there.  The type class mechanism is reduced to its proper place when one starts with modules."
    - id: 7835
      author: Edward Z. Yang
      date: "2014-09-06 06:17:16"
      content: "Robert: The problem here is not a question of language design, but how end-users write libraries. Education, that is."
    - id: 7838
      author: Derek Dreyer
      date: "2014-09-06 11:55:48"
      content: |
        I agree with your main point, Edward.  I actually think the anti-modularity of Haskell's existing type classes -- and the way this anti-modularity is baked into the Haskell ecosystem -- is more pernicious than that introduced by open type families because the problem with type classes is more subtle and easily overlooked.  The whole idea of "Set t" being a type is fundamentally anti-modular because its meaning depends on Ord t, and that forces a single global definition of Ord t in order to avoid accidental incoherence (sets of t's ordered one way being confused with sets of t's ordered another way).  But offhand one might not realize this because allowing different instances in different scopes would not cause a "segfault".  Open type families have at least the benefit of forcing this issue out into the open.  But when they were introduced, the idea of supporting separate modular development was not yet a primary concern I guess, so it didn't raise any red flags.
        
        FWIW, I think it's a very interesting question how to support such types cleanly in the framework of modular type classes.  I came up with a rough design for this, which I never wrote up, but I did give a talk about it at WG2.8 in 2007.  Here are the slides: 
        
        http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/24/slides/derek.pdf
        
        The basic idea was that actually modular type classes work much better when you add applicative functors to the mix, and then you can give a nice story about what "Set t" means.  In fact, I argued that modular type classes are the killer app for applicative functors!
        
        Basically, "Set t" is a type which only makes sense in a scope where there is a canonical instance X : Ord t, at which point Set t elaborates to Set(X).t.  If no canonical instance is in scope, the type simply does not make sense.  With enough syntactic tricks, you can make this convenient to program with in most cases.  See for instance slides 31-33.  I should really work this story out more carefully.  (Scott and I started doing so a few years ago, but got, um, sidetracked by Backpack. :-)
    - id: 12922
      author: songzh
      date: "2015-03-04 22:22:18"
      content: |
        I am Newbie  of Haskell, but I have encountered this problem several times. I suppose that the open type family problem or overlapped class instances are very similar. with each other. What I would say is  if we can assign each type instance of a family a Double to sort the matching order of the instances, would that be OK? 
        
            type precedence 1.0 F Int
            type instance F Int = Int
        
        or 
        
            {-# PRECEDENCE 1.0 #-}
            type instance F Int = Int
        
        The problem can be solved since there is a very good chance to have a Double in between any 2 type instances, so the user can always sneak in an instance. And maybe it can be dynamically set in different modules.
    - id: 12926
      author: Edward Z. Yang
      date: "2015-03-04 23:54:50"
      content: "songzh: Unfortunately not: problem with overlap is when you have conflicting instances which are not visible at the same time. Then you won't be able to apply your rules because there is only one match. However, it is still unsound..."
    - id: 14476
      author: Anonymous
      date: "2015-06-01 19:52:20"
      content: |
        I have quite difference idea:
        
        In your second example, you say "should the following module A be a permissible implementation of the signature?" but, that is backward! Module A's definition is permissible; it is module B which is wrong.
        
        Module A should be allowed to expose or don't expose the instance based if they want to.
        
        If the type instance is exposed, then it is clearly a type error; module B overrides the instance but that overridden instance isn't used because it doesn't match the instance where f is defined.
        
        If the type instance is not exposed, then it is also an error, because it doesn't even know whether or not f is a function; there is no type information to deduce that.
        
        However, now you write:  data T x = C (F x -&gt; x);  Now what? It depend if C is exposed, as well as whether or not the kind of T contains the (F x) constraint. However, you can add the constraint to the constructor instead:  data T :: (* -&gt; *) where { C :: forall x. (F x) =&gt; (F x -&gt; x) -&gt; T x; };  In either case, since F is a family rather than a class, it is an error to expose C unless T has the (F x) constraint on the datatype itself rather than only the constructor.
        
        But, now what about this?  data T :: * where { C :: forall x. (F x -&gt; x) -&gt; T; };  Now the constructor C is useless if F is an open family, because it is impossible to do anything with it; the constraint (F x) in the constructor can be implied, but it is impossible to put it into the type declaration too. Nobody can read back a value of type T, unless perhaps, T now has a module/scope constraint too. Therefore, anything define of type T has to be in scope, meaning, it is impossible to expose it unless used only in a polymorphic way.
        
        However, families and classes should be of three kinds: open, closed, and automatic. You should be allowed to define all three (in the automatic case, you probably have to define in terms of other automatics together with certain kind of macros and type-level codes; Typeable should be an automatic class (N.B. this will not allow you to use a Typeable constraint unless either the type is not polymorphic, or a Typeable constraint is explicitly specified!)).
        
        Closed families would avoid the problems mentioned above.
    - id: 14477
      author: Anonymous
      date: "2015-06-01 20:03:54"
      content: "P.S. In your \"f :: Typeable a =&gt; a -&gt; F a\" case, you need to give type of f implicitly a (F a) constraint, in addition to the (Typeable a) that it already has. You also need (Typeable (F a)) as well in this case. Since the instances (F Int) do not match, eqT is Nothing; (f 0 True) is still OK but now it evaulates as (undefined True) and it is a runtime error; it doesn't segfault."
    - id: 14959
      author: Edward Z. Yang
      date: "2015-07-06 18:38:30"
      content: |
        > If the type instance is not exposed, then it is also an error, because it doesn’t even know whether or not f is a function; there is no type information to deduce that.
        
        In the second example, module B knows that f is a function because the defined instance IN module B says that F (mentioned in the type signature of f) reduces to a function. So you might say defining instances in B is not kosher, but that's equivalent to the "require all instances in the signature" proposal.
        
        Because I didn't agree with the statement here, I didn't follow the subsequent discussion of type families mentioned in constructors.
        
        > Closed families would avoid the problems mentioned above.
        
        I agree. Thus this post is about *open* type families.
        
        > In your “f :: Typeable a => a -> F a” case, you need to give type of f implicitly a (F a) constraint, in addition to the (Typeable a) that it already has. You also need (Typeable (F a)) as well in this case. 
        
        This is not true. A mention of a type family as 'F a' in a type signature does not induce any extra constraints.
    - id: 20939
      author: pyon
      date: "2016-07-11 11:12:50"
      content: |
        You say:
        
        &gt; I have a parting remark for the ML community, as type classes make their way in from Haskell: when you do get type classes in your language, don’t make the same mistake as the Haskell community and start using them to enforce invariants in APIs.
        
        There is nothing wrong with enforcing invariants in APIs. The problem is not using types correctly: In Haskell, the programmer has to tell the type system that the type `Set t` is parameterized by a type `t`, and only then that most operations on `Set t`s (e.g., `insert`) happen to be parameterized by an `Ord t` instance. As with any false assumptions, this causes us to reach wacky conclusions: if you have multiple `Ord` instances on a single type `t`, the type system will generate a single shared `Set t` type for all of them. The insistence on globally unique instances restores the validity of what the type system can derive, at the expense of modularity.
        
        ML doesn't have this limitation. In ML, the set abstraction is parameterized by an entire order structure, not just its carrier type. This reflects the actual math more accurately, and you get better type checking as a result: You can generate unequal set types from unequal order structures defined on the same carrier type.
        
        I'd also like to remark that open type families feel very much like typedef members in C++. If that alone isn't enough of an indictment of the feature, I don't know what could be.
    - id: 20941
      author: Edward Z. Yang
      date: "2016-07-11 11:23:47"
      content: "pyon: I *certainly* agree that with ML modules, you can do this correctly. But how do you write the API for a Set data type using modular type classes? As far as I can tell, you don't! But the temptation to use type classes here is *extremely* great (Rust succumbed!)"
    - id: 20942
      author: pyon
      date: "2016-07-11 12:14:26"
      content: |
        I don't know how to format code in comments, so I posted a gist: https://gist.github.com/eduardoleon/f9289748c5d1fde76afb773d225d2ae2
        
        The basic idea is that the unification phase of type checking has to take into consideration the result of elaborating modular type classes.
    - id: 20943
      author: Edward Z. Yang
      date: "2016-07-11 12:19:08"
      content: "Ah! Does this actually work? If so, very cool."
    - id: 20944
      author: pyon
      date: "2016-07-11 12:27:43"
      content: |
        The API for sets would be defined using normal ML signatures. The only thing that changes is that the semantics of the module language is changed so that functor names now also denote the result of implicitly applying them to type classes (nominated structures).
        
            using IntOrd
            
            (*  `Set` elaborates to `Set(IntOrd)`,
             *  so `val xs : Set(IntOrd).t`  *)
            val xs = Set.fromList [1,2,3];
            
            (*  shadows previous `using` declaration  *)
            using Backwards(IntOrd)
            
            (*  `Heap` elaborates to `Set(Backwards(IntOrd))`,
             *  so `val ys : Set(Backwards(IntOrd)).t`  *)
            val ys = Set.fromList [4,5,6];
            
            (*  type error: can't unify `xs`'s type `Set(IntOrd).t`
             *  with expected type `Set(Backwards(IntOrd)).t`  *)
            val zs = Set.union (xs, ys)
        
        In other words, the constraint solver component of the type checker now has to take into account the result of elaborating modular type classes.
    - id: 20945
      author: pyon
      date: "2016-07-11 12:28:29"
      content: "Errr, sorry for the last post. I thought the previous one hadn't correctly been submitted."
---

One of the major open problems for building a module system in Haskell is the treatment of type classes, which I have [discussed previously](http://blog.ezyang.com/2014/07/type-classes-confluence-coherence-global-uniqueness/) on this blog. I've noted how the current mode of use in type classes in Haskell assume “global uniqueness”, which is inherently anti-modular; breaking this assumption risks violating the encapsulation of many existing data types.

As if we have a choice.

In fact, our hand is forced by the presence of **open type families** in Haskell, which are feature many similar properties to type classes, but with the added property that global uniqueness is *required* for type safety. We don't have a choice (unless we want type classes with associated types to behave differently from type classes): we have to figure out how to reconcile the inherent non-modularity of type families with the Backpack module system.

In this blog post, I want to carefully lay out why open type families are *inherently* unmodular and propose some solutions for managing this unmodularity. If you know what the problem is, you can skip the first two sections and go straight to the proposed solutions section.

------------------------------------------------------------------------

Before we talk about open type family instances, it's first worth emphasizing the (intuitive) fact that a signature of a module is supposed to be able to *hide* information about its implementation. Here's a simple example:

    module A where
        x :: Int

    module B where
        import A
        y = 0
        z = x + y

Here, `A` is a signature, while `B` is a module which imports the signature. One of the points of a module system is that we should be able to type check `B` with respect to `A`, without knowing anything about what module we actually use as the implementation. Furthermore, if this type checking succeeds, then for *any* implementation which provides the interface of `A`, the combined program should also type check. This should hold even if the implementation of `A` defines other identifiers not mentioned in the signature:

    module A where
        x = 1
        y = 2

If `B` had directly imported this implementation, the identifier `y` would be ambiguous; but the signature *filtered out* the declarations so that `B` only sees the identifiers in the signature.

------------------------------------------------------------------------

With this in mind, let's now consider the analogous situation with open type families. Assuming that we have some type family `F` defined in the prelude, we have the same example:

    module A where
        type instance F Int
        f :: F Bool

    module B where
        import A
        type instance F Bool = Int -> Bool
        x = f 2

Now, should the following module `A` be a permissible implementation of the signature?

    module A where
        type instance F Int = Int
        type instance F Bool = Int
        f = 42

If we view this example with the glasses off, we might conclude that it is a permissible implementation. After all, the implementation of `A` provides an extra type instance, yes, but when this happened previously with a (value-level) declaration, it was hidden by the signature.

But if put our glasses on and look at the example as a whole, something bad has happened: we're attempting to use the integer 42 as a function from integers to booleans. The trouble is that `F Bool` has been given different types in the module `A` and module `B`, and this is unsound... like, *segfault* unsound. And if we think about it some more, this should not be surprising: we already knew it was unsound to have overlapping type families (and eagerly check for this), and signature-style hiding is an easy way to allow overlap to sneak in.

The distressing conclusion: **open type families are not modular.**

------------------------------------------------------------------------

So, what does this mean? Should we throw our hands up and give up giving Haskell a new module system? Obviously, we’re not going to go without a fight. Here are some ways to counter the problem.

# The basic proposal: require all instances in the signature

The simplest and most straightforward way to solve the unsoundness is to require that a signature mention all of the family instances that are *transitively* exported by the module. So, in our previous example, the implementation of `A` does not satisfy the signature because it has an instance which is not mentioned in the signature, but would satisfy this signature:

    module A where
        type instance F Int
        type instance F Bool

While at first glance this might not seem too onerous, it's important to note that this requirement is *transitive*. If `A` happens to import another module `Internal`, which itself has its own type family instances, *those must be represented in the signature as well.* (It's easy to imagine this spinning out of control for type classes, where any of the forty imports at the top of your file may be bringing in any manner of type classes into scope.) There are two major user-visible consequences:

1.  Module imports are *not* an implementation detail—you need to replicate this structure in the signature file, and
2.  Adding instances is *always* a backwards-incompatible change (there is no weakening).

Of course, as Richard pointed out to me, this is *already* the case for Haskell programs (and you just hoped that adding that one extra instance was "OK").

Despite its unfriendliness, this proposal serves as the basis for the rest of the proposals, which you can conceptualize as trying to characterize, “When can I avoid having to write all of the instances in my signature?”

# Extension 1: The orphan restriction

Suppose that I write the following two modules:

    module A where
        data T = T
        type instance F T = Bool

    module B where
        import A
        type instance F T = Int -> Int

While it is true that these two type instances are overlapping and rightly rejected, they are not equally at fault: in particular, the instance in module `B` is an *orphan*. An orphan instance is an instance for type class/family `F` and data type `T` (it just needs to occur anywhere on the left-hand side) which lives in a module that defines neither. (`A` is not an orphan since the instance lives in the same module as the definition of data type `T`).

What we might wonder is, “If we disallowed all orphan instances, could this rule out the possibility of overlap?” The answer is, “Yes! (...with some technicalities).” Here are the rules:

1.  The signature must mention all what we will call *ragamuffin instances* transitively exported by implementations being considered. An instance of a family `F` is a *ragamuffin* if it is not defined with the family definition, or with the type constructor at the head in the first parameter. (Or some specific parameter, decided on a per-family basis.) All orphan instances are ragamuffins, but not all ragamuffins are orphans.
2.  A signature exporting a type family must mention *all* instances which are defined in the same module as the definition of the type family.
3.  It is strictly optional to mention non-ragamuffin instances in a signature.

(Aside: I don't think this is the most flexible version of the rule that is safe, but I do believe it is the most straightforward.) The whole point of these rules is to make it impossible to write an overlapping instance, while only requiring local checking when an instance is being written. Why did we need to strengthen the orphan condition into a ragamuffin condition to get this non-overlap? The answer is that absence of orphans does not imply absence of overlap, as this simple example shows:

    module A where
        data A = A
        type instance F A y = Int

    module B where
        data B = B
        type instance F x B = Bool -> Bool

Here, the two instances of `F` are overlapping, but neither are orphans (since their left-hand sides mention a data type which was defined in the module.) However, the `B` instance is a ragamuffin instance, because `B` is not mentioned in the first argument of `F`. (Of course, it doesn't really matter if you check the first argument or the second argument, as long as you're consistent.)

Another way to think about this rule is that open type family instances are not standalone instances but rather metadata that is associated with a type constructor *when it is constructed*. In this way, non-ragamuffin type family instances are modular!

A major downside of this technique, however, is that it doesn't really do anything for the legitimate uses of orphan instances in the Haskell ecosystem: when third-parties defined both the type family (or type class) and the data type, and you need the instance for your own purposes.

# Extension 2: Orphan resolution

This proposal is based off of one that Edward Kmett has been floating around, but which I've refined. The motivation is to give a better story for offering the functionality of orphan instances without gunking up the module system. The gist of the proposal is to allow the package manager to selectively enable/disable orphan definitions; however, to properly explain it, I'd like to do first is describe a few situations involving orphan type class instances. (The examples use type classes rather than type families because the use-cases are more clear. If you imagine that the type classes in question have associated types, then the situation is the same as that for open type families.)

The story begins with a third-party library which defined a data type `T` but did not provide an instance that you needed:

    module Data.Foo where
        data Foo = Foo

    module MyApp where
        import Data.Foo
        fooString = show Foo -- XXX no instance for Show

If you really need the instance, you might be tempted to just go ahead and define it:

    module MyApp where
        import Data.Foo
        instance Show Foo where -- orphan
            show Foo = "Foo"
        fooString = show Foo

Later, you upgrade `Data.Foo` to version 1.0.0, which does define a `Show` instance, and now your overlapping instance error! Uh oh.

How do we get ourselves out of the mess? A clue is how many package authors currently “get out of jail” by using preprocessor macros:

    {-# LANGUAGE CPP #-}
    module MyApp where
        import Data.Foo
    #if MIN_VERSION_foo(1,0,0)
        instance Show Foo where -- orphan
            show Foo = "Foo"
    #endif
        fooString = show Foo

Morally, we'd like to hide the orphan instance when the real instance is available: there are two variations of `MyApp` which we want to transparently switch between: one which defines the orphan instance, and one which does not and uses the non-orphan instance defined in the `Data.Foo`. The choice depends on which `foo` was chosen, a decision made by the package manager.

Let's mix things up a little. There is no reason the instance has to be a non-orphan coming from `Data.Foo`. Another library might have defined its own orphan instance:

    module MyOtherApp where
        import Data.Foo
        instance Show Foo where ... -- orphan
        otherFooString = show Foo

    module MyApp where
        import Data.Foo
        instance Show Foo where ... -- orphan
        fooString = show Foo

    module Main where
        import MyOtherApp
        import MyApp
        main = print (fooString ++ otherFooString ++ show Foo)

It's a bit awful to get this to work with preprocessor macros, but there are *two* ways we can manually resolve the overlap: we can erase the orphan instance from `MyOtherApp`, or we can erase the orphan instance from `MyApp`. A priori, there is no reason to prefer one or the other. However, depending on which one is erased, `Main` may have to be compiled *differently* (if the code in the instances is different). Furthermore, we need to setup a *new* (instance-only) import between the module who defines the instance to the module whose instance was erased.

There are a few takeaways from these examples. First, the most natural way of resolving overlapping orphan instances is to simply “delete” the overlapping instances; however, which instance to delete is a global decision. Second, *which* overlapping orphan instances are enabled affects compilation: you may need to add module dependencies to be able to compile your modules. Thus, we might imagine that a solution allows us to do both of these, without modifying source code.

Here is the game plan: as before, packages can define orphan instances. However, the list of orphan instances a package defines is part of the metadata of the package, and the instance itself may or may not be used when we actually compile the package (or its dependencies). When we do dependency resolution on a set of packages, we have to consider the set of orphan instances being provided and only enable a set which is non-overlapping, the so called **orphan resolution**. Furthermore, we need to add an extra dependency from packages whose instances were disabled to the package who is the sole definer of an instance (this might constrain which orphan instance we can actually pick as the canonical instance).

The nice thing about this proposal is that it solves an already existing pain point for type class users, namely defining an orphan type class instance without breaking when upstream adds a proper instance. But you might also think of it as a big hack, and it requires cooperation from the package manager (or some other tool which manages the orphan resolution).

------------------------------------------------------------------------

The extensions to the basic proposal are not mutually exclusive, but it's an open question whether or not the complexity they incur are worth the benefits they bring to existing uses of orphan instances. And of course, there may other ways of solving the problem which I have not described here, but this smorgasbord seems to be the most plausible at the moment.

At ICFP, I had an interesting conversation with Derek Dreyer, where he mentioned that when open type families were originally going into GHC, he had warned Simon that they were not going to be modular. With the recent addition of closed type families, many of the major use-cases for open type families stated in the original paper have been superseded. However, even if open type families had never been added to Haskell, we still might have needed to adopt these solutions: the *global uniqueness of instances* is deeply ingrained in the Haskell community, and even if in some cases we are lax about enforcing this constraint, it doesn't mean we should actively encourage people to break it.

I have a parting remark for the ML community, as type classes make their way in from Haskell: when you do get type classes in your language, don’t make the same mistake as the Haskell community and start using them to enforce invariants in APIs. This way leads to the global uniqueness of instances, and the loss of modularity may be too steep a price to pay.

------------------------------------------------------------------------

*Postscript.* One natural thing to wonder, is if overlapping type family instances are OK if one of the instances “is not externally visible.” Of course, the devil is in the details; what do we mean by external visibility of type family instances of `F`?

For some definitions of visibility, we can find an equivalent, local transformation which has the same effect. For example, if we never use the instance *at all*, it certainly OK to have overlap. In that case, it would also have been fine to delete the instance altogether. As another example, we could require that there are no (transitive) mentions of the type family `F` in the signature of the module. However, eliminating the mention of the type family requires knowing enough parameters and equations to reduce: in which case the type family could have been replaced with a local, closed type family.

One definition that definitely does *not* work is if `F` can be mentioned with some unspecified type variables. Here is a function which coerces an `Int` into a function:

    module A where
      type instance F Int = Int
      f :: Typeable a => a -> F a
      f x = case eqT of
        Just Refl -> x :: Int
        Nothing -> undefined

    module ASig where
      f :: Typeable a => a -> F a

    module B where
      import ASig
      type instance F Int = Bool -> Bool
      g :: Bool
      g = f 0 True -- oops

...the point being that, even if a signature doesn't directly mention the overlapping instance `F Int`, type refinement (usually by some GADT-like structure) can mean that an offending instance can be used internally.
