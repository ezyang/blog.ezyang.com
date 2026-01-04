---
title: "Expression-level Backpack"
date: 2015-03-31 19:29:03
slug: expression-level-backpack
draft: true
categories: [Backpack, Haskell]
---

Reading about [Philipp Schuster’s](http://haskellexists.blogspot.de/2015/03/a-very-first-step-towards-fragment.html) experimentation with fragmenting modules into per expression "modules" got me thinking about how Backpack would work in such a context. It’s in fact, very simple: modules are now used to organize mix-in linking of *expressions*. In this regime packages are no longer necessary, modules are all you need.

The problem with this proposal, is that it is a very invasive change to the Haskell source language and seems difficult to implement. But it's interesting to think about.

# A basic example

Let’s start with a simple example, suppose you are writing a module, and you have some integer constant which you would like to make parametrizable by users of your module:

    module Crypto where
      keySize = 512
      mkKey = ...

All you need to make this parametrizable is to replace `keySize` with just a signature:

    module Crypto where
      keySize :: Int
      mkKey = ...

You can still typecheck this module, but you can't compile it. To fill in the keySize, simply write down an implementation for it:

    module Main where
      import Crypto
      keySize = 1024
      main = ...

(In traditional Haskell, this would result in a conflicting definition, but in this world, a hole assigned to the same name as an implementation causes that hole to be filled in with that implementation.)

For those coming from the object-oriented world, this should look very similar to mix-ins in those settings. An important difference, however, is that you can play the same game with *types*.

# Extended namespace support

Since imports are to be used for specifying implementations of holes, there are some extensions for importing and exporting namespaces which would make this system easier to use.

The most important such change is to make the Haskell module namespace truly hierarchical. Take this example:

    module Prelude where
      data Bool = True | False
      not :: Bool -> Bool

    module A where
      import Prelude
      x = not True

    module B where
      import Prelude
      y = not False

Here, `A` and `B` really need to also be exporting `module Prelude`, since eventually someone needs to implement it. However, `module A ( module Prelude )` would be inappropriate, as this simply flattens all the available identifiers of `Prelude` into the scope. An alternate keyword would not flatten, but make the module available lower in the hierarchy:

    module B(y, nested Prelude) where
      import Prelude
      y = not False

    module C where
      import B
      z = ... B.Prelude.not ...

Additionally, *namespace-qualified* definitions of expressions and types should be permitted:

    module Main where
      import qualified Crypto
      Crypto.keySize = 1024
      main = ...

Nested module declarations could be used to achieve the same effect:

    module Main where
      import qualified Crypto
      module Crypto where
        keySize = 1024
      main = ...

Namespace-qualified definitions would be useful for type synonyms too, which could be used to assert that two types are type equal:

    module A where
      import qualified B
      import qualified C
      type B.T = C.T

# Type equality

Modules continue to serve a role as mere *namespaces*: these namespaces can cause linking, but at the end of the day the core unit of compilation is an expression—thus, type identity must follow. Here’s an example of this in action:

    module Types where
      data A = ...
      data B = ...
      data TyA = TyA A
      data TyB = TyB A B

    module Main where
      import Types (A, B as B1, TyA as TyA1, TyB as TyB1)
      import Types (A, B as B2, TyA as TyA2, TyB as TyB2)
      data A = A
      data B1 = B1
      data B2 = B2

Here, `A` and `B` are type holes in the `Types` module, and `Main` includes `Types` multiple times, filling in the `B` hole with different types. Inside `Main`, `TyB1` and `TyB2` are considered type-distinct, but `TyA1` and `TyA2` are type-equal! This is contrary to how Paper Backpack works (let alone how we are implementing Backpack!)

# Open questions

This scheme seems deliciously simple, which makes one wonder, why hasn’t it been proposed before? Here are some possibilities:

1.  It’s difficult to write an ahead-of-time compiler when your unit of compilation is an expression, both in terms of the compilation itself, and also *what you are compiling to*. (Object file per expression?) GHC is certainly not written this way, and it would take a lot of work to make it so.
2.  Type inference? Folklore is that the module language must be kept separate, lest type inference become impossible to do. It’s not clear how this proposal side-steps this problem, except insofar as much as its roots in Backpack’s design.
3.  Type classes operate in an extra-linguistic way compared to normal dependencies on expressions and types. It’s unclear what it’s even supposed to mean when you’ve atomized the unit of modularity to an expression and imports are purely a mechanism for namespace management.
4.  Intrusiveness to Haskell? Backpack could largely get away with the claim that it was an addition to Haskell that didn't affect the core language (not entirely true because it needed to introduce a new source-level type-checking judgement for signatures), but here we make no pretense about the changes to Haskell proper. With this level of intrusiveness, could this really just be one of the many ML-style module systems that mark the research landscape which I haven’t seen yet?

Further investigation is necessary!
