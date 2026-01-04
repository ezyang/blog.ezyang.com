---
title: "Dive into Backpack: Module reexports"
date: 2014-10-03 02:23:06
slug: backpack-module-reexports
draft: true
categories: [Backpack]
---

In this blog series, we’ll go in-depth into the new features that make up Backpack. The idea is that this series of posts should be something of a user manual for features related to Haskell’s module system.

------------------------------------------------------------------------

`reexported-modules` are a new field

Cabal files now support an extra field, `reexported-modules` which specify a list of modules to reexport from this package. For example, if I write in my Cabal file:

    name: bar
    ...
    build-depends: foo
    reexported-modules: Data.Foo

Supposing that `Data.Foo` is defined in library `foo`, if I have another package which depends on `bar` but not on `foo`, I will still be able to see and use the `Data.Foo` module, despite it being in a hidden package.

In isolation, the most important use-case of this feature is *refactoring a monolithic package into multiple packages.* For example, it used to be possible to just depend on Edward Kmett’s [category-extras](https://hackage.haskell.org/package/category-extras) and get all of the category-goodness; but now this package serves purely as a meta-package which doesn't actually export any modules itself. This is slightly annoying for users who don't want to be bothered about the differences between `semigroupoids` and `semigroupid-extras`, and want it all: they now have to list each package in their Cabal file. With module reexports, `category-extras` can reexport modules from its diverse dependencies, and now serve as a programmer useful metapackage, as opposed to just documentation.

Now, the clever ones among you might be wondering: “Couldn’t I have done this exact same thing with a combination of `PackageImports` and a shim module?” :

    {-# LANGUAGE PackageImports #-}
    module Data.Foo(module Data.Foo) where
        import "orig-package" Data.Foo

The key difference is what happens when you depend on both the original package and the reexporting package: now GHC will consider the module `Data.Foo` ambiguous:

    Ambiguous module name `Data.Foo':
      it was found in multiple packages: g-0.1 h-0.1

Module reexports truly reuse the same module, so if multiple packages provide the same module, GHC won’t consider the definition ambiguous.

The full syntax of a module reexport supports specifying a specific package for the reexport to come from, and also renaming the reexport. Thus:

> reexported-modules: orig-pkg:Name as NewName

This line reexports module `Name` from `orig-pkg` under the new name \`\`NewName.
