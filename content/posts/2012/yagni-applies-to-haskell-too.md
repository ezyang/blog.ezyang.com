---
title: "YAGNI applies to Haskell too"
date: 2012-02-04 18:49:50
slug: yagni-applies-to-haskell-too
draft: true
categories: [Haskell]
---

There comes a time in every Haskeller’s life when a choice must be made: should the type of a function be this? :

    f :: (MonadState s m, MonadReader r m) => m Int

Or this? :

    f :: MyMonad Int

I believe judicious application of the [you ain't gonna need it](http://en.wikipedia.org/wiki/YAGNI) principle applies here. Unless you have good reason to believe that you are actually going to need the extra features, less is more, since:

- Haskell code doesn't take very much time to write, and
- Far reaching global changes are easy to do correctly, thanks to a nice type checker. (Additionally, the motions of going through your codebase and fixing up all of the errors also serves nicely to make you think about all of the code dependencies, and what they are doing.)

The extra cognitive overhead of something that you "might need later” probably isn’t worth it. In fact, going for the simple design first may *still* be worth it even if you know a more complicated feature is going to be needed later, because it's just so easy to rewrite Haskell code.

This advice may be less applicable to library writers. But, as is the case with most products, it’s a mistake to try to please every possible developer who might use your library at the cost of overall user experience, and it’s not clear you’ll even know what generalities people are going to want in practice.
