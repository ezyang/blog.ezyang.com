---
title: "Unintuitive facts about Safe Haskell"
date: 2012-09-17 20:34:45
slug: common-misconceptions-about-safe-haskell
categories: [Haskell]
comments:
    - id: 4206
      author: Dag
      date: "2012-09-18 09:09:43"
      content: "You might want to rephrase the title of this post; it makes you think the sub-headlines are the misconceptions rather than the counter-facts."
    - id: 4207
      author: Edward Z. Yang
      date: "2012-09-18 23:42:46"
      content: "Fair point; I've twiddled the title. (A little bird once told me that when debunking myths, you should state the truth in bold, not the myth.)"
    - id: 4239
      author: Eric Kow
      date: "2012-09-25 04:48:00"
      content: Pity the old URL has stuck though.
    - id: 4240
      author: Eric Kow
      date: "2012-09-25 04:49:17"
      content: "(but +1 for 21st century debunking!)"
    - id: 4250
      author: Jim Stuttard
      date: "2012-09-27 06:02:02"
      content: "Is \"to make it distrustful, you have to pass -fpackage-trust.\" correct? If so, it seems very counter-intuitive."
    - id: 4254
      author: Edward Z. Yang
      date: "2012-09-27 13:22:26"
      content: "The sense here is, \"Please start checking for package trust\"!"
    - id: 6203
      author: Omari Norman
      date: "2013-09-03 18:54:51"
      content: |
        My experience with Safe Haskell is that it is impossible to use it to enforce safe style, becuase there are too many packages that have unsafe functions in modules that are not marked Trustworthy. Some of these packages are in Haskell Platform and some even come with GHC.  For instance, Data.Time in the time package is not trustworthy, and none of the regular expression libraries (text-icu, pcre-light, or the big glob of regex packages) are trustworthy either.
        
        This means that if I have a module that imports any of these modules, Safe Haskell will declare my module unsafe, which spreads up the chain to any module that imports my module.  One way around this is to slap a Trustworthy pragma on my module--which sort of defeats the point of using Safe Haskell to enforce good style.  Alternatively I can ask the package maintainer to add a Trustworthy pragma to her module--which works, assuming the maintainer wants to do this--but it also foists Safe Haskell work on folks who do not want to be bothered with it.
        
        Safe Haskell seems quite reasonable for folks who want to use it for hardened systems--the "restricted IO monad" scenario given in the GHC Safe Haskell manual.  In that situation it's actually reasonable for the admin to go through "unsafe" packages like time, make sure they are not doing bad things, and recompile them with -XTrustworthy.  But this is entirely too much work for someone who just wants to use Safe Haskell to enforce or even advise on good style.
        
        I would think a way to make this better would be to add some sort of GHC flag that would allow the user to easily mark a package as Trustworthy.  With this flag, any module in the package that does not bear an Unsafe pragma but that would otherwise be Unsafe would instead be considered Trustworthy.  The key is that the client could easily make this declaration, rather than foisting this duty upon the other library maintainer who might not care about Safe Haskell.  I should be able to easily say "OK, Bryan O'Sullivan does not like Safe Haskell so he does not mark things as Trustworthy but I trust that his APIs have signatures I can trust, and I looked at his source and it's fine, so I trust his package."  Right now the only way to do that is to recompile his package or get him to add pragmas, which makes no sense seeing as the end result is the same--I am declaring that I trust this package.  Trusting a package should be easier than this.
    - id: 6204
      author: Edward Z. Yang
      date: "2013-09-04 03:54:43"
      content: "You should be careful about terminology here, because you can run  <code>ghc-pkg -trust</code> (it just won't do what you want). It is possible the easiest way to do this is to pass <code>--with-ghc-option=-XTrustworthy</code> when cabal install'ing, to trust an entire package. There is also a surgical approach, where you create a module that reexports other modules, and mark that as trustworthy: this is your, \"It's fine to use these functions\" file, although that is a little heavyweight."
    - id: 6205
      author: Omari Norman
      date: "2013-09-04 13:12:31"
      content: |
        True, both of these methods (recompilation or using shims) are possible. I think using the shim is the least undesirable solution under these circumstances.  My point is that both of these solutions are kludges that make more work, when the end result is the same: I want to trust a package because I have examined and verified the code, or just because I trust whoever wrote it.  I think Safe Haskell should have an option for just that; right now, it doesn't.  If I have added a package to my list of trusted packages using "ghc-pkg -trust", I would think the system could, in effect, mark a module in the package as Trustworthy, assuming the module does not already bear a Safe Haskell pragma.
        
        Safe Haskell seems to be placing a premium on the package maintainer marking a module as Trustworthy, as it's a pain for the client to mark something Trustworthy without recompilation or shims.  Why not make it easier for me to examine the code and draw my own conclusions.
        
        As it is, package maintainers have chafed at slapping "Trustworthy" pragmas on their code, and I think they're right.  It should be easier for me to say "I trust this code" without the maintainer having to raise his hand first and say "trust me."
    - id: 22207
      author: "Markov chain poem trainer+generator in 29 sloc of Haskell - A geek with a hat"
      date: "2018-01-21 13:51:50"
      content: "[&#8230;] Common misconceptions about Safe Haskell [&#8230;]"
---

[Safe Haskell](http://www.haskell.org/ghc/docs/7.4.2/html/users_guide/safe-haskell.html) is a new language pragma for GHC which allows you to run untrusted code on top of a trusted code base. There are some common misconceptions about how Safe Haskell works in practice. In this post, I’d like to help correct some of these misunderstandings.

# \[`system 'rm -Rf /' :: IO ExitCode`\] is accepted by Safe Haskell

Although an IO action here is certainly unsafe, it is not rejected by Safe Haskell per se, because the type of this expression clearly expresses the fact that the operation may have arbitrary side effects. Your obligation in the trusted code base is to not run untrusted code in the IO monad! If you need to allow limited input/output, you must define a restricted IO monad, which is described in the manual.

# Safe Haskell programs can hang

Even with `killThread`, it is all to easy to permanently tie up a capability by creating a [non-allocating infinite loop](http://hackage.haskell.org/trac/ghc/ticket/367). This bug has been open for seven years now, but we consider this a major deficiency in Safe Haskell, and are looking for ways to prevent this from occurring. But as things are now, Safe Haskell programs need to be kept under check using operating system level measures, rather than just Haskell's thread management protocols.

# Users may mistrust `Trustworthy` modules

The `Trustworthy` keyword is used to mark modules which use unsafe language features and/or modules in a “safe” way. The safety of this is vouched for by the maintainer, who inserts this pragma into the top of the module file. Caveat emptor! After all, there is no reason that you should necessarily believe a maintainer who makes such a claim. So, separately, you can trust a package, via the `ghc-pkg` database or the `-trust` flag. But GHC also allows you to take the package maintainer at their word, and in fact does so by default; to make it distrustful, you have to pass `-fpackage-trust`. The upshot is this:

| Module trusted?   | (no flags) | `-fpackage-trust` |
|-------------------|------------|-------------------|
| Package untrusted | Yes        | No                |
| Package trusted   | Yes        | Yes               |

If you are serious about using Safe Haskell to run untrusted code, you should always run with `-fpackage-trust`, and carefully confer trusted status to packages in your database. If you’re just using Safe Haskell as a way of enforcing code style, the default are pretty good.

# Explicit untrust is important for maintaining encapsulation

Safe Haskell offers safety inference, which automatically determines if a module is safe by checking if it would compile with the `-XSafe` flag. Safe inferred modules can then be freely used by untrusted code. Now, suppose that this module (inferred safe) was actually `Data.HTML.Internal`, which exported constructors to the inner data type which allowed a user to violate internal invariants of the data structure (e.g. escaping). That doesn’t seem very safe!

The sense in which this is safe is subtle: the correctness of the trusted code base cannot rely on any invariants supplied by the untrusted code. For example, if the untrusted code defines its own buggy implementation of a binary tree, catching the bugginess of the untrusted code is out of scope for Safe Haskell’s mission. But if our TCB expects a properly escaped `HTML` value with no embedded JavaScript, the violation of encapsulation of this type could mean the untrusted code could inject XSS.

David Terei and I have some ideas for making the expression of trust more flexible with regards to package boundaries, but we still haven't quite come to agreement on the right design. (Hopefully we will soon!)

# Conclusion

Safe Haskell is at its heart a very simple idea, but there are some sharp edges, especially when Safe Haskell asks you to make distinctions that aren't normally made in Haskell programs. Still, Safe Haskell is rather unique: while there certainly are widely used sandboxed programming languages (Java and JavaScript come to mind), Safe Haskell goes even further, and allows you to specify your own, custom security policies. Combine that with a massive ecosystem of libraries that play well with this feature, and you have a system that you really can’t find anywhere else in the programming languages universe.
