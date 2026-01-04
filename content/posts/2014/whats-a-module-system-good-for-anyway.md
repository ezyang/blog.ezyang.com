---
title: "What's a module system good for anyway?"
date: 2014-08-09 19:21:19
slug: whats-a-module-system-good-for-anyway
categories: [Backpack, Haskell]
comments:
    - id: 7348
      author: eccstartup
      date: "2014-08-09 23:34:58"
      content: "The examples about cabal are so right. Resolving dependencies are more and more difficult nowadays. I don't know what a new haskeller will feel when he has just installed the haskell-platform 2014.2.0 and tries \"cabal install music-suite\". Simply a new commit (https://github.com/music-suite/music-score/commit/8434150698debdd60b92a481c7fa4eaa5aa9f20a) not in hackage will cause install failed. Hope that this can be solved one day to open the door of haskell to more people."
    - id: 7349
      author: NP
      date: "2014-08-10 04:20:04"
      content: |
        Really cool to see work on modules. 
        They are both important and practical in a distributed world.
        
        Abstraction need to be first class in every possible form.
        
        I hope this will come to other mainstream languages as well.
    - id: 7351
      author: tawnypigeon
      date: "2014-08-10 06:47:22"
      content: "As a new haskeller, it's heartening to see people trying to fix what from my perspective are gaping holes in the Haskell environment. The first time you encounter Cabal hell it's like discovering Santa's not real."
    - id: 7352
      author: Anders Leino
      date: "2014-08-10 06:49:58"
      content: |
        Typo: "day-to-day problems of a working programming", I think you mean "programmer".
        
        I'm glad this kind of issues are getting attention!
        
        <strong>Edward:</strong> Thanks, fixed!
    - id: 7354
      author: glaebhoerl
      date: "2014-08-10 11:10:12"
      content: |
        If designing an effective and principled type class to capture what String, ByteString, and Text "have in common" is difficult, then why would designing a module signature to do the same thing be any easier? I think these are the same problem.
        
        But fear not, I've heard that monoid-subclasses is the "right" solution to this problem: http://hackage.haskell.org/package/monoid-subclasses
        
        (In particular TextualMonoid: http://hackage.haskell.org/package/monoid-subclasses-0.3.6/docs/Data-Monoid-Textual.html)
    - id: 7356
      author: Erik Erlandson
      date: "2014-08-10 12:14:54"
      content: "Another wish-list property I think would be useful is if compatibility between client and library was aware of which subset of a library's signature the client was using.  If a client is only using part of a module, changes to the other parts are not necessarily problematic."
    - id: 7357
      author: Edward Z. Yang
      date: "2014-08-10 13:46:38"
      content: "glaebhoerl: Great question. I think if you wanted to design \"one\" module signature to capture string representations, you would certainly still have the same problem. However, in an Backpack or ML-like module system, it is a lot less expensive to have multiple module signatures than it is to have multiple type classes: to implement a new type class, you have to explicitly write an instance; but to implement a module signature, all you need is for the module to be set up in the right way."
    - id: 7358
      author: Nolan
      date: "2014-08-10 16:50:23"
      content: |
        Thank you! I agree with eccstartup. As a newcomer to Haskell, it took months of frustration trying to figure out how to resolve Cabal hell on OSX. Then I tried several distributions of Linux inside a VM and kept running into the same issues. Finally got some relief by using Stackage. 
        
        Now that I understand the nitty gritty details of Cabal, how to use sandboxes, the two package install locations, and the highly useful--but poorly documented Cabal flags--and so forth, I'm usually good to go. But it took a lot of effort and pain to get there. This made learning Haskell much more difficult. And I learned about the upper bound flag just today! After six months of daily study. 
        
        Haskell is the most rewarding language I've ever encountered. If I had known about it as an undergrad at Georgia Tech, I probably would have continued on with an advanced degree. But I was so jaded after years of Java and other C-like languages. Haskell really reignited my passion for computing. 
        
        I can't wait to see these ideas implemented. Your contributions  will open up Haskell to a much wider audience. And that bodes well for the future of software. Excellent work!
    - id: 7362
      author: JL
      date: "2014-08-10 23:54:25"
      content: |
        glaebhoerl: in my mind they aren't really the same problem, for a rather peculiar reason.  Even though many of the functions in e.g. `Data.Text` can be easily written in terms of smaller components, nobody really thinks that users should have to do so themselves.  Rather, users should be able to import `Data.Text` and have the full API available to them.  However, nobody wants to use large monolithic typeclasses (like ListLike), and really nobody is interested in writing new instances of monolithic typeclasses.  So while people are willing to write all the functions for the module, they aren't willing to list them all again in a class interface.  So hopefully there will be less resistance to using a module system like Backpack.
        
        I'm also not at all convinced that monoid-subclasses is any better in this respect.  The Textual monoid is yet another monolithic interface, and even though there are default definitions for most of the functions, nobody would actually want to implement instances that way because the performance will suffer (especially for optimized structures like Text).  And I find it particularly unsatisfying that the monoid subset has the same type as the full monoid.
    - id: 7369
      author: Scott Kilpatrick
      date: "2014-08-11 07:28:49"
      content: "A big reason not to use type classes for something like String is that your whole program never needs to switch between different String representations, most likely. Why would you want all this parameterized stuff for the programmer to constantly abstract and apply? Type inference and implicit instance resolution helps with this, but why bother? And, modulo some rewriting, the compiled code would concern itself with all that abstraction and application. Why not parameterize *everything* over the abstraction *once* and then let the whole compiled program be fixed to a particular representation?"
    - id: 7375
      author: glaebhoerl
      date: "2014-08-11 09:29:58"
      content: |
        @JL Note that TextualMonoid sits atop a hierarchy of well-justified finer-grained abstractions, all of which (including TextualMonoid) have laws. So while it may or may not be the "best possible" formulation (I'm not qualified to judge), I think it's pretty clear that it represents substantial progress over the likes of ListLike. But I'm not familiar enough with the package to debate specific points.
        
        Wandering over to the GitHub page, I see that the README has a nice overview and further justification: https://github.com/blamario/monoid-subclasses (I have no idea why this overview isn't also reproduced on the Hackage page!)
        
        Which in turn links to a paper: https://github.com/blamario/monoid-subclasses/wiki/Files/HaskellSymposium2013.pdf
    - id: 7397
      author: JL
      date: "2014-08-12 00:07:45"
      content: |
        @glaebhoerl I disagree that the finer-grained abstraction of TextualMonoid is well-justified.  That's actually my biggest complaint about the whole system.  I'll try to clarify: FactorialMonoid requires that every instance be reduceable to prime factors, but every prime factor has the same type as the full monoid.  For example, the prime factors of Strings are the empty list and singleton strings for each `Char`.  However, for any given `a :: String` you don't know (at the type level) if it's a factor or not.  If you want to make that distinction, you now need to add a type family (or fundep), but this makes operating on those elements awkward (because you can no longer `mappend` them together).  If you go further down this road, you end up with something closer to ListLike (or the newer mono-traversable).
        
        You can even formulate analogs of the monoid laws and require ListLike instances conform to them; an ad-hoc definition is that ListLike things should act like lists (mainly, for `t` to have a valid ListLike instance, `instance Monoid t` should be isomorphic to the free monoid).  Incidentally, mono-traversable doesn't restrict itself to these instances, which is fine IMHO because it doesn't claim to represent list-like structures, merely traversable structures.  But the only difference is whether or not instances observe the required laws, the code is virtually identical (or at least, mono-traversable is what ListLike would look like if it weren't several years older).
        
        In the end, I see ListLike and TextualMonoid as both essentially implementation details of the same idea, presenting slightly different interfaces.  Both are typeclass-based, though, with all the baggage that brings.  If Haskell had ML-style modules, most likely neither would have come into being.
    - id: 7414
      author: Neil Bartlett
      date: "2014-08-13 10:52:56"
      content: |
        This is really exciting. I'm mostly a Java developer, only dabbling in Haskell, but I've already encountered many of the modularity problems you describe.
        
        In the Java world we have a module system called OSGi, which isn't universally used but is very powerful -- at least within the bounds of what is possible with the language and virtual machine. In particular we use and enforce Semantic Versioning, i.e. the version attached to an export is required to actually *mean* something. Only then can the consumers and providers of an API actually rely on the exported version when they try to import with a version range.
        
        In addition I have worked on a tool (http://bndtools.org/) that detects when an API has changed incompatibly without properly "bumping" the version (this can be integrated into a CI build so that these errors are caught as early as possible). The detection is based on static inspection of the Java interfaces, by comparing them against the previously released artifact. Of course this works best with pure interfaces, and we cannot in general infer semantic changes in programmatic code.
        
        One would have thought that, if this kind of tooling is possible in Java, then it would certainly be possible in Haskell. I would love to investigate whether any of the experience we have gained can be reused.
    - id: 7541
      author: Edward Z. Yang
      date: "2014-08-21 08:41:02"
      content: "Neil: It's great to hear that Java already has a tool for enforcing semantic versions. How well does bndtools work in practice? I've worked on a few Java projects and not run against it: it seems most Java projects operate under the model where you vendor all of your dependencies."
    - id: 7635
      author: Neil Bartlett
      date: "2014-08-26 21:09:31"
      content: |
        Bndtools semantic versioning (we call this feature "baselining") does work very well in practice so long as you principally design your APIs around pure interfaces. Most Java APIs are not well designed in the first place so the tool is of limited utility there.
        
        As an example of how it works, suppose we previously released version 1.0.0 of a package and now add a method to an interface. This is not a breaking change for callers/consumers of the API, so the version of the package will be bumped to 1.1.0. The caller module will import the package with a range of "[1.0.0, 2.0.0)", meaning it accepts any version from 1.0.0 inclusive up to 2.0.0 exclusive. On the other hand, such a change IS breaking for implementers/providers of the interface, because they cannot implicitly provide the new method. They import with the range "[1.0.0, 1.1.0)" and so would never be allowed to wire up to the newer version.
        
        Then there are interfaces that are intended to be implemented by the consumer role... callback or listener interfaces are good examples. In these, you really cannot ever add methods without breaking backwards compatibility for everybody using your API. So adding a method to one of these would result in a bump up to version 2.0.0. This is automated as well, as the developer you just have to annotate your interface type to indicate whether it's intended for the consumer or provider role.
         
        I'm afraid it's not surprising that you didn't run into this tool. OSGi itself is used only by a small segment of the overall Java community, and Bndtools users form a subset within that.
        
        Anyway... I will read the Backpack documentation with great interest. Thanks for your blog post.
    - id: 19395
      author: Edward Z. Yang
      date: "2015-11-28 21:58:58"
      content: "Here is a very nice explanation of why modules would be desirable for Rust: https://internals.rust-lang.org/t/parameterized-modules/2883"
    - id: 36260
      author: Tophy
      date: "2025-10-01 10:46:17"
      content: "I can't speak to the issues with cabal as I've never experienced them. But I have a very similar question as glaebhoerl. I don't really understand how modules and type classes differ for the String example. If anything, I would just think maybe we should have some simple ToString or String class that all strings implement. Rust has ToString and it works fine. This just seems like a historical accident with how Haskell developed. All this is to say that I see that you've identified a problem and given a possible solution. What I don't see is where this possible solution is compared to other possible solutions. If we're imagining creating a language from scratch, do we really want to give up what type classes offer for this? Or can we find a reasonable way to solve this with type classes?"
    - id: 36261
      author: Tophy
      date: "2025-10-01 10:56:02"
      content: "Also another question I have is whether or not something like Idris solves this. In Idris you can just have incoherent instances of interfaces by naming interfaces. If no name is provided at the call site, the default interface is used. Doesn't this go a long way in solving what modules solve?"
---

This summer, I've been working at Microsoft Research implementing [Backpack](http://plv.mpi-sws.org/backpack/), a module system for Haskell. Interestingly, Backpack is not really a single monolothic feature, but, rather, an agglomeration of small, infrastructural changes which combine together in an interesting way. In this series of blog posts, I want to talk about what these individual features are, as well as how the whole is greater than the sum of the parts.

But first, there's an important question that I need to answer: **What's a module system good for anyway?** Why should you, an average Haskell programmer, care about such nebulous things as *module systems* and *modularity*. At the end of the day, you want your tools to solve specific problems you have, and it is sometimes difficult to understand what problem a module system like Backpack solves. As [tomejaguar puts it](http://www.reddit.com/r/haskell/comments/28v6c9/backpack_an_mllike_module_system_for_haskell/cierxc1): "Can someone explain clearly the precise problem that Backpack addresses? I've read the paper and I know the problem is 'modularity' but I fear I am lacking the imagination to really grasp what the issue is."

Look no further. In this blog post, I want to talk concretely about problems Haskellers have today, explain what the underlying causes of these problems are, and say why a module system could help you out.

# The String, Text, ByteString problem

As experienced Haskellers [are](http://blog.ezyang.com/2010/08/strings-in-haskell/) [well](http://stackoverflow.com/questions/19608745/data-text-vs-string) [aware](http://a-dimit.blogspot.com/2012/04/strings-in-haskell.html), there are multitude of string types in Haskell: String, ByteString (both lazy and strict), Text (also both lazy and strict). To make matters worse, there is no one "correct" choice of a string type: different types are appropriate in different cases. String is convenient and native to Haskell'98, but very slow; ByteString is fast but are simply arrays of bytes; Text is slower but Unicode aware.

In an ideal world, a programmer might choose the string representation most appropriate for their application, and write all their code accordingly. However, this is little solace for library writers, who don't know what string type their users are using! What's a library writer to do? There are only a few choices:

1.  They "commit" to one particular string representation, leaving users to manually convert from one representation to another when there is a mismatch. Or, more likely, the library writer used the default because it was easy. Examples: [base](https://hackage.haskell.org/package/base) (uses Strings because it completely predates the other representations), [diagrams](https://hackage.haskell.org/package/diagrams) (uses Strings because it doesn't really do heavy string manipulation).
2.  They can provide separate functions for each variant, perhaps identically named but placed in separate modules. This pattern is frequently employed to support both strict/lazy variants Text and ByteStringExamples: [aeson](http://hackage.haskell.org/package/aeson) (providing decode/decodeStrict for lazy/strict ByteString), [attoparsec](https://hackage.haskell.org/package/attoparsec) (providing Data.Attoparsec.ByteString/Data.Attoparsec.ByteString.Lazy), [lens](http://hackage.haskell.org/package/lens) (providing Data.ByteString.Lazy.Lens/Data.ByteString.Strict.Lens).
3.  They can use type-classes to overload functions to work with multiple representations. The particular type class used hugely varies: there is [ListLike](https://hackage.haskell.org/package/ListLike), which is used by a handful of packages, but a large portion of packages simply roll their own. Examples: SqlValue in [HDBC](http://hackage.haskell.org/package/HDBC), an internal StringLike in [tagsoup](https://hackage.haskell.org/package/tagsoup), and yet another internal StringLike in [web-encodings](http://hackage.haskell.org/package/web-encodings).

The last two methods have different trade offs. Defining separate functions as in (2) is a straightforward and easy to understand approach, but you are still saying *no* to modularity: the ability to support multiple string representations. Despite providing implementations for each representation, the user still has to commit to particular representation when they do an import. If they want to change their string representation, they have to go through all of their modules and rename their imports; and if they want to support multiple representations, they'll still have to write separate modules for each of them.

Using type classes (3) to regain modularity may seem like an attractive approach. But this approach has both practical and theoretical problems. First and foremost, how do you choose which methods go into the type class? Ideally, you'd pick a minimal set, from which all other operations could be derived. However, many operations are most efficient when directly implemented, which leads to a bloated type class, and a rough time for other people who have their own string types and need to write their own instances. Second, type classes make your type signatures more ugly `String -> String` to `StringLike s => s -> s` and can make type inference more difficult (for example, by introducing ambiguity). Finally, the type class `StringLike` has a very different character from the type class `Monad`, which has a minimal set of operations and laws governing their operation. It is difficult (or impossible) to characterize what the laws of an interface like this should be. All-in-all, it's much less pleasant to program against type classes than concrete implementations.

Wouldn't it be nice if I could `import String`, giving me the type `String` and operations on it, but then later decide which concrete implementation I want to instantiate it with? This is something a module system can do for you! This [Reddit thread](http://www.reddit.com/r/haskell/comments/28v6c9/backpack_an_mllike_module_system_for_haskell/cierxc1) describes a number of other situations where an ML-style module would come in handy.

(PS: Why can't you just write a pile of preprocessor macros to swap in the implementation you want? The answer is, "Yes, you can; but how are you going to type check the thing, without trying it against every single implementation?")

# Destructive package reinstalls

Have you ever gotten this error message when attempting to install a new package? :

    $ cabal install hakyll
    cabal: The following packages are likely to be broken by the reinstalls:
    pandoc-1.9.4.5
    Graphalyze-0.14.0.0
    Use --force-reinstalls if you want to install anyway.

Somehow, Cabal has concluded that the only way to install hakyll is to reinstall some dependency. Here's one situation where a situation like this could come about:

1.  pandoc and Graphalyze are compiled against the latest unordered-containers-0.2.5.0, which itself was compiled against the latest hashable-1.2.2.0.
2.  hakyll also has a dependency on unordered-containers and hashable, but it has an upper bound restriction on hashable which excludes the latest hashable version. Cabal decides we need to install an old version of hashable, say hashable-0.1.4.5.
3.  If hashable-0.1.4.5 is installed, we also need to build unordered-containers against this older version for Hakyll to see consistent types. However, the resulting version is the same as the preexisting version: thus, reinstall!

The root cause of this error an invariant Cabal currently enforces on a package database: there can only be *one* instance of a package for any given package name and version. In particular, this means that it is not possible to install a package multiple times, compiled against different dependencies. This is a bit troublesome, because sometimes you really do want the same package installed multiple times with different dependencies: as seen above, it may be the only way to fulfill the version bounds of all packages involved. Currently, the only way to work around this problem is to use a Cabal sandbox (or blow away your package database and reinstall everything, which is basically the same thing).

You might be wondering, however, how could a module system possibly help with this? It doesn't... at least, not directly. Rather, nondestructive reinstalls of a package are a critical feature for implementing a module system like Backpack (a package may be installed multiple times with different concrete implementations of modules). Implementing Backpack necessitates fixing this problem, moving Haskell's package management a lot closer to that of Nix's or NPM.

# Version bounds and the neglected PVP

While we're on the subject of cabal-install giving errors, have you ever gotten this error attempting to install a new package? :

    $ cabal install hledger-0.18
    Resolving dependencies...
    cabal: Could not resolve dependencies:
    # pile of output

There are a number of possible reasons why this could occur, but usually it's because some of the packages involved have over-constrained version bounds (especially upper bounds), resulting in an unsatisfiable set of constraints. To add insult to injury, often these bounds have no grounding in reality (the package author simply guessed the range) and removing it would result in a working compilation. This situation is so common that Cabal has a flag `--allow-newer` which lets you override the upper bounds of packages. The annoyance of managing bounds has lead to the development of tools like [cabal-bounds](https://github.com/dan-t/cabal-bounds), which try to make it less tedious to keep upper bounds up-to-date.

But as much as we like to rag on them, version bounds have a very important function: they prevent you from attempting to compile packages against dependencies which don't work at all! An under-constrained set of version bounds can easily have compiling against a version of the dependency which doesn't type check.

How can a module system help? At the end of the day, version numbers are trying to capture something about the API exported by a package, described by the [package versioning policy](http://www.haskell.org/haskellwiki/Package_versioning_policy). But the current state-of-the-art requires a user to manually translate changes to the API into version numbers: an error prone process, even when assisted [by](http://code.haskell.org/gtk2hs/tools/apidiff/) [various](http://hackage.haskell.org/package/precis) [tools](http://hackage.haskell.org/package/check-pvp). A module system, on the other hand, turns the API into a first-class entity understood by the compiler itself: a *module signature.* Wouldn't it be great if packages depended upon signatures rather than versions: then you would never have to worry about version numbers being inaccurate with respect to type checking. (Of course, versions would still be useful for recording changes to semantics not seen in the types, but their role here would be secondary in importance.) Some full disclosure is warranted here: I am not going to have this implemented by the end of my internship, but I'm hoping to make some good infrastructural contributions toward it.

# Conclusion

If you skimmed the introduction to the Backpack paper, you might have come away with the impression that Backpack is something about random number generators, recursive linking and applicative semantics. While these are all true "facts" about Backpack, they understate the impact a good module system can have on the day-to-day problems of a working programmer. In this post, I hope I've elucidated some of these problems, even if I haven't convinced you that a module system like Backpack actually goes about solving these problems: that's for the next series of posts. Stay tuned!
