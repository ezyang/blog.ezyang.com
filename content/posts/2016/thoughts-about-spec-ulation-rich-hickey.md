---
title: "Thoughts about Spec-ulation (Rich Hickey)"
date: 2016-12-16 19:26:00
slug: thoughts-about-spec-ulation-rich-hickey
categories: [Backpack, Programming Languages]
comments:
    - id: 21683
      author: Tikhon Jelvis
      date: "2016-12-16 22:03:25"
      content: |
        I've been playing around with Nix recently, and it feels pretty similar: any time you do something, it gets saved with a hash, so old derivations simply *can't* break. The Nix store keeps around non-overlapping versions of everything, with new names generated automatically.
        
        Maybe we could go really far and have content-addressable function names :).
        
        Just like Nix, I think you'd need some moral equivalent of garbage collection to make this practical for larger projects. Supporting multiple versions of an API takes resources (whether it's developer time on security/bug fixes, compiler time, source code size, binary size or whatever else) and resources are never infinite. In the case of services rather than libraries it's even worse: old versions of an API may require you to maintain large legacy databases or run expensive computations on your servers.
        
        However, I don't know how you could make something GC-like work if you have a public project where you simply *can't* keep track of all your consumers. But, thinking about it, I could see an approach like this working inside a company. I've heard good cases for large internal monorepos, and I think a similar dynamic could play out here: by having full access to all the *consumers* of a library, you could both avoid breaking compatibility by aggressively retaining versions of your API *and* still manage the size of your project by removing things that weren't in use any longer. (You could also expedite this process for things you really didn't like by sending angry emails to your colleagues.)
    - id: 21684
      author: Anonymous
      date: "2016-12-17 02:48:56"
      content: |
        Was it elm that has a package manager that forces you to bump the semantic version number every time you change an interface?
        
        Never removing functions is a bad idea, IMHO:
        
        You'll end up with incompatibilities: a user might use a new version of a function to create data, but an old version of another function to consume the value. In general: if your interface has N functions, you add k more "updated" functions, you get k^2 modes of usage of your lib -- for each of the k new functions, you have the choice between using the new or the old version. Now you could say: well, why don't you just limit yourself to only updating whole modules? Sure, then lots of this ambiguity would be gone, but you'd have gained little over semantic versioning, right?
        
        Also, versioning, I believe, would now be implicitly buried in your code. With semantic versioning, you are stating declaratively, and I'm one central location what your dependencies are. Sounds better to me.
        
        But I have only read your representation of Rick's argument (and he's a clever guy), so I suspect that i might have gotten the wrong idea..
    - id: 21685
      author: Edward Z. Yang
      date: "2016-12-17 03:11:01"
      content: |
        <blockquote>You’ll end up with incompatibilities: a user might use a new version of a function to create data, but an old version of another function to consume the value.</blockquote>
        
        Because Clojure is an untyped language, authors in Clojure spend a lot of time thinking about what the specification of their "maps" (the primary form of data that gets passed around) is. As a result (and this is related to Clojure not believing in data abstraction), I suspect data representations in Clojure are a lot more stable than in the languages you usually program in, which means that using the new and old functions interchangeably... will probably work! (Unless you make a breaking change to the data representation: Rich claims this happens rarely, and you should put everything in another namespace.)
        
        <blockquote>Sure, then lots of this ambiguity would be gone, but you’d have gained little over semantic versioning, right?</blockquote>
        
        There is a bit of nuance here. In principle, there is no difference between a package named "mylib2" and a package named "mylib" with the semantic major version 2. In practice, matters are very different. Let's take Haskell Cabal as a technical example that I know a lot about: what's the difference between creating a new package, and bumping the major version? For one, the tooling won't let me have multiple versions of the same package in my project, but mylib2 and mylib can coexist too. How about a social example? Most package systems have provisions for upgrading from version 1.0 to 2.0. Changing the package, however, always requires manual intervention.
        
        <blockquote>Also, versioning, I believe, would now be implicitly buried in your code. With semantic versioning, you are stating declaratively, and I’m one central location what your dependencies are. Sounds better to me.</blockquote>
        
        Something I didn't mention is that Rich's talk is in the context of Spec (http://clojure.org/about/spec) which is a new library for explicitly stating declarative specifications of code, in far more detail than a single version number (semantic or otherwise) can. Spec is an enabling technology for this sort of strategy.
    - id: 21689
      author: Yawar
      date: "2016-12-17 22:19:13"
      content: "For an example of these 'infinitely' backwards-compatible APIs, see most REST APIs for commercial services, e.g. Google ad data, Stripe payment data, etc. They use namespacing: so you have URIs like https://example.com/v1/locations, https://example.com/v201609/customers, etc."
    - id: 21690
      author: pyon
      date: "2016-12-18 00:14:18"
      content: |
        Hickey is right that different “versions” ought to be regarded as different programs, but that alone doesn't explain why dependency hell arises, let alone fix it. His solution is, quite frankly, even worse than the disease: as soon as you release a program, you and your users are forced to live with any mistakes contained in it. Forever.
        
        Dependency hell arises because our tooling for managing dependencies isn't built upon a good formal notion of what it actually means for a program to depend on another. (The idea that this can be reified into a versioning scheme is laughable.) Informally, a program Foo depends on a program Bar iff Foo uses (but doesn't itself implement) functionality that is contained in Bar's specification. Thus, it is more accurate to say that Foo depends on Bar's specification, rather than Bar itself. You could substitute Bar with another program Qux (or Bar-v2), so long as Qux meets Bar's specification. (I know you have used at least one ML dialect, so I could cut the noise, and call Foo a functor, and Bar and Qux suitable arguments. But I wrote the long-winded version for the benefit of other commenters.)
        
        More generally, two programs are substitutable with one another iff they meet a common specification that captures perfectly what the program must do for you. The only way you can justify never ever modifying a program is if the implementation is the specification. But that is plain bad software engineering: the implementation of a nontrivial program necessarily contains details that its users don't care about.
        
        So, to summarize, we need to depend less on names and version numbers, and more on structural package types.
    - id: 21695
      author: clacke
      date: "2016-12-19 08:11:55"
      content: |
        <blockquote>
        The prevailing style of Clojure code is that data types consist of immutable records with public fields that are passed around. And so a change to the representation of the data is a possibly a breaking change; non-breaking representation changes are simply not done. (I suspect a similar ethos explains why duplicated dependencies in node.js work as well as they do.)
        </blockquote>
        Internal representation is not the key. I believe rather the node.js solution is information hiding, duck typing and adherence to the Law of Demeter. Most libraries are more library than framework, which helps isolate them from consumers of consumers, and the frameworks that do exist, like Express, have had stable expectations on the API of the objects they consume and produce.
    - id: 21706
      author: Bob
      date: "2016-12-20 19:40:06"
      content: |
        Paul Chiusano is working on a system that works with immutable code. He has made a usable prototype and has gained some very good insights by using it to implement a distributed search engine.
        It's call Unison. http://unisonweb.org/
    - id: 21716
      author: Anonymous
      date: "2016-12-23 07:42:55"
      content: |
        &gt; You’ll end up with incompatibilities: a user might use a new version of a function to create data, but an old version of another function to consume the value.
        
        If I introduce a new version of a function Foo2 into my library I make sure it is compatible with the rest of my library just like if I had added an entirely new function Bar.
        
        There is a slight increase in complexity for each extra function in a library. However, in practice we see it is possible to maintain high quality backwards compatible APIs without ending up with Foo35 and Bar21.
    - id: 22565
      author: Anonymous
      date: "2018-11-08 02:56:35"
      content: |
        &gt; So, to summarize, we need to depend less on names and version numbers, and more on structural package types.
        
        This is what I understood he meant from the talk. You would still have versions, but they are always backward compatible in that they still satisfy the same spec. So the spec is immutable, and version bumps never alter the spec. In fact, he argues one should use Clojure.Spec to define that spec, and continue to test that it has not changed.
        
        You're free to change the implementation, and bump the version when you do, as long as the Spec is not broken.
        
        If you want to change the Spec, you just rename the whole thing.
        
        So the name should map to the spec.
        The version should map to the implementation.
    - id: 24560
      author: Szabolcs
      date: "2020-10-11 07:03:12"
      content: |
        @Tikhon Jelvis
        
        &gt; Maybe we could go really far and have content-addressable function names :).
        
        That's the raison d'être of the language Unison (https://www.unisonweb.org/)!
        
        If you are into experimenting with the concept, give it a try!
---

Rich Hickey recently gave a [keynote](https://www.youtube.com/watch?v=oyLBGkS5ICk) at Clojure/conj 2016, meditating on the problems of versioning, specification and backwards compatibility in language ecosystems. In it, Rich considers the ["extremist" view](http://blog.ezyang.com/2012/11/extremist-programming/), *what if we built a language ecosystem, where you never, ever broke backwards compatibility.*

A large portion of the talk is spent grappling with the ramifications of this perspective. For example:

1.  Suppose you want to make a backwards-compatibility breaking change to a function. Don't *mutate* the function, Richard says, give the function another name.
2.  OK, but how about if there is some systematic change you need to apply to many functions? That's still not an excuse: create a new namespace, and put all the functions there.
3.  What if there's a function you really don't like, and you really want to get rid of it? No, don't remove it, create a new namespace with that function absent.
4.  Does this sound like a lot of work to remove things? Yeah. So don't remove things!

In general, Rich wants us to avoid breakage by turning all changes into *accretion*, where the old and new can coexist. "We need to bring functional programming \[immutability\] to the library ecosystem," he says, "dependency hell is just mutability hell." And to do this, there need to be tools for you to make a commitment to what it is that a library provides and requires, and not accidentally breaking this commitment when you release new versions of your software.

He says a lot more in the talk, so I encourage you to give it a watch if you want to hear the whole picture.

------------------------------------------------------------------------

In general, I'm in favor of this line of thinking, because my feeling is that a large amount of breakage associated with software change that is just a product of negligence; breakage not for any good reason, breakage that could have been avoided if there was a little more help from tooling.

That being said, I do have some thoughts about topics that are not so prominently featured in his talk.

**Accretion is not a silver bullet... if you believe in data hiding.** In his talk, Rich implies that backwards compatibility can be maintained simply by committing not to "remove things". As a Haskeller, this sounds obviously false to me: if I change the internal representation of some abstract type (or even the internal invariants), I *cannot* just load up both old and new copies of the library and expect to pass values of this type between the two. Indeed, the typechecker won't even let you do this even if the representation hasn't changed.

But, at least for Clojure, I think Rich is right. The reason is this: [Clojure doesn't believe data hiding](http://codequarterly.com/2011/rich-hickey/)! The [prevailing style](http://clojure.org/reference/datatypes) of Clojure code is that data types consist of immutable records with public fields that are passed around. And so a change to the representation of the data is a possibly a breaking change; non-breaking representation changes are simply not done. (I suspect a similar ethos explains why [duplicated dependencies in node.js](http://stackoverflow.com/questions/25268545/why-does-npms-policy-of-duplicated-dependencies-work) work as well as they do.)

I am not sure how I feel about this. I am personally a big believer in data abstraction, but I often admire the pragmatics of "everything is a map". (I [tweeted](https://twitter.com/ezyang/status/809704816150597633) about this earlier today, which provoked some thoughtful discussion.)

**Harmful APIs.** At several points in the talk, Rich makes fun of developers who are obsessed with taking away features from their users. ("I hate this function. I hate it, I hate it, I hate that people call it, I just want it out of my life.") This downplays the very real, very important reasons why infinite backwards compatibility has been harmful to the software we write today.

One need look no further than the [systems with decades of backwards compatibility](https://youtu.be/oyLBGkS5ICk?t=1h8m18s) that Rich cites: the Unix APIs, Java and HTML. In all these cases, backwards compatibility has lead to harmful APIs sticking around far longer than they should: [strncpy](https://randomascii.wordpress.com/2013/04/03/stop-using-strncpy-already/), [gets](http://stackoverflow.com/questions/1694036/why-is-the-gets-function-so-dangerous-that-it-should-not-be-used), legacy parsers of HTML (XSS), [Java antipatterns](http://www.odi.ch/prog/design/newbies.php), etc. And there are examples galore in Android, C libraries, everywhere.

In my opinion, library authors should design APIs in such a way that it is easy to do the right thing, and hard to do the wrong thing. And yes, that means sometimes that means you that you need to stop people from using insecure or easy-to-get wrong library calls.

**Semantic versioning doesn't cause cascading version bumps, lack of version ranges is the cause.** In the slide ["Do deps force Versioning?"](https://youtu.be/oyLBGkS5ICk?t=13m49s), Rich describe a problem in the Clojure ecosystem which is that, when following semantic versioning, a new release of a package often causes cascading version bumps in the system.

While the problem of cascading version bumps is [a real question](https://github.com/mojombo/semver/issues/148) that applies to semantic versioning in general, the "cascading version bumps" Rich is referring to in the Clojure ecosystem stem from a much more mundane source: best practices is to [specify a specific version of a dependency](https://nelsonmorris.net/2012/07/31/do-not-use-version-ranges-in-project-clj.html) in your package metadata. When a new version of a dependency comes out, you need to bump the version of a package so that you can update the recorded version of the dependency... and so forth.

I'm not saying that Clojure is *wrong* for doing things this way (version ranges have their own challenges), but in his talk Rich implies that this is a failure of semantic versioning... which it's not. If you use version ranges and aren't in the habit of reexporting APIs from your dependencies, updating the version range of a dependency is not a breaking change. If you have a solver that picks a single copy of a library for the entire application, you can even expose types from your dependency in your API.

------------------------------------------------------------------------

Overall, I am glad that Clojure is thinking about how to put backwards compatibility first and foremost: often, it is in the most extreme applications of a principle that we learn the most. Is it the end of the story? No; but I hope that all languages continue slowly moving towards explicit specifications and tooling to help you live up to your promises.
