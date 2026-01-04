---
title: "Polyglot programming"
date: 2011-10-12 22:22:54
slug: polyglot-programming
categories: [Computer Science]
comments:
    - id: 3013
      author: Chris2048
      date: "2011-10-12 23:44:16"
      content: |
        Would this work? Don't different PLs have thir own culture and communities, programming styles and idioms?
        
        Maybe what's important is the philosophy behind the PL, the underlying reasons and justifications for styles and idioms, and how the various features of the language influence the style; and as a measure of the PL, how well different features fit toghether, isn't OO a philosophy as much as a set of features, or maybe a style that influences the feature-set?
        
        I know what I just wrote is vague, because how you'd actually do this, or what I'd define a 'philosophy' or a 'feature' is also vague, but if it where easy, designing a PL would be too. I have a feeling analysing not just a specific PL, but PLs in general will be terribly complicated (a computationally 'hard' problem); but maybe making PL design choices explicit, as well as the reasoning for stylistic choices is a good first step.
    - id: 3015
      author: Thomas
      date: "2011-10-13 05:45:24"
      content: "Each of these topics would easily fill two hours and more. I'd stick to a well-defined set of languages where each one exemplifies different dimensions of the PL design space. Look at the underlying model of computation: von Neumann vs. lambda calculus vs. actors etc. Even if you stick to sequential programming you still have a formidable task ahead of you. On the language learning side: Idioms and where they come from. A practitioners dictionary of terms used in different languages and by the people who use them (religiously). I'd stay away from FFI, execution environments, ABIs, operating system interfaces, etc. I have a hard time seeing how that would fit in with the rest of the topics while still being a coherent and focused whole. Try not to think about the topics you want to discuss, but rather about the story you want to tell. Otherwise you might end up with just a grab bag of different topics, anecdotes and such. There's probably a better use for your and the audience's time. (Of course this is a highly subjective recommendation and might not even remotely work for you, so, when in doubt, just disregard it.)"
    - id: 3018
      author: Greg Weber
      date: "2011-10-13 11:17:08"
      content: |
        Interoperability is often achieved through a database, messaging (AMQP) library, binary protocol (RPC), JSON or XML (over HTTP) rather than directly with an FFI.
        
        Modern programming is largely about code re-use. It is much easier to learn a language than to gain the practical experience with a language's ecosystem of libraries and frameworks that is required for true productivity. The cheat sheet you should be looking for is something like ruby-toolbox which recommends libraries to you.
        
        On the one hand this makes polygot programming somewhat of a myth, but on the other hand you can try to find a productive set of libraries/framework for a given problem domain that happens to use a particular language.
        Python or R for scientific number crunching libraries. Ruby for web development libraries, C for when efficiency is the only concern, Haskell for hard problems or when efficiency is a major concern, Java/Scala for when you are requied to use the JVM, C#/F# when .Net is mandated, Objective-C for Mac/iOS.
    - id: 3019
      author: Ken
      date: "2011-10-14 01:18:22"
      content: "Stanford's CS242 is taught sort of like this."
    - id: 3026
      author: Edward Z. Yang
      date: "2011-10-15 12:44:51"
      content: |
        Chris: Yes! So you do have to do some learning every time you learn a new programming language. But not nearly as much as you think, and especially little if all you are doing is modifying software in the context of a large project, which has its own idioms anyway. If we can characterize what constitutes the "philosophy" of a programming language, I think we're in a much better position to learn it.
        
        Thomas: On the one hand, I completely agree with what you're saying: it's a lot of material and not enough time to do it. On the other hand, I don't actually want to do a "History of Programming Languages" class (it's worthwhile, but not what I want to do.) I'm aiming for a something of a more practical, "hand first" story. But I should definitely figure out a story.
        
        Greg: Library ecosystem is extremely important, and I think there is a gap where we don't actually tell people how to find libraries, assess them, etc. It's something that you currently learn by experience. As for whether or not polyglot programming is a myth, for a project being built ground up, you can probably shepherd everything into one language: but if you working with a platform with lots of existing software, you'll probably be interacting with all sorts of different languages anyway.
        
        Ken: Ah, interesting. IMO, too much about the languages, not enough about the peripheral issues.
    - id: 3032
      author: Adam Turoff
      date: "2011-10-17 16:29:30"
      content: |
        It sounds like what you're looking for is an armchair anthropology/psychology approach to programming language communities.
        
        Knowing the history is important not that it tells you how things work, as much as it tells you what practitioners prefer and consider easy/possible/impossible.  Otherwise, the history of programming languages is too rich and deep to cover in 2 hours, and doesn't lead you down the path you say you want to visit.
        
        If this is close to what you want to cover, then the other issue to address is overall emphasis of the community.  If you squint in just the right way, Perl is a Lisp, but has very little in common with Lisp or Scheme communities.  Community building, building running software and sharing reusable code are virtues in Perl, while Lisp/Scheme communities are much less collaborative, don't have a united point of view and are significantly more academic.  On paper, Python is similar to Perl, but the knobs are tuned differently and the net result is very different, especially after the decline of Perl in the 2000's and the rise of Ruby and Python in the same timeframe.
        
        Library ecosystems, for example, are a factor, but they are more a consequence rather than a defining characteristic.  CPAN is both a saving grace and a boat anchor for Perl.  Python may not have a single coherent library ecosystem, but sub communities like Django fill in with their own local norms and ecosystems.
    - id: 3033
      author: Edward Z. Yang
      date: "2011-10-17 16:48:51"
      content: "It's funny that you should mention \"anthropology\", because I am broadly interested in this topic, and I don't think very much serious work has been done here. But I think this is because doing anthropology on \"live\" communities is difficult, so the approximation I'm going for is whether or not you can get to the point where you can (1) write running code in that language, and (2) write it in a way that doesn't cause people to gouge their eyes out. (But maybe it's not necessarily good.)"
---

Being back in town over MIT's *Infinite Activities Period* is making me think about what kind of short lecture I want to try teaching. I've been turning over the idea of a polyglot programming class in my head: the idea is that while most people feel really comfortable with only one or two programming languages, you can learn how to make this knowledge work for you in almost any programming language you could possible language.

Unfortunately, I don’t have a good idea of what these skills actually are, nor do I have a sense for what kinds of things people would want to know. Nor do I think that I could jam this into two hours of lecturing: topics that I probably would want to cover are:

*History of Programming Languages.* Knowing how all the lineages tie together will help you figure out when a language feature will work the way you expect it to (since they just stole it from another language in the same line), and when, actually, it won’t work at all. It lets you nicely encapsulate the main big ideas of language features, which you can then explore the infinite variations of. It gives you groups of languages which mostly have the same idioms.

*Street smarts and bootstrapping.* What are the first things you should look for when you’re getting acquainted with a new language? Syntax? Cheat sheets? Tutorials? How to organize this torrent of information, what to do first, where to ask questions, what to learn how to do. How to interpret error messages you know nothing about. How to navigate the development ecosystem and assess libraries you know nothing about. How to source dive code in languages you know nothing about. Common bumps on the road towards Hello World. Unusual and universal ways of debugging.

*Interoperability and FFI.* What are the basic building blocks for higher-level data types in most of these languages, and what do they look like in memory? How do you make lots of different languages talk to each other efficiently! How about garbage collection, reference pinning and concurrency? Common impedance mismatches between languages.

Suggestions and comments would be appreciated.
