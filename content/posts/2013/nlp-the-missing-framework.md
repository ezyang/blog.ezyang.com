---
title: "NLP: the missing framework"
date: 2013-01-02 00:00:30
slug: nlp-the-missing-framework
categories: [Computer Science]
comments:
    - id: 5808
      author: Abdulsattar
      date: "2013-01-03 00:21:36"
      content: "FWIW, Rails has Inflector (http://api.rubyonrails.org/classes/ActiveSupport/Inflector.html) that does the pluralization."
    - id: 5813
      author: Edward Z. Yang
      date: "2013-01-03 04:13:59"
      content: |
        Abdulsattar: Yes, it also cannot ever be improved, because it is being used to generate identifiers for tables, and any improvements to it would constitute backwards-compatibility breakage. Ouch!
        
        Chung-chieh Shan: Very nice article!
    - id: 5809
      author: "Chung-chieh Shan"
      date: "2013-01-03 01:48:16"
      content: "\"To improve on the out-of-the-box models and algorithms\" is <a href=\"http://zinkov.com/posts/2012-06-27-why-prob-programming-matters/\" rel=\"nofollow\">why probabilistic programming matters</a>."
    - id: 5811
      author: Dag
      date: "2013-01-03 02:11:16"
      content: |
        For canonicalization, I recently released the snowball package as bindings to the C stemmer library.  It should be fast, correct, safe and easy to use.  I'm using it to implement full-text search with acid-state.  http://hackage.haskell.org/package/snowball
        
        I'd love to see more packages for generically useful NLP like you suggest, with support for a wide range of languages.  It seems many of the Haskell NLP packages are only for processing English.
    - id: 5818
      author: fho
      date: "2013-01-03 10:54:38"
      content: "Shouldn't be this book a good start? http://nlpwp.org/"
    - id: 5825
      author: Christopher Manning
      date: "2013-01-03 12:02:39"
      content: "Thanks for your shout-out for NLP! I think much of what you observe in terms of the need to get practical tools to people is right.  But, interestingly, nearly all your examples are on the natural language generation side (which is also where traditional localization work occurs), whereas at present about 90% on work in NLP is on the natural language analysis and understanding side...."
    - id: 5826
      author: Edward Z. Yang
      date: "2013-01-03 12:06:45"
      content: "Christopher: Well, I wouldn't say *all* of the examples, but yes, I do get the impression natural language generation is somewhat underserved by the academic community. To be fair, it is a relatively new subfield of NLP."
    - id: 5830
      author: Vlad Niculae
      date: "2013-01-03 20:53:23"
      content: |
        It's true the examples seem pretty far from what someone working with NLP is facing every day, but they are important and they definitely fit under the NLP umbrella.
        
        The current state of application-focused NLP toolkits is quite poor and most existing packages have ever-so-slightly incompatible input/output formats needing trivial but so-not-elegant glue code, like so much of NLTK is. Also, so much interfacing needs to be done via the filesystem or stdout capturing.
        
        Licensing is also a big pain: could anyone point me to a phrase structure parser under a BSD-compatible license?
        
        The most unified and elegant package I have seen yet is SENNA but its license is not permissive enough and it needs an exposed API instead of just piping.
        
        I think we should ask ourselves a couple of big questions:
        What are the low-level tasks that we master well enough to put them in the missing framework?
        What do we want the intermediate data structures to look like? (NLTK doesn't attempt any kind of consistency here but I think this is crucial for a _framework_)
    - id: 5927
      author: Bob Carpenter
      date: "2013-01-07 18:21:38"
      content: |
        It sure would be nice if there were some community standard approaches in NLP. But you won't even find consensus on morphology (word forms) or syntax (sentence forms).  
        
        Even something like pluralization is a tricky business.  Sure, you can handle simple cases in English with a couple of regexes to deal with "box" to "boxes" and "car" to "cars", but it's a little trickier with "try" to "tries" vs. "bay" to "bays", and then we get to cases of full infixing ("goose" to "geese"), Latin ("automaton" to "automata" or "addendum" to "addenda").  So you at least need a big dictionary on top of some simple rules.   Things get much harder in languages like Arabic where morphology isn't done by simple affixing.  LingPipe, by the way, doesn't do morphology --- we've worked on it in the past for customers, but we never came up with anything stable for the distribution.  
        
        Hopefully we addressed some of the framework consistency issues with classifier, tagger, and chunker interfaces, but these are by no means universally accepted.   Even different clusterers produce different outputs (K-means, soft-K-means, LDA, etc.) Interface tools like UIMA deal with glueing components together in a different, more JSON-like or XML-like, way.  But they run into problems trying to glue a parser with one notion of tokenization (say that takes "John's" as a single word vs. treating it as two tokens "John"+"'s" or three tokens "John" + "'" + "s"), or that uses a different unicode normalizing form ("o" plus a combining "umlaut" vs. the single Latin1 character "o with umlaut").
    - id: 5934
      author: Aarne Ranta
      date: "2013-01-19 08:57:26"
      content: |
        Many of these issues are addressed in GF, http://www.grammaticalframework.org/ which has a grammar library for 26 languages intended to be used as a software library in e.g. NLG and web applications. The library has been developed during 12 years by more than 40 contributors.
        
        Incidentally, the "pluralization" of nouns in languages like Arabic was one of the early examples in the development of the library: http://dl.acm.org/citation.cfm?id=1654579
        
        While GF is a special-purpose (functional) programming language, it has bindings in C, C++, Haskell, Java, Javascript, and Python to use "embedded grammars". We welcome new bindings and API's and also requests for them.
        
        In my experience, it is relatively easy to reuse the linguistic data from GF (or from other open-source places) in new formats once it is available, whereas creating the linguistic knowledge in the first place is never-ending work that should be shared as much as possible. Contributions are welcome to this as well!
    - id: 5935
      author: Edward Z. Yang
      date: "2013-01-19 20:27:01"
      content: "Nice! I wasn't aware of GF going into writing this article. You should publicize it more :-)"
    - id: 5936
      author: Aarne Ranta
      date: "2013-01-21 10:47:16"
      content: |
        Thanks! Yes, we are trying hard to publicize it more, and I'm grateful for this opportunity that you created for us :-) and the new arguments you gave for this kind of work.
        
        It is true that 90% of work in NLP is analysis rather than generation. This is perhaps one reason why the work is not so widely received. However, following the good old idea of "reversible" language descriptions (as in DCG and later in XFST) useful for both analysis and generation, we are now working on tasks such as hybrid open-domain translation. The lack of comprehensive and reliable language resources, in particular freely available ones, is an acute problem there as well. And the trend in the research community is promising, as the pendulum is swinging back http://elanguage.net/journals/lilt/article/download/2581/2545
        
        And yes, we try to release everything with BSD compatible licenses, including our phrase structure parser of English (as soon as we have resolved its dependencies on the OALD, which only allows non-commercial uses).
    - id: 5937
      author: Rogan Creswick
      date: "2013-01-21 15:05:05"
      content: |
        re: GF --
        
        I implemented a simple example to show how GF can be used for internationalization  a year or so back; it's up on github:
        
         * https://github.com/creswick/gfI8N
        
        Great call-to-arms, Ed!
        
        --Rogan
    - id: 5942
      author: Michal Boleslav Měchura
      date: "2013-01-23 12:57:12"
      content: |
        Yes! I couldn't have said it better myself! :-)
        
        My particular pet hate is run-time string concatenation, by which I mean things like the above-mentioned "You have " + n + "message(s)." These things are a nightmare to localize (and globalize).
        
        What we need here is proper natural language generation, so we can write language-independent functions like youHaveMsgs(3, "en") and the human-readable text will be generated in the desired language.
        
        An open-source JavaScript framework that makes this kind of functionality available in multiple languages would be a great thing to have. Nothing like this exists yet, as far as I know, but the GF project seems in a good position to produce something like this eventually.
    - id: 5966
      author: Edward Z. Yang
      date: "2013-01-31 17:45:24"
      content: Comments are closed due to spam.
---

So you want to make a web app. In today’s world, there is a panoply of software to assist you: you can use an all-in-one framework, or you can grab libraries to deal with the common needs of templating, database access, interactivity, etc. These libraries unify common functionality and take care of edge-cases you might otherwise not have the resources to deal with.

But there is one tool which is conspicuously absent: the *natural language processing* library.

“Now wait!” you may be saying, “of course there are NLP libraries, [nltk](http://nltk.org/) and [lingpipe](http://alias-i.com/lingpipe/) come to mind.” Sure, but are you actually using these libraries? “Maybe not, but my application doesn’t need NLP, you see.”

The thing is, you *are* doing language processing in your application, even if you don’t realize it: “string concatenation” is really just a simple form of [natural language generation](http://en.wikipedia.org/wiki/Natural_language_generation), a subfield of NLP in its own right. \[1\] If you need to perform a more complicated task, such as pluralize nouns, capitalize sentences or change the grammatical form of verbs, you’ll need linguistic data. \[2\] This data is an essential part of many traditional NLP tasks. However, if you need to pluralize something *today*, you’re more likely to copy-paste a [list of regexes](http://kuwamoto.org/2007/12/17/improved-pluralizing-in-php-actionscript-and-ror/) off the Internet rather than think, “Hm, I should install an NLP library.” Part of this is because, while NLP libraries do contain this data, it is not publicized well.

It’s also worth considering if your application could benefit from any traditional NLP, including keyword generation, canonicalization (When are two things written slightly differently the same?), [language identification](http://code.google.com/p/guess-language/), full-text search, autocompletion, topic detection and clustering, content summarization, parsing human-written dates and locations, etc. While it’s rare for an application to need all of these features, most would benefit from a few of them. For example, a blog application might want keyword generation to generate tags, full-text search to search posts, content summarization for non-fullpage views of posts, and date parsing for scheduling posts. These features tend to be absent, however, because they are often difficult to implement properly. Modern approaches often require models to be trained on large corpora of data—so-called *data-driven models*. Most of the time, this setup cost doesn’t seem worth it; if the feature is to be implemented (e.g. as an extension), a bag of heuristics is quicker.

Both of these problems hint at the trouble with current NLP frameworks: they assume that users are interested in building NLP systems, as opposed to *using* NLP systems. I shouldn’t need a PhD in computational linguistics to get my nouns to pluralize correctly or parse dates robustly. I shouldn’t need a PhD to get passable results on conventional, well-studied NLP applications. The default expectation should not be that users need to train a model: pre-existing models can easily be reused. Although there is an upper limit to how good an NLP algorithm can do without any tuning, the principled approach can still offer improvements over heuristics. But even more importantly, once a model is being used, developers who want to improve their results can train their own model on text from their own application, which is likely to carry domain-specific terminology and patterns. The library should be initially easy to use, and principled enough to be a gateway drug into the wonderful world of computational linguistics. Who knows what other applications could arise when developers recognize NLP as an accessible tool for their toolbox? \[3\]

Here is my call to arms: I want to see all of the current “baby-NLP” functionality collected together into a single place, where they get benefit from shared linguistic data and serve as easy-to-use features that initially attract developers. I would like to see more complicated but useful NLP technology become more accessible to a non-linguistic audience. And I would like all of this to be based on principled NLP foundations, so that it is possible to improve on the out-of-the-box models and algorithms. NLP practitioners are often very careful not to [overstate what their systems are capable of](http://languagelog.ldc.upenn.edu/nll/?p=3565) (in contrast to the irrational exuberance of the 1980s). That’s OK: sometimes, the bar really is *that low.*

Thanks to Gregory Price, Eric Kow and Richard Tibbetts for helping review earlier drafts of this article.

------------------------------------------------------------------------

\[1\] As a field, natural language generation doesn’t really consider string concatenation to be a true method; instead, it is interested in how to generate text from a *functional description of intent*. One neat example is [referring expression generation](http://hub.darcs.net/kowey/antfarm).

\[2\] For example, the functionality (e.g. [pluralization rules](https://gerrit.wikimedia.org/r/gitweb?p=mediawiki/core.git;a=tree;f=languages;hb=HEAD) collected in the `language/` folder in MediaWiki. MediaWiki is one of the most international open source projects, and I find it a fascinating source of information about linguistic oddities in foreign languages.

\[3\] As an example, I'd like to sketch how natural language generation can assist internationalization of applications. Suppose that you would like to let a user know that “you have three new messages.” The most obvious way to implement this would be with: `printf("You have %d new message(s).", numMessages)`. Now, there are a number of shortcuts that have been taken here: we always print out a numeric digit, rather than AP style which uses English for numbers between zero and nine, and we’ve sidestepped whether or not “message” should be pluralized by tacking on an (s) on the end.

If we’d like to handle those cases, the next obvious thing to do is to add a few new functions: we’ll need a function `apnumber` to convert `3` to `three`, and we’ll need a function `pluralize` to convert `message` into `messages` when `numMessages` is greater than one. So you would end up with something like `printf("You have %s new %s", apnumber(numMessages), pluralize("message", numMessages))`. This is the ad hoc approach which will work reasonably well on English but will get you into trouble when you realize other languages have things like noun-adjective agreement (“nouveau message” versus “nouveaux messages”). Internationalization frameworks have long recognized and offered mechanisms for dealing with these cases; however, the average English-based project is unlikely to know about these problems until they internationalize.

However, there exists a representation which is agnostic to these issues. Consider the [dependency grammar](http://nlp.stanford.edu:8080/parser/) of this sentence, which we have extracted with a little NLP:

    nsubj(have-2, You-1)
    root(ROOT-0, have-2)
    num(messages-5, three-3)
    amod(messages-5, new-4)
    dobj(have-2, messages-5)

We might ask, “Given data of this form, can we automatically generate an appropriate sentence in some language, which conveys the information and is grammatically correct?” That is a pretty hard task: it is the fundamental question of NLG. (It's not quite equivalent to machine translation, since we might require a user to add extra information about the functional intent that would otherwise be very hard to extract from text.) While it would be cool if we had a magic black box which could crank out the resulting sentences, even today, the tools developed by NLG may help reduce translator burden and increase flexibility. I think that’s well worth investigating.
