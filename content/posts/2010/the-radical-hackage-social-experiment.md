---
title: "A radical Hackage social experiment"
date: 2010-08-11 09:00:26
slug: the-radical-hackage-social-experiment
categories: [Haskell, Programming]
comments:
    - id: 882
      author: Neil Brown
      date: "2010-08-11 10:55:34"
      content: |
        There is a set of different problems that can occur.  When you start out, there are no libraries that do what you want.  Then there is one library, then (inevitably) there are several.  At this point you either have the problem that none are any good (enter the strike-force, which leads on to...) or that some are good and some are bad, but you don't know which.  You may then get to the problem were several are good and you're spoilt for choice, but in a way this is the same problem as having some good and some bad.
        
        So there are several problems.  Not having any libraries is one that I think does slowly get solved by having a mass of programmers using the language.  The current size of Hackage is proof that the problem of "no libraries" is mostly being solved.  I also believe that the problem of low quality is one that by-and-large gets sorted out too.  The reason that several libraries always develop for a similar need is because the early ones aren't suitable or are low quality (in a way, from an individual's perspective, these are the same problem).
        
        The problem that really needs to be solved is judging quality/suitability when an individual visits Hackage for a library.  I think it's best to look at sites like Amazon for this.  Amazon invites quantitative and qualitative feedback on each product to give future users a better idea of how suitable the item is for them to purchase.  So we could allow users to leave feedback: I used this library and found it good for X but poor for Y.  The main difference from Amazon is that libraries can change over time (unlike books or films, which generally don't), so there needs to be a way to automatically degrade and ultimately delete old feedback.  If we could encourage a culture of leaving this feedback (preferably constructive) it would both help library users, and incentivise library developers.
    - id: 884
      author: ksf
      date: "2010-08-11 12:04:30"
      content: |
        When it comes to managing feedback, I strongly recommend having a look at http://www.public-software-group.org/liquid_feedback .
        
        We could then vote individual libraries and versions into categories like "Heavy-duty battery", "Tactical fusion reactor: Use with caution", "Convenience solar panel", and, most importantly, track all the reservations people have before they are willing to promote a library out of the bazaar into the categories.
        
        The system is designed to systemically slate non-constructive feedback for ignorance (and does so successfully) as well as to ease (and thus ensure) participation by allowing people to delegate votes on both case-by-case or general basis.
    - id: 885
      author: Asger Ottar Alstrup
      date: "2010-08-11 13:20:29"
      content: |
        A very simple suggestion is to require the authors to rate the libraries themselves on different measures of quality:
        
        - Correctness
        - Readability
        - Complexity
        - Completeness
        - Documentation
        - Performance
        - Scalability
        
        Most often, library authors know about problems with their library if they are asked about them. It is just so that noone does.
    - id: 886
      author: Edward Z. Yang
      date: "2010-08-11 13:55:15"
      content: |
        These are all great and thought-provoking comments!
        
        Neil, while I agree with your assessment of how the bazaar model gradually improves the qualities of libraries, there is something to be said for Haskellers’ obsession with names (I’m thinking of the fgl fork). Many pieces of functionality have logical namespaces to go into, and when one library takes those module names others feel bad about reusing them (a few notable exceptions might be the monad transformers mess.)
        
        ksf, that system sounds quite interesting, although I'm having difficulty finding evidence that it deals with non-constructive feedback successfully.
        
        Asger, that’s quite an interesting proposal. My experience with Haskell has been the opposite: many, many modules are marked Experimental in their Haddock docs when, really, they've been around for years and are about as stable as you can get.  You'd also need a mechanism for amending obviously wrong ratings.
    - id: 887
      author: ksf
      date: "2010-08-11 14:25:09"
      content: |
        In the default configuration, Liquid Feedback requires new proposals to pass a quorum before being admitted to the discussion phase. As you can't make a proposal pass the quorum, or comment on it without also supporting it (or make a counter-proposal) trolls just tend to get ignored, as there are negative incentives in place that prevent people that would fall for it from giving it the attention it wants.
        
        There's an interview with one of the main authors at http://chaosradio.ccc.de/cre158.html , (German) which goes into some details on why LF is, unlike other delegated voting systems, also usable for communities which don't necessarily tend to strive for concord.
        
        It's hard to find evidence in the form of ignored troll posts in the existing test instances (because trolls are smart enough to notice that the system works against them), but if you look at the German Pirate Party, their wiki and mailing lists and compare them to the LF instances, the difference in discussion quality is staggering.
    - id: 888
      author: j_king
      date: "2010-08-11 15:21:51"
      content: |
        I say follow the CPAN model. It's very efficient and time-tested.
        
        Package quality can be determined by providing as many metrics as you can think of and letting the programmer decide what is important. Automated smoke-screen testing, test coverage reports, community ratings, number of downloads, documentation; all of these metrics help me decide which perl modules are worthwhile.
    - id: 892
      author: Chris Smith
      date: "2010-08-12 01:54:53"
      content: |
        Definitely, both are needed.  What hackage does very well right now is that it makes very nearly the entirety of Haskell library and package development available in a common way.  Unlike in, say, C++, I can go to one place and get a list of practically all of the libraries available to me.  Adding features like user ratings, reverse dependencies, and other metrics makes this better.
        
        What Hackage currently does not do at all is to collect software into well-known official packages, coordinate namespaces, and police compatibility.  To the extent that this is added to Hackage, IMO, it needs to be clearly kept at a level above the role Hackage currently plays.  As Duncan very wisely said in a discussion once, it would be nice if Hackage recognized and supported the role of "distro maintainer", whose jobs would be to choose, collect, and prod the authors of high quality packages until they fit together and work seamlessly.
        
        But Hackage certainly should not be modified to require this work of package authors.  It should perhaps be modified to support this work by people who step up for the role.  The power of these people would essentially be limited to their choice to include a package or not in their distro, or to communicate with authors what conditions are there on including the package in their distro.  So someone building and throwing out some code for a research paper can still do so... and distro maintainers that have higher standards for quality, documentation, etc. can exclude the package.
        
        This naturally extends to the point where the "standard" Haskell library is guarded over by a core of highly trusted veteran Haskellers... but they do so by pulling from and testing and integrating the code in the very open Hackage that we have today.
    - id: 1094
      author: Jens Petersen
      date: "2010-08-29 01:21:31"
      content: |
        Late to the party as usual...
        
        My suggestion coming as a distro maintainer would be to have stable, testing, and unstable repos (or tagging) of packages and a user-based QA approval mechanism for promoting packages from unstable to testing, and from testing to stable.  This would give users a clearer idea of the quality and stability of packages and how much use and testing they have had.  Brand new packages would probably go into unstable first.  Bugfixes to current stable packages would go first into testing and if no problem were found they could be promoted into stable once some experienced users +1'ed them or after a period of no -1's.  Newbies would probably be recommended only to use stable, whereas more experienced users might play more with testing and unstable packages.  (This is scheme is based on the development and updates process for Fedora, which I think overall works pretty well.)
    - id: 11989
      author: James Crayne
      date: "2015-01-09 01:05:55"
      content: |
        I like that almost everything is on hackage, i don't mind being the judge of quality myself even though it does sometimes cost me the time of fussing with some libraries that ultimately are not well suited to my purposes before finding those that are.
        
        Maybe hackage should include an unofficial section that just finds cabalized stuff that was published to the interent, on github or gitorious or what not but never uploaded to hackage. Those results can show up in searches when you add '--internet' to the command line or select it in a drop down on the site.  If we have some rating system, might as well throw them into the mix as well and if some end up being highly rated.  The internet-archive should probably be some how filtered to exclude minor forks.
---

*Prologue.* This post is an attempt to solidify some of the thoughts about the upcoming Hackage 2.0 that have been discussed around the Galois lunch table. Note that I have never overseen the emergence of a language into mainstream, so take what I say with a grain of salt. The thesis is that Hackage can revolutionize what it means to program in Haskell if it combines the cathedral (Python), the bazaar (Perl/CPAN), and the wheels of social collaboration (Wikipedia, StackOverflow, Github).

New programming languages are a dime a dozen: one only needs to stroll down the [OSCON Emerging Languages track](http://emerginglangs.com/speakers/) to see why. As programmers, our natural curiosity is directed towards the language itself: “What problems does it solve? What does it look like?” As engineers, we might ask “What is its runtime system?” As computer scientists, we might ask: “What novel research has been incorporated into this language?” When a language solves a problem we can relate to or shows off fancy new technology, our interest is whetted, and we look more closely.

But as the language grows and gains mindshare, as it moves beyond the “emerging” phase and into “emergent”, at some point, *the language stops being important.* Instead, it is the community around the language that takes over: both socially and technically. A community of people and a community of code—the libraries, frameworks, platforms. An engineer asks: “Ok. I need to do X. Is there a library that fills this need?”

The successful languages are the ones that can unambiguously answer, “Yes.” It’s a bit of an obvious statement, really, since the popular languages attract developers who write more libraries which attracts more developers: a positive feedback loop. It’s also not helpful for languages seeking to break into the mainstream.

Tune down the popularity level a little, and then you can see languages defined by the mechanism by which developers can get the functionality they need. Two immediate examples are Python and Perl.

Python has the mantra: “batteries included,” comparing a language without libraries to a fancy piece of technology that doesn’t have batteries: pretty but—at the moment—pretty useless. The [Python documentation](http://www.python.org/about/) boasts about the fact that any piece of basic functionality is only an import away on a vanilla Python install. The Python standard library itself follows a cathedral model: commits are restricted to members of [python-dev](http://www.python.org/dev/committers), a list of about 120 trusted people. Major additions to the standard library, including the [addition of new modules](http://www.python.org/dev/peps/pep-0002/) most go through a [rigorous proposal process](http://www.python.org/dev/peps/) in which they demonstrate that your module is accepted, widely used and will be actively maintained. If a maintainer disappears, python-dev takes stewardship of the module while a new maintainer is found, or deprecates the module if no one is willing to step up to maintain it. This model has lead to over [three hundred relatively high-quality modules](http://docs.python.org/library/) in the standard library.

On the other hand, Perl has adopted the bazaar model with [CPAN](http://www.cpan.org), to the point where the slow release cycle of core Perl has meant that some core modules have been dual-lifed: that is, they exist in both the core and CPAN. Absolutely anyone can upload to CPAN: the result is over 20,000 modules and a resource many Perl developers consider indispensable. Beyond its spartan home interface, there is also [massive testing infrastructure](http://deps.cpantesters.org/) for all of CPAN and a [ratings system](http://cpanratings.perl.org/) (perhaps of dubious utility). CPAN has inspired similar bazaar style repositories across many programming languages (curiously enough, some of the most popular langauges—C and Java—have largely resisted this trend).

It’s a tall order for any language to build up over a hundred trusted committers or a thriving community on the scale of CPAN. But without this very mechanism, the language is dead out of the water. The average engineer would have to rewrite too much functionality for it to be useful as a general purpose language.

Which brings us back to the original point: where does Hackage stand?

The recent results from the [State of the Haskell 2010 survey](http://blog.johantibell.com/2010/08/results-from-state-of-haskell-2010.html) gives voice to the feeling that any Haskell programmer who has attempted to use Hackage has gotten. There are *too many libraries without enough quality.*

How do we fix this? After all, it is all open source made by volunteers: you can’t go around telling people to make their libraries better. Does one increase the set of core modules—that is, the Haskell platform—and the number of core contributors, requiring a rigorous quality review (the Python model)? Or do you let natural evolution take place and add mechanisms for measuring popularity (the Perl model)?

To succeed, I believe Hackage needs to do both. And if it succeeds, I believe that it may become *the* model for growing your standard library.

The cathedral model is the obvious solution to rapidly increase the quality of a small number of packages. Don Stewart has employed this to good effect before: [bytestring](http://hackage.haskell.org/package/bytestring) started off as a hobby project, before the Haskell community realized how important efficiently packed strings were. A “strike team” of experienced Haskellers was assembled and the code was heavily improved, fleshed out and documented, generating several papers in the process. Now bytestring is an extremely well tuned library that is the basis for efficient input and output in Haskell. Don has suggested that we should adopt similar strike teams for the really important pieces of functionality. We can encourage this process by taking libraries that are deemed important into a shared repository that people not the primary maintainer can still help do basic maintenance and bugfixes.

But this process is not scalable. For one, growing a set of trusted maintainers is difficult. The current base libraries are maintained by a very small number of people: one has to wonder how much time the Simons spend maintaining `base` when they could be doing work on GHC. And you can only convince most people to take maintainership of `X` packages before they wise up. (Active maintainership of even a single package can be extremely time consuming.)

[Hackage 2.0](http://cogracenotes.wordpress.com/2010/08/08/hackage-on-sparky/) is directed at facilitating the Bazaar model. Package popularity and reverse dependencies can help a developer figure out whether or not it is something worth using.

But if we consider both developers and package maintainers, we are tackling a complex socio-technical problem, for which we don’t have a good idea what will revolutionize the bazaar. Would a StackOverflow style reputation system encourage maintainers to polish their documentation? Would a Wikipedian culture of rewarding contributors with increased privileges help select the group of trusted stewards? Would the ability to fork any package instantly ala GitHub help us get over our obsession with official packages? Most of these ideas have not been attempted with a system so integral to the fabric of a programming language, and we have no way of telling if they will work or not without implementing them!

I am cautiously optimistic that we are at the cusp of a major transformation of what Hackage represents to the Haskell community. But to make this happen, we need your help. Vive la révolution!

*Credit.* Most of these ideas are not mine. I just wrote them down. Don Stewart, in particular, has been thinking a lot about this problem.
