---
title: "Haskell, The Hard Sell"
date: 2010-03-17 09:00:13
slug: haskell-the-hard-sell
categories: [Haskell, SIPB]
comments:
    - id: 208
      author: Steve
      date: "2010-03-17 11:55:28"
      content: "Absolutely agree.  There are certain applications at work, written in Python, C, or Java, that I'd love to replace with Haskell.  However, even though I mostly work on my own, I can't justify doing this, because if it would make _my_ life easier, I know I'm not going to be around forever.  What are the chances that the next guy to maintain this code is going to know, or be willing to learn, Haskell?  Right now, I think pretty low.  If I'd made the decision early on to use it, maybe I'd be able to make it a hiring requirement, but since the code base is already well established, I can't really justify introducing an esoteric language into the mix even if I think it would make the code better.  So unfortunately, for now, I have to stick to learning Haskell in my spare time on external projects."
    - id: 209
      author: Edward Kmett
      date: "2010-03-17 13:16:35"
      content: |
        You may find the following link useful from the "who else uses it perspective". You may recall Linspire better from their earlier days as "Lindows."
        
        http://lambda-the-ultimate.org/node/1506
        
        That said, I only use Haskell in the office when I'm writing one offs, test-harness code, or I am trying to check to make sure that a model is sound via quickCheck. I've resigned myself to the fact that it is unproductive of me to try to convert the average office worker.
    - id: 212
      author: Edward Z. Yang
      date: "2010-03-17 15:52:13"
      content: "Edward, I don't think Linspire gets very many points since it has now become a defunct project. :-("
    - id: 213
      author: Patrick Hurst
      date: "2010-03-17 16:01:17"
      content: "One thing that I think makes it easier to write static-cat in Haskell is that there already exists a perfectly functional version; if the Haskell one ever dies due to lack of Haskellers, you can always fall back to the C version."
    - id: 214
      author: Edward Z. Yang
      date: "2010-03-17 16:09:05"
      content: "Patrick, while this is certainly true, there is certainly the, \"Well, that code hasn't been used in a long time... how do we know it's still functional?\" Or perhaps it got culled from the codebase because it was dead code; we do that sometimes."
    - id: 215
      author: Patrick Hurst
      date: "2010-03-17 19:13:05"
      content: |
        i,i
        Occasionally, to make sure the C version works, install it on a couple of the servers and see if anybody complains.
    - id: 221
      author: Roger
      date: "2010-03-18 11:21:32"
      content: |
        Great post. I've done a lot of hacking in Haskell, and have thoroughly enjoyed it. My big caveat, however, is that the ecosystem is a pain to deal with, so as much fun as it is to develop in, maintaining code that's even a few months old can be a headache. A lot of what's in Hackage is thesis-work abandonware, and much of it won't even compile on the current version of GHC. 
        
        I honestly don't see the situation changing until Haskell steps out of its academic niche a bit more. 
        
        That said, I'm optimistic. Haskell is like geek catnip. The community will grow and mature, and eventually, we will find a way to iron out the problems.
    - id: 226
      author: Luke Hoersten
      date: "2010-03-21 11:15:54"
      content: "I run into these problems all the time. I moved my team from perforce to hg, centos to ubuntu, VC++ to emacs, and to me, the language is just another tool to swap. I'm convinced it's necessary and possible to start using better languages. I think the biggest problem is only a surface issue with Haskell: it's declarative. For example, learning Erlang seems easy for C/Python programmers because processes are like objects and actors are easy. Another analogy: it's almost impossible to see what you're missing when using Svn and someone is trying to sell Hg but once you've learned Hg, it feels like typing with only two fingers to continue on Svn. Distributed Revision Control and Meta-programming (Ruby) have both broken into the mainstream and are at least acceptable to suggest to your boss. Declarative, functional, and concurrency-oriented have not but somebody has to do it."
    - id: 304
      author: Dafydd Harries
      date: "2010-04-13 15:38:49"
      content: "I think that many of these questions are circumvented in cases like these where the program is simple enough that, if all else fails, it can be replaced in relatively short order."
---

Last week I talked about how [we replaced a small C program with an equivalent piece of Haskell code.](http://blog.ezyang.com/2010/03/replacing-small-c-programs-with-haskell/) As much as I'd like to say that we deployed the code and there was much rejoicing and client side caching, the real story is a little more complicated than that. There were some really good questions that we had to consider:

1.  *How many maintainers at any given time know the language?* The [Scripts](http://scripts.mit.edu) project is student-run, and has an unusually high turnover rate: any given maintainer is only guaranteed to be around for four to five years (maybe a little longer if they stick around town, but besides a few notable exceptions, most people move on after their time as a student). This means at any given point we have to worry about whether or not the sum knowledge of the active contributors is enough to cover all facets of the system, and facility in a language is critical to being able to administrate the component effectively (students we are, we frequently don both the sysadmin and developer hats). In a corporate setting, this is less prominent, but it still plays a factor: employees switch from one group to another and eventually people leave or retire. We have two current maintainers who are fairly fluent in Haskell. The long-term sustainability of this approach is uncertain, and hinges on our ability to attract prospective students who know or are interested in learning Haskell; in the worst case, people may crack open the code, say "what the fuck is this" and rewrite it in another language.

2.  *How many maintainers at any given time feel comfortable hacking in the language?* While superficially similar to the first point, it's actually quite different; posed differently, it's the difference between "can I write a full program in this language" and "can I effectively make changes to a program written in this language." At a certain level of fluency, a programmer picks up a special feat: the ability to look at any C/Fortran derived language and lift any knowledge they need about the syntax from the surrounding code. It's the difference between learning syntax, and learning a new programming paradigm. We may not be simultaneously Python/Perl/PHP/Java/Ruby/C experts, but the lessons in these languages carry over to one another, and many of us have working "hacker" knowledge in all of them. But Haskell is different: it's lineage is among that of Lisp, Miranda and ML, and the imperative knowledge simply *does not translate.* One hopes that it's still possible to tell what any given chunk of Haskell code does, but it's a strictly read-only capability.

3.  *Who else uses it?* For one of the team members, migrating from Subversion to Git was a pretty hard sell, but at this point, minus the missing infrastructure for doing the migration properly, he's basically been convinced that this is the right way forward. One of the big reasons this was ok, though, was because they were able to list of projects (Linux, our kernel; AFS, our filesystem; Fedora, our distro) that they used regularly that also used Git. We can't say the same for Haskell: the "big" open-source high-visibility applications in Haskell are Xmonad and Darcs, of which many people have never used. As a student group, we have far more latitude to experiment with new technology, but lack of ubiquity means greater risk, and corporations are allergic to that kind of risk.

4.  *Is the ecosystem mature?* Internally, we've given the Ruby maintainers and packagers a lot of flak for a terrible record at backwards compatibility (one instance left us unable to globally update our Rails instances because the code would automatically break the site if it detected a version mismatch). You see a little bit of the same in Haskell: static-cat doesn't actually build on a stock Fedora 11 server with the default packages installed, due to an old version of the cgi module that uses the Exception backwards compatibility wrapper and thus is incompatible with the rest of the exception handling code in the program. Further investigation reveals that the cgi module is not actually being actively maintained, and the Fedora `cabal2spec` script is buggy. I've personally had experiences of coming back to some Haskell code with up-to-date libraries from Hackage and finding that API drift has made my code not compile anymore. Cabal install refuses to upgrade all of your packages in one go.

    There are many ways to work around this. A mitigating factor is that once you've compiled a Haskell program, you don't have to worry about package composition anymore. Workarounds include rewriting our code to be forwards and backwards compatible, doing stupid Fedora packaging tricks to make both versions of cgi live on our servers, convincing upstream that they really want to take the new version, or maintaining a separate system wide cabal install. But it's not ideal, and it makes people wonder.

I'm quite blessed to be working in an environment where the first point is really *the* point. Can we introduce Haskell into the codebase and expect to be able to maintain it in the long run? There'll always be C hackers on the team (or at least, there better be; some of our most important security properties are wrapped up in a patch to a kernel module), but will there always be Haskell hackers on the team? There's no way to really know the answer to the question.

I personally remain optimistic. It's an experiment, and you're not going to get any better chance to make this happen than in this environment. The presence of Haskell code may attract contributors to the project that may not have been originally drawn by the fact that, down beneath it all, we're a "gratis shared web hosting provider" for our community. Haskell seems singularly aligned to be *the* language to break into mainstream (sorry Simon!) And when was there ever any innovation without a little risk?
