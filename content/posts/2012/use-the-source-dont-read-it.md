---
title: "Use the source, don't read it"
date: 2012-04-17 12:51:49
slug: use-the-source-dont-read-it
categories: [Software Engineering]
comments:
    - id: 3653
      author: Anonymous
      date: "2012-04-18 07:16:29"
      content: "Grep is universal, except for unaccessible web pages which have pictures of text on them. Ironically, pages like this one."
    - id: 3654
      author: Tim Daly
      date: "2012-04-18 09:32:23"
      content: |
        This is why we need literate programming. Write the source in book form.
        A table of contents, an introduction, chapter by chapter, and an index.
        Write to communicate ideas, structure from ideas to implementation.
        The book includes the actual source code. The net result is that code 
        has fewer bugs (since the programmer has to review the code and the
        code review team has a clue about why the code was written) and the
        code will live longer because non-authors can understand and maintain it.
        The time has come for professional programmers to "raise their game"
        and write to communicate to humans as well as the machine.
        
        Tim Daly
    - id: 3655
      author: Oircibaf
      date: "2012-04-18 11:46:57"
      content: "Great article! But don't forget comments in the code! It helps a lot."
    - id: 3656
      author: Thiago Negri
      date: "2012-04-18 11:58:49"
      content: |
        I don't get why you post text as image.
        
        I really like your rants, but I prefer the text ones. They are easier to read, skim, etc.
        
        Just like "Anonymous" said, the contents of this post is lost in the internet. No one searching for advice will find it, unless they can infer the content from the title and are willing to take a peek at the link. Grep, Google, Bing or whatever search engine is blind to this content.
        
        Back to the matter;
        
        Is this a response to Jeff Atwood's recent post "Learn to Read the Source, Luke"?
        
        I see you both defending the same point of view: you need to understand what you are using, even if it's just the outline. As a problem appears, you should seek enlightment thru source code reading/skimming/browsing _before_ shouting out your problem in StackOverflow with no real understanding of what's going on.
        
        Agreed.
    - id: 3660
      author: "The Morning Brew - Chris Alcock &raquo; The Morning Brew #1088"
      date: "2012-04-19 04:32:31"
      content: "[...] Use the source, don&#8217;t read it - Inside 233 has an excellent little (hand written/drawn) guide to how you should best make use of the source access that open source gives you, discussing a number of useful approaches to &#8216;reading&#8217; and comprehending source code. [...]"
    - id: 3661
      author: Bugger
      date: "2012-04-19 05:50:37"
      content: "When you have to read source it's because you're using shitty code. If the things works and is documented you don't care about sources."
    - id: 3662
      author: Hmm
      date: "2012-04-19 14:12:03"
      content: |
        @Bugger: This is the kind of One True Scotsman argument that gets bandied about anytime people talk about coding: "REAL CODE doesn't have errors, is documented flawlessly, and cures world peace HURRRR". The truth is, no interface/technical doc is 100% usable or reliable. 
        
        Also, what happens when you're using a slightly different web browser/operating system patch level/cup stacker firmware than the software was written for, and it breaks? Is that "shitty code"? No. It's just a fact of life that we're trying to hit a moving target every time we sit down at an IDE (or vim, or notepad, or whatever). Calm down, Bugger, and try to take this post in the spirit it was intended.
    - id: 3664
      author: TheRedCircuit
      date: "2012-04-19 21:59:49"
      content: |
        I liked the handwriting and drawing (even if Instapaper choked on it).  Also, I really like the minimalist design of this blog. 
        As to the content, well designed code can be very easy to read, and a tremendous pleasure when you have to add a feature or otherwise mod it.  I agreed with pretty much everything you wrote.
    - id: 3665
      author: Optional
      date: "2012-04-20 05:39:33"
      content: |
        What shall we read today?
        
        There's romance, horror, mystery, fantasy, science fiction, women’s fiction (oops, delete), biography, autobiography, fairy stories, bedtime stories....
    - id: 3667
      author: Anonymous
      date: "2012-04-20 06:43:23"
      content: |
        "Grep is universal" in a universe where nobody has windows?
        [Not only because of lack of implementation... since there are a few. But because people may not know something like grep exists at all]
    - id: 3668
      author: Kay
      date: "2012-04-20 07:50:28"
      content: |
        I agree, it's important to examine the source, no necessarily read it. The first thing  I do with source code is run it through applications like enterprise architect and visual studio's auto-diagramming features. 
        
        I can't say you shouldn't evaluate code at all as some people do (hence, last project I found code that a contractor had written which was feeding business data to a free web service in inda he was using for processing). But I agree, it's not necessary to intimately understand every line, just check for traps!
    - id: 3670
      author: fubar
      date: "2012-04-20 10:17:18"
      content: |
        @Anonymous: "“Grep is universal” in a universe where nobody has windows?"
        a) grep did not originate on Windows
        b) I've never met a professional (or even amatuer) programmer who hasn't heard of grep
    - id: 3671
      author: scotchfaster
      date: "2012-04-20 11:32:26"
      content: "The fastest method of understanding code isn't mentioned here: use the debugger. Say you're looking at a bit of code, but you don't understand how a user interaction causes it to be called, or what the inputs are - just set a breakpoint, do the thing that gets it to be called and look at the call stack and any parameters. If the breakpoint isn't hit, you might be looking at dead code, or just in the wrong place. Or if you're just trying to understand an algorithm, don't mentally execute it, step through it."
    - id: 3672
      author: nickels
      date: "2012-04-20 12:46:44"
      content: |
        really cool, really creative article!
        And spot on for figuring out a new piece of code!
    - id: 3679
      author: Anonymous
      date: "2012-04-22 11:56:55"
      content: "Coming this April...A stupid author presents...to an audience of stupid programmers...\"101 Excuses to Stay Stupid\""
    - id: 3681
      author: Steve Klebanoff
      date: "2012-04-22 23:56:22"
      content: |
        Awesome to see this post displayed in a fun, visual way. 
        
        Adding debugging output is also a useful way to understand process flow of source code, and allows you to easily try out functions on your own data from your console.
    - id: 3686
      author: Anonymous
      date: "2012-04-24 00:47:02"
      content: |
        Grep is universal? Maybe in unix.
        
        In windows cmd:
        mydirectory&gt;grep /?
        'grep' is not recognized as an internal or external command,
        operable program or batch file.
        
        In windows Powershell:
        PS mydirectory&gt; grep /?
        The term 'grep' is not recognized as the name of a cmdlet, function, script file, or operable program. Check the spelling of the name, or if a path was included, verify that the path is correct and try again.
        At line:1 char:5
        + grep &lt;&lt;&lt;&lt;  /?
            + CategoryInfo          : ObjectNotFound: (grep:String) [], CommandNotFoundException
            + FullyQualifiedErrorId : CommandNotFoundException
    - id: 6199
      author: Sandeep
      date: "2013-08-23 01:33:49"
      content: "I really found this article useful. Most new programmers might find reading the source code overwhelming, but this article may clear most of their doubts and make them understand that the strategies they used to read any book applies to reading source code as well. Thanks for this illustrative and creative article (which looks more attractive than formal neatly typed ones). Looking forward for more such articles."
    - id: 6200
      author: Sandeep
      date: "2013-08-23 01:50:07"
      content: "In addition, I also found tools like CTAGS and CSCOPE more useful in browsing the codebase (especially useful for small to medium sized codebases)"
    - id: 33797
      author: Pietro Carrara
      date: "2024-10-21 10:03:45"
      content: |
        Really useful guidance. Unfortunately, had to learn to skim through the hard way (noticing I was taking too long to finish my tasks while tackling legacy code).
        
        Learning to read through others' (potentially old) code is really a fundamental skill.
---

![image](/img/use-the-source.png)
