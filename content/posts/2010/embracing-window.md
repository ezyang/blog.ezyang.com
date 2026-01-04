---
title: "Embracing Windows"
date: 2010-09-06 09:00:27
slug: embracing-window
categories: [Meta, Toolbox]
comments:
    - id: 1128
      author: Thomas
      date: "2010-09-06 11:29:58"
      content: |
        I have had a similar experience with Windows coming back to me. I've stopped using Windows earlier though and all "real" development was subsequently done on GNU/Linux using vim and makefiles essentially. For a project I had the opportunity to try and write in C# (which I think is basically what Java should be/have been although some features are missing and the vendor lock-in makes me nervous) using Visual Studio and it was overall a great experience. As an IDE I like VS better than Eclipse or Xcode. I would mark the day that Microsoft decides to port VS to Mac OS and maybe even GNU/Linux and other unixoid operating systems in my calendar and have a celebration every year. ;-)
        
        Say what you will about Microsoft in general and Windows in particular, but VS is certainly one of the pieces they (mostly) got right although the Microsoft C/C++ compiler may not be the greatest, but once you get into commercial tools you could also get a license for the Intel C compiler. And talking about access to licenses: There's the fantastic MSDNAA which provides free licenses for most Microsoft products (with the most prominent omission being Office, but who needs that anyway?) to students of participating universities.
        
        Doing cross-platform development using C++ with a Visual Studio project on Windows and a Makefile for Mac OS and GNU/Linux (either 32 or 64bit) has also been relatively painless and works well. There are a few quirks (like subtle differences between standard C math library implementations that break compilation on Windows and missing library support for e.g. newer versions of OpenGL), but that are problems that are easy to fix.
    - id: 1138
      author: Andrew Pennebaker
      date: "2010-09-09 10:46:32"
      content: |
        Ever since I discovered UNIX, I've had it in for Windows. I only have Windows to test Windows ports of my software and to play video games.
        
        Xcode isn't perfect, but it's always been a pleasure building Mac applications. Visual Studio and its clunky C++/C#/VB/F# schizoid collection of languages presents a barrier to me every time. Consistency is crucial, and UNIX has that in spades over DOS.
    - id: 1943
      author: Tyr
      date: "2011-03-21 00:35:41"
      content: |
        I actually use both Linux (Ubuntu) and Windows 7.  I have most of my programming stuff set up on my Linux side, but winGHCi is nice enough that I can write Haskell in 7 painlessly.  But if I'm building any complex projects (such as ghc), I tend to do it in Linux, as apt-get is much nicer than googling for everything.
        
        Before Windows 7 came out, I was spending more time in Linux, but 7 is much nicer than xp, and I found myself using it more and more.  If it weren't for 7 I would probably be running ubuntu almost exclusively.
---

*Some things come round full circle.*

As a high schooler, I was a real Windows enthusiast. A budding programmer, I accumulated a complete development environment out of necessity, a mix of Cygwin, handwritten batch scripts, PuTTY, LogMeIn, a homegrown set of PHP build scripts and Notepad++. I was so devoted to the cause I even got a [single patch into Git](http://repo.or.cz/w/git.git/commit/36ad53ffee6ed5b7c277cde660f526fd8ce3d68f), for the purpose of making Git play nicely with plink on Windows. The setup worked, but it always felt like a patchwork of different components, all not quite seeing eye-to-eye with each other. When I discovered that Linux was able to offer me an unbelievably coherent development environment, I jumped ship and said goodbye to Windows.

*Some things come round full circle.* Windows has a way of coming back to you eventually. The [product I worked on over the summer](http://www.galois.com/technology/communications_security/cryptol) at Galois had to support Windows, and I consequently devoted days of effort getting my changes to build properly on Windows. I then went on to [hacking GHC](http://blog.ezyang.com/2010/08/interrupting-ghc/), and Simon Marlow asked me to implement the equivalent feature in Windows.

I’ve decided that I should stop shunning Microsoft Windows as the developer’s black sheep of the operating systems. Like it or not, Windows is here to stay; even if I never boot my laptop into Windows, as a developer it is good practice to think about and test my code on Windows. It might even be the case that Windows is a *perfectly reasonable* underlying platform to develop on.

There seem to be two reasons why developers might find targeting other platforms to be annoying:

- They don’t have access to a computer running that operating system, which makes debugging the problems extremely annoying—after all, this is why a reproduceable test-case is the gold standard of bug reporting. We should have easy to access and easy to use build servers setup to let people play in these different environments. This involves putting down some money to buy the appropriate licenses, which open-source authors might be reluctant to do: people at places with site licenses might be able to help by donating boxes for these people to play in (the same way companies and universities donate disk space and bandwidth for mirrors).
- They have to learn about another platform, with all of its intricacies and gotchas. On the one hand, this is annoying because “I already know how to do this in Unix, and now I have to spend N minutes to figure out how to do it on Windows, and spend another N minutes figuring out why it doesn’t work in some edge case.” On the other hand, learning a platform that does something you already know how to do can be kind of fun: you get to see different design decisions and develop multiple perspectives on the same problem, which I have found has always helped me out for problem solving.

There remain parts of Windows programming that I continue to have no interest in: for example, I find the vagaries of manifest files to be fairly uninteresting. But then again, I find packaging in Linux distributions to be uninteresting. Stop blaming Windows!
