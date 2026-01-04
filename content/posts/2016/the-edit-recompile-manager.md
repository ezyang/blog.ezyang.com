---
title: "The Edit-Recompile Manager"
date: 2016-09-02 20:40:43
slug: the-edit-recompile-manager
categories: [Programming]
comments:
    - id: 21204
      author: Christopher Wells
      date: "2016-09-04 13:57:29"
      content: |
        You bring up some good points.
        
        In the past I tended to be more on the "use the system package manger" side of the debate, however, as you noted, language package managers and system package managers do lend themselves to different purposes.
        
        However, the issue with viewing language package managers as just "edit-recompile managers" is that in practice they tend not to be used only in such a way. Developers working on applications written in some languages with package managers often opt to distribute the application primarily through the language package manager rather than through system package managers.
        
        This often happens for decent reasons. Packaging applications for the numerous existing system package managers is not often an easy process, especially if none of the developers of the application are familiar with any of the Linux distros that use the given system package managers.
        
        Additionally, the packaging process is made more difficult by the need for dependencies. Should the dependencies also be packages in the system packager manager? What if the developers of a library the application depends on have not distributed the library through the specific system package manager? How do you handle libraries across language package managers and system package managers?
        
        Unfortunately, there don't really seem to be any "right" answers to those questions. Thus, developers have an incentive to just distribute their application through the related language package manager and mostly ignore system package managers.
        
        Luckily, there are some developers of popular applications who put in a good amount of effort to distribute their applications on system package managers. However, such a process in not very straightforward and can cause complications in how the installation is handled and how it interacts with other libraries and applications. For example, Python libraries installed both through a system package manager and the associated language package manager can cause one to try installing the libraries over top the other, leading to issues in the installation process of dependent applications.
        
        However, on the whole, the process of distributing applications on system package managers is not as intuitive as it should be. This discourages application developers from distributing their applications on system package managers.
        
        Until the process of packaging software across many different languages through system package managers becomes an easier and more standardized process, we really won't see language package managers be properly used as edit-recompile managers, rather than their current usage of being both edit-recompile manager and application install managers.
---

A common claim I keep seeing repeated is that there are too many language-specific package managers, and that we should use a distribution's package manager instead. As an example, I opened the most recent [HN discussion](https://news.ycombinator.com/item?id=12187888) related to package managers, and sure enough the [third comment](https://news.ycombinator.com/item?id=12189483) was on this (very) dead horse. ([But](https://news.ycombinator.com/item?id=12026745) [wait!](https://news.ycombinator.com/item?id=11469315) [There's](https://news.ycombinator.com/item?id=11088125) [more](https://news.ycombinator.com/item?id=10662927).) But it rarely feels like there is any forward progress on these threads. Why?

Here is my hypothesis: these two camps of people are talking past each other, because the term "package manager" has been overloaded to mean two things:

1.  For end-users, it denotes an install manager, primarily responsible for *installing* some useful software so that they can use it. Software here usually gets installed once, and then used for a long time.
2.  For developers, it denotes an **edit-recompile manager**: a piece of software for letting you take a software project under development and (re)build it, as quickly as possible. The installation of packages is a *means*, but it is not the *end*.

It should be clear that while these two use-cases have some shared mechanism, the priorities are overwhelmingly different:

- End-users don't care about how a package is built, just that the things they want to install have been built. For developers, speed on rebuild is an *overriding* concern. To achieve this performance, a deep understanding of the structure of the programming language is needed.
- End-users usually just want one version of any piece software. Developers use multiple versions, because that is the cost of doing business with a diverse, rapidly updated, decentralized package ecosystem.
- End-users care about it "just working": thus, a distribution package manager emphasizes control over the full stack (usually requiring root.) Developers care about flexibility for the software they are rebuilding and don't mind if a little setup is needed.

So the next time someone says that there are too many language-specific package managers, mentally replace "package manager" with "edit-recompile manager". Does the complaint still make sense? Maybe it does, but not in the usual sense: what they may actually be advocating for is an *interface* between these two worlds. And that seems like a project that is both tractable and worth doing.
