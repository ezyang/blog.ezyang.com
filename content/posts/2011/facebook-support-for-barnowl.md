---
title: "Facebook support for BarnOwl"
date: 2011-07-13 09:00:15
slug: facebook-support-for-barnowl
categories: [SIPB]
comments:
    - id: 2789
      author: Ben
      date: "2011-07-13 09:33:14"
      content: |
        Hi Edward,
        
        Regarding your postscript - python has eventlet and gevent that let you write code that looks blocking but utilizes green threads and an eventloop. Have you seen them?
        
        Ben
    - id: 2790
      author: lumi
      date: "2011-07-13 10:35:57"
      content: "Have you looked at Perl's Coro, and Coro::AnyEvent?"
    - id: 2791
      author: Edward Z. Yang
      date: "2011-07-13 10:40:37"
      content: "My impression of libraries that are strapped on top of an existing language ecosystem is that they really, really don't play nice with existing code (but I'll happily be proven wrong on this point.) The end result is you have to think very carefully about whether or not code you use is interoperable with what effectively is a new runtime system."
    - id: 2792
      author: Ben
      date: "2011-07-13 10:55:33"
      content: "My experience with eventlet has been very positive so far! Everything (apart from C extensions) just magically becomes non blocking, but is still imperative and easy to reason about."
    - id: 2793
      author: Edward Z. Yang
      date: "2011-07-13 11:12:54"
      content: "Looks like they use monkeypatching to make things work. It is worth remarking that these are cooperative threading libraries, so you have to remember where you might lose control, and where you have to explicitly yield."
    - id: 2794
      author: Brian
      date: "2011-07-14 10:24:48"
      content: "Haskell: popular, imperative, green threads, looks blocking, kqueue under the hood."
    - id: 2795
      author: Ben
      date: "2011-07-14 10:31:09"
      content: "Yeah, I certainly wouldn't throw eventlet at anything CPU intensive, but for any app where you're just sitting in the middle of lots of IO it's ideal. I use it in a webapp bridging between websockets and zeromq and it's ok for that."
    - id: 2796
      author: Edward Z. Yang
      date: "2011-07-14 11:19:05"
      content: |
        Brian: I was wondering when someone would say that! :-)
        
        Ben: Right. And for unrelated reasons, CPU intensive parallelism doesn't work anyway (for example, Python and the GIL). I don't have a sense for how much of a problem this is in practice, but one of my concerns is latency in user interfaces, where even a second processing delay can noticeabley impact user experience.
---

This one's for the MIT crowd. This morning, I finished my [Facebook module for BarnOwl](https://github.com/ezyang/barnowl) to my satisfaction (my satisfaction being asynchronous support for Facebook API calls, i.e. no more random freezing!) Getting it to run on Linerva was a bit involved, however, so here is the recipe.

1.  Setup a local CPAN installation using the [instructions at sipb.mit.edu](http://sipb.mit.edu/doc/cpan/), using `local::lib`. Don’t forget to add the setup code to `.bashrc.mine`, not `.bashrc`, and then source them. Don't forget to follow prerequisites: otherwise, CPAN will give a lot of prompts.
2.  Install all of the CPAN dependencies you need. For the Facebook module, this means `Facebook::Graph` and `AnyEvent::HTTP`. I suggest using `notest`, since `Any::Moose` seems to fail a harmless test on Linerva. `Facebook::Graph` fails several tests, but don't worry about it since we'll be using a pre-packaged version. If you want to use other modules, you will need to install them in CPAN as well.
3.  Clone BarnOwl to a local directory (`git clone git://github.com/ezyang/barnowl.git barnowl`), `./autogen.sh`, `configure` and `make`.
4.  Run using `./barnowl`, and then type the command `:facebook-auth` and follow the instructions!

Happy Facebooking!

*Postscript.* I am really, really surprised that there is not a popular imperative language that has green threads and pre-emptive scheduling, allowing you to actually write code that looks blocking, although it uses an event loop under the hood. Maybe it’s because being safe while being pre-emptive is hard...

*Known bugs.* Read/write authentication bug has been fixed. We seem to be tickling some bugs in BarnOwl's event loop implementation, which is causing crashing on the order of day (making it tough to debug). Keep a backup instance of BarnOwl handy.
