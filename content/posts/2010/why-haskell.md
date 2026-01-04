---
title: "Why Haskell? The big question"
date: 2010-01-08 12:00:03
slug: why-haskell
categories: [Haskell]
comments:
    - id: 27
      author: Axman6
      date: "2010-01-11 01:13:24"
      content: "It should be noted that the State monad is completely functional, and I don't believe it concedes anything about imperative programming being useful. It on the contrary shows that you can achieve the same sorts of things in functional languages that you can in imperative ones, while feeling it's imperative, and having it all completely pure and functional."
    - id: 28
      author: cdsmith
      date: "2010-01-11 01:32:11"
      content: |
        I think the two of you are talking about different things there.  The State monad certainly is completely functional in the sense that is built from purely functional Haskell with no special language support (aside from the general do-notation).  This is important, in particular because a purely functional language is able to express things like that.
        
        At the same time, though, use of the State monad *is* an imperative style of programming, and it often adds complexity to code that could otherwise have been simpler.  It's the same type of complexity that makes imperative code more likely to contain many types of bugs.  So while it's nice that Haskell is expressive enough to build that abstraction, it's still quite reasonable to wonder whether the complexity is necessary to begin with.
    - id: 29
      author: Edward Z. Yang
      date: "2010-01-11 01:39:15"
      content: |
        I probably used inappropriate/misleading terminology in that sentence.  And I also do not mean to belittle the State monad, which is far easier to reason about then spaghetti state in imperative programs.
        
        But when I build a finite state machine because I need, say, an HTML5 parser, I am tying myself to not only an implementation but also a way of thinking about the problem domain. And while it is not difficult to do theme and variations on this base implementation, it is substantially more difficult to find a paradigm that is better equipped to solve the problem more elegantly and more quickly.
        
        Maybe such a solution doesn't exist, or maybe it's difficult to understand in other ways. Maybe the State monad really is good enough. But I can't help and wonder.
        
        (Edit: This was in response to Axman6's comment; I hadn't seen cdsmith's comment yet.)
    - id: 30
      author: Neil Mitchell
      date: "2010-01-11 04:39:46"
      content: |
        There is an HTML 5 parser already: http://community.haskell.org/~ndm/tagsoup.
        
        And it's implemented exactly per the spec, in a very natural way, without any state monads/FSA etc. Given enough thought you can almost always come up with the right abstraction.
    - id: 31
      author: Don Stewart
      date: "2010-01-11 05:30:44"
      content: |
        You might not be aware, but there are almost 2000 libraries on Hackage. More than any other functional language, http://hackage.haskell.org/packages/archive/pkg-list.html and quite close to Python's central library count.
        
        You can usually assume there is already a library for what you need.
    - id: 32
      author: Edward Z. Yang
      date: "2010-01-11 11:10:48"
      content: |
        Neil, that's a pleasant surprise to hear, and I think strengthens my argument (although I'd have to look closely to see if it was implemented exactly per spec.)
        
        Don, I knew that Hackage had a lot of libraries, but I didn't realize it was already two thousand. :-) My perspective here is slightly influenced by the fact that, several months ago, Haskell didn't really have mature libraries for SQL manipulation (I haven't looked at it since, and the landscape may have changed greatly since then.) I'm more than willing to be shown wrong on this front.
    - id: 33
      author: Anonymous
      date: "2010-01-11 19:59:23"
      content: |
        &gt; Why Haskell rocks: the static analysis is complete enough that if it compiles, it runs correctly.
        
        Oh sure, you could never type + 2 instead of + 1 by accident because all errors are type errors. :P
    - id: 34
      author: Raoul Duke
      date: "2010-01-11 20:21:13"
      content: |
        also, please see Clojure's new Protocols.
        
        http://www.assembla.com/wiki/show/clojure/Protocols
    - id: 35
      author: Edward Z. Yang
      date: "2010-01-11 21:18:11"
      content: |
        > because all errors are type errors. :P
        
        That's exactly true, and one of the reasons why types are so useful. They're powerful, but they're not too powerful as to make their meaning unclear again: as Simon Peyton Jones has put it, "crisp and compact."
        
        A lot of software these days is algorithmically boring. I think this code reaps a lot of benefits from static type checking.
    - id: 36
      author: dave glasser
      date: "2010-01-11 22:42:49"
      content: "For what it's worth, it's not true that \"Google has banned [Python] from living in public facing code\", though it is certainly a distinct minority of user-facing servers."
    - id: 37
      author: Thanassis
      date: "2010-01-12 09:20:32"
      content: "I've been learning Haskell in the last two months, and even though I love the brevity (a-la Python) and type safety (a-la C++) of Haskell, I find myself disappointed with Haskell's speed... I wrote a simple Mandelbrot calculation in different ways (list comprehensions, tail recursion), and found it to be MUCH slower than the corresponding C/C++ code. The final blow was when I realized that I didn't have to use my own complex type and could use the library's Complex... only to see the speed get even slower!  Also disappointing is that being new to Haskell (but very experienced in the imperative world - x86 ASM, C++, Python, etc) I can't seem to \"trace\" the reasons behind the speed differences (since the actual code doing the work is actually very little, there is obviously a lot of \"hidden\" code that is generated from GHC). I invite you for a simple brainstorming (mandelbrot's calculations are very simple) at \"ttsiodras at the well known gmail dot com\", so we can have a look together and see what's going on..."
    - id: 38
      author: Neil Mitchell
      date: "2010-01-12 10:13:42"
      content: |
        Edward: Take a look with the spec beside you, I've got cross references to each paragraph of the spec, and have deliberately used the same conventions/naming as the spec.
        
        In a few places I've violated the spec, but always with comments as to the reasons, and always with the minimal changes possible. All the changes to the spec are to be more liberal, because the spec didn't accept enough HTML for tagsoup (and they are fairly rare).
    - id: 39
      author: Achilleas Margaritis
      date: "2010-01-12 10:24:51"
      content: |
        &gt; if it compiles, it runs correctly.
        
        It's a myth really. Take Darcs, for example: The Haskell community was proud about Darcs, the source control system written in Haskell. It compiles fine, but it's plugged with bugs; very serious bugs.
        
        I once had my professor and ML supporter (at Brunel) insisting that an ML multithreaded program never deadlocked, but, to his surprise, it did.
        
        The truth is that logic errors like mistyping + 2 instead of + 1 cannot be caught by any compiler, and will never be caught.
    - id: 40
      author: Artur
      date: "2010-01-12 12:36:27"
      content: |
        About 4 years ago I've written a Haskell server component which did some massive computations, parsed/generated XML and communicated through sockets. For purely mathematical code Haskell is beautyfull and gives new productivity level. But when real world is encountered (I/O, performance, debugging) it sucks. Libraries for it are limited in functionality and monad programming is a bad thing in terms of software engineering (mainly because it enforces program structure, makes simple changes hard and is tottaly impractical).
        
        I also love Python and currently my preffered design is to write small Haskell/Scala components which are controlled by a Python process in a simples possible manner (like stdin/stdout communication, or execnet in case of Scala).
    - id: 41
      author: Anonymous
      date: "2010-01-12 18:32:36"
      content: |
        Thanassis:
        
        make sure you are not using infinite-precision arithmetic.
    - id: 44
      author: Thanassis
      date: "2010-01-13 04:01:50"
      content: |
        I used a pair of Float in my first versions, and then switched to Complex Float - so I believe I am not using infinite precision (only 32-bit floats). I uploaded the code on "github dot com slash ttsiodras slash haskellMandelbrot"
        (the blog doesn't allow me to post URLs) - any feedback on why the library Complex Float are 10times slower than a pair of Float most welcome...
    - id: 69
      author: Daniel Lyons
      date: "2010-02-05 18:53:42"
      content: |
        Achilleas,
        
        I certainly agree that it's an overstatement to say that Haskell catches all errors (if it did, there wouldn't be an XMonad bug tracker), I think it's phenomenal how well it can catch everyday mistakes. If we're going to achieve progress in software correctness, we need all the help we can get.
        
        The real problem isn't that Haskell doesn't catch everything, or even that enthusiastic users think that it does. The problem is that there are a lot of programmers who aren't going to learn Haskell, and many of them need or want these benefits as much as we do.
---

Language selection is a contentious thing, and often a compromise between "pick the right language for the job" and "use as few languages as possible to increase mindshare." Google, for example, [limits the programming languages](http://steve-yegge.blogspot.com/2007/06/rhino-on-rails.html) their employees are allowed to use; and I have come to associate picking whatever language you want for your own projects as irresponsible, having once been told, "Yeah... that project was written in X and no one besides the guy who wrote it knows X... probably not a good use of your time to work on it." Of course, I've been quite culpable of this myself; I wrote the member dues tracking system for the Assassins' Guild in Haskell, and unless a miracle happens I am kind of doubtful future maintainers will be able to deal with it.

When I am not being irresponsible, Python is my favored language for most of my scripting needs, and as such I am painfully aware of quirks in the language that Haskell would smooth away.

- Python code is dynamically typed and variables have no scoping. Brain-o typing errors, variable misnamings and plain ole broken code isn't caught unless a code path is exercised. *What makes it better:* `pylint -e` catches large classes of errors (but you commonly have to wade through recursion limit errors to find it, and I strongly believe any error checking not built in the compiler is doomed to be ignored by the people who need it most), as is full code coverage on whatever automated tests you have. *Why Haskell rocks:* the static analysis is complete enough that if it compiles, it runs correctly.
- Python is slow. If you don't believe it: ask yourself why the runtime can't be loaded quickly enough to make running Python as CGI tenable, or why Google has banned it from living in public facing code, or why engineers treat rewriting a Python daemon in C++ as the inevitable conclusion when you just can't wring out anymore speed. *What makes it better:* Not everything has to be blazing fast. *Why Haskell rocks:* Insane people writing insane compilers like GHC which compile into native binaries and have absolutely epic speed.
- Python has an limit to comprehensible code golfing. While duplication of high-level code structure is no where as bad in Python as it might be for Java or C++, attempts to purify code even further often lead to highly incomprehensible higher order functions that require copious documentation. As people say, "Don't write Haskell in Python." *Why Haskell rocks:* The type system not only becomes essential to the documentation of the code, it also serves as a framework by which a user can "snap" together combinators and data like legoblocks, leading to a much higher tolerance of complexity.
- Python has inherited an aging object-oriented paradigm. However, I am increasingly convinced that typeclass based systems ([Go](http://golang.org/) is one decidedly imperative language that has picked them up) are the way to go. In combination with type signatures, they provide the two primary benefits of OOP: a logical organization of functionality and polymorphism, without all of the complicated gunk that is multiple inheritance, mix-ins, metaclasses, etc. *Why Haskell rocks:* First-class support for type-classes.
- Python has abysmal threading support. It has the global interpreter lock. *Why Haskell rocks:* It not only has fast, green threads and the notion of purity to make splitting computations feasible, it has made it extremely simple to experiment with scheduling algorithms with the computation. I can't say much more in this field, because I have very little experience writing parallel Haskell code.

But I would cringe to attempt to write in Haskell one of the large projects that I have done in imperative languages like PHP or Python (I mention these two particular languages, because within them I have built [two](http://htmlpurifier.org) [systems](http://scripts.mit.edu/wizard) that are actually large), for these very important reasons:

- Haskell has not grown sufficient library support to become fully multiparadigm. I am highly skeptical that a straight-forward port of any given piece of Python code would be possible; despite great advances in shepherding the more dynamic features of Python into Haskell's type system with packages such as Text.Printf, action at a distance intrinsic of an imperative program would require heavy IO wizardry in Haskell.
- It's not obvious which problems in the imperative domain truly are better kept in the imperative domain, as [James Hague](http://prog21.dadgum.com/54.html) has mused recently. The Haskell community is fairly unified in its stance that as little code should be in the IO monad as possible, but when we bring in small bits of the imperative world to help us in cases such as the State monad, we acknowledge the imperative paradigm is useful... or at least an easy way out. Perhaps if we tried harder we could find a more elegant, maintainable, purely functional solution; and one of the favorite pastimes of academics is to figure these things out. But this is hard even for those used to thinking functionally, and the answers often need to be discovered, let alone implemented.
- All of the engineering folklore, wisdom and best practices that have been accumulated from years of hacking on large, imperative codebases may or may not apply to functional codebases anymore. If functional libraries are encouraged to be as decoupled as possible, do we need to allow for further decoupling in the API? Does pure code need to be logged, or is its determinism make it trivial to debug? What testing do we need, and how much trust do we put in the types? How does API documentation and cross-referencing need to evolve for functional codebases? How the heck do you go ahead and debug a logic error in a section of pure Haskell code?

Yet, there are [companies](http://www.galois.com/) that are putting out production size codebases in Haskell, which makes me optimistic that answers to these questions will soon become public knowledge; if not for Haskell, for some other purely functional language. And the "classic" solutions in the imperative world have often lead to insidious errors, especially in a world of multithreaded applications, and we *must not* settle for "good enough." Software sucks, but purely functional systems with strong, flexible types have the promise to eliminate large swaths of this suckage. And *that* is why I Haskell.
