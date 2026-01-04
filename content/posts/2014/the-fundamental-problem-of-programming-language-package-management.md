---
title: "The fundamental problem of programming language package management"
date: 2014-08-21 09:02:53
slug: the-fundamental-problem-of-programming-language-package-management
categories: [Software Engineering]
comments:
    - id: 7542
      author: Anonymous
      date: "2014-08-21 11:49:19"
      content: |
        Hi Edward,
        
        I would recommend looking at the philosophy and design documentation behind Adept (https://github.com/adept-dm/adept). They're definitely very close to the "pinned" corner, but I think their way of doing is very interesting, i.e. complete separation of "find a new solution with newer dependencies" and "download and build with the pre-recorded solution". 
        
        The fact that it's actually informed by the failings of the Maven/Ivy approaches and the need for properly handling binary (in)compatibility in the Scala ecosystem makes me think that it's quite a promising approach.
    - id: 7543
      author: Edward Z. Yang
      date: "2014-08-21 11:55:33"
      content: "Thanks for the pointer, Anonymous. The documentation is fairly thin on the ground: are they innovating on the side of “find a new solution with newer dependencies”?"
    - id: 7544
      author: Anonymous
      date: "2014-08-21 12:17:45"
      content: |
        Oh, the design document seems to be buried in one of the obsolete wiki pages. Here it is:
        
        https://docs.google.com/document/d/1xU9m2zxva2eKhiXVYYqjmZieaWPJY0mDbEmZ_pE5P5c
        
        I'm not really an expert on the way Cabal works, so I couldn't really tell you if there's any innovation in terms of resolution algorithms -- I can only suggest skimming the design doc :).
        
        I guess it's kind of tangential to your post, but for me one of the key things is separating the "download and build" bit out and the fact that they use hashes in artifact URLs (which would correspond to source downloads from Hackage), which means you don't need HTTP which in turn makes local mirroring completely trivial via a simple HTTP proxy. It's been a neverending source of frustration for me (and my company) that simple spurious network outages can mean that your Continuous Integration cannot build anything. (You can mitigate this via Maven proxies, but that's yet another complex piece of software which needs to be set up, kept up to date and maintained.)
        
        Anyway, hope the design doc above gives a little more information :).
    - id: 7547
      author: Steve Klabnik
      date: "2014-08-21 12:52:36"
      content: |
        &gt; if someone tells you that package management is solved, just reimplement Bundler, I entreat you: think about decentralization as well!
        
        Can you comment on how Bundler isn't decentralized under your criteria? Adding a new `source` line is all it takes to "maintaining their own central authority, or deferring to someone else's central authority".
    - id: 7549
      author: Mark Coiffier
      date: "2014-08-21 14:22:39"
      content: |
        Good evening,
        
        I wrote a script (available at https://github.com/lih/World) a few months ago to solve the problems of package management in Haskell because I had the exact problems with the existing tools that you just described. For some reason, I found myself in need to maintain several libraries with annoying dependency graphs (too complex to be painlessly maintained by hand, yet too simple to warrant a centralized approach).
        
        The basic idea of this script is that you can maintain several build environments, each with its own package ecosystem, within which libraries and/or executables may be compiled. You just need to select one or several environments (which can share common packages) and try a build to generate the executables and object files required by all those environments (whose source files should be contained in one of the environment's packages).
        
        Source files for Hackage dependencies are automatically downloaded as needed, and local source files override distant ones, so that you may patch an existing package if need be. 
        
        Dependencies are automatically deduced from import lists and source file locations, so there is no need to specify them anywhere (you still need to list all the packages in the environments, though). That means the script can (and does) also automatically generate an up-to-date cabal file just from source information, and with that a source archive that can be uploaded to Hackage. More to the point, it can generate *all* source archives for local packages with the appropriate inter-dependencies, which can then be uploaded in batch to Hackage without fear of discordance.
        
        I must be a little biased, but I have been using this script exclusively to build all my Haskell projects for the past few months, and I never think about going back to cabal-install or dependency hell.
        
        If you have some time to spare, I would love some feedback about it. If not, it was a great post nonetheless :-)
    - id: 7550
      author: Luca Bruno
      date: "2014-08-21 15:16:59"
      content: "Maybe you are looking for nix: http://nixos.org/nix/"
    - id: 7551
      author: Brian Hurt
      date: "2014-08-21 15:20:58"
      content: |
        A couple of ideas:
        
        One, include a recommender system, or at least a rating system.  One of the problems that cabal has is people trying to figure out which package of several different possibilities they should use.
        
        Another is to have official releases.  This is sort of like pinning, except instead of a single version, you can pin multiple versions.  This is exactly how OS package managers work.  This gives some way to detect, and drop, unmaintained libraries.  If your code doesn't work with the latest packages, and you don't fix it, then it needs to be dropped.
        
        The third idea requires language support: being able to link in multiple different versions of a library in different parts of the same program.  A real problem I've had: module X depends upon version 1 of library W, and doesn't work with version 2.  Module Y depends upon version 2 of W, and doesn't work with version 1.  I need to use both X and Y.  Being able to link in scoped way would help, so I could link two different versions of W into my program.  Of course,  this will almost certainly require changes to the language (for example, type class implementations would have to be scoped).
    - id: 7554
      author: Anonymous
      date: "2014-08-21 15:58:06"
      content: |
        I really think Nix/NixOs solves most all dependency hell problems.  It has built-in tools that utilize other package managers.  So it will download the cpp, java, ruby and haskell dependencies of your project, and build them all individually, with their appropriate package managers and tools and then build your project itself.  All without polluting or relying upon the system environment.  
        
        https://nixos.org/nix/
        
        "Nix is a purely functional package manager. This means that it treats packages like values in purely functional programming languages such as Haskell — they are built by functions that don’t have side-effects, and they never change after they have been built."
        
        "Nix is a powerful package manager for Linux and other Unix systems that makes package management reliable and reproducible. It provides atomic upgrades and rollbacks, side-by-side installation of multiple versions of a package, multi-user package management and easy setup of build environments"
    - id: 7555
      author: Brian Hurt
      date: "2014-08-21 16:22:18"
      content: |
        And a good example of what not to do:
        
        http://forums.thedailywtf.com/forums/t/27755.aspx
    - id: 7556
      author: Anonymous
      date: "2014-08-21 16:43:08"
      content: |
        (That same Anonymous as before.)
        
        It just occurred to me that it probably wasn't very clear from my previous posts. The Adept approach (or pinning, in general) would mean that a distributor of a package could actually *know* that their package would install (via cabal), simply because the resolution of the dependencies would actually be distributed along with the package itself. This is one of the things that sorely missing from Hackage at the moment. You can sort of attempt it like Michael Snoyman did with his yesod-platform (which pinned everything), but it's... suboptimal.
    - id: 7557
      author: GuessSO
      date: "2014-08-21 17:19:10"
      content: "Checkout the OSGi \"BND\" Tool."
    - id: 7558
      author: Dave Herman
      date: "2014-08-21 17:33:09"
      content: |
        I'm not sure I understand what you're getting at with decentralization. But your critique of pinning is where I disagree the most strongly. Repeatable/deterministic builds are central to the workflow of building and deploying production applications. So that's certainly "king" as you say, but this:
        
        &gt; these package managers solve the decentralization problem by simply pretending the ecosystem doesn't exist once you have pinned the versions
        
        is just not accurate. The way tools like Bundler work is that they create a separate file that enables deterministic builds, but the standard workflow of pinning does not in any way divorce a project from the ecosystem. Let me elaborate.
        
        AIUI, there are key differences between how Cabal does pinning and how tools like Bundler, PLaneT (I'm not familiar with the new Racket package manager), and Cargo do it. With Cabal, I believe that pinning overwrites your manifest, which actually erases information expressing programmer intent. In Bundler et al, the normal MO is to express just the major version you depend on or a major version and a *minimum* minor version, but you are allowing the package manager to maintain responsibility over getting the most recent version and specifying in the lockfile what version you built with. Effectively with semver your manifest expresses two things: the version of the API (major version), and the bug-compatibility (minimum minor version, or minor version ranges in the relatively rarer cases where there are known bugs in later minor versions); by contrast the lockfile maintains a distinct and orthogonal set of information: roughly, the exact versions of all packages in the last known successful build of the project.
        
        Keeping the manifest and lockfile separate preserves developer intent about the dependency graph, allowing you to continue evolving the manifest and to update your lockfile and refresh the local dependency cache as you go along -- in short, pinning does not cut you off from the ecosystem. At this point, the only issue is ensuring that this update process has sufficiently low cognitive overhead. Now, even a whole-hog update is just a one-liner (`bundle update`). But Bundler and Cargo go a step further with "conservative updates": whenever you modify your manifest (e.g. to add new dependencies or change versions of existing dependencies) and ask bundle to install the new dependencies, which is an everyday part of the process of hacking on a project, it determines the minimum subgraph of the dependency graph that has been perturbed and automatically updates and re-pins them. In other words, since the changes to the manifest are already perturbing a subgraph, they have to be rebuilt and retested anyway, so the package manager might as well go ahead and automatically upgrade them now. So this goes further towards lowering the cognitive overhead of updating packages within a pinning model.
        
        Now, you raised concern about security updates. I don't think this is actually an argument against having repeatable builds as the default mode of operation. Instead, it only argues for (a) making updating from the ecosystem low-overhead (as I described above) and (b) good tooling for notifying programmers about vulnerabilities. The Ruby ecosystem has developed tools, so that all you do is:
        
        1. The tool monitors your lockfile and notifies you when there's a known vulnerability.
        2. You update that dependency's version in your manifest and install.
        
        That said, you could go even further and build this monitoring into the package manager itself: allow package maintainers to flag their package in the registry as having a vulnerability so the package manager itself can warn clients that a pinned version has a known vulnerability so that they can update.
        
        To sum up, supporting deterministic builds as the default workflow is important for predictably building applications, and if designed well it doesn't at all necessitate cutting a project off from the ecosystem.
    - id: 7560
      author: Edward Z. Yang
      date: "2014-08-21 19:05:46"
      content: |
        My ignorance of Bundler is showing!
        
        Steve Klabnik: My understanding is that while Bundler, like many other package systems, allows you to point to a different central repository, that's about as much rope as it gives you. I agree, this is essential to decentralization, but it's not so good if the average user can't use it! (I'll remark that the manpage says: "It is possible, but not recommended as of Bundler 1.7, to add multiple global source lines. Each of these sources MUST be a valid Rubygems repository.")
        
        Dave Herman: That's the recommended Bundler workflow? That's great! It's a far cry from when I did system administration for scripts.mit.edu and got to work with virtualenv, which at the time was the state of the art for version pinning in Python. No one ever updated their dependencies, it was just really terrible. On the other hand, when you do run 'bundler update', you are back to the good old days of trying to figure out why your dependencies are breaking, and I think we can still improve there. (If you don't mind, I'll probably go and tweak the text there a bit.) Something that I don't think I really articulated well in the post is that the approaches are not exclusive: I think you do want to support all of the workflows.
        
        Stepping back a bit, why do I think the emphasis on "decentralization" is important? I think it has to do with perception. What is a package manager? The naive answer is, "It's how I get packages that other people published onto my machine." But if you build a package manager like this, it will not be a very good package manager. Will it be a good package manager if you implement "pinning"? Not if you misunderstand why Bundler pinning is implemented so well! It's clearer if you realize that the package manager is actually just a way to interact with the larger ecosystem, in more ways than just "download the latest version."
    - id: 7563
      author: Edward Z. Yang
      date: "2014-08-21 19:18:02"
      content: "To the commentors mentioning Nix: I do think Nix is probably the closest we have to a suitable package manager for programming languages that originated from operating systems. But my understanding of how the Nix package index works is that, although Nix will keep old versions around, the actual index that represents the \"current latest versions of everything\" is something that must be centrally managed. True, it's pretty flexible, since in Nix this index is an actual program, but it's not ideal."
    - id: 7564
      author: Luca Bruno
      date: "2014-08-21 20:06:51"
      content: "Write a Nix expression for any version you want. It's no very clear what you mean by \"index\"."
    - id: 7565
      author: Edward Z. Yang
      date: "2014-08-21 20:16:54"
      content: "Luca: By index, I'm referring to the environment of available Nix expressions. A not very good example is supposing you want to patch glibc and run a custom version of it. In Nix, I'd need to somehow get updated Nix expressions for all of the packages depending on glibc. This is a bad example because the default Nix expressions actually do have hooks for this particular use-case, but it's not hard to imagine needing to do something quite different and having difficulty."
    - id: 7566
      author: Yehuda Katz
      date: "2014-08-21 21:05:46"
      content: |
        Ed,
        
        One of the things that I'm pretty proud of about the Bundler ecosystem is that many of the problems you're alluding to have been solved... in a decentralized way!
        
        For example, https://gemfury.com/ is a "registry as a service", which allows you to create your own private registry and add it to your Gemfile.
        
        https://hakiri.io/facets is a project that will scan your Gemfile.lock as it is updated on Github, looking for security vulnerabilities (which are collected in machine-readable form at https://github.com/rubysec/ruby-advisory-db and used by a number of tools) and warning you to update. The Gemfile.lock is especially useful here, because it's a way for tools like Facets to know, accurately, which versions of a package are *actually* being used.
        
        https://www.ruby-toolbox.com/ addresses another one of your concerns, that it's important to be able to learn about the available choices for a given category of packages, and quickly understand things like relative popularity and how well-maintained they are.
        
        In principle, you could argue that these tools should all be part of one big Centrally Planned package manager, but I've found that loosely coupling these tools, and allowing the community to improve on individual pieces over time, has led to an extremely high-quality ecosystem of tools that addresses many of your concerns.
        
        Finally, I'll note that when we designed bundler in the first place, we chose a lockfile + conservative updating model after explicitly evaluating a number of existing solutions, including Python's virtualenv. Indeed, we explicitly considered many of your concerns in the original design, which is why we had support for multiple registries and packages distributed through Github from the beginning.
    - id: 7571
      author: Anonymous
      date: "2014-08-22 04:01:27"
      content: "I still don't get the problem with running a patched glibc, sorry. You are able to get two whole different set of packages by simply calling a couple of functions and overriding a couple of attribute sets. Have packages all with glibc X, and all with glibc Y, in two different variables like pkgs1, and pkgs2."
    - id: 7617
      author: KJ
      date: "2014-08-26 06:28:51"
      content: |
        Agreed. I've been teaching myself development for a few months now.
        
        How the f_ck could I have to use Bundler, Composer, Bower, Grunt, Apt, etc. etc. for just one project?
    - id: 7618
      author: KJ
      date: "2014-08-26 06:30:07"
      content: Wait I forgot berkshelf and who knows what else.
    - id: 7619
      author: Fredrik
      date: "2014-08-26 06:44:12"
      content: |
        Hey,
        The author of Adept here.
        The essence of your reflections are very close to my own: decentralization is important.
        
        I am going to update the docs, they were written when I was still unsure how the design will be looking, but I have had to work on other projects to make my living this summer. I haven't pinned it down yet :P and we are also still open for changes. I have figured out a way to be able to get more time for Adept now though, so I will update the docs soon, but for now, hopefully, this explanation can enlighten you about how Adept works.
        
        The core of Adept are "variants" and "requirements". Requirements is what a user would typically define and consists of an id and an arbitrary set of constraints. Variants defines a module, and has an id and some attributes (as well as information about artifacts and its own requirements). The constraints and attributes are key/value pairs (you can have more than one value of a key, e.g. "api-version-compatibility" -&gt; ["1", "2", "3"]) and matching them is done on equality only: so for a variant to match a constraint it has to have all the key/values defined in the constraint in its attributes.
        
        On resolution, Adept loads a set of variants and tries to find variants matching the ids and constraints defined by the user. Resolution is only successful if there is exactly one variant found per id matching the constraints. This makes the resolution algorithm is extremely simple (it is only ~200 LOC) and the bulk of the logic is dedicated to search for UNIQUE possible matches that is deeper in the tree. The reason is to be able for Adept to for example automatically pick the right compatible libraries. If you for example have a  scala library which is cross-compiled for say scala 2.10 and 2.11 (scala is not binary compatible, so a lib compiled for 2.10  of Scala is not compatible with with 2.11). The mechanism is generic though so you can imagine this works for really any platform/library.
        
        If all variants of an Id where loaded and checked each time, a user would have to specify their constraints very tightly thus pinning down their dependencies (in addition to being very slow). To avoid this, there are another concept called "ranking files" for each id. The ranking files defines which variants should be preferred over others for this given version of the metadata. In essence the ranking files and the variants, constitutes a compatibility matrix for a module.
        
        To describe what I have described up to now, let us take an example: Imagine there is a module which is "semantically versioned", and there are 5 versions, 3 for 1.0.x and 2 versions for 1.1.x.
        So you would have a variant for each release. The variants' attributes would have a version and a "binary-version", and all variants with the same binary-version would be in the same ranking file:
        1.0.x-series rankfile                         |  1.1.x-series rankfile
        [version 1.0.2, binary-version 1.0] | [version 1.1.1, binary-version 1.1] 
        [version 1.0.1, binary-version 1.0] | [version 1.1.0, binary-version 1.1] 
        [version 1.0.0, binary-version 1.0] |
        
        To be able to keep resolution results reproducible and stable the set of variants that are loaded are defined in a "context", which tells Adept where the metadata is found and which version of the metadata to use (the metadata is versioned in Git).
        When the contexts are loaded, the versions of the metadata is checked and the latest one is picked and _only_  the latest variants for each rank file are picked.
        
        This will effectively force the user to specify the binary-version she/he wants to use: if she/he does not pick a binary-version in this case, Adept would fail, because there isn't one UNIQUE way to resolve. To update to the latest release of the 1.0.x series (in this case), a user would simply ask Adept to update the metadata. The ranking changes and a newer binary compatible version of the variants are chosen. Since metadata is versioned, the author can evolve the module to have binary version at later, without fearing to suddenly break builds for existing builds. Authors can also define different matrixes matching their needs (either have a single column or something more complicated with different variants for different platforms).
        
        Since metadata is versioned (and stored in separate repositories) and strongly defined (in the contexts) it is possible for it to be decentralized/distributed, thus making it possible to use metadata from more or less anywhere. The constraint-based resolution and versioned metadata makes it possible for authors to avoid pinning users down.
        
        This is a lot of information and I think it is rather different take on dependency management, but I have also tried to make sure it is easy to use. The stable and simple design of the resolution mechanism, makes it easy to create tooling on top of it, so for a user it ends up being simpler than the package/dependency managers I know.
        
        You should also note there are design decisions that I have made in order to make Adept very fast, cachable, deterministic and easy to use. Have a look here for more information: https://github.com/adepthub/adepthub-ext/blob/master/concepts.md and https://github.com/adepthub/adepthub-ext/blob/master/guide.md for more details on how it would be to extend a build tool with it.
        
        
        Thanks to Anonymous for linking to Adept! I need help to get more traction on Adept, so if you feel up for it ping me on github - the resolution engine is built to be portable to other platforms and I will be happy to discuss the design to make that happen.
    - id: 7620
      author: Thomas Leonard
      date: "2014-08-26 08:17:20"
      content: |
        "The Git of package management doesn't exist yet".
        
        You might find this interesting: http://0install.net/why.html:
        
        "Zero Install is a decentralised cross-platform software installation system available under the LGPL. It allows software developers to publish programs directly from their own web-sites, while supporting features familiar from centralised distribution repositories such as shared libraries, automatic updates and digital signatures. It is intended to complement, rather than replace, the operating system's package management. 0install packages never interfere with those provided by the distribution."
        
        It's already available in most distribution repositories:
        
        # Debian, ubuntu
        apt-get install zeroinstall-injector
        
        # OpenSUSE
        zypper install zeroinstall-injector
        
        # Fedora
        yum install 0install
        
        # Red Hat
        yum install zeroinstall-injector
        
        # Arch
        yaourt -S zeroinstall-injector
        
        ...
        
        Also, here's an article about decentralised package management (2007, so a bit dated):
        
        http://www.osnews.com/story/16956/Decentralised-Installation-Systems/
    - id: 7622
      author: "Ertugrul \"ertes\" Söylemez"
      date: "2014-08-26 09:48:38"
      content: |
        Hi Edward,
        
        Nix has been mentioned a few times.  I'm a Nix(OS) enthusiast with good reason.  Allow me to explain.
        
        Nix is actually a fully fledged lazy functional programming language.  That is, you have the power of abstraction.  Nixpkgs is not a "package repository", but a real library written in Nix.  What does a modern functional library do to give you parametricity?  Higher order functions!  In other words, what you call "hooks" is exactly what I'd expect Nixpkgs to do to give me the flexibility to override glibc.  This is principled and well understood, in contrast to what most "package managers" do, if they allow this at all.
        
        Nix really solves all of the problems you mentioned in an elegant way.  It made writing Haskell a joy again and even beats cabal-install's sandboxes without actually giving up Cabal as a build system.  It also made deploying Haskell projects a joy, whether as a command line tool, a daemon or even as a little program that runs at system boot, whether on my local machine, on our servers or in the cloud – without the fear that I might have dependency/configuration problems on other machines that I didn't have locally.  I could even request that modified glibc just for my little program without touching the rest of the system, and, notably, without touching Nixpkgs; something that is completely outside of the scope of Cabal.
        
        Haskell is an example of a well integrated language.  The selection of those is currently a bit limited (I know of two good examples, Haskell and Python, and one good counterexample, Ruby), but this can change with community size.
        
        Also portability becomes much less of a problem.  For example when writing a C/C++ program you can insist on Nix and save yourself the nightmare of using autoconf/automake/libtool.  Nixpkgs has all of that baked in in a sensible way.  Currently not many platforms are supported (practically only Linux and OS/X), but this will change with a growing community.
        
        Of all the approaches I've seen, including most of what has been mentioned here in the comments, Nix is the only approach that I would call a *solution*.
    - id: 7623
      author: Jake
      date: "2014-08-26 10:10:11"
      content: |
        .Net has 1 package manager: NuGet.
        It works well, and is built-into the main dev tool; Visual Studio.
        
        Compare this to Java which have dozen, all of which equally suck.
        As much as they suck on their own, their integration into the crappy Java IDEs suck even more.
        
        I'm glad I don't have to use Java on a daily basis.
    - id: 7624
      author: David
      date: "2014-08-26 10:33:16"
      content: "Originally everyone had whatever programs came out of the wild forests. They'd be docile enough to help plough in the applications fields, producing a difficult youngling every few years before they died. Then the POSIX/autoconf trade came, and once-exotic libraries became available making farms instantly more productive. Interaction between the programs remained more of a curiosity than a must-have. If you didn't have a libpng, and hadn't spent the long nights delivering a young imagemagick then your harvest would be a bland pile of dithered but saleable gifs. Still, continuing advances in quality meant diseases due to ABI change were disappearing and the life expectancy of adult binary forms became much longer, making on-farm source births much rarer. Structured breeding programs took off. Packaging laws and libtool took the otherwise wild and evolving features, tamed them and dressed them in version numbers. Rising overall system wealth meant each household OS could afford abstraction layers and runtime checks, keeping dozens of the versioned programs organised, stabled and regularly checked by a vet. Professional wranglers and breeders would of course still have to deal with the messy business of direct integration - deciding which offspring were healthy enough to keep, and guessing which ones wouldn't tear another's throat out if released. Regular farmers trusted that these professionals got these decisions right, but often there were horrific accidents and fights, sheepishly explained away by bad luck. But taming nature is science's work, and slowly the taxonomies and analyses proposed by academics with soft hands and sharp eyes, influenced how the breeders thought about their work. The industry saw developers who wanted to conserve the old traditional lines in their breeding, and those who embraced progress though seemingly wild and experimental processes. ... tbd"
    - id: 7625
      author: Anonymous
      date: "2014-08-26 11:25:14"
      content: "I don't see how decentralization is the key difference. I can download a system package, and install it in my home dir. What exactly can language specific package managers do beyond this that is useful?"
    - id: 7657
      author: "Is there a single answer to the problem of package management? | Smash Company"
      date: "2014-08-27 18:05:30"
      content: "[&#8230;] Interesting: [&#8230;]"
    - id: 7668
      author: Wolfram
      date: "2014-08-28 10:57:27"
      content: |
        More related work: CUDF:
        http://www.mancoosi.org/cudf/
    - id: 7710
      author: Robert Falkowitz
      date: "2014-08-31 07:00:28"
      content: Thank you for considering the rightful properties of some one or other of these package managers.
    - id: 7715
      author: "TechNewsLetter vol:3 | Vishnu-Tech-Notes"
      date: "2014-08-31 15:42:14"
      content: "[&#8230;] The fundamental problem of programming language package management [&#8230;]"
    - id: 7722
      author: "Links &amp; reads for 2014 Week 35 | Martin&#039;s Weekly Curations"
      date: "2014-09-01 07:29:00"
      content: "[&#8230;] The fundamental problem of programming language package management [&#8230;]"
    - id: 7867
      author: Martin Keegan
      date: "2014-09-10 07:02:20"
      content: I like to affect that the package manager for C is UNIX.
    - id: 7886
      author: Luca Bruno
      date: "2014-09-12 04:01:41"
      content: |
        Edward: well, let's talk about the "something quite different and being difficult" then :-) Provide a real example where Nix may not work.
        
        Patching glibc is not an easy task however, I must admit. It forces to rebuild the world. However, with proper configuration, you are able to not rebuild the world: https://nixos.org/wiki/Security_Updates
        
        There you can see you say: instead of using the usual glibc, use this one and don't rebuild the world. If it breaks ABI it's your own responsibility.
    - id: 8155
      author: "The Importance of Human Resource Management | Create Resumes | Find Jobs | FastJobz.Com"
      date: "2014-09-23 20:53:01"
      content: "[&#8230;] roots of most management problems lie in the organization&#8217;s misuse of its human resources. In most cases this is not [&#8230;]"
    - id: 12611
      author: Sam Lowry
      date: "2015-02-09 15:08:06"
      content: "So many posts, but none mentioned yet pkgsrc."
    - id: 12689
      author: "Lazy Reading for 2015/02/22 &#8211; DragonFly BSD Digest"
      date: "2015-02-22 09:35:39"
      content: "[&#8230;] &#8220;Why are there so many goddamn package managers?&#8221;  (via) [&#8230;]"
    - id: 20509
      author: jul
      date: "2016-02-13 11:59:21"
      content: |
        Stable + pinned... binary source distribution
        
        There is a 3rd way ... called source distributions and people making very lightweight patch to integrate the upstream in current respecting some rules of compatibility by fixing the version numbers. 
        
        It requires to give up on using heuristique and code when human do a better job. 
        
        That may explain how BSD with 1/1000 of the firepower of linux achieve similar results (at the price of not integrating shitty softwares).
---

Why are there so many goddamn package managers? They sprawl across both operating systems (apt, yum, pacman, Homebrew) as well as for programming languages (Bundler, Cabal, Composer, CPAN, CRAN, CTAN, EasyInstall, Go Get, Maven, npm, NuGet, OPAM, PEAR, pip, RubyGems, etc etc etc). "It is a truth universally acknowledged that a programming language must be in want of a package manager." What is the fatal attraction of package management that makes programming language after programming language jump off this cliff? Why can't we just, you know, [reuse](http://www.standalone-sysadmin.com/blog/2014/03/just-what-we-need-another-package-manager/) an existing package manager?

You can probably think of a few reasons why trying to use apt to manage your Ruby gems would end in tears. "System and language package managers are completely different! Distributions are vetted, but that's completely unreasonable for most libraries tossed up on GitHub. Distributions move too slowly. Every programming language is different. The different communities don't talk to each other. Distributions install packages globally. I want control over what libraries are used." These reasons are all *right*, but they are missing the essence of the problem.

The fundamental problem is that programming languages package management is **decentralized**.

This decentralization starts with the central premise of a package manager: that is, to install software and libraries that would otherwise not be locally available. Even with an idealized, centralized distribution curating the packages, there are still two parties involved: the distribution and the *programmer* who is building applications locally on top of these libraries. In real life, however, the library ecosystem is further fragmented, composed of packages provided by a huge variety of developers. Sure, the packages may all be uploaded and indexed in one place, but that doesn't mean that any given author knows about any other given package. And then there's what the Perl world calls DarkPAN: the uncountable lines of code which probably exist, but which we have no insight into because they are locked away on proprietary servers and source code repositories. Decentralization can only be avoided when you control absolutely *all* of the lines of code in your application.. but in that case, you hardly need a package manager, do you? (By the way, my industry friends tell me this is basically mandatory for software projects beyond a certain size, like the Windows operating system or the Google Chrome browser.)

Decentralized systems are hard. Really, really hard. Unless you design your package manager accordingly, your developers *will* fall into dependency hell. Nor is there a one "right" way to solve this problem: I can identify at least three distinct approaches to the problem among the emerging generation of package managers, each of which has their benefits and downsides.

**Pinned versions.** Perhaps the most popular school of thought is that developers should aggressively pin package versions; this approach advocated by Ruby's Bundler, PHP's Composer, Python's virtualenv and pip, and generally any package manager which describes itself as inspired by the Ruby/node.js communities (e.g. Java's Gradle, Rust's Cargo). Reproduceability of builds is king: these package managers solve the decentralization problem by simply pretending the ecosystem doesn't exist once you have pinned the versions. The primary benefit of this approach is that you are always in control of the code you are running. Of course, the downside of this approach is that you are always in control of the code you are running. An all-to-common occurrence is for dependencies to be pinned, and then forgotten about, even if there are important security updates to the libraries involved. Keeping bundled dependencies up-to-date requires developer cycles--cycles that more often than not are spent on other things (like new features).

**A stable distribution.** If bundling requires every individual application developer to spend effort keeping dependencies up-to-date and testing if they keep working with their application, we might wonder if there is a way to centralize this effort. This leads to the second school of thought: to *centralize* the package repository, creating a blessed distribution of packages which are known to play well together, and which will receive bug fixes and security fixes while maintaining backwards compatibility. In programming languages, this is much less common: the two I am aware of are Anaconda for Python and Stackage for Haskell. But if we look closely, this model is *exactly the same* as the model of most operating system distributions. As a system administrator, I often recommend my users use libraries that are provided by the operating system as much as possible. They won't take backwards incompatible changes until we do a release upgrade, and at the same time you'll still get bugfixes and security updates for your code. (You won't get the new hotness, but that's essentially contradictory with stability!)

**Embracing decentralization.** Up until now, both of these approaches have thrown out decentralization, requiring a central authority, either the application developer or the distribution manager, for updates. Is this throwing out the baby with the bathwater? The primary downside of centralization is the huge amount of *work* it takes to maintain a stable distribution or keep an individual application up-to-date. Furthermore, one might not expect the entirety of the universe to be compatible with one another, but this doesn't stop subsets of packages from being useful together. An ideal decentralized ecosystem distributes the problem of identifying what subsets of packages *work* across everyone participating in the system. Which brings us to the fundamental, unanswered question of programming languages package management:

> *How can we create a decentralized package ecosystem that works?*

Here are a few things that can help:

1.  **Stronger encapsulation for dependencies.** One of the reasons why dependency hell is so insidious is the dependency of a package is often an inextricable part of its outwards facing API: thus, the choice of a dependency is not a local choice, but rather a global choice which affects the entire application. Of course, if a library uses some library internally, but this choice is entirely an implementation detail, this *shouldn't* result in any sort of global constraint. Node.js's NPM takes this choice to its logical extreme: by default, it doesn't deduplicate dependencies at all, giving each library its own copy of each of its dependencies. While I'm [a little dubious](http://stackoverflow.com/questions/25268545/why-does-npms-policy-of-duplicated-dependencies-work) about duplicating everything (it certainly occurs in the Java/Maven ecosystem), I certainly agree that keeping dependency constraints local improves *composability.*
2.  **Advancing semantic versioning.** In a decentralized system, it's especially important that library writers give *accurate* information, so that tools and users can make informed decisions. Wishful, invented version ranges and artistic version number bumps simply exacerbate an already hard problem (as I mentioned in my [previous post](http://blog.ezyang.com/2014/08/whats-a-module-system-good-for-anyway/)). If you can [enforce semantic versioning](http://bndtools.org/), or better yet, ditch semantic versions and record the true, *type-level* dependency on interfaces, our tools can make better choices. The gold standard of information in a decentralized system is, "Is package A compatible with package B", and this information is often difficult (or impossible, for dynamically typed systems) to calculate.
3.  **Centralization as a special-case.** The point of a decentralized system is that every participant can make policy choices which are appropriate for them. This includes maintaining their own central authority, or deferring to someone else's central authority: centralization is a special-case. If we suspect users are going to attempt to create their own, operating system style stable distributions, we need to give them the tools to do so... and make them easy to use!

For a long time, the source control management ecosystem was completely focused on centralized systems. Distributed version control systems such as Git fundamentally changed the landscape: although Git may be more difficult to use than Subversion for a non-technical user, the benefits of decentralization are diverse. The Git of package management doesn't exist yet: if someone tells you that package management is solved, just reimplement Bundler, I entreat you: think about decentralization as well!
