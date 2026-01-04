---
title: "The convergence of compilers, build systems and package managers"
date: 2015-12-07 01:50:28
slug: the-convergence-of-compilers-build-systems-and-package-managers
categories: [Compilers]
comments:
    - id: 19596
      author: Dan Aloni
      date: "2015-12-07 04:19:04"
      content: |
        I think that in order to main the abstraction between build steps and build system, the dependency problem better be solved by using the OS to sandbox the compilation tools. This way the dependency is only specified once. It's a rather novel method, not taken but the myriad of build tools out there, but only by selected few. See an example for a build tool that implements such method:
        
        https://github.com/ElastiLotem/buildsome
    - id: 19597
      author: Marc Coiffier
      date: "2015-12-07 06:13:22"
      content: |
        Hi, 
        
        I'm currently writing a compiler for Curly (https://marc.coiffier.net/projects/curly.html), a language of my invention, and this tools falls within the three categories you describe : it can build one or several binaries in one go, automatically download all their dependencies, as they are needed, from distant repositories, and also allows builds to be specified by Makefile-like configuration files. It even has its own native protocol for serving libraries if you want to share yours and have no knowledge of how to setup a server, thus acting as full-fledged package manager.
        
        I realized, like the creators of Go and Rust, that the only reason compilers didn't solve all their problems is that no one designed them to. Writing a HTTP request code isn't that hard when you've designed and implemented a whole language, so compilers should have been able to retrieve their own libraries for a long time now, but we've let package managers and build systems do that for them.
        
        You can even avoid dependency hell (!) if you let the compiler solve its own dependencies, since it is the only tool that knows how they are used.
        
        Plus, all these features don't take much effort or much room to add. As an example, Curly is ~3000 lines of code long, while still performing all the duties of a compiler, package manager and build system.
        
        In conclusion, I am hugely in favor of monolithic build tools, since there are precious few reasons why we separate builds into three stages in the first place.
    - id: 19599
      author: gasche
      date: "2015-12-07 09:49:50"
      content: |
        This approach may not be going far enough; computing dependencies for a program can be difficult, and in particular it may be impossible to do in one pass: you may need to compile (or at least type-check) some parts of the module to have enough information to resolve dependencies in later parts of the module.
        
        One solution to this is to give full control to the compiler over the build process. Invert the control interface that you propose, and instead call the compiler with a higher-order builder argument to which the compiler can ask: "I know need dependency [foo], please build it and return the module interface so I can keep working". Another approach would be for the compiler to return some sort of continuation (this is all easier when thinking of everything inside a single runtime, à la Oleg's "Delimited continuations in operating systems", instead of the weird mix of systems that a command shell provides) of the build, that contains enough information about its partial compilation state to be re-invoked further without redoing a prefix of the compilation work (re-doing could be quadratic for modules with an odd dependency structure).
    - id: 19601
      author: dcreager
      date: "2015-12-07 10:52:19"
      content: "A “top-down” build system (like <a href=\"https://github.com/apenwarr/redo\" rel=\"nofollow\">redo</a>) would also work well here.  Instead of assuming that you need to know the full dependency tree before you can start building, you just start building whatever top-level target you want.  The first time you perform a build, you know that you need to compile all of the source files that belong to the target, and the act of performing that first build tells you any other dependencies that were used.  The next time you perform a build, if any of those have changed, you know the target is out of date and needs to be rebuilt."
    - id: 19610
      author: phraktle
      date: "2015-12-07 13:55:29"
      content: |
        An integrated tool should also provide additional infrastructure, that I would argue these days comprise the foundations of any usable language toolchain, such as IDE operational support (e.g. error reporting, code formatters, autocomplete, refactoring, debugging, etc). It would be very valuable for a new language to standardize some of these facilities as part of its core toolchain. Go has already done so with formatting. Easy integration with anyone's favorite editor should help a lot with adoption. Scripted refactorings could make library version updates easy (just bundle a set of refactoring operations that anyone can execute on their codebase). Supporting live REPLs with hot code swapping is another useful aspect.
        
        Arguably, language designers these days should consider these part of the features of a toolchain. With this scope in mind, you certainly end up with different design, not a "single file compiler".
    - id: 19611
      author: "Bookmarks for December 6th through December 7th | Chris&#039;s Digital Detritus"
      date: "2015-12-07 14:00:34"
      content: "[&#8230;] The convergence of compilers, build systems and package managers : Inside 736-131 &#8211; [&#8230;]"
    - id: 19612
      author: Edward Z. Yang
      date: "2015-12-07 14:08:47"
      content: |
        Dan: I learned about buildsome relatively recently, and I think it is a very cool approach. For those of you who are not familiar, buildsome hooks into the file system API (using LD_PRELOAD) so that it can block file calls to dependencies which have not been built yet.
        
        I would say there a few downsides with this approach:
        
        1. buildsome only will tell you what the true dependencies of a command you run is: you still have to teach buildsome what command to run in the first place. A tool like Cabal comes with a lot of smarts to figure out what flags to pass to the compiler in the first place, which cannot be that easily extracted.
        2. Many people would say that hooking into the file system API with LD_PRELOAD is bad design. I'm not sure I buy into this argument, but I do think that this would be a negative for some, from a complexity perspective.
        3. You have to deal with compilers probing multiple locations/listing directories. Not impossible, but it can greatly reduce precision. (Hermetic builds alleviate this somewhat, but this can't be enforced by a build system like buildsome! Bazel can be hermetic because you had to pre-declare the dependencies.)
        
        dcreager, redo is very similar philosophically to buildsome, but redo works around the lack of actually intercepting the file system in two ways: (1) you still have to explicitly call redo-ifchange, although you can assume that the compiler has output a list of what dependencies it looked at, and (2) it works around the lack of continuations by simply rerunning the do script if you discover that a rebuild is necessary at some late point in the script. Another build system which is philosophically similar to redo is Shake: http://shakebuild.com/
        
        gasche, one way to look at the buildsome is that it is implementing a limited form of continuation passing, where the call back to the build system is the compiler says, "I need dependency foo" (we only need one-shot continuations, in which case a continuation system is basically just multithreading with blocking communication.) So in this way, a system like buildsome is very close to what you might actually want.
        
        But, when I have conversations with people who build build systems, they would say a language which behaves this way is being actively hostile to useful builds. Should we encourage such bad behavior? Maybe we should!
    - id: 19613
      author: Edward Z. Yang
      date: "2015-12-07 14:12:47"
      content: "phraktle: There is a very real downside with designing your compiler as a closed ecosystem. Live REPLs, hot code swapping, this is not new stuff: Smalltalk systems had this stuff in the 70s. But put a modern programmer in front of Squeak Smalltalk, and they will be very confused. Where are my files? What are these images? You mean I can't use my favorite editor? How do I run a Smalltalk script?"
    - id: 19614
      author: phraktle
      date: "2015-12-07 15:06:34"
      content: |
        I meant that these concerns should be provided as a core API / library that any IDE can reuse. For a given language, there are many benefits of having a canonical compiler, canonical code formatting, canonical AST representation that can be inspected/manipulated by various tools, and even higher level concepts (not just AST, but a semantic graph that can be manipulated). So my point is precisely that you should be able to use your favorite editor, but without feeling like a second class citizen or without being able to migrate to something else 5-10 years down the road. The current approach is that every editor/IDE reimplements code parsing/manipulation. 
        
        Everyone's favorite editor stood the test of time surprisingly well, so those will not go away easily (see https://en.wikipedia.org/wiki/Lindy_effect). Thus a good language toolchain should make it easy to use any such editor/IDE and make them full-featured.
        
        Squeak/SmallTalk tried to contain these all, which didn't work out that well in the long run (as opposed to environments that kept source as text files). Same goes for many other attempts at such a level of integration (remember https://en.wikipedia.org/wiki/Intentional_programming ? see also https://www.youtube.com/watch?v=tSnnfUj1XCQ :)
        
        Great concepts to have as integrated, but goes against the Lindy effect: source code stored as text is not going away. Emacs/vi is not going away. However, the need for much higher level of programmer productivity (e.g. comfort, easier manipulation) is increasing. So a good language toolchain should try and bridge even more between the dumb text files and the comfortable editors.
    - id: 19615
      author: gasche
      date: "2015-12-07 17:04:32"
      content: |
        I'm a bit surprised by the idea that stracing compilers to intercept module loading is a "novel" idea. We've known about it inside the OCaml community for years, and Alain Frisch (one of the OCaml compiler maintainers) did experiments to build mixed OCaml/C programs with it ( it was recently mentioned again in this discussion: http://lists.ocaml.org/pipermail/platform/2013-March/000374.html ). The reason we haven't really pushed for that is that this is very OS-specific -- I have no idea how syscall interception works on Windows for example.
        
        I agree it is similar to a form of delimited continuations as I described, but I think it falls short in term of robustness. Giving control to the compiler could be the easiest way out -- knowing that, under systems with a form of continuation capture (binary checkpointing), the command called by the compiler can always do the capture and return to the prompt.
        
        Another advantage for giving more control to the compiler is that it then has the ability to share resources across builds (if it calls the build on the new targets without starting a new process), for example compile interface files already loaded in memory (for a common library that all modules in my project use) don't need to be loaded again. This could again be done at the OS level (I guess with the right mmap trick or whatever), but more fragile. The OCaml does some alpha-renaming/refreshing of type variables when it loads polymorphic types from a file, and this would need to be re-engineered.
    - id: 19616
      author: Edward Z. Yang
      date: "2015-12-07 17:21:12"
      content: |
        gasche: Oh, I certainly am not claiming that it is novel, but I don't think it is widely considered a "reasonable way" to do things.  I had a brief Twitter discussion with the buildsome developers; they claim that these tricks can also be done on Windows, but they simply haven't had a need to implement.  A more practical problem is that you often need administrative privileges to exercise these facilities (this is certainly true on OS X).
        
        I don't really know how to implement cross-process continuations, except using operating system facilities. As you mentioned earlier, same-process is a lot easier to manage, but there is a lot of pressure for separate process, because it is more parallelizable, it is more isolated, and it handles the multiple language case better.
    - id: 19619
      author: Edward Z. Yang
      date: "2015-12-07 20:01:26"
      content: "I had a discussion with one of my labmates about this, and his perspective was, \"Fine, they should be monolithic! I want my (e.g.) Rust compiler to know everything about how to compile Rust.\" He found the stated examples of why one would need a compiler/build-system separation fairly unconvincing; there does not seem to be any pressing need to actually parallelize over the module level; if the package manager parallelizes over packages, and the build system parallelizes/incrementalizes compilation of packages, well, that ought to be good enough for anyone."
    - id: 19621
      author: Tristan Hume
      date: "2015-12-07 21:54:17"
      content: |
        [x-post from HN comments]
        
        This is a good analysis. A potential option for your proposed build output format is http://nixos.org/nix/ expressions.
        
        Nix is already kind of a fusion of a package manager and a build system. It's rather mature, has a decent ecosystem and does a lot of what he is looking for:
        
        - Complete determinism, handling of multiple versions, total dependency graphs
        - Parallel builds (using that dependency graph)
        - Distribution
        
        One major benefit of a solution like generating Nix files over compiler integration is that it works for cross-language dependencies. A lot of the time integrated solutions break down is for things like if a C extension of a Ruby gem relies on the presence of imagemagick. Nix has no problem handling that kind of dependency.
        
        Also of course it is a lot less work to generate Nix expressions than it is to write a package manager. There are already scripts like https://github.com/NixOS/cabal2nix which already solve problems of the packaging system they replace.
    - id: 19623
      author: Juan Benet
      date: "2015-12-07 22:37:17"
      content: |
        Hey-- great post. Agree with so much here. Agree also with the nix approaches.
        
        The IPFS Team -- https://ipfs.io -- has been thinking about much of this in our design of IPFS and our toolchain. We seek to improve computing in general by providing a transport not just for streams, but for data structures. A data fabric that simplifies distributed computing. (We are very aligned with nix, blaze, etc). We're improving package management at the moment, but we'll be working down towards code.
        
        In my personal view, intelligent code manipulation will "soon" extend all the way to the code itself-- not just source files, but the ASTs themselves-- imagine {compiling, building, and package managing} _ASTs directly_, not text streams.
        
        And-- on the monolithic vs not, define protocols, interfaces, and modules, that provide the necessary information. Well designed interfaces + modules can leverage the best of monolithic things without encumbering as much.
    - id: 19627
      author: Anonymous
      date: "2015-12-08 00:25:16"
      content: |
        Curious about what you think of conda: https://github.com/conda/conda  http://conda.pydata.org/docs/index.html .
        
        It is both package manager and build system for arbitrary binaries with support on OSX, Linux, and Windows.  Conda currently ships llvm and mingw there is are no technical challenges to shipper other compilers
    - id: 19628
      author: Jonathan
      date: "2015-12-08 01:40:24"
      content: "I think that most language designers seriously underestimate how hard it is to do packaging properly. Most language ecosystem packaging systems have some serious shortcomings. My advice for any would-be packaging system inventor is: at least consider how hard the job will be for a distribution packager to take your modules and package them up for their distribution. Perl got this right (possibly by accident); Ruby got it wrong."
    - id: 19629
      author: Edward Z. Yang
      date: "2015-12-08 02:24:10"
      content: "Hi Jonathan!  In what sense do think Perl got this right? (Are you referring to CPAN?)"
    - id: 19636
      author: Flavio
      date: "2015-12-08 07:57:23"
      content: |
        For Haskell specifically, suppose your project uses libraries X and Y, which respectively depend transitively on versions 1.0 and 2.0 from another library Z, then the compiler in theory is able to track down all uses of DataType X and figure out if it is crossing boundaries from where version 1.0 uses it to where version 2.0 uses it (in most real programs it doesn't). 
        
        That would allow us to cut down a lot of build failures where the transitive use of different library versions would not cause bugs/problems...
        
        GHC would also need to annotate the exported names of functions, datatypes, typeclasses with some versioning or hashing scheme...
        
        Not directly related to the post, but it also goes in the direction that the compiler needs to output more information for the build systems, and sometimes even for themselves...
    - id: 19637
      author: Jussi
      date: "2015-12-08 09:25:10"
      content: |
        The Meson build system (http://mesonbuild.com) is a pretty close approximation to what you propose with the exception that it does not have a compiler. It compiles C/C++/others and has a cross platform package manager. It also has an "IDE integration" mode where you can extract a lot of the build information in JSON with a helper tool.
        
        The build definitions of Meson are designed to be composable so you can take any Meson project and use it as a subproject of any other project. This is very different to other build systems that mostly define the build system as a monolithic part of a single project.
        
        This is the most practical approach for package management. A build system can not subsume the compiler as real world requirements dictate that you must be able to support  MSVC, Clang, Gcc and possibly others.
    - id: 19642
      author: "B.S. is for build system | The Complexity Elephant"
      date: "2015-12-08 13:32:43"
      content: "[&#8230;] I have recently stumbled upon this insightfull (TM) article: The Convergence Of Build Systems And Package Managers [&#8230;]"
    - id: 19643
      author: rssh
      date: "2015-12-08 13:55:22"
      content: "One problem --  exists multi-language systems.  Maybe a right approach to modularization is to specify compiler as API provider, which can be called by IDE-s and build systems, and return information about code navigation or dependencies in some universal format."
    - id: 20089
      author: "fogus: The best things and stuff of 2015"
      date: "2015-12-29 11:44:17"
      content: "[&#8230;] The convergence of compilers, build systems and package managers &#8211; Edward Z. Yang explores the idea of holistic approaches to solving module systems by integrating them into languages themselves or by designing better abstraction around them. [&#8230;]"
    - id: 20537
      author: Anonymous
      date: "2016-03-06 14:45:13"
      content: |
        I think the solution is many libraries, but monolithic executable(s). For example, I'd like as much IO as possible to be ripped out of the GHC library--certainly front-end IO. Similarly, the Cabal library could similar do constraint solving etc on the parsed representation of cabal files. This opens up:
        
         - Cabal-install would exclusively deal with the filesystem, and use Shake to do so; Setup.hs would be replaced with an ability to provide a plugin with additional/overriding Shake rules.
        
         - Stuff like https://github.com/haskell/haskell-ide-engine can be less duct-tape-y. It could just use the "pure" GHC Cabal libraries, in order to support non-file based editing--a cool idea in general, and a simply-practical one for many browser-based IDEs.
        
        The biggest question is can the backend---LLVM in particular---be taught to not touch the filesystem unless told to?
    - id: 21680
      author: "Propelling developer experience through configuration - Webdesign Journal"
      date: "2016-12-16 14:55:55"
      content: "[&#8230;] Far too many package managers. [&#8230;]"
---

Abstract. *The traditional abstraction barriers between compiler, build system and package manager are increasingly ill-suited for IDEs, parallel build systems, and modern source code organization. Recent compilers like go and rustc are equipped with a fully-fledged build systems; semantic build systems like Bazel and Gradle also expect to manage the packaging of software. Does this mean we should jettison these abstraction barriers? It seems worthwhile to look for new interfaces which can accommodate these use-cases.*

Traditionally, one can understand the tooling of a programming language in three parts:

- The **compiler** takes a single source file and transforms it into an object file. (Examples: `ghc -c`, `go tool 6g`, `javac` and `gcc -c`.)
- The **build system** takes a collection of source files (and metadata) and transforms them into the final build product. It does this by invoking the compiler multiple times. (Examples: `go build`, `Setup build`, `make`, `ant compile`.) Often, the build system also knows how to *install* the build product in question.
- The **package manager** takes a package name, and retrieves and builds the package and its dependencies, and installs them into some store. It does this by invoking the build systems of each package. (Examples: `cabal install`, `cargo install`, `maven package`.)

This separation constitutes an abstraction barrier which allows these components to be separately provided. For example, a single build system can work with multiple different compilers (gcc versus clang); conversely, a compiler may be invoked from a user's custom build system. A library may be packaged for both its native language package manager as well as a Linux distribution's packaging system; conversely, a package manager may be indifferent to how a library actually gets built. In today's software ecosystem, these abstraction barriers are used heavily, with good effect!

However, there are an increasing number of use-cases which cannot be adequately handled using these abstraction barriers:

- A build system needs to know what order to build source files in; however, the canonical source for this information is inside the import/include declarations of the source file. This information must either be duplicated inside the build system, or the build system must call the compiler in order to compute the dependency graph to be used. In any case, a compiler cannot *just* be a dumb source-to-object-file converter: it must know how to emit dependencies of files (e.g., `gcc -M`). There is no standardized format for this information, except perhaps a `Makefile` stub.
- The dependency problem is further exacerbated when module dependencies can be cyclic. A build system must know how to resolve cycles, either by compiling strongly connected components of modules at a time, or compiling against "interface" files, which permit separate compilation. This was one of the problems which [motivated the Rust developers](https://github.com/rust-lang/rfcs/pull/1317#issuecomment-161729336) to not expose a one-source-at-a-time compiler.
- The best parallelization can be achieved with a fine-grained dependency graph over source files. However, the most desirable place to implement parallelization is the package manager, as an invocation of the package manager will cause the most code to be compiled. Thus, a system like Bazel unifies both the build system and the package manager, so that parallelism can be achieved over the entire build. (Another example is GHC's build system, which parallelizes compilation of all wired-in packages on a per-module basis.)
- IDEs want in-depth information from the compiler beyond a `-c` style interface. But they cannot invoke the compiler directly, because the only way to invoke the compiler with the right flags and the right environment is via the build system / the package manager. Go's built in build-system means that it can more easily provide a tool like `go oracle`; otherwise, `go oracle` would need to be able to accommodate external build systems.
- Certain language features are actively hostile to build systems; only the compiler has enough smarts to figure out how to manage the build. Good examples include macros (especially macros that can access the filesystem), other forms of compile-time metaprogramming, and compiler plugins.

Thus, the temptation is to roll up these components into a single monolithic tool that does everything. There are many benefits: a single tool is easier to develop, gives a more uniform user experience, and doesn't require the developers to specify a well-defined API between the different components. The downside? You can't swap out pieces of a monolithic system.

I think it is well worth considering how we can preserve this separation of concerns, even in the face of these features. Unfortunately, I don't know what the correct API is, but here is a strawman proposal: every compiler and build system writer should have an alternative mode which lets a user ask the query, "How do I make `$output` file?" This mode returns (1) the dependencies of that file, and (2) a recipe for how to make it. The idea is to place the dependency-finding logic in the compiler (the canonical place to put it), while letting an external tool actually handle building the dependencies. But there are a lot of details this proposal doesn't cover.

What do you think about the convergence of compiler, build system and package manager? Do you think they *should* be monolithic? If not, what do you think the right API to support these new use cases should be? I'd love to know what you think.
