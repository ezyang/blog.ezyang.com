---
title: "Replacing small C programs with Haskell"
date: 2010-03-10 09:00:24
slug: replacing-small-c-programs-with-haskell
categories: [C, Haskell]
comments:
    - id: 149
      author: Daniel Lyons
      date: "2010-03-10 11:50:14"
      content: "I'm under the impression that Haskell, at least with the default GHC setup, compiles to machine code without going through C unless you use the GCC backend."
    - id: 150
      author: Edward Z. Yang
      date: "2010-03-10 11:52:48"
      content: "That's correct; GHC has a native code generator. If you pass -fvia-C it compiles \"via C\". There's also the exciting new work that uses LLVM as the intermediate compilation stage."
    - id: 151
      author: Greg
      date: "2010-03-10 12:02:14"
      content: |
        You forgot to
        
        $ cd scripts-static-cat.git/hooks
        $ mv post-update.sample post-update
        $ chmod +x post-update
        $ ./post-update
        
        so we can clone that repo! Any reason it's not on GitHub?
    - id: 152
      author: Edward Z. Yang
      date: "2010-03-10 12:08:14"
      content: "Oh, right: there's a more clone friendly URL at git://andersk.mit.edu/scripts-static-cat.git"
    - id: 154
      author: ethicszen
      date: "2010-03-10 17:06:50"
      content: "consider using strip + upx on the executable, it should reduce the size by about 75%"
    - id: 155
      author: Michael Mirold
      date: "2010-03-10 17:24:11"
      content: "Just out of interest... is the perl variant pure CGI, i.e. a new interpreter is created for every request? Or do you employ Modperl or FastCGI?"
    - id: 156
      author: Edward Z. Yang
      date: "2010-03-10 18:07:35"
      content: |
        Hi Michael; the Perl variant is pure CGI. The FastCGI version is faster than any of the CGI binaries, although once you FastCGI-ize the Haskell version it once again beats out the Perl.
        
        It's quite possible that FastCGI makes both Perl and Haskell fast enough to make the difference unimportant, but we don't quite understand under what circumstances FastCGI processes die, so having 3000 instances (one for every user on our system) is not a good idea.
    - id: 158
      author: andersk
      date: "2010-03-10 23:18:47"
      content: "ethicszen: strip doesn’t help significantly (the debugging info that gets stripped wouldn’t have been loaded at runtime anyway), and upx makes it slower by a factor of 11."
    - id: 159
      author: Michael Mirold
      date: "2010-03-11 05:09:18"
      content: "Hi Edward, usually (at least that is true for my projects) you only have a preforked pool of FastCGI worker processes that are fed their requests via IPC (from e.g. Apache). Never had any problems with that. On the other hand, you are certainly right that nothing beats the \"statelessness\" of pure CGI :-)"
    - id: 162
      author: Graham
      date: "2010-03-12 05:22:42"
      content: |
        You mention that "More generally, the class of languages (Haskell is just one of a few) that compile straight to C seem to be becoming more and more attractive replacements for tight C programs with high performance requirements."
        
        Do you have any evidence for that? I'm trying to convince some people of the benefits of such an approach.
    - id: 167
      author: Edward Z. Yang
      date: "2010-03-12 10:52:51"
      content: "Hi Graham, the reason often flaunted for writing a particular piece of code in C is that it needs to be fast: the idea behind this blog post was that it's possible to write equivalent Haskell code that gives up a statistically insignificant amount of speed.  As for other reasons, I try to address them in <a href=\"http://blog.ezyang.com/2010/01/why-haskell/\" rel=\"nofollow\">a previous blog post</a>, but in particular, C is a low level language and Haskell is a high level language, and it's a lot more pleasant to program in one of them!"
    - id: 169
      author: Graham
      date: "2010-03-12 12:17:07"
      content: "I understand. It's the \"...becoming more and more attractive...\" bit I'm most interested in. As opposed to any ol' higher level language, you mention that those which are implemented with C backends are becoming more popular. I'd like this to be true. Do you have evidence? I know Erlang goes this route, but starting a list of such languages is not the same as demonstrating their increasing usage. Nice post."
    - id: 170
      author: Edward Z. Yang
      date: "2010-03-12 12:31:59"
      content: "You're absolutely correct, and unfortunately all I can give you is anecdotal evidence from the projects I work on (this project) and hearsay from other people in the industry. Certainly the truth is many projects do not need the speed of a C backend."
    - id: 178
      author: Kevin RIggle
      date: "2010-03-13 17:38:27"
      content: "What's the advantage of compiling Haskell to native code via C over compiling directly to native code?"
    - id: 180
      author: Edward Z. Yang
      date: "2010-03-13 19:36:25"
      content: "Hey Kevin, the improvement as I understand it (I haven't done any empirical tests myself) is that the GCC and LLVM backends have a much larger accumulated store of knowledge about assembly optimizations than the GHC developers have been able to pack into the native code generator that comes with GHC."
    - id: 210
      author: Andres Salomon
      date: "2010-03-17 13:43:05"
      content: |
        Others have worked on a similar project; you may find it useful to coordinate with them.
        
        http://creativebrief.thoughtdistrict.com/wp-content/uploads/2009/07/funny-pictures-static-kitten.jpg
        
        (Sorry.)
    - id: 211
      author: Edward Z. Yang
      date: "2010-03-17 13:48:52"
      content: "You say it as if we haven't already beaten this joke to death internally. :-)"
    - id: 218
      author: Daniel Lyons
      date: "2010-03-18 03:25:40"
      content: |
        Edward,
        
        With my first remark, I'm just pointing out that when you say "the class of languages (Haskell is just one of a few) that compile straight to C", Haskell is usually compiled straight to _the machine_, skipping right past C code.
        
        This is in contrast to Erlang, which has a custom virtual machine, and does not compile either to machine code or to C code. Erlang remains competitive in the performance department, though you can see in the shootout that it suffers compared to GHC and most other compiled-to-the-machine languages, with parallelism being the generally prescribed remedy. 
        
        If we're discussing implementation language, there may be a correlation between it and speed of the implemented language, but it's easy enough to find any number of counterexamples that I wouldn't put much weight on it.
    - id: 219
      author: Edward Z. Yang
      date: "2010-03-18 03:36:53"
      content: "Hi Daniel, I now see what you're specifically referring to. I've reworded the post to be more clear."
    - id: 1949
      author: Anonymous
      date: "2011-03-22 21:19:49"
      content: "should have done it in asm.  time coding is greater  but time savings in performance of program over long term makes up for it.  but i guess asm is too difficult for you.  visual basic is good for programmers who like \"easy\" job."
    - id: 3288
      author: foljs
      date: "2012-01-02 11:40:55"
      content: |
        <b>should have done it in asm. time coding is greater but time savings in performance of program over long term makes up for it. but i guess asm is too difficult for you. visual basic is good for programmers who like “easy” job.</b>
        
        Who let the 15 year olds in?
    - id: 3289
      author: Tim Daly
      date: "2012-01-02 11:46:47"
      content: |
        You've written a frankenscript that will exist nowhere else.
        Somebody, someday will have to maintain it.
        Fortunately, you wrote it in Haskell which has literate programming support.
        You DID write it as a literate program, right? You wrote down WHY you 
        had to write it as a separate program, right? Good lad. You get a gold star.
    - id: 3290
      author: Edward Z. Yang
      date: "2012-01-02 11:57:06"
      content: "Tim: The relevant documentation you're referring to is http://scripts.mit.edu/wiki/Technical_overview_of_scripts.mit.edu"
---

C is the classic go-to tool for small programs that need to be really fast. When [scripts.mit.edu](http://scripts.mit.edu/) needed a small program to be [a glorified cat](http://scripts.mit.edu/trac/browser/trunk/server/common/oursrc/execsys/static-cat.c.pre) that also added useful HTTP headers to the beginning of its output, there was no question about it: it would be written in C, and it would be fast; the speed of our static content serving depended on it! (The grotty technical details: our webserver is based off of a networked filesystem, and we wanted to avoid giving Apache too many credentials in case it got compromised. Thus, we patched our kernel to enforce an extra stipulation that you must be running as some user id in order to read those files off the filesystem. Apache runs as it's own user, so we need another *small* program to act as the go-between.)

It's also a frankenscript, a program that grew out of the very specific needs of our project that you will not find anywhere else in the world. As such, it's critically important that the program is concise and well-defined; both properties that are quite hard to get in C code. And it only gets worse when you want to add features. There were a number of small features (last modified by headers, byte ranges) as well as a number of large features (FastCGI support). None of the development team was relishing the thought of doubling the size of the C file to add all of these enhancements, and rewriting the program in a scripting language would cause a performance hit. Benchmarks of replacing the script with a Perl CGI made the script ten times slower (this translates into four times slower when doing an end-to-end Apache test).

But there is another way! Anders writes:

> So I had this realization: replacing it with a compiled Haskell CGI script would probably let us keep the same performance. Plus it would be easy to port to FastCGI since Haskell’s FastCGI library has the same interface.

And a few weeks later, voila: [static-cat in Haskell](http://andersk.mit.edu/gitweb/scripts-static-cat.git). We then saw the following benchmarks:

    $ ab -n 100 http://andersk.scripts.mit.edu/static-cat.cgi/hello/hello.html
    Requests per second:    15.68 [#/sec] (mean)
    $ ab -n 100 http://andersk.scripts.mit.edu/static-cat.perl.cgi/hello/hello.html
    Requests per second:    7.50 [#/sec] (mean)
    $ ab -n 100 http://andersk.scripts.mit.edu/static-cat.c.cgi/hello/hello.html
    Requests per second:    16.59 [#/sec] (mean)

Microbenchmarking reveals a 4ms difference without Apache, which Anders suspects is due to the size of the Haskell executable. There is certainly some performance snooping to be done, but the Haskell version is more than twice as fast as the Perl version on the end-to-end test.

More generally, the class of languages (Haskell is just one of a few) that compile into native code seem to be becoming more and more attractive replacements for tight C programs with high performance requirements. This is quite exciting, although it hinges on whether or not you can convince your development team that introducing Haskell to the mix of languages you use is a good idea. More on this in another blog post.
