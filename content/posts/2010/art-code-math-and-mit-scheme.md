---
title: "Art. Code. Math. (And mit-scheme)"
date: 2010-04-30 09:00:49
slug: art-code-math-and-mit-scheme
categories: [Music, Personal, Programming, Scheme]
comments:
    - id: 365
      author: Cesium
      date: "2010-04-30 09:41:36"
      content: "Well, consider this: music is bound by the imperfection of the musicians, painting demarcated by the edges of the canvas, dance constrained by the laws of physics. Your programs have none of those limitations, but they have another of their own -- to produce the desired outcome. Moreover, every form of art, fine or performing, has its own set of unstated conventions and tropes (though it may be argued that the best pieces are those that intentionally violate them). Everything has constraints; art consists of what you can build in the space between them."
    - id: 370
      author: Edward Z. Yang
      date: "2010-05-03 02:15:11"
      content: "Yes! But the constraints on music are so much more relaxed and fluid than the constraints on engineering."
    - id: 1759
      author: Luca
      date: "2011-02-07 11:56:25"
      content: |
        The --batch-option works for me (ubuntu 10.4, sheme 7.7.20) and saved my day (and the following!). 
        I really don't undestand why people insist in spitting out
        those "info" that are useful (may be) 1 time over zillions
        of executions.
        
        Thanks, Luca
    - id: 2731
      author: Anonymous
      date: "2011-06-21 13:37:10"
      content: |
        batch does not work here; $2 doesn't work for  me either.  How about:
        
        function run-scheme {
            # run-scheme LOAD EVAL
            #   LOAD - Scheme file to load
            #   EVAL - Scheme expression to evaluate
            typeset name=${1##/*/}
            shift
            mit-scheme --load ${name} --eval "($*)" &lt;/dev/null
        }
        
        Thanks for the idea.
    - id: 2732
      author: Edward Z. Yang
      date: "2011-06-21 13:50:10"
      content: "What shell, and version of mit-scheme, are you using?"
---

I was in rehearsal today, doodling away second oboe for Saint Saens' Organ Symphony for the nth time, and it occurred to me: I've listened to and played this piece of music enough times to know the full overall flow as well as a good chunk of the orchestral parts, not just mine. So when the hymnal calls give way to the triumphant entrance of the organ in the last movement, or when the tempos start shifting, simultaneously speeding up and slowing down, at the end of the piece, it's not surprising; it's almost inevitable. Couldn't have it any other way.

But we *could* have had it another way; Saint Saens could have decided that he wanted to move around the second movement or introduce another theme or any other multitude of changes. But he composed this piece, and this piece alone, and that is what has been enshrined as beauty.

And it got me thinking about the first problem on my computability problem set, which asked me to show a fundamental truth of the universe (well, within the boundaries of the math philosophers); nonnegotiable, unmoving, universal. Or the programs I write, certainly a creative process but firmly anchored to the tangible realm via requirements and specifications. How creative those mathematicians and programmers needed to be to craft elegant proofs and programs, and yet how far away from artists they yet are.

*Non sequitur.* MIT/GNU Scheme loves spewing out lots of extra banner crud when you run it, even when you don't actually want to use the interactive REPL and just run some mit-scheme code. As it turns out, the maintainer of mit-scheme made the following decision:

> In the past my (CPH) policy for a stable release was that the documentation had to be updated for the release before it went out. In practice, this has meant that there have been no stable releases in recent years. As of this release, we will no longer consider updated documentation a prerequisite for a stable release.

Uh, what?

Anyway, there's this wonderful undocumented option named `--batch-mode` which suppresses entry messages. However, in 7.7.90 (default in Ubuntu Karmic, and don't you dare try compiling it yourself; you need mit-scheme to compile mit-scheme), it doesn't suppress the "Loading..." messages, so you need to invoke load with the following hack:

    # run-scheme LOAD EVAL
    #   LOAD - Scheme file to load
    #   EVAL - Scheme expression to evaluate
    run-scheme() {
        # --batch-mode doesn't work completely on mit-scheme 7.7.90, in
        # particular it fails to suppress Loading... messages.  As a result,
        # we require this elaborate work-around.
        mit-scheme --batch-mode --eval \
            "(begin (set! load/suppress-loading-message? #t) \
                    (load \"$1\") $2)" </dev/null
    }

It's, to put it lightly, kind of disappointing.
