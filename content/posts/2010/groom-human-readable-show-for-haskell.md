---
title: "Groom: human readable Show for Haskell"
date: 2010-07-07 09:00:34
slug: groom-human-readable-show-for-haskell
categories: [Haskell]
comments:
    - id: 641
      author: Juho Vepsäläinen
      date: "2010-07-07 09:16:22"
      content: |
        Cool idea.
        
        Could you please fix the image links (they should point to http://blog.ezyang.com/img/groom/messy.png and http://blog.ezyang.com/img/groom/clean.png)?
    - id: 642
      author: Neil Mitchell
      date: "2010-07-07 09:56:23"
      content: All your images seem to 404 for me...
    - id: 643
      author: zygoloid
      date: "2010-07-07 09:59:44"
      content: "Your images point to http://blog.ezyang.com/2010/07/groom-human-readable-show-for-haskell/img/groom/messy.png but should point to http://blog.ezyang.com/img/groom/messy.png. For the benefit of other readers:"
    - id: 644
      author: Anonymous
      date: "2010-07-07 10:00:07"
      content: |
        they are relative urls, lacking a "/"
        
        http://blog.ezyang.com/img/groom/messy.png
        http://blog.ezyang.com/img/groom/clean.png
    - id: 645
      author: Vincent
      date: "2010-07-07 10:07:05"
      content: |
        I see the images from the main page
         (urls like: http://blog.ezyang.com/img/groom/messy.png)
        
        but not from the article page
         (urls like: http://blog.ezyang.com/2010/07/groom-human-readable-show-for-haskell/img/groom/messy.png)
    - id: 646
      author: Gleb
      date: "2010-07-07 10:09:42"
      content: "<a href=\"http://hackage.haskell.org/package/ipprint\" rel=\"nofollow\">ipprint</a> package does pretty much the same thing."
    - id: 647
      author: Edward Amsden
      date: "2010-07-07 10:16:26"
      content: They are broken here as well.
    - id: 648
      author: Kartik Agaram
      date: "2010-07-07 10:18:28"
      content: "Needs a leading '/'."
    - id: 649
      author: Edward Z. Yang
      date: "2010-07-07 13:03:06"
      content: "Oops! Fixed now."
    - id: 650
      author: Edward Z. Yang
      date: "2010-07-07 13:09:15"
      content: "Gleb, it looks like someone’s beat me to the punch. It looks like ipprint also does a bunch of extra magic like check what your terminal size is and format the code accordingly; that’s pretty cool. :-)"
    - id: 651
      author: hydo
      date: "2010-07-07 13:45:59"
      content: "Oh, that's just AWESOME.  Thank you!"
---

Tapping away at a complex datastructure, I find myself facing a veritable wall of Babel.

![image](/img/groom/messy.png)

“Zounds!” I exclaim, “The GHC gods have cursed me once again with a derived Show instance with no whitespace!” I mutter discontently to myself, and begin pairing up parentheses and brackets, scanning the sheet of text for some discernible feature that may tell me of the data I am looking for.

But then, a thought comes to me: “Show is specified to be a valid Haskell expression without whitespace. What if I parsed it and then pretty-printed the resulting AST?”

Four lines of code later (with the help of `Language.Haskell`)...

![image](/img/groom/clean.png)

[Ah, much better!](http://hackage.haskell.org/package/groom)

*How to use it.* In your shell:

    cabal install groom

and in your program:

    import Text.Groom
    main = putStrLn . groom $ yourDataStructureHere

*Update.* Gleb writes in to mention [ipprint](http://hackage.haskell.org/package/ipprint) which does essentially the same thing but also has a function for `putStrLn . show` and has some tweaked defaults including knowledge of your terminal size.

*Update 2.* Don mentions to me the [pretty-show](http://hackage.haskell.org/package/pretty-show) package by Iavor S. Diatchki which also does similar functionality, and comes with an executable that lets you prettify output offline!
