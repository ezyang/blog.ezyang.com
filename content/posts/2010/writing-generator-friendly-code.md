---
title: "Writing generator friendly code"
date: 2010-03-01 12:00:34
slug: writing-generator-friendly-code
categories: [Python]
comments:
    - id: 125
      author: Michael Foord
      date: "2010-03-02 14:19:14"
      content: |
        Forking an infinite generator is hard? Doesn't itertools.tee do that for you?
        
        http://docs.python.org/library/itertools.html#itertools.tee
    - id: 126
      author: Edward Z. Yang
      date: "2010-03-02 14:25:40"
      content: "It's not as effortless as in a lazy language, where you don't need to tee the lazy list at all. The generator is mutating state under-the-hood, which means you have to be very careful if you're passing them around."
    - id: 127
      author: Jason C
      date: "2010-03-02 17:11:08"
      content: |
        You might find this question on Stack Overflow interesting:
        http://stackoverflow.com/questions/2322642/index-and-slice-a-generator-in-python
    - id: 130
      author: Anonymous
      date: "2010-03-04 00:23:41"
      content: "The problem with passing generators to functions is that even if they don't call len or need arbitrary access, they may want to walk through the list twice. That works fine for generator objects like xrange, but if not if you are using a generator function or generator expression."
---

I've come a long ways from [complaining to the html5lib list that the Python version gratuitously used generators, making it hard to port to PHP](http://www.mail-archive.com/html5lib-discuss@googlegroups.com/msg00241.html). Having now drunk the laziness kool-aid in Haskell, I enjoy trying to make my code fit the generator idiom. While Python generators have notable downsides compared to infinite lazy lists (for example, forking them for multiple use is nontrivial), they're pretty nice.

Unfortunately, the majority of code I see that expects to see lists isn't robust enough to accept generators too, and it breaks my heart when I have to say `list(generator)`. I'll forgive you if you're expecting O(1) accesses of arbitrary indexes in your internal code, but all too often I see code that only needs sequential access, only to botch it all up by calling `len()`. Duck typing won't save you there.

The trick for making code generator friendly is simple: **use the iteration interface.** Don't mutate the list. Don't ask for arbitrary items. Don't ask for the length. This also is a hint that `for range(0, len(l))` is *absolutely* the wrong way to traverse a list; if you need indices, use `enumerate`.

**Update (September 1, 2012).** Hilariously enough, PHP has [finally gotten generators.](https://wiki.php.net/rfc/generators#vote)
