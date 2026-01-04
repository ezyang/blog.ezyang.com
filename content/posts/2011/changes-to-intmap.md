---
title: "Changes to IntMap"
date: 2011-08-01 09:00:27
slug: changes-to-intmap
categories: [Haskell]
comments:
    - id: 2875
      author: Johan Tibell
      date: "2011-08-01 16:41:35"
      content: "Great. We should sync up to make sure Data.HashMap.{Lazy,Strict} work the same way as the Data.IntMap ones."
    - id: 2877
      author: Anonymous
      date: "2011-08-02 07:58:07"
      content: |
        Would this be a good time to merge in the functionality of
        
        http://hackage.haskell.org/package/enummapset
        
        into the `containers` package?
    - id: 2878
      author: Edward Z. Yang
      date: "2011-08-02 19:30:36"
      content: "Anonymous: Usually, a proposal goes out to libraries and gets discussed. It does sound like, at the very least, these will have to get updated with the new release (well, they don't have to, but it would be useful for this to be done.) Please send mail!"
    - id: 2883
      author: Roman Cheplyaka
      date: "2011-08-03 19:24:30"
      content: "I guess you meant \"non-empty\" instead of \"non-bottom\" in the first paragraph (although one can argue that bottom is empty in a suitable ordering :)."
    - id: 2885
      author: Herbert Valerio Riedel
      date: "2011-08-04 16:00:43"
      content: "Will this require changes to the deepseq package?"
    - id: 2886
      author: Edward Z. Yang
      date: "2011-08-04 22:48:06"
      content: "Nope, no need."
    - id: 2888
      author: Edward Z. Yang
      date: "2011-08-05 00:12:21"
      content: "Roman: In fact, I mean both! :-)"
    - id: 2891
      author: Roman Cheplyaka
      date: "2011-08-05 08:57:23"
      content: "If `Data.IntMap.map (\\_ -&gt; undefined) m == undefined` holds for every (in fact, for *any*) non-bottom `m`, then it also holds for a bottom one, by monotonicity. So I don't see a need for that constraint."
    - id: 2892
      author: Edward Z. Yang
      date: "2011-08-05 09:14:12"
      content: True.
    - id: 2895
      author: Edward Z. Yang
      date: "2011-08-05 22:31:07"
      content: "Johan: I took a gander at your module. It looks like our module structure is basically the same, except you use bang patterns more liberally and rely on different modules to do a bit of your heavy lifting in terms of strict/lazy. I would appreciate an eyeball over my implementation, to make sure i didn't do any brainos."
---

As it stands, it is impossible to define certain value-strict operations on [IntMaps](http://hackage.haskell.org/packages/archive/containers/0.4.0.0/doc/html/Data-IntMap.html) with the current containers API. The reader is invited, for example, to try efficiently implementing `map :: (a -> b) -> IntMap a -> IntMap b`, in such a way that for a non-bottom and non-empty map `m`, `Data.IntMap.map (\_ -> undefined) m == undefined`.

Now, we could have just added a lot of apostrophe suffixed operations to the existing API, which would have greatly blown it up in size, but [following conversation on libraries@haskell.org](http://www.haskell.org/pipermail/libraries/2011-May/016362.html), we’ve decided we will be splitting up the module into two modules: `Data.IntMap.Strict` and `Data.IntMap.Lazy`. For backwards compatibility, `Data.IntMap` will be the lazy version of the module, and the current value-strict functions residing in this module will be deprecated.

The details of what happened are a little subtle. Here is the reader’s digest version:

- The `IntMap` in `Data.IntMap.Strict` and the `IntMap` in `Data.IntMap.Lazy` are exactly the same map; there is no runtime or type level difference between the two. The user can swap between “implementations” by importing one module or another, but we won’t prevent you from using lazy functions on strict maps. You can convert lazy maps to strict ones using `seqFoldable`.
- Similarly, if you pass a map with lazy values to a strict function, the function will do the maximally lazy operation on the map that would still result in correct operation in the strict case. Usually, this means that the lazy value probably won’t get evaluated... unless it is.
- Most type class instances remain valid for both strict and lazy maps, however, `Functor` and `Traversable` do *not* have valid “strict” versions which obey the appropriate laws, so we’ve selected the lazy implementation for them.
- The lazy and strict folds remain, because whether or not a fold is strict is independent of whether or not the data structure is value strict or spine strict.

I hacked up a first version for the strict module at Hac Phi on Sunday, you can [see it here.](http://hpaste.org/49733) The full implementation can be [found here.](https://github.com/ezyang/packages-containers)
