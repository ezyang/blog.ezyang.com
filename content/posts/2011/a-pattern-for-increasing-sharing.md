---
title: "A pattern for increasing sharing"
date: 2011-06-17 12:55:32
slug: a-pattern-for-increasing-sharing
categories: [Haskell]
comments:
    - id: 2644
      author: "Chung-chieh Shan"
      date: "2011-06-17 16:42:20"
      content: "<a href=\"http://conway.rutgers.edu/~ccshan/wiki/blog/posts/WalkZip1/\" rel=\"nofollow\">Another instance</a> (\"to preserve sharing and save memory\")"
    - id: 2646
      author: Anonymous
      date: "2011-06-17 20:22:22"
      content: "Doesn't CL already do this since ages?"
    - id: 2647
      author: Edward Z. Yang
      date: "2011-06-17 20:28:14"
      content: "Anonymous: What do you mean by “this”?"
    - id: 2650
      author: Quentin Moser
      date: "2011-06-18 05:45:07"
      content: "I think I've seen a function like this called \"update\" before, but I can't remember where. At least it does sound like a good name."
    - id: 2686
      author: gasche
      date: "2011-06-19 03:04:33"
      content: |
        This is also a widely known folklore trick in the ML communities. In OCaml there is an (ugly) (==) : 'a -&gt; 'a -&gt; bool operator that is not the usual (=) comparison but test for "physical equality" (pointer equality) instead, and allow to reason about sharing in the code. Below is an example which is not the most efficient (it keeps testing for == after the test failed once) but illustrates the use:
        
        <pre>
        let rec map f li =
          match li with
            | [] -&gt; li
            | x::xs -&gt;
              let x' = f x in
              let xs' = map f xs in
              if x == x' &amp;&amp; xs == xs' then li
              else x' :: xs'
        </pre>
        
        "==" is very bad from a semantics point of view : it forces you to reason about pointer identity, and may break encapsulation of existential types. Using Maybe here is cleaner, however the code logic (which does reason about sharing) stays the same.
        
        I have found an example using this technique in the (otherwise not-really-related) paper "Multi-Return Function Calls" by Olin Shivers and David Fisher (2004). They talk about a "parsimonious filter function" that preserves sharing on the longest suffix of elements all retained. They use the novel control-flow structure presented in their paper, but for your purposes it is equivalent to using Maybe (it's "1 + a" as structured control-flow rather than data). Maybe "parsimonious" is a good name for this idea?
    - id: 2710
      author: Chris Kuklewicz
      date: "2011-06-19 18:25:02"
      content: |
        I just uploaded a new package Transhare to hackage.  It is my abstracted solution to the sharing pattern for (a -&gt; Maybe a) transformers, inspired by your post.  I doubt the formatting will survive in this comment, but the list and tree instances are modified uses of applicative:
        
        <pre>
            instance Transhare [] where
                transR t = let tL x@((:) v vs) = fromO x $ (:)  t v  transR t vs
                                 tL x = Original x
                           in tL
        
            instance Transhare Tree where
                transR t = \ a@(Node value children) -&gt; fromO a $ Node  t value  transR (transR t) children
        </pre>
        
        
        [1] http://hackage.haskell.org/package/Transhare-0.9
    - id: 2714
      author: Twan van Laarhoven
      date: "2011-06-19 21:48:57"
      content: |
        Another thing these Maybe returns are good for is fixed points,
        
        <pre>
            fix :: (a -&gt; Maybe a) -&gt; a -&gt; a
            fix f x = case f x of
              Nothing -&gt; x
              Just x' -&gt; f x'
        </pre>
        
        This is the same semantics as you use: Nothing means that the output is the same as the input. 
        
        
        Now for some combinators:
        
        <pre>
            same :: Maybe a
            mapSame :: (a -&gt; b) -&gt; (a -&gt; Maybe a) -&gt; a -&gt; Maybe b
            mapSame2 :: (a -&gt; b -&gt; c) -&gt; (a -&gt; Maybe a) -&gt; a -&gt; (b -&gt; Maybe b) -&gt; b -&gt; Maybe c
        
            sharingMap f [] = same
            sharingMap f (x : xs) = mapSame2 (:) f x (sharingMap f) xs
        </pre>
        
        I don't really like this, there are too many parameters. An alternative is to also return the value if it is the same, since you might need it later on:
        
        <pre>
            data Change a = Same a | Change a
            
            instance Applicative Change where {
                pure = Same -- ??
                Same f  Same x = Same (f x)
                Change f  Same x = Change (f x)
                -- etc, you get the point
            }
            
            ifSame :: a -&gt; Change a -&gt; Change a
            ifSame x (Same _) = Same x
            ifSame _ change = change
            
            sharingMap f [] = Same []
            sharingMap f xxs@(x:xs) = ifSame xxs $ (:)  f x  sharingMap f xs
        </pre>
        
        The problem is that here you could lie about being the same, or you could forget to check. But the latter only matters for sharing. You can even convert between the two forms,
        
        <pre>
            ($?) :: (a -&gt; Maybe a) -&gt; (a -&gt; Change a)
            change :: Change a -&gt; Maybe a
            
            sharingMap f [] = Nothing
            sharingMap f xxs@(x:xs) = change $ (:)  f $? x  sharingMap f $? xs
        </pre>
        
        But I don't think this buys you anything compared to just sticking with Change.
    - id: 2720
      author: Chris Kuklewicz
      date: "2011-06-20 03:44:41"
      content: |
        Twan van Laarhove: your post is substantially identical to the Transhare [1] I just uploaded last night.  The names are slightly different, and I went slightly further and defined a class Transhare to lift (a -&gt; Maybe a) or (a -&gt; Change a) to containers.
        
        [1] http://hackage.haskell.org/packages/archive/Transhare/0.9/doc/html/Data-Transhare.html
---

I recently encountered the following pattern while writing some Haskell code, and was surprised to find there was not really any support for it in the standard libraries. I don’t know what it’s called (neither did Simon Peyton-Jones, when I mentioned it to him), so if someone does know, please shout out. The pattern is this: many times an endomorphic map (the map function is `a -> a`) will not make very many changes to the underlying data structure. If we implement the map straight-forwardly, we will have to reconstruct the entire spine of the recursive data structure. However, if we use instead the function `a -> Maybe a`, we can reuse old pieces of the map if there were no changes to it. (Regular readers of my blog may recognize this situation from [this post.](http://blog.ezyang.com/2011/06/pinpointing-space-leaks-in-big-programs/)) So what is such an alternative map `(a -> Maybe a) -> f a -> Maybe (f a)` called?

One guess it might be the `traverse` function in [Data.Traversable](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Traversable.html#v:traverse): it certainly has a very similar type signature: `Applicative f => (a -> f b) -> t a -> f (t b)`. However, the semantics are subtly different, as you can see from this example:

    Data.Traversable> traverse (\x -> if x == 2 then Just 3 else Nothing) [1,2,3]
    Nothing

Recall that our function only returns `Nothing` in the event of no change. Thus, we *should* have gotten the result `Just [1,3,3]`: the first and third elements of the list unchanged, and the second element of the list with its new value.

How would we implement such a function for lists? Here’s a simple implementation:

    nonSharingMap :: (a -> Maybe a) -> [a] -> Maybe [a]
    nonSharingMap f xs = let (b, r) = foldr g (False, []) (zip xs (map f xs))
                         in if b then Just r else Nothing
        where g (y, Nothing) (b, ys) = (b,     y:ys)
              g (_, Just y)  (_, ys) = (True,  y:ys)

But we can do better than this. Consider a situation where all elements of the list except the head stay the same:

![image](/img/map-sharing.png)

We would like to share the tail of the list between the old and new versions. With a little head-scratching, and the realization that `tails` shares, we can write this version:

    sharingMap :: (a -> Maybe a) -> [a] -> Maybe [a]
    sharingMap f xs = let (b, r) = foldr g (False, []) (zip3 (tails xs) xs (map f xs))
                         in if b then Just r else Nothing
        where g (_,   y, Nothing) (True, ys)  = (True,  y:ys)
              g (_,   _, Just y)  (True, ys)  = (True,  y:ys)
              g (ys', _, Nothing) (False, _)  = (False, ys')
              g (_,   _, Just y)  (False, ys) = (True,  y:ys)

Open questions: what is this pattern called? Why doesn’t it follow the usual applicative structure? Does it fulfill some higher order pattern? Also, this scheme isn’t fully compositional: if I pass you a `Nothing`, you have no access to the original version in case there was a change elsewhere in the structure: `(Bool, a)` might be a little more compositional. Does this mean this is an example of the state monad? What about sharing?

*Update.* Anders Kaseorg writes in with a much more straight-forward, directly recursive version of the function:

    sharingMap f [] = Nothing
    sharingMap f (x : xs) = case (f x, sharingMap f xs) of
      (Nothing, Nothing) -> Nothing
      (y, ys) -> Just (fromMaybe x y : fromMaybe xs ys)

I haven't checked, but one hope of expressing the function in terms of `foldr` and `zip3` is that one may be able to get it to fuse. Of course, for actual recursive spine-strict data types, you usually won't be able to fuse, and so a more straightforward presentation is more normal.
