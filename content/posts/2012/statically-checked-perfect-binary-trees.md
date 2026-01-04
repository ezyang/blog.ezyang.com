---
title: "Two ways of representing perfect binary trees"
date: 2012-08-04 11:04:33
slug: statically-checked-perfect-binary-trees
categories: [Haskell]
math: true
comments:
    - id: 3907
      author: Mikhail Glushenkov
      date: "2012-08-04 15:37:33"
      content: "Very interesting, thanks!"
    - id: 3909
      author: "Conal Elliott (@conal)"
      date: "2012-08-04 20:00:23"
      content: "I call these two styles \"right-folded\" and \"left-folded\" or \"top-down\" and \"bottom-up\" trees in http://conal.net/blog/posts/from-tries-to-trees . If you use the pair functor in both cases, the relationship becomes much clearer and the Functor, Applicative, and Foldable instance bodies become textually identical to each other. Moreover, the left-folded/bottom-up version leads to Guy Blelloch's work-efficient parallel scan algorithm, while the right-folded/top-down instance leads to a work-inefficient algorithm, as shown in http://conal.net/blog/posts/parallel-tree-scanning-by-composition . I've been exploring the hypothesis that nested/non-regular data types--while historically more exotic than their regular counterparts--are usually better suited to natural, efficient data-parallel algorithms."
    - id: 3912
      author: Jeremy Gibbons
      date: "2012-08-05 08:29:51"
      content: "Nested datatypes are appealing, but the appeal fades when you look closer at them. The obvious folds over nested datatypes are constrained by the types to be polymorphic functions, so sum isn't a fold in this sense; so Richard Bird and Ross Paterson came up with the \"generalised folds\" that you cite, which do encompass monomorphic functions such as sum. However, these generalised folds no longer always take linear time, because of repeated maps; this led Ralf Hinze to come up with \"efficient generalized folds\" (at WGP 2000; see http://www.cs.ox.ac.uk/ralf.hinze/publications/#P10), which are linear-time. But in order to do this, Ralf had to sacrifice the universal property that makes folds so convenient to reason with; that led Clare Martin, Ian Bayley and me to \"disciplined, efficient, generalized folds\" (in FAC, 2004; see http://www.cs.ox.ac.uk/publications/publication2373-abstract.html), simultaneously enjoying useful types, efficient evaluation, and effective laws. This works, but you end up knee-deep in functor categories when trying to explain it - perhaps someone can come up with \"simple, disciplined, efficient, generalized folds for nested datatypes\"?"
    - id: 21389
      author: JT
      date: "2016-10-07 16:31:05"
      content: |
        Very interesting post, thanks for sharing this!
        I've been playing around with the tree a bit.
        A function to select (get? access?) an element
        is easy to write:
        
        <pre>
            select :: Int -&gt; B a -&gt; a;
            select n (One x) = x;
            select n (Two x) =
                if
                    even n
                then
                    fst (select (div n 2) x)
                else
                    snd (select (div n 2) x);
        </pre>
        
        However I'm stuck with writing the corresponding update-function,
        which would return a copy of the tree with one element modified.
        
        <pre>
            update :: a -&gt; Int -&gt; B a -&gt; B a;
            update = ?
        </pre>
        
        Any ideas? I'm very curious about this :)
    - id: 21391
      author: Edward Z. Yang
      date: "2016-10-08 03:35:25"
      content: |
        Just like how catamorphism was implemented, you need to pass in a continuation which will do the actual update. Here's some working code:
        
        <pre>
        update :: a -> Int -> B a -> B a;
        update y i t = go (\_ -> y) i t
          where
            go :: (a -> a) -> Int -> B a -> B a
            go k i (One x) = One (k x)
            go k i (Two x) =
                Two $ if even i
                        then go (\(l,r) -> (k l,r)) (div i 2) x
                        else go (\(l,r) -> (l,k r)) (div i 2) x
        </pre>
    - id: 24406
      author: Edward Kmett
      date: "2020-09-17 00:19:38"
      content: |
        I realize this is now multiple years later, but to play devil's advocate:
        
        There is one advantage to the first GADT-based encoding, however, in addition to being simpler for a typical Haskeller to grok.
        
        Let's consider a slight variant where we have nodes inside the tree as well as at the leaves.
        
        data L i a where
            L :: a -&gt; L Z a
            N :: a -&gt; L i a -&gt; L i a -&gt; L (S i) a
        
        Now, after stepping into just one data constructor I can get at an 'a'. On the other hand, in the nested data type case, I have to get through all my twos to get down to the data. One is linear in the depth of my target in the tree, the other is linear in the height of the tree.
        
        Using L i as a component in, say, a skew binary random access list is also much more useful than the alternative, because drop can be implemented in logarithmic time, while in the nested data case, you have to transform the sub-trees blowing things out to linear, and of course, more damningly, because cons goes from constant to linear in the nested case as well.
---

A common simplification when discussing many divide and conquer algorithms is the assumption that the input list has a size which is a power of two. As such, one might wonder: *how do we encode lists that have power of two sizes*, in a way that lists that don’t have this property are unrepresentable? One observation is that such lists are *perfect binary trees*, so if we have an encoding for perfect binary trees, we also have an encoding for power of two lists. Here are two well-known ways to do such an encoding in Haskell: one using GADTs and one using nested data-types. We claim that the nested data-types solution is superior.

This post is literate, but you will need some type system features:

    {-# LANGUAGE ScopedTypeVariables, GADTs, ImpredicativeTypes #-}

# GADTs

One approach is to encode the size of the tree into the type, and then assert that the sizes of two trees are the same. This is pretty easy to do with GADTs:

    data Z
    data S n

    data L i a where
        L :: a -> L Z a
        N :: L i a -> L i a -> L (S i) a

By reusing the type variable `i`, the constructor of `N` ensures that we any two trees we combine must have the same size. These trees can be destructed like normal binary trees:

    exampleL = N (N (L 1) (L 2)) (N (L 3) (L 4))

    toListL :: L i a -> [a] -- type signature is necessary!
    toListL (L x) = [x]
    toListL (N l r) = toListL l ++ toListL r

Creating these trees from ordinary lists is a little delicate, since the `i` type variable needs to be handled with care. Existentials over lists work fairly well:

    data L' a = forall i. L' { unL' :: L i a }
    data Ex a = forall i. Ex [L i a]

    fromListL :: [a] -> L' a
    fromListL xs = g (Ex (map L xs))
      where
        g (Ex [x]) = L' x
        g (Ex xs)  = g (Ex (f xs))
        f (x:y:xs) = (N x y) : f xs
        f _ = []

# Nested data-types

Another approach is to literally build up a type isomorphic to a 2^n size tuple (modulo laziness). For example, in the case of a 4-tuple, we’d like to just say `((1, 2), (3, 4))`. There is still, however, the pesky question of how one does recursion over such a structure. The technique to use here is bootstrapping, described in Adam Buchsbaum in his thesis and popularized by Chris Okasaki:

    data B a = Two (B (a, a)) | One a
        deriving Show

Notice how the recursive mention of `B` does not hold `a`, but `(a, a)`: this is so-called “non-uniform” recursion. Every time we apply a `Two` constructor, the size of our tuple doubles, until we top it off:

    exampleB = Two (Two (One ((1,2), (3,4))))

    fromListB :: [a] -> B a
    fromListB [x] = One x
    fromListB xs = Two (fromListB (pairs xs))
        where pairs (x:y:xs) = (x,y) : pairs xs
              pairs _ = []

    toListB :: B a -> [a]
    toListB (One x) = [x]
    toListB (Two c) = concatMap (\(x,y) -> [x,y]) (toListB c)

# Which is better?

At first glance, the GADT approach seems more appealing, since when destructing it, the data type looks and feels a lot like an ordinary binary tree. However, it is much easier to parse user data into nested data types than GADTs (due to the fact that Haskell is not a dependently typed language). Ralf Hinze, in his paper [Perfect Trees and Bit-reversal Permutations](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.46.1095), gives another argument in favor of nested datatypes:

> Comparing \[perfect trees and the usual definition of binary trees\] it is fairly obvious that the first representation is more concise than the second one. If we estimate the space usage of an *k*-ary constructor at *k+1* cells, we have that a perfect tree of rank *n* consumes *(2^n-1)3+(n+1)2* cells with the first and *(2^n-1)3+2*2^n\* with the second. \[The difference coming from all of the extra leaf nodes.\]

Nevertheless, destructing the nested data type tree is very weird, and we might feel better about the “exotic” nested data type if there was an efficient transformation from the catamorphism `(n :: t a -> t a -> t a , z :: a -> t a)` on traditional trees:

    cataL :: (t a -> t a -> t a, a -> t a) -> L i a -> t a
    cataL (n,z) (N l r) = n (cataL (n,z) l) (cataL (n,z) r)
    cataL (n,z) (L x) = z x

to a catamorphism `(f :: a -> t a, g :: t (a, a) -> t a)` on our nested data-type tree:

    cataB :: (forall a. a -> t a, forall a. t (a, a) -> t a) -> B a -> t a
    cataB (f,g) (One a) = f a
    cataB (f,g) (Two t) = g (cataB (f,g) t)

This conversion is possible, though, alas, it is not a catamorphism:

    cataLB :: forall t a. (t a -> t a -> t a, a -> t a) -> B a -> t a
    cataLB (n,z) t = f t z
      where
        f :: forall b. B b -> (b -> t a) -> t a
        f (One a) z = z a
        f (Two t) z = f t (\(l,r) -> n (z l) (z r))

The idea is to create a function `(a -> t a) -> t a`, which we then pass `z` in order to get the final result. This is the time honored difference list/continuation passing trick, where we build up a chain of function invocations rather than attempt to build up the result directly, since ordinarily the catamorphism on nested data-type trees proceeds in the wrong direction. But now, we can easily perform any fold we would have done on ordinary trees on our nested data-type trees, which resolves any lingering concerns we may have had. Nested data types are superior... from a representation size perspective, in any case. (See Jeremy's comment for another take on the issue, though.)

For further reading, check out [Generalised folds for nested datatypes](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.42.1517) (Richard Bird, Ross Paterson).
