---
title: "Problem Set: The Codensity Transformation"
date: 2012-01-07 03:00:20
slug: problem-set-the-codensity-transformation
categories: [Haskell]
comments:
    - id: 3305
      author: Eric
      date: "2012-01-07 09:45:02"
      content: |
        I believe you have a typo in the `listrep` function.
        
        listrep = ([a] -&gt; [a]) -&gt; [a]
        
        This looks like a type signature, so I think you mean to use `::`. Also, I think `Hughes a -&gt; [a]` would make the intended functionality a bit more clear :-)
    - id: 3306
      author: Edward Z. Yang
      date: "2012-01-07 14:16:19"
      content: "Heh, that's what I get for not testing the \"easy\" part of the problem set. :^)"
    - id: 3318
      author: Dan P
      date: "2012-01-12 21:11:52"
      content: "Just curious. Why is this written in .hs style with -- comments instead of in .lhs? Wouldn't the latter look nicer?"
    - id: 3322
      author: Edward Z. Yang
      date: "2012-01-13 12:42:30"
      content: "I think it was probably because I wanted to emphasize the code, as opposed to the text."
    - id: 3324
      author: Dan P
      date: "2012-01-14 18:34:05"
      content: "I looked at the codensity papers and blog posts a couple of years back, but doing these exercises really made it clear. Thanks!"
    - id: 3328
      author: Pseudonym
      date: "2012-01-15 23:11:59"
      content: "Did anyone else find it easier to implement join and fmap instead of bind?"
    - id: 3329
      author: Edward Z. Yang
      date: "2012-01-15 23:28:37"
      content: "Pseudonym: That's not too uncommon; join is a bit simpler, categorically speaking."
    - id: 4197
      author: Ryan Ingram
      date: "2012-09-17 22:34:08"
      content: "I don't understand the bonus section, my 'C r m a' version worked just fine.  The only tricky bit was that the type of 'abs' was 'Monad m =&gt; C a m a -&gt; m a'."
    - id: 4208
      author: Edward Z. Yang
      date: "2012-09-18 23:54:02"
      content: "Yep, 'abs' is the problem. The upshot is that you can't explicitly instantiate r if you want to be polymorphic in a, so it has less degrees of freedom than you might want."
    - id: 6191
      author: Matthew Brecknell
      date: "2013-08-06 20:01:39"
      content: "I found it quite difficult to identify an actual (asymptotic) inefficiency in the first tree substitution sample, compared to the improved version. I can see that it performs repeated traversals, but the cost of each traversal is linearly bounded by the cost of constructing the next row of leaves. Looking at Janis Voigtländer's 2008 paper, it looks like it's only when the tree is partially demanded that an asymptotic inefficiency is observed. Is that the case? If so, I wonder if there's a more obvious example you could use. Would the IOSpec example be too complex? What about building a pathologically imbalanced tree instead of a fully balanced one?"
    - id: 6192
      author: Edward Z. Yang
      date: "2013-08-06 21:17:53"
      content: "Yes, it is possible that there is no asymptotic inefficiency in this example. I haven't understood the example well enough. Would you like to try your hand at coming up with a better example?"
    - id: 6340
      author: Eugene Kirpichov
      date: "2014-01-05 16:07:16"
      content: |
        Thanks for the post, it really helped!
        
        I have a few clarifying questions about the final part:
        1) "FreeLike f m" means "m is a free monad for f"?
        2) "FreeLike f (C m)" means "(C m) is a more efficient free monad for f than m"?
        3) In both of these cases - both m and C m must be isomorphic to (Free f) because a functor's free monad is unique up to isomorphism, right?
        4) My instance FreeLike f (C m) didn't use the monad instances even though your comment suggested that they can be used. It looks like this:
        
        instance FreeLike f m =&gt; FreeLike f (C m) where
          wrap fca = C $ \g -&gt; wrap (fmap (`unC` g) fca)
        
        Is this correct?
        
        Also, even after writing this instance, I haven't yet wrapped my head around what it intuitively means... I guess I just need to spend more time staring at it.
    - id: 6343
      author: Edward Z. Yang
      date: "2014-01-06 14:51:41"
      content: |
        1) Yes, or to be more precise, 'm' is isomorphic to the free monad over 'f'.
        2) No, not necessarily; all we are saying here is that C is free-like (iso to the free monad). But it is true the point of C is to be more efficient.
        3) Yes. One way to think about it is that FreeLike is the universal property of the free construction, so by showing that it is inhabited (and it upholds the properties you need it to; we can't show those here because Haskell is not dependently typed), then they must be iso to each other. I will comment that I'm taking this on faith, and I haven't seen anyone sit down and actually formally prove this out.
        4) The monad instance is not necessarily needed for the solution. Your solution is correct.
---

Have you ever wondered how the *codensity transformation*, a surprisingly general trick for speeding up the execution of certain types of monads, worked, but never could understand the paper or Edward Kmett's blog posts on the subject?

Look no further: below is a *problem set* for learning how this transformation works.

The idea behind these exercises is to get you comfortable with the types involved in the codensity transformation, achieved by using the types to guide yourself to the only possible implementation. We warm up with the classic concrete instance for leafy trees, and then generalize over all free monads (don't worry if you don't know what that is: we'll define it and give some warmup exercises).

Experience writing in continuation-passing style may be useful, although in practice this amounts to "listen to the types!"

Solutions and more commentary may be found in Janis Voigtlander's paper "\`Asymptotic Improvement of Computations over Free Monads. \<http://www.iai.uni-bonn.de/~jv/mpc08.pdf\>\`\_"

To read more, see Edward Kmett's excellent article which further generalizes this concept:

- <http://comonad.com/reader/2011/free-monads-for-less/>
- <http://comonad.com/reader/2011/free-monads-for-less-2/>
- <http://comonad.com/reader/2011/free-monads-for-less-3/>

If there is a demand, I can add a hints section for the exercises.

    {-# LANGUAGE Rank2Types, MultiParamTypeClasses, FlexibleInstances #-}
    import Prelude hiding (abs)

    _EXERCISE_ = undefined

    -----------------------------------------------------------------------------
    -- Warmup: Hughes lists
    -----------------------------------------------------------------------------

    -- Experienced Haskellers should feel free to skip this section.

    -- We first consider the problem of left-associative list append.  In
    -- order to see the difficulty, we will hand-evaluate a lazy language.
    -- For the sake of being as mechanical as possible, here are the
    -- operational semantics, where e1, e2 are expressions and x is a
    -- variable, and e1[e2/x] is replace all instances of x in e1 with e2.
    --
    --        e1 ==> e1'
    --   ---------------------
    --     e1 e2 ==> e1' e2
    --
    --   (\x -> e1[x]) e2 ==> e1[e2/x]
    --
    -- For reference, the definition of append is as follows:
    --
    --      a ++ b = foldr (:) b a
    --
    -- Assume that, on forcing a saturated foldr, its third argument is
    -- forced, as follows:
    --
    --                e1 ==> e1'
    --    -----------------------------------
    --      foldr f e2 e1 ==> foldr f e2 e1'
    --
    --  foldr f e2 (x:xs) ==> f x (foldr f e2 xs)
    --
    -- Hand evaluate this implementation by forcing the head constructor,
    -- assuming 'as' is not null:

    listsample as bs cs = (as ++ bs) ++ cs

    -- Solution:
    --
    --        (as ++ bs) ++ cs
    --      = foldr (:) cs (as ++ bs)
    --      = foldr (:) cs (foldr (:) bs as)
    --      = foldr (:) cs (foldr (:) bs (a:as'))
    --      = foldr (:) cs (a : foldr (:) b as')
    --      = a : foldr (:) cs (foldr (:) bs as')
    --
    -- Convince yourself that this takes linear time per append, and that
    -- processing each element of the resulting tail of the list will also
    -- take linear time.

    -- We now present Hughes lists:

    type Hughes a = [a] -> [a]

    listrep :: Hughes a -> [a]
    listrep = _EXERCISE_

    append :: Hughes a -> Hughes a -> Hughes a
    append = _EXERCISE_

    -- Now, hand evaluate your implementation on this sample, assuming all
    -- arguments are saturated.

    listsample' a b c = listrep (append (append a b) c)

    -- Solution:
    --
    --        listrep (append (append a b) c)
    --      = (\l -> l []) (append (append a b) c)
    --      = (append (append a b) c) []
    --      = (\z -> (append a b) (c z)) []
    --      = (append a b) (c [])
    --      = (\z -> a (b z)) (c [])
    --      = a (b (c []))
    --
    -- Convince yourself that the result requires only constant time per
    -- element, assuming a, b and c are of the form (\z -> a1:a2:...:z).
    -- Notice the left-associativity has been converted into
    -- right-associative function application.

    -- The codensity transformation operates on similar principles.  This
    -- ends the warmup.

    -----------------------------------------------------------------------------
    -- Case for leafy trees
    -----------------------------------------------------------------------------

    -- Some simple definitions of trees

    data Tree a = Leaf a | Node (Tree a) (Tree a)

    -- Here is the obvious monad definition for trees, where each leaf
    -- is substituted with a new tree.

    instance Monad Tree where
        return = Leaf
        Leaf a   >>= f = f a
        Node l r >>= f = Node (l >>= f) (r >>= f)

    -- You should convince yourself of the performance problem with this
    -- code by considering what happens if you force it to normal form.

    sample = (Leaf 0 >>= f) >>= f
        where f n = Node (Leaf (n + 1)) (Leaf (n + 1))

    -- Let's fix this problem.  Now abstract over the /leaves/ of the tree

    newtype CTree a = CTree { unCTree :: forall r. (a -> Tree r) -> Tree r }

    -- Please write functions which witness the isomorphism between the
    -- abstract and concrete versions of trees.

    treerep :: Tree a -> CTree a
    treerep = _EXERCISE_

    treeabs :: CTree a -> Tree a
    treeabs = _EXERCISE_

    -- How do you construct a node in the case of the abstract version?
    -- It is trivial for concrete trees.

    class Monad m => TreeLike m where
        node :: m a -> m a -> m a
        leaf :: a -> m a
        leaf = return

    instance TreeLike Tree where
        node = Node

    instance TreeLike CTree where
        node = _EXERCISE_

    -- As they are isomorphic, the monad instance carries over too.  Don't
    -- use rep/abs in your implementation.

    instance Monad CTree where
        return = _EXERCISE_
        (>>=)  = _EXERCISE_ -- try explicitly writing out the types of the arguments

    -- We now gain efficiency by operating on the /abstracted/ version as
    -- opposed to the ordinary one.

    treeimprove :: (forall m. TreeLike m => m a) -> Tree a
    treeimprove m = treeabs m

    -- You should convince yourself of the efficiency of this code.
    -- Remember that expressions inside lambda abstraction don't evaluate
    -- until the lambda is applied.

    sample' = treeabs ((leaf 0 >>= f) >>= f)
        where f n = node (leaf (n + 1)) (leaf (n + 1))

    -----------------------------------------------------------------------------
    -- General case
    -----------------------------------------------------------------------------

    -- Basic properties about free monads

    data Free f a = Return a | Wrap (f (Free f a))
    instance Functor f => Monad (Free f) where
        return = _EXERCISE_
        (>>=)  = _EXERCISE_ -- tricky!

    -- Leafy trees are a special case, with F as the functor. Please write
    -- functions which witness this isomorphism.

    data F a = N a a

    freeFToTree :: Free F a -> Tree a
    freeFToTree = _EXERCISE_

    treeToFreeF :: Tree a -> Free F a
    treeToFreeF = _EXERCISE_

    -- We now define an abstract version of arbitrary monads, analogous to
    -- abstracted trees.  Witness an isomorphism.

    newtype C m a = C { unC :: forall r. (a -> m r) -> m r }

    rep :: Monad m => m a -> C m a
    rep = _EXERCISE_

    abs :: Monad m => C m a -> m a
    abs = _EXERCISE_

    -- Implement the monad instance from scratch, without rep/abs.

    instance Monad (C m) where
        return = _EXERCISE_
        (>>=)  = _EXERCISE_ -- also tricky; if you get stuck, look at the
                            -- implementation for CTrees

    -- By analogy of TreeLike for free monads, this typeclass allows
    -- the construction of non-Return values.

    class (Functor f, Monad m) => FreeLike f m where
        wrap :: f (m a) -> m a

    instance Functor f => FreeLike f (Free f) where
        wrap = Wrap

    instance FreeLike f m => FreeLike f (C m) where
        -- Toughest one of the bunch. Remember that you have 'wrap' available for the
        -- inner type as well as functor and monad instances.
        wrap = _EXERCISE_

    -- And for our fruits, we now have a fully abstract improver!

    improve :: Functor f => (forall m. FreeLike f m => m a) -> Free f a
    improve m = abs m

    -- Bonus: Why is the universal quantification over 'r' needed?   What if
    -- we wrote C r m a = ...?  Try copypasting your definitions for that
    -- case.
