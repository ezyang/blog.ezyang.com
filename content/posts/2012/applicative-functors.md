---
title: "Applicative functors"
date: 2012-08-16 05:34:30
slug: applicative-functors
categories: [Haskell]
math: true
comments:
    - id: 3963
      author: Brent Yorgey
      date: "2012-08-16 07:32:06"
      content: |
        Cool, thanks!  I knew about the alternative presentation in terms of Monoidal, but had never actually seen/paid attention to the alternative presentation of the laws.  Much nicer indeed.  I may have to go stick that in the Typeclassopedia...
        
        Note that there is a semi-nice way to interpret the usual Applicative laws, in that they give a procedure for transforming any Applicative expression into a canonical form with exactly one call to 'pure' at the beginning, followed by a left-nested chain of 's.  Though that doesn't help with the intuition as to WHY these laws should hold.
    - id: 3965
      author: Mikhail Glushenkov
      date: "2012-08-16 16:45:10"
      content: "Answers to the exercises (spoiler alert!): https://gist.github.com/3373487"
    - id: 3968
      author: Derek Elkins
      date: "2012-08-16 19:33:48"
      content: "The second presentation is a witness to the fact that applicative functors are lax monoidal functors, as the second presentation directly lines up with the typical presentation of lax monoidal functors."
    - id: 3969
      author: Edward Z. Yang
      date: "2012-08-16 20:22:46"
      content: |
        Brent: Yeah, I think they deserve a mention, much the same way the alternate formulation of bind with 'join' deserves a mention. The procedure for transformation is kind of neat, I wonder if someone has coded it up.
        
        Derek: I avoided mentioning them because I... don't really know what a lax monoidal functor is. ;-)
    - id: 3999
      author: Derek Elkins
      date: "2012-08-19 14:14:10"
      content: |
        A category is a monoidal category if it has a monoidal product, ⊗, that is associative and has a unit I (up to isomorphism).  A functor is monoidal if it preserves the monoidal structure up to isomorphism, i.e. FI ≅ I and F(A⊗B) ≅ FA⊗FB plus preserving the arrows witnessing associativity and the left/right unit laws.  A functor is lax monoidal if instead of an isomorphism you simply have a natural transformation (going from right to left using the above isomorphisms) and it's oplax monoidal if you have a natural transformation going the other way.
        
        Identifying () with I, (,) with ⊗ and writing unit :: () -&gt; f () and (**) :: (f a, f b) -&gt; f (a,b) [another name for unit and (**) could be liftA0 and liftA2].
    - id: 4004
      author: Tomas Petricek
      date: "2012-08-21 08:57:52"
      content: |
        I always liked the 'Monoidal' definition of applicative functors better than the 'Applicative' definition. I suppose that's because I look at it from a different perspective. I'm not a reguler Haskell programmer (where the applicative style is very useful), but an ML/F# programmer (where the other view seems more useful to me - in fact, I even created a simple syntactic extension for using 'Monoidal' computations in F# that is related to the syntax for monads in an interesting way). 
        
        I failed to describe that in a brief comment, so here is a longer reply as a blog post: http://tomasp.net/blog/applicative-functors.aspx
    - id: 4011
      author: Frank Atanassow
      date: "2012-08-23 06:09:36"
      content: |
        Lax monoidal functors are a bit more general than applicative functors on the same category. Applicative functors are lax monoidal functors for the monoidal fragment of the cartesian structure on a category, that is, for the tensor product given by the cartesian product. In general, a monoidal functor could use a different monoidal structure, for example the co-cartesian one.
        
        The fact that the laws for applicative functors look ugly while the laws for the lax monoidal presentation look intuitive suggests to me that our term syntax is not optimal because it is stuck in an old paradigm. Hmm.
    - id: 4013
      author: Pseudonym
      date: "2012-08-23 21:52:21"
      content: |
        One other observation. The only "law" for Monoidal which has an equals sign rather than a congruence isn't a "law" in the sense that it's a burden that someone writing a Monoidal instance has to prove. It's just the free theorem for (**).
        
        Compare this with the free theorem for (), which has a naturality precondition that seems very hard to work with:
        
        (forall h k. g . k = h k . f
                    =&gt;
                     fmap h p = y)
        =&gt;
        fmap g (p  q) = y  fmap f q
    - id: 6589
      author: Michael Baker
      date: "2014-04-27 15:18:53"
      content: "Would you mind explaining the statement \"but the laws it obeys are quite atrocious\"? I don't have the context that allows me to know what makes a law nice and what makes it ugly, but you seem to have some specific feelings about what makes a good law."
    - id: 12132
      author: Paolo G. Giarrusso
      date: "2015-01-16 04:46:18"
      content: |
        &gt; It seems that there is a general pattern where the API which has nice formulations of laws is not convenient to program with, and the formulation which is nice to program with does not have nice laws.
        
        That might be because we seem to define "nice laws" as "ones we're familiar with from algebra", and algebra does not have binders — at best, you have combinatory logic or CCCs, where you encode variables somehow, but where few like to program.
        
        However, monads do have a nice non-standard formulation (with `&gt;=&gt;`) which is almost natural — but you'd need to encode values `v: V` with functions from `Unit -&gt; V`, like global elements in category theory.
    - id: 21668
      author: "Graphs à la carte | no time"
      date: "2016-12-12 20:29:18"
      content: "[&#8230;] side note: the type signature of box reminds me of this blog post by Edward Yang and makes me wonder if Functor, Foldable plus idempotent and commutative Monoid [&#8230;]"
    - id: 21923
      author: Fylwind
      date: "2017-04-10 01:05:15"
      content: |
        &gt; The procedure for transformation is kind of neat, I wonder if someone has coded it up.
        
        Isn't that what Control.Applicative.Free does?  It looks like the `Ap f a` data type is in canonical form:
        
        &gt; Pure ∷ a → Ap f a
        &gt; Ap ∷ f a → Ap f (a → b) → Ap f b
        
        For some reason, the ordering of Ap constructor is backward, but if we define `RAp = flip Ap`, then there is a correspondence between Brent's canonical form and the free applicative:
        
        &gt; ((pure f ⊛ u1) ⊛ u2) ⊛ u3 …
        
        &gt; ((Pure f `RAp` u1) `RAp` u2) `RAp` u3 …
    - id: 22017
      author: Nadrieril
      date: "2017-06-22 18:42:11"
      content: |
        There's another formulation of applicatives that I have found, but I'm not sure if it actually holds:
        
        f is Applicative iff arrows of the form `f(a -&gt; b)` form a category.
        
        There's a nice similarity with the definition of monads and comonads via kleisli and cokleisli categories.
        
        I can define instances that typecheck but I'm having trouble proving laws. I think it needs something stronger than Category but I don't know what.
        
        data AppArrow f a b = AA (f (a -&gt; b))
        
        instance Applicative f =&gt; Category (AppArrow f) where
          id = AA (pure id)
          (AA f) . (AA g) = AA ((.)  f  g)
        
        instance (Functor f, Category (AppArrow f)) =&gt; Applicative f where
          pure x = fmap ($ x) id_AA
          ff  fx = fmap ($ ()) (ff ._AA fmap const fx)
        where id_AA and ._AA are the id and composition obtained by wrapping/unwrapping the AppArrow category
        
        I'm posting this here in case someone finds it interesting or wrong.
---

<div class="container center">

*On the importance of primary sources.*

</div>

(Introductory material ahead.) Most readers of this blog should have at least a passing familiarity with [applicative functors](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html):

    class Applicative f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b

This interface is quite convenient for day-to-day programming (in particular, it makes for the nice `f <$> a <*> b <*> c` idiom), but the laws it obeys are quite atrocious:

    [identity] pure id <*> v = v
    [composition] pure (.) <*> u <*> v <*> w = u <*> (v <*> w) 
    [homomorphism] pure f <*> pure x = pure (f x) 
    [interchange] u <*> pure y = pure ($ y) <*> u

So, if you (like me twenty-four hours ago) haven’t seen it already, you should show that this interface is equivalent to Applicative:

    class Functor f => Monoidal f where
      unit :: f ()
      (**) :: f a -> f b -> f (a,b)

(By the way, if you haven’t shown that `join :: m (m a) -> m a` for monads is equivalent to `bind :: m a -> (a -> m b) -> m b`, you should do that too.) The laws for this formulation are *much* nicer:

    [naturality] fmap (f *** g) (u ** v) = fmap f u ** fmap g v
    [left identity] unit ** v ≅ v
    [right identity] u ** unit ≅ u
    [associativity] u ** (v ** w) ≅ (u ** v) ** w

Where `f *** g = \(x,y) -> (f x, g y)`. I’ve prettied things up a bit by using “is isomorphic to” in order to suppress the differences between `((), a)` and `a`, as well as `(a,(b,c))` and `((a,b),c)`, for strict equalities you’ll need some extra functions to massage the results into the right types. It seems that there is a general pattern where the API which has nice formulations of laws is not convenient to program with, and the formulation which is nice to program with does not have nice laws. C’est la vie... but at least they’re equivalent!

With this formulation, it becomes trivial to state what laws commutative applicative functors obey:

    [commutativity] u ** v ≅ v ** u

The original paper [Applicative Programming With Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html) is well worth a read. Check it out! That concludes this public service announcement.
