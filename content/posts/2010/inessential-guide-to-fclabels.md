---
title: "Inessential guide to fclabels"
date: 2010-04-28 09:00:04
slug: inessential-guide-to-fclabels
categories: [Haskell]
comments:
    - id: 363
      author: Edward Kmett
      date: "2010-04-28 15:15:32"
      content: |
        Another useful set of operations are the getM, setM and modM operations which use a label to access fields in your monad's state:
        
        <pre>getM :: MonadState m s =&gt; (s :-&gt; r) -&gt; m r
        setM :: MonadState m s =&gt; (s :-&gt; r) -&gt; r -&gt; m ()
        modM :: MonadState m s =&gt; (s :-&gt; r) -&gt; (r -&gt; r) -&gt; m ()</pre>
        
        setM has an alias (=:)
        
        With these you can say things like
        
        <pre>data MyState = MyState { _bar :: Int, _baz :: Int, _fresh :: Int } 
        $(deriveAccessors [''MyState])
        
        
        foo = do
            bar =: 12
            baz =: 23
            x  L10nState
        
        instance L10n L10nState where
            l10n = id
           
        locale :: L10n s =&gt; s :-&gt; Locale
        locale = locale' . l10n</pre>
        
        Now I can 
        
        <pre>getM locale </pre>
        
        or 
        
        <pre>locale =: "en@lolcat" </pre>
        
        in any monad that offers state which tells me it contains localization support.
        
        You can even define little convenience functions like:
        
        <pre>(+=) :: (MonadState s m, Num a) =&gt; (s :-&gt; a) -&gt; a -&gt; m a
        x += n = do
            modM x (+ n)
            getM x
        
        eval x = do
           var &lt;- nextVar += 1
           ...</pre>
        
        and your code can pick up an entirely imperative feel if you so desire.
    - id: 367
      author: anonymous
      date: "2010-05-01 16:18:52"
      content: |
        cool!
        if you continue and/or finish the series with an inessential guide to lenses (http://hackage.haskell.org/package/lenses) and to sec (http://hackage.haskell.org/package/sec) each you are my hero!
    - id: 3224
      author: Oliver Batchelor
      date: "2011-12-15 07:53:12"
      content: "Neat! Looks like a great package, why do we need \"proper\" records in haskell if we have this again?"
    - id: 3225
      author: Edward Z. Yang
      date: "2011-12-15 13:53:41"
      content: "Oliver: Well, fclabels is built on top of proper records, so you need one for the other ;-)"
    - id: 3902
      author: Nicolas Trangez
      date: "2012-08-02 11:16:48"
      content: "@Edward Z. Yang: whilst it's true fclabels is built on top of records to get the TH/deriveAccessors part working, there's no reason one can't write the necessary instances manually. As such there's no need for record syntax to get fclabels to work. Or am I missing something?"
    - id: 3906
      author: Edward Z. Yang
      date: "2012-08-03 05:35:01"
      content: "Sure. I guess the real point here is, you want someone to do the deriving for you, and the deriving has to be based off of some data, and that data will essentially be some sort of record system. I don't think I was claiming you need record syntax (except that you need a way of expressing primitive setters.)"
---

Last time I did an [Inessential guide to data-accessor](http://blog.ezyang.com/2010/04/inessential-guide-to-data-accessor/) and everyone told me, "You should use fclabels instead!" So here's the partner guide, the inessential guide to fclabels. Like data-accessor the goal is to make record access and editing not suck. However, it gives you some more useful abstractions. It uses Template Haskell on top of your records, so it is not compatible with data-accessor.

*Identification.* There are three tell-tale signs:

1.  Type signatures that contain `:->` in them ("Oh, that kind of looks like a function arrow... but it's not? Curious!"),
2.  Records that contain fields with a leading underscore (as opposed to data-accessor's convention of an trailing underscore), and
3.  An `import Prelude hiding (id, (.), mod)`, with an import from `Control.Category` to replace them.

*Interpreting types.* A *label* is signified by `r :-> a` which contains a getter `r -> a` and a setter `a -> r -> r`. Internally, a wrapped label is simply a *point*, a structure consisting of `r -> a` and `b -> r -> r`, with `a` required to be equal to `b`. (As we will see later, a point is useful in its own right, but not for basic functionality.)

*Accessing record fields.*

    get fieldname record

*Setting record fields.*

    set fieldname newval record

*Modifying record fields.* For `fieldname :: f a :-> a`, `modifier` should have type `a -> a`.

    mod fieldname modifier record

*Accessing, setting and modifying sub-record fields.* Composition is done with the period operator `(.)`, but you can't use the one from the Prelude since that only works with functions. The composition is treated as if you were you composing the getter.

    get (innerField . outerField) record
    set (innerField . outerField) newVal record
    mod (innerField . outerField) modifier record

*Accessor over applicative.* You can use `fmapL` to lift an accessor into an applicative context. This is useful if your record is actually `Maybe r` (You can turn `r :-> a` into `Maybe r :-> Maybe a`).

But wait, there's more!

*More fun with views.* Remember that a point is a getter and a setter, but they don't have to be for the same types. Combined with a clever applicative instance, we can use this to incrementally build up a label composed of multiple labels. The result looks a lot like a view that you'd be able to create on a relational database. The recipe is:

1.  Have the constructor for the resulting type (e.g. `(,)`, the tuple constructor),
2.  Have all of the accessors for the resulting type (e.g. `fst` and `snd`), and
3.  Have the labels you would like to compose together (say, `label1` and `label2`).

Combine, with `for`, each accessor for the resulting type (2) with the label to be accessed with that accessor (3), combine all of these resulting points with the constructor for the resulting type with the applicative instance, i.e. `<$>` and `<*>`, and then stick it in a label with `Label`:

    (,) <$> fst `for` label1 <*> snd `for` label2

Amazingly, you won't be able to mix up which argument an accessor (2) should be placed in; the result won't typecheck! (See the *Postscript* for a more detailed argument.)

*More fun with lenses.* A function implies directionality: a to b. But light can filter through a lense either way, and thus a lense represents a bidirectional function. We can apply filter a label `f :-> a` through a lense `a :<->: b` to get a new label `f :-> b` (remember that composition with a regular function is insufficient since we need to put values in as well as take values out). One has to be careful about what direction your lense is pointed. If `label :: r :-> a`, `in :: b -> a` and `out :: a -> b`, then:

    (out <-> in) `iso` label :: r :-> b
    (in <-> out) `osi` label :: r :-> b

The other directions won't typecheck if `a != b`.

You can lift a lense into a functor using `lmap` (it simply runs `fmap` on both directions).

*Further reading.* The [Hackage documentation](http://hackage.haskell.org/package/fclabels) has a ton of excellent examples.

*Postscript.* With our original example in mind:

    label1 :: r -> a
    label2 :: r -> b
    (,) <$> fst `for` label1 <*> snd `for` label2 :: r :-> (a, b)

We consider the types of the points we've constructed, before combining them with the applicative instance:

    fst `for` label1 :: Point Person (a, b) a
    snd `for` label2 :: Point Person (a, b) b

We have a shared applicative functor `Point Person (a, b)`, and if we treat that as `f`, clearly:

    (,) :: a -> b -> (a, b)
    fst `for` label1 :: f a
    snd `for` label2 :: f b
    (,) <$> fst `for` label1 <*> snd `for` label2 :: f (a, b)

which is equivalent to `Point Person (a, b) (a, b)`, which is a valid `Label`.

But what is `for` doing? The source code documentation says:

> Combine a partial destructor with a label into something easily used in the applicative instance for the hidden Point datatype. Internally uses the covariant in getter, contravariant in setter bi-functioral-map function. (Please refer to the example because this function is just not explainable on its own.)

Well, I'm going to ignore this advice, since you've seen the example already. Let's parse this. `for` is covariant in getter `r -> a` and contravariant in setter `a -> f -> f`. These terms are from category theory describing functors. A covariant functor is a "normal" functor, whereas a contravariant functor is one with composition flipped around. So while normally `fmap f g == f . g`, in the contravariant world `fmap f g == g . f`:

    for :: (i -> o) -> (f :-> o) -> Point f i o
    for a b = dimap id a (unLabel b)

Well, we're not doing much interesting to the getter, but we're mapping `a :: (a, b) -> a` (in our example) onto the setter `a -> f -> f`. Luckily (for the befuddled), the covariant map doesn't typecheck (`(a, b) != (f -> f)`), but the contravariant map does: `(a, b) -> f -> f`, which is a new setter that takes `(a, b)`, precisely what we expected from the type signature.

So, `for` sets up our setters and partially our getter, and the applicative instance finishes setting up our getter.
