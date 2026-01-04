---
title: "Understanding strictness of functions"
date: 2011-08-23 03:20:58
slug: understanding-strictness-of-functions
draft: true
categories: [Haskell]
---

If you’ve been programming Haskell a bit, you’ve probably seen equations that look like this:

    seq ⊥ y = ⊥

This is not a program you could actually write, since unrestricted pattern-matching against ⊥ (spoken “bottom”, and most easily spelled as `undefined` in a Haskell program) is equivalent to solving the halting problem. But halting problem notwithstanding, we write equations like this in order to compactly state the *strictness* of a function. Strictness is not a question of black and white: it comes in shades of gray.

Thus, when we say a function is “strict in both arguments” or “spine strict”, we’re trying to make more subtle distinctions about what color of strictness the function has. But it gets even worse than that: how “strict” a function is depends on what context it appears. If you need to get a picture of that situation, [check out some guy’s comic about it](http://blog.ezyang.com/2011/04/the-haskell-heap/). But in everyday parlance, we want to be able to say things about the strictness of functions, we need to do away with ghosts and presents and use symbols.

The point of the post is to tell you how. At the end, you will be able to:

1.  Read equations of this form and figure out how such a function must work, operationally speaking, and
2.  Take an arbitrary fragment of code, and write down the appropriate denotational equations for its behavior.

# Reading

Let’s start simple. :

    (+) :: Num a => a -> a -> a
    ⊥ + y = ⊥
    x + ⊥ = ⊥

The basic premise behind an equation like this is we pass some function bottoms (or, in later examples, data structures containing bottoms) and see how it reacts. Does it output a bottom? Does it output a data structure with a bottom? If we probe the function with enough of these values, we can get a basic idea of what’s going on inside. You can even play along at home:

    Prelude> undefined + 3
    *** Exception: Prelude.undefined
    Prelude> 3 + undefined
    *** Exception: Prelude.undefined
    Prelude> 3 + 3
    6

But the goal in the second part will be able to figure this out just by inspecting the code.

When you’re given these equations, someone else has already done all of this hard experimental work for you, and even more so, they’ve done it for all possible values. It is in fact the case that no matter what `y` I choose, if I write `⊥ + y` I will get back ⊥. This actually says something very important about the function: it says that it is “strict in its first argument.” If you open up a denotational semantics textbook, they will in fact define what it means for a function to be strict in an argument with this property.

But this may seem a bit circular, and we’re attempting to develop an operational intuition from these denotational equations, so let’s consider all of the possible ways by which a function could output ⊥ when given a ⊥ argument. It could actually need to, you know, know the value of the argument in order to do the computation: this is certainly true for `+`.

It could be manually requesting the argument be evaluated, even though it’s throwing away the result, as is the case for `seq`:

    seq ⊥ y = ⊥

It could also simply be passing the argument through unchanged:

    id ⊥ = ⊥

Which might seem a little odd, since `id` isn’t "doing” anything: the intuition you should have here is that the context itself is evaluating the argument. And finally, a function might always return a different ⊥, no matter what you do... :

    (\_ -> ⊥) ⊥ = ⊥

This function is certainly not doing anything to its argument, but since the requirement is fulfilled, it’s strict too.

So, assuming none of the degenerate cases apply (the identity or constant case), producing a bottom requires us to actually use the argument or force its usage using `seq`, and voila: we have insight into the inner workings of the function. To summarize, here are the ways of producing bottom, given a bottom:

1.  *Primitive usage.* Use it in a primitive computation.
2.  *Forced usage.* Use it with `seq`.
3.  *Identity.* Return the argument.
4.  *Constant bottom.* Return your own bottom.

(Rules one and two need to be cleaned up a little, but we’ll do that in the second part.)

## Laziness

Sometimes these equations depend on the values of other arguments: essentially any short-circuiting control operator takes advantage of this fact:

    (&&) :: Bool -> Bool -> Bool
    False && ⊥ = False
    True && ⊥ = ⊥
    ⊥ && y = ⊥

Logical and is *not* strict in its second argument, because there exists a value I could pass to the first argument that results in a non-bottom result. However, it is strict in the first argument. What does this tell us? We know that the function always inspects its first argument, and sometimes inspects its second argument: given referential transparency, it doesn’t take too much imagination to figure out how it decides when to inspect its second argument.

Don’t take this for granted (perhaps because you were lulled into a false sense of security do to it’s similarity to every strict language also in existence.) If I had written the equations:

    x && ⊥ = ⊥
    ⊥ && False = False
    ⊥ && True = ⊥

You would have a very different picture of how the function was structured inside, even if the pattern bindings looked like they should be equivalent. And if you saw this:

    False && ⊥ = False
    True  && ⊥ = ⊥
    ⊥ && False = False
    ⊥ && True = ⊥

Maybe you should go read [this blog post.](http://blog.ezyang.com/2010/12/gin-and-monotonic/)

## Projections

When we speak of a function being strict in an argument, we are actually talking one very specific evaluation context, evaluation to “weak head normal form.” This evaluation is just enough to discover if the right-hand side of our equation is ⊥, but nothing beyond that. For [flat data types](http://blog.ezyang.com/2010/12/hussling-haskell-types-into-hasse-diagrams/), this is the only type of evaluation available, but this is not true for data constructors. We have a richer choice than just ⊥ or `x`: maybe I have a `Just ⊥` or a `2:3:⊥` list. We can speak of bottoms “lurking” inside lazy data structures.

With this rich set of data types we now have shades of gray of strictness. They affect us, both in terms of what a function returns, and how it consumes its arguments. Here are two motivating examples. The first considers return values:

    singleton0  _ = [0]
    singleton   x = [x]
    singleton'  x = x `seq` [x]

`singleton'` is the only strict function in this set (as we’ve defined it), but there is also a sense in which `singleton` seems stricter than `singleton0`.

The second considers arguments:

    head  (x:_) = x
    head' (x:xs) = length xs `seq` x

Both of them fulfill the equation `f ⊥ = ⊥`, but there is also a sense in which `head' (x:⊥) = ⊥` is stricter than `head (x:⊥) = x`. Can we make these notions precise? In this section we introduce the tool of *projections*.

To keep things simple (perhaps artificially so), let’s consider about as simple a data constructor as possible, the lift or “box” constructor (so named because it lifts a flat domain into a more interesting one):

    data Lift a = L a

The reader is encouraged to draw a Hasse diagram for this type. Consider a function of type `Lift a -> Lift a`. If we think about the interesting ways in which a caller might use values of this type, we might think of these two functions:

    ident  (L x) = L x
    strict (L x) = x `seq` L x

There are many more functions, but these two functions are special, in that they are polymorphic and idempotent: if I apply them again nothing else happens. Their equations look like this:

    ident ⊥ = ⊥
    ident (L ⊥) = L ⊥
    ident (L x) = L x

    strict ⊥ = ⊥
    strict (L ⊥) = ⊥
    strict (L x) = L x

The `ident` function is simply identity; it does no work. The `strict` function evaluates the inside of the container, and then returns the wrapped up result. All of them are “strict” in the sense we defined earlier (look for ⊥ as an argument!) but `strict` seems very clearly “stricter”, since it’s unwrapping and evaluating the inner term! (Note also that `strict x` actually does something, as opposed to `seq x x`).

In our original discussion of strictness, we only went as far as the `ident` context: we looked for a ⊥ on the right hand side of the equation. But suppose we took our result, and looked for ⊥ or a `L ⊥` on the right hand side: this is the `strict` context. It's easier for arguments to be strict in the strict context, since we’re forcing more things.

The trick at the heart of mechanisms like `rnf`, if you only have an `IO` function for the `ident` context, e.g. `evaluate`, you can convert any other context into the `ident` context by composing the projection with it:

    _ <- evaluate (ident (L (error "Bang!"))) -- outputs nothing
    _ <- evaluate (strict (L (error "Bang!"))) -- outputs "error: Bang!"

What about inputs? We could try picking a variety of different types of inputs with bottoms sprinkled all over, but we would never quite know if we’d picked enough to truly characterize the function. But remember the curious property about bottom: if we ever so slightly touch it, it blows up in our faces. So if we want to say that a function *doesn’t* touch a particular sub-expression, we can replace it with a bottom and see what happens. We say an expression is α-strict in a β-strict context if `β . f == β . f . α`: `α` inserts bottoms in the places we think are unused in the `β` evaluation context.

There are two more projections that are worth thinking about:

    absent (L _) = L ⊥
    bottom (L _) = ⊥

Who is lazier than who? You can’t always say, but the hierarchy of the projections looks like this:

PICTURE

We can define these functions for arbitrary data-types (compositionally, even, since every type is just sums and products in the end), and then use them to talk about strictness. To recall our earlier examples:

    singleton0  _ = [0]
    singleton   x = [x]
    singleton'  x = x `seq` [x]

    head  (x:_) = x
    head' (x:xs) = length xs `seq` x

We need to define some functions for lists:

    nil [] = ⊥
    nil (x:xs) = x:(nil xs)

    hd [] = []
    hd (⊥:xs) = ⊥
    hd (x:xs) = x:(hd xs) -- note: the cons here is lazy!

    tl [] = []
    tl (x:⊥) = ⊥
    tl (x:xs) = tl xs `seq` x:xs

What are each of these functions doing, operationally speaking? `nil` is the archetype of a function that only works on infinite lists: it blows up if it hits a nil list constructor (unsurprisingly, strictness analysis here doesn’t help very much for optimization purposes.) `hd` is the archetype of a function that evaluates its values as you move down the list: it is your value-strict list. And `tl` is the archetype of a function that forces all of the cons-cells of a list: it is your spine-strict list. Note that we can combine `hd` and `tl` to get a spine-strict and value-strict list, and we can combine `nil` and `hd` to get a value-strict infinite list. (What happens if you combine `tl` and `nil`?)

If we include the obvious definitions for `ident` and `bottom`, we can characterize our functions:

- `singleton0` is bottom-strict in an ident-strict context (maximally lazy.)
- `singleton` is bottom-strict only in a tl-strict context (as long as you don’t look at the values, you’re OK!)
- `singleton'` is bottom-strict only in a bottom-strict context (it always looks at its arguments!)
- `head` is hd-and-tl-strict in an ident-strict context.
- `head'` is only hd-strict in an ident-strict context.

Verifying these assessments is left as an exercise for the reader.

PICTURE

# Writing

Writing
