---
title: "My type signature overfloweth"
date: 2010-09-01 09:00:35
slug: my-type-signature-overfloweth
categories: [Haskell]
comments:
    - id: 1102
      author: Don Stewart
      date: "2010-09-01 10:38:44"
      content: "Just a quick clarification, Matthew Sackman is the author of the \"sessions\" package (with the long types). \"simple-sessions\" is separate work, done by Tov. They were developed approximately concurrently, but aren't related to each other."
    - id: 1105
      author: Edward Z. Yang
      date: "2010-09-01 10:59:58"
      content: "Good catch Don. I've fixed the post."
    - id: 1104
      author: Anonymous
      date: "2010-09-01 10:56:48"
      content: "This reminds me of the \"Haskell Type Constraints Unleashed\" paper: http://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/constraint_families.pdf"
    - id: 1106
      author: Tim Carstens
      date: "2010-09-01 11:54:22"
      content: |
        Exploding type signatures is a pretty obnoxious problem.  Luckily there are a couple handy tricks you can use to get some extra mileage out of GHC towards mitigating the issue.
        
        Certainly it is obnoxious to write complex type signatures, particularly when the signatures aren't especially readable.  On the other hand, if type inference isn't getting you where you need to be, you can help it along by applying partial signatures.  GHC doesn't have any special syntax for this, but by writing functions like
        
        <code>
        intTuple :: (a, b, a, Int, b) -&gt; (a, b, a, Int, b)
        intTuple x = x
        </code>
        
        you can help things along.
        
        Another trick is to use quasi-quoters in your type signatures.  GHC will allow home-grown quoters, as in
        
        <code>
        foo :: $( [$my_crazy_type_qq| (1,3) (2,5) (4)::Int |] )
        foo = ...
        </code>
        
        though (if memory serves) Haddock gets really upset about such type signatures ("haddock: internal Haddock or GHC error: renameType"), the rule about type variables getting implicitly quantified doesn't apply to type variables introduced by a quasi-quoter, and you'll need to provide your own instances of <code>Language.Haskell.TH.Syntax.Lift</code> for things like <code>Language.Haskell.TH.Type</code> (though the <a href="http://hackage.haskell.org/package/th-lift" rel="nofollow">th-lift</a> package can help in this).
        
        In terms of reading and understanding crazy type signatures, a wrapper utility around GHC can be really handy.  The <a href="http://hackage.haskell.org/package/hint" rel="nofollow">hint</a> package provides an easy-to-use wrapper around libghc, and it includes utility functions for evaluating types.  You can then parse the type signature provided by GHC and re-output it in a more human-readable form, which may prove useful when generating documentation or during debugging.
    - id: 1108
      author: Alexey Romanov
      date: "2010-09-01 13:48:46"
      content: |
        &gt; However, since there is only one kind in Haskell
        
        Um. I understand what you mean, but I am not quite sure how to say it properly. "Only one lower kind", perhaps (given that * -&gt; * and the rest are called higher kinds)?
    - id: 1109
      author: Edward Z. Yang
      date: "2010-09-01 14:01:37"
      content: "How does \"primitive kind\" sound?"
    - id: 1120
      author: Alexey Romanov
      date: "2010-09-04 02:59:19"
      content: "Yes, it works if there is no established term."
---

I’ve recently started researching the use of *session types* for practical coding, a thought that has been in the back of my mind ever since I was part of a team that built a networked collaborative text editor and spent a lot of time closely vetting the server and the client to ensure that they had implemented the correct protocols. The essence of such protocols is often relatively simple, but can quickly become complicated in the presence of error flow (for example, resynchronizing after a disconnection). Error conditions also happen to be difficult to automatically test! Thus, static types seem like an attractive way of tackling this task.

There are three implementations of session types in Haskell: [sessions](http://hackage.haskell.org/package/sessions), [full-sessions](http://hackage.haskell.org/package/full-sessions) and [simple-sessions](http://hackage.haskell.org/package/simple-sessions). If you were feeling particularly naive, you might try going to the [Haddock page](http://hackage.haskell.org/packages/archive/sessions/2008.7.18/doc/html/Control-Concurrent-Session.html) to get a feel for what the API looks like. Before you continue reading, please inspect that page.

------------------------------------------------------------------------

Done gouging your eyes out? Let’s proceed.

In an interview in *Coders at Work*, Simon Peyton Jones mentioned that one of the notable benefits of types is that it gives a concise, crisp description of what a function might do. That API is anything from concise and crisp, and it’s certainly not something that I could figure out just by looking at the corresponding function definition. Accordingly, one of the key selling points of current encodings of session types is that they do not break type inference: we give up on our user understanding what the gaggle of typeclasses means, and only expect to transfer one bit of information, “Do the protocols match?”

This is not a problem that is fundamental to session types: any functionality that makes extensive use typeclasses can easily fall prey to these long type signatures. I have two (rather half-baked) thoughts on how this complexity might be rendered more nicely to the user, although not eliminated:

- A favorite pastime of type system hackers is a type-level encoding of naturals, using Peano numbers `Z` and `S a`, attached to something like `Vector (S (S Z))`. Vector is a type constructor of kind `* -> *`. However, since there is only one primitive kind in Haskell, we could actually pass any type to Vector, say `Vector Int`, which would be a nonsensical. One way to prevent this from occurring is to declare our Peano numbers instances of a typeclass `Nat`, and then declare `Nat a => Vector a`. But, since `a` is used precisely once in any such a statement, wouldn’t it be great if instead we could write `Vector :: Nat -> *`? If you need to specify type equality, you could imagine some sort of type pattern matching `concat :: Vector a -> Vector b -> Vector c with c ~ a :+: b`. [Collapsing types and kinds](http://byorgey.wordpress.com/2010/08/05/typed-type-level-programming-in-haskell-part-iv-collapsing-types-and-kinds/) is an interesting step in this direction.
- When mathematicians present proofs, they might explicitly specify “for all F such that F is a field...”, but more frequently, they’ll say something like, “in the following proof, assume the following variable naming conventions.” With this, they get to avoid having to repeatedly explicitly redeclare what all of their variable names mean. An analogous system for type variables would go a long way towards reducing long type signatures.

But actually, that has nothing to do with what I’m currently looking at.

------------------------------------------------------------------------

Here’s what I am looking at: session types suffer from another type signature explosion phenomenon: any function in the protocol contains, in its type, a complete specification of the entire protocol continuing from that point in time. As [Neubauer and Thiemann admit (PDF)](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.70.7370&rep=rep1&type=pdf), the “session type corresponding to full SMTP is quite unreadable.” The two lines of inquiry I am pursuing are as follows:

- Can building exception support into session types (currently an open problem) allow for much simpler session types by allowing most cases to elide the session types corresponding to error cases?
- Can we use `type` to permit a single global specification of the protocol, which individual functions then simply refer to? Do we need something a little more powerful?

At this point, I’ve just been doing thinking and paper reading, but I hope to start hacking on code soon. I’d love to hear your thoughts though.
