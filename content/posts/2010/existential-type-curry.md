---
title: "Existential type-curry"
date: 2010-10-15 09:00:02
slug: existential-type-curry
categories: [Haskell, Math]
math: true
comments:
    - id: 1288
      author: Derek Elkins
      date: "2010-10-15 09:35:25"
      content: |
        The polymorphic lambda calculus doesn't correspond to first-order logic.  In Haskell we might take forall a. a -&gt; a and instantiate it to Nat -&gt; Nat.  Thus a is quantified over propositions which is exactly what first order logic doesn't do.  In first order logic we might write forall n : Nat. ... but it certainly doesn't make sense to write forall n : Nat. n -&gt; n, what does 3 -&gt; 3 mean?  First order logic corresponds to a simple form of dependent typing.  The polymorphic lambda calculus corresponds to a second order logic minus the first order parts.  The paper, "Lectures on the Curry-Howard Isomorphism", calls this "propositional second order logic."  I highly recommend reading "Lectures on the Curry-Howard Isomorphism."
        
        Also of note is that runST has the form of (forall x. A[x]) -&gt; B.  It is interesting to consider what the difference would be if it instead had the type exists x. A[x] -&gt; B.
    - id: 1289
      author: Edward Z. Yang
      date: "2010-10-15 10:22:04"
      content: "Yeah, I was a bit confused on that point. I've reworded it, hopefully the new wording is correct."
    - id: 1290
      author: Dan Doel
      date: "2010-10-15 16:18:42"
      content: |
        These work out fairly nicely in Agda. And for the pair of intuitionistically equivalent formulae, it's clear that what's going on is (un)currying:
        
        http://hpaste.org/40605/forall_exists
        
        I didn't bother postulating a classical axiom to prove 'impossible'.
    - id: 1292
      author: wren ng thornton
      date: "2010-10-15 19:11:53"
      content: |
        Still a bit off:
        
        The simply types lambda calculus corresponds with (intuitionistic) first-order propositional logic. System F (aka polymorphism over types) corresponds with second-order propositional logic. Whereas dependent types (i.e., LF) correspond with first-order predicate calculus.
        
        You don't get any quantification in LF that you don't get in STLC, you just get predicates instead of being restricted to propositions. In System F you get full quantification over propositions, but not quantification over individuals (unless your "individuals" are just propositions).
    - id: 1297
      author: Edward Z. Yang
      date: "2010-10-16 08:00:16"
      content: "Ok, reworded it some more."
    - id: 1298
      author: Dan Doel
      date: "2010-10-16 15:05:52"
      content: |
        Incidentally, here:
        
        http://code.haskell.org/~dolio/agda-share/html/DepCat.html
        
        is a development in Agda that tries to follow close to the category theoretic way of presenting this stuff, and shows how you get from (Σ i:I. F i) → R to Π i:I. (F i → R).
    - id: 1905
      author: Eric Kow
      date: "2011-03-09 08:21:31"
      content: "If I may be a bit off-topic, one of the fun things about natural language is that sentences like \"everybody loves somebody\" (or a better example, \"every boxer loves a woman\") are ambiguous. It's just that we tend to prefer one of the meanings than the other (see first logical form).  A fun thing to do is to think about \"every boxer loves a woman... and her name is Mia\""
    - id: 1911
      author: Edward Z. Yang
      date: "2011-03-10 21:58:09"
      content: "Heh, that's a good twist. A bit like fruit flies like a banana, time flies like an arrow."
---

This post is for those of you have always wondered why we have a `forall` keyword in Haskell but no `exists` keyword. Most of the existing tutorials on the web take a very *operational* viewpoint to what an existential type is, and show that placing the forall in the “right place” results in the correct behavior. I’m going to take a different approach and use the Curry-Howard isomorphism to explain the translation. Some of the logic examples are shamelessly stolen from Aaron Coble’s Logic and Proof lecture notes.

------------------------------------------------------------------------

First, a little logic brush up. (Feel free to skip.)

<div class="container center">

\$P \to (Q \land R)\$

</div>

At the very bottom of the hierarchy of logic systems lies propositional logic. Whenever you write a non-polymorphic function in Haskell, your function definition corresponds to a statement in propositional logic—this is the simply typed lambda calculus. You get some propositional symbols P, Q and R (corresponding to types) and some logical connectives \$\lnot \land \lor \to \iff\$. In particular, \$\to\$ corresponds to the function arrow `->`, so you can read \$P \to Q\$ as the type `P -> Q`.

<div class="container center">

\$\forall xy.\\ P(x) = P(y)\$

</div>

The next step up is first-order predicate logic, which allows you to use the quantifiers ∀ and ∃ on variables ranging over individuals x, y and z (the predicates take individuals and return propositions). Logical formulas in this system start to look a lot like Haskell polymorphism, but it actually corresponds to dependent types: individuals are terms, not types.

For the purpose of this post, we’ll instead have x, y and z to range over propositions (types), except for two examples of first order logic to get some intuition for quantifiers. Then polymorphic function definitions are statements in what is called propositional second order logic.

Propositional second order logic gives us a bit of rope, and we can do some fairly unintuitive things with it. Existential types are one such application. However, most Haskellers have a pretty good intuition for polymorphic functions like `id :: a -> a`, which actually have an ∀ quantifier at the very beginning, like `id :: forall a. a -> a` or \$\forall x. x \to x\$. What I’d like to do next is make the connection between our intuitive sense of polymorphic functions and our intuitive sense of a universal quantifier.

------------------------------------------------------------------------

Consider the following English sentence: *All professors can teach and do research.* We can translate this into a statement in first-order logic (x ranging over individuals):

<div class="container center">

\$\forall x.\\ \mathrm{professor}(x) \to \mathrm{teaches}(x) \land \mathrm{researches}(x)\$

</div>

The intuition for the trick of “narrowing” a universally quantified variable by placing it in an implication corresponds directly to the implicit dictionary passing that occurs when you use a type class (which also narrows a universally quantified variable).

We can do similar translations for the existential quantifier. *Everybody loves somebody* and *there is somebody that everybody loves* correspond to, respectively:

<div class="container center">

\$\forall x\\ \exists y\\ \mathrm{loves}(x, y)\$

\$\exists y\\ \forall x\\ \mathrm{loves}(x, y)\$

</div>

Take a moment to convince yourself that these are not the same statements, and figure out which direction implication goes.

We’ll now jump straight to the implication equivalences, which are the punchline, so to speak. Here, x ranges over propositions (i.e. types).

<div class="container center">

\$(\exists x\\ A\[x\]) \to B \equiv \forall x\\ (A\[x\] \to B)\$

\$(\forall x\\ A\[x\]) \to B \equiv \exists x\\ (A\[x\] \to B)\$

</div>

Consider the first equivalence: intuitively, it states that we can simulate a function that takes an existential type by using `forall x. (A x -> B)`. This is precisely the existential data constructor:

    data OpaqueBox = forall a. OpaqueBox a

which has the type `forall a. (a -> OpaqueBox)`.

The second proposition is a little trickier to grasp: in the right to left direction, it seems clear that if there exists an inference A(x) to B for *some* x, if I provide *all* x I will get B. However, from left to right, if I provide *all* A(x) to get B, one of those A(x) will have to have been used but I have no good way of figuring out which one.

We can rigorously prove this equivalence with [sequent calculus](http://en.wikipedia.org/wiki/Sequent_calculus). We can think of these as “deduction rules” much like modus ponens (if A then B, A; therefore, B). However, statements in the sequent calculus take the form \$\Gamma \vdash \Delta\$, where Γ is the set of propositions which conjunctively form the assumption, and Δ is the set of propositions which disjunctively form the result. (The \$\vdash\$ is called a “turnstile” and indicates implication.)

    $\cfrac{\Gamma \vdash A, \Delta \qquad \Sigma, B \vdash \Pi}{\Gamma, \Sigma, A\rightarrow B \vdash \Delta, \Pi} \quad  ({\rightarrow }L)$      $\cfrac{\Gamma, A \vdash B, \Delta}{\Gamma \vdash A \rightarrow B, \Delta} \quad ({\rightarrow}R)$

        $\cfrac{\Gamma, A[t/x] \vdash \Delta}{\Gamma, \forall x A \vdash \Delta} \quad  ({\forall}L)$      $\cfrac{\Gamma \vdash A[y/x], \Delta}{\Gamma \vdash \forall x A, \Delta} \quad  ({\forall}R)$

         $\cfrac{\Gamma, A[y/x] \vdash \Delta}{\Gamma, \exists x A \vdash \Delta} \quad  ({\exists}L)$      $\cfrac{\Gamma \vdash A[t/x], \Delta}{\Gamma \vdash \exists x A, \Delta} \quad  ({\exists}R)$

\$\forall L\$ and \$\exists R\$, in particular, are quite interesting: \$\forall L\$ says I can make any assumed proposition “polymorphic” by picking some subterm and replacing all instances of it with a newly universally quantified variable (it’s a stronger assumption, so we’re weakening our entailment). We can indeed do this in Haskell (as one might transform `(Int -> Bool) -> Int -> Bool` into `(a -> b) -> a -> b`), so long as our proof doesn’t peek at the actual type to perform its computation. \$\exists R\$, on the other hand, says that I can take any resulting proposition and “hide” my work by saying something weaker: instead of A\[t\], I merely say there exists some x for which A\[x\] is true. This corresponds nicely to the intuition of an existential type hiding representation. Another nice duality is that universal quantification hides information inside the proof, while existential quantification hides information outside of the proof.

\$\forall R\$ and \$\exists L\$ don’t do as much work, but they are a little tricky to use: any universal quantification on the right side of the turnstile can create/destroy a free variable, and any existential quantification on the left side can create/destroy a free variable. Note that \$\forall L\$ and \$\exists R\$ cannot be used this way; while they can use existing free variables, they can’t create or destroy them.

Here is the proof in both directions of the equivalence. What we’re trying to prove lives on the bottom; tautologies are at the top.

![image](/img/sequent-easy.png)

The proofs are nicely symmetrical: one uses ∀L and ∃L, and the other ∀R and ∃R. The application of the →R “uncurries” each entailment. Furthermore, the fact that both proofs are constructive indicates that there is this equivalence is one that can be witnessed by a Haskell program! You can check out a [Coq version of the proof](http://codepad.org/vr1wO4O3) from kmc.

------------------------------------------------------------------------

*Postscript.* I picked the wrong equivalence initially, but I felt it would be a shame not to share it. Here is the proof for: \$\exists x\\ (A\[x\] \to B) \vdash (\forall x\\ A\[x\]) \to B\$.

![image](/img/sequent.png)

This is done entirely with intuitionistic logic: the other direction requires classical logic. This is left as an exercise for the reader, the [solution is here](http://hpaste.org/40584/x_ax__b__x_ax__b) by monochrom. There is also [a version from kmc in Coq](http://hpaste.org/40585/ezyangs_theorem) in both directions. This result has an interesting implication for existentials over functions: we can translate from an existential to a universal, but not back!
