---
title: "Induction and logical relations"
date: 2013-09-18 19:35:37
slug: induction-and-logical-relations
categories: [Agda]
comments:
    - id: 6240
      author: Joachim Breitner
      date: "2013-09-19 03:19:25"
      content: |
        Nice Post. I recently defined a similar relatedness predicate for untyped lambda calculus in Isabelle, but then you have the additional problem that it is neither normal indcutive definition (non-positive occurrences) nor a regular recursive definition (the recursion is not necessarily well-founded). In the end I defined a whole family of approximation (which are inductive) and took the limit of them.
        
        What is the E occurring in your definitions of V? Is it a typo (it seems that should be V) or do I understand something not?
    - id: 6241
      author: Edward Z. Yang
      date: "2013-09-19 04:26:25"
      content: |
        Interesting: usually when the recursive definition is not well-founded, you can do something like step-indexing to get it well-founded again. Perhaps that is what you did defining the family.
        
        It's not a typo; V only applies to values, so you also need a relation that works with expressions (essentially, both expressions normalize to some value, and those values are related by V). It's all in the code. :)
    - id: 6242
      author: Adam Chlipala
      date: "2013-09-19 08:40:58"
      content: |
        The "much less nice" V[[...]] definition seems to be "much less nice" only because it departs from common conventions even at the informal level, IMO.
        
        First, why not just use a polymorphic equality predicate/type family to formalize that two Booleans must be equal?  This generalizes much better to, e.g., natural numbers as the base type.
        
        Second, why define this relation on expressions instead of values?  With values, by construction the only possible pattern-matching cases are the interesting ones.  This works even better when you define the set of values recursively rather than inductively (e.g., "values(Bool) = bool").  It wouldn't be hard to avoid the extra "Void" cases.
        
        I'm used to seeing a relation defined for values, with a relation for expressions defined trivially on top, with no further recursion.
    - id: 6243
      author: Chris Warburton
      date: "2013-09-19 08:51:29"
      content: "Spotted this on Planet Haskell today, and I've been hitting exactly this issue myself in Coq! I've been defining my equivalence relations as inductive types, which is fine for simple cases like untyped SK calculus, but as soon as I add a type environment I can't get function application to go through. I'll try playing around with a non-inductive model like this tonight. Thanks for the insight!"
    - id: 6244
      author: Edward Z. Yang
      date: "2013-09-19 13:43:56"
      content: |
        Adam: That is good advice; I will try restructuring the code around what you suggested and see if it helps. I suppose the reason the code was setup this way was because I attempt to transcribe the relation out of Amal's notes; but it does seem like your way will work out better.
        
        Chris: I'm not sure you mean "type environment" here, but yes, logical relations are probably what you want here.
    - id: 6245
      author: Edward Z. Yang
      date: "2013-09-19 19:47:25"
      content: |
        Adam: OK, I looked at this more carefully, and I'm not sure I understand your comments.
        
        Let's try to redo some of these definitions using your suggestions, since there is some funny dependency chain here. Our first task is to redefine the value family to be a function rather than an inductive definition, as it is currently:
        
        <pre>  data value :  {τ : Tp} → [] ⊢ τ → Set where
            Value/true : value #t
            Value/false : value #f
            Value/lam : {τ₁ τ₂ : Tp} {e : [] ,, τ₁ ⊢ τ₂} → value (lam e)</pre>
        
        If I try to retain exactly the same type family signature, I end up with this:
        
        <pre>  value! : {τ : Tp} → [] ⊢ τ → Set
          value! #t = Unit
          value! #f = Unit
          value! (if e then e₁ else e₂) = Void
          value! (var x) = Void
          value! (lam e) = Unit
          value! (app e e₁) = Void</pre>
        
        OK; that seems not so good. Unfortunately, I can't just use a <code>values(Bool) = bool</code>  style definition, because I want to identify syntax trees as values, and here values and expressions are all balled up together:
        
        <pre>  data _⊢_ (Γ : Ctx) : Tp → Set where
            #t  : Γ ⊢ bool
            #f  : Γ ⊢ bool
            if_then_else_ : {τ : Tp} → Γ ⊢ bool → Γ ⊢ τ → Γ ⊢ τ → Γ ⊢ τ
            var : {τ : Tp} 
                → τ ∈ Γ
                → Γ ⊢ τ 
            lam : {τ1 τ2 : Tp} 
                → Γ ,, τ1 ⊢ τ2
                → Γ ⊢ τ1 ⇒ τ2
            app : {τ1 τ2 : Tp} 
                → Γ ⊢ τ1 ⇒ τ2 
                → Γ ⊢ τ1 
                → Γ ⊢ τ2</pre>
        
        So is the suggestion that this expression should be split into two mutually recursive data types?
        
        Carrying on, suppose we do go ahead and use value as it is currently (inductively) defined. So we'd like to now change V so it only takes values as arguments. We can do this without too much trouble:
        
        <pre>
          mutual
            V!⟦_⟧ : (τ : Tp) → {e e' : [] ⊢ τ} → value e → value e' → Set
            V!⟦_⟧ bool Value/true Value/true = Unit
            V!⟦_⟧ bool Value/false Value/false = Unit
            V!⟦_⟧ bool _ _ = Void
            V!⟦_⟧ (τ₁ ⇒ τ₂) {lam e} {lam e'} Value/lam Value/lam =
              {a a' : [] ⊢ τ₁} {v : value a} {v' : value a'} → V!⟦ τ₁ ⟧ v v' → E!⟦ τ₂ ⟧ (subst1 a e) (subst1 a' e')
        
            E!⟦_⟧ : (τ : Tp) → [] ⊢ τ → [] ⊢ τ → Set
            E!⟦ τ ⟧ e e' = Σ (λ v → Σ (λ v' → Σ {_} {_} {e ⇓ v} (λ n → Σ {_} {_} {e' ⇓ v'} λ n' → V!⟦ τ ⟧ (fst n) (fst n'))))
        </pre>
        
        We've eliminated the void from the lambda case, though not for the boolean case. I can't directly use the suggestion of a polymorphic equality, because the value predicates under question have different types. I suppose I can still assert equality on the original expressions however:
        
        <pre>   V!⟦_⟧ bool {e} {e'} _ _ = e == e'</pre>
        
        Continuing, forcing us to cough up proofs of value-ness for V is a bit irritating, because it means that substitutions also have to carry proofs that what they are substituting the terms with are values. (<code>[] ⊢c Γ</code> in this development). I stopped at that point, though I think I could also redefine substitutions suitably. (What is unfortunate is that now the proofs are specialized for CBV, since in CBN the substitution doesn't have to result in a value).
    - id: 6246
      author: "Jean-Luc Delatre"
      date: "2013-09-20 14:02:43"
      content: "Just in case you're not aware, there is not point painfully doing inductive proofs \"by hand\" when this can be done in fully automated fashion with \"cyclic proofs\" checked by Büchi automata inclusion, see Brotherston/Gorogiannis prover: http://www.eis.mdx.ac.uk/staffpages/nikosgkorogiannis/"
    - id: 6250
      author: Adam Chlipala
      date: "2013-09-23 19:37:35"
      content: |
        What I was suggesting with values was, intentionally avoiding Agda notation because I'd just get it wrong:
        
        value(Bool) = bool
        value(t1 -&gt; t2) = t1 |- t2
        
        That is, you do return an instance of the expression type in the arrow case.
---

Logical relations are a proof technique which allow you to prove things such as normalization (*all programs terminate*) and program equivalence (*these two programs are observationally equivalent under all program contexts*). If you haven't ever encountered these before, I highly recommend [Amal Ahmed's OPLSS lectures](http://www.cs.uoregon.edu/research/summerschool/summer13/curriculum.html) on the subject; you can find videos and notes from yours truly. (You should also be able to access her lectures from previous years.) This post is an excuse to talk about [a formalization of two logical relations proofs in Agda](https://github.com/ezyang/lr-agda/blob/master/STLC-CBV.agda) I worked on during OPLSS and the weeks afterwards. I'm not going to walk through the code, but I do want expand on two points about logical relations:

1.  They work when simple induction would not, and
2.  The logical relation is not an inductive definition.

The full development is in the [lr-agda repository on GitHub](https://github.com/ezyang/lr-agda/). Many thanks to Dan Licata for providing the initial development as a homework assignment for his OPLSS Agda course and for bushwhacking the substitution lemmas which are often the greatest impediment to doing proofs about the lambda calculus.

------------------------------------------------------------------------

If you didn't know any better, you might try to prove normalization inductively, as follows:

> To show that all programs normalize to a value, let us proceed inductively on the typing derivations. For example, in the application case, we need to show `e1 e2` normalizes to some value `v`, given that `e1` normalizes to `v1` and `e2` normalizes to `v2`. Well, the type of `v1` is `t1 -> t2`, which means `v1 = λx. e'`. Uh oh: this should step to `e'[v2/x]`, but I don’t know anything about this expression (`e'` could be anything). Stuck!

What is the extra *oomph* that logical relations gives you, that allows you to prove what was previously unprovable by usual induction? Let's think about our second proof sketch: the problem was that we didn't know anything `e'`. If we knew something extra about it, say, "Well, for some appropriate v, e'\[v/x\] will normalize," then we'd be able to make the proof go through. So if this definition of `WN` was our old proof goal:

    WN : (τ : Tp) → [] ⊢ τ → Set
    WN τ e = e ⇓

then what we'd like to do is extend this definition to include that "extra stuff":

    WN : (τ : Tp) → [] ⊢ τ → Set
    WN τ e = e ⇓ × WN' τ e

> At this point, it would be good to make some remarks about how to read the Agda code here. WN is a *type family*, that is, `WN τ e` is the unary logical relation of type `τ` for expression `e`. The type of a type is `Tp`, which is a simple inductive definition; the type of a term is a more complicated `[] ⊢ τ` (utilizing Agda's mixfix operators; without them, you might write it `Expr [] τ`), which not only tells us that `e` is an expression, but *well-typed*, having the type `τ` under the empty context `[]`. (This is an instance of the general pattern, where an inductive definition coincides with a well-formedness derivation, in this case the typing derivation.) `e ⇓` is another mixfix operator, which is defined to be a traditional normalization (there exists some value v such that e reduces to v, e.g. `Σ (λ v → value v × e ↦* v)`).

But what is this extra stuff? In the case of simple types, e.g. booleans, we don't actually need anything extra, since we're never going to try to apply it like a function:

    WN' : (τ : Tp) → [] ⊢ τ → Set
    WN' bool e = Unit

For a function type, let us say that a function is WN (i.e. in the logical relation) if, when given a WN argument, it produces a WN result. (Because we refer to WN, this is in fact a mutually recursive definition.) This statement is in fact the key proof idea! :

    WN' (τ1 ⇒ τ2) e = (e1 : [] ⊢ τ1) → WN τ1 e1 → WN τ2 (app e e1)

There are a number of further details, but essentially, when you redo the proof, proving WN instead of plain old normalization, you no longer get stuck on the application case. Great! The flip-side, however, is that the proof in the lambda case is no longer trivial; you have to do a bit of work to show that the extra stuff (WN') holds. This was described to me as the "balloon" principle.

The two sides of the balloon are the "use of the inductive hypothesis" and the "proof obligation". When you have a weak inductive hypothesis, it doesn't give you very much, but you don't have to work as hard to prove it either. When you strengthen the inductive hypothesis, you can prove more things with it; however, your proof obligation is correspondingly increased. In the context of a normalization proof, the "use of the inductive hypothesis" shows up in the application case, and the "proof obligation" shows up in the lambda case. When you attempt the straightforward inductive proof, the lambda case is trivial, but the inductive hypothesis is so weak that the application case is impossible. In the logical relations proof, the application case falls out easily from the induction hypothesis, but you have to do more work in the lambda case.

------------------------------------------------------------------------

We now briefly take a step back, to remark about the way we have defined the WN' type family, on our way to the discussion of why WN' is not an *inductive definition*. In Agda, there are often two ways of defining a type family: it can be done as a recursive function, or it can be done as an inductive definitions. A simple example of this is in the definition of a length indexed list. The standard inductive definition goes like this:

    data Vec : Set → Nat → Set where
      vnil : {A : Set} → Vec A 0
      vcons : {A : Set} → A → Vec A n → Vec A (S n)

But I could also build the list out of regular old products, using a recursive function on the index:

    Vec : Set → Nat → Set
    Vec A 0 = Unit
    Vec A (S n) = A × Vec A n

The two different encodings have their own strengths and weaknesses: using a recursive function often means that certain equalities are definitional (whereas you'd have to prove a lemma with the inductive definition), but an inductive definition lets you do case-analysis on the different possibilities.

Sometimes, it is simply not possible to do the inductive definition, and this is the case for a logical relation. This strengthening of the inductive hypothesis is to blame:

    data WN' : τ → [] ⊢ τ → Set where
      WN/bool : {e : [] ⊢ bool} → WN' bool e
      WN/⇒ : {e1 : [] ⊢ τ1} → (WN τ1 e1 → WN τ2 (app e e1)) → WN' (τ1 ⇒ τ2) e

Agda doesn't complain about inductive-recursive definitions (though one should beware: they are not too well metatheoretically grounded at the moment), but it will complain about this definition. The problem is [a familiar one](http://blog.ezyang.com/2012/09/y-combinator-and-strict-positivity/): WN does not occur in strictly positive positions; in particular, it shows up as an argument to a function contained by the WN/⇒ constructor. So we can't use this!

As it turns out, the inability to define the logical relation inductively is not a big deal for normalization. However, it causes a bit more headaches for more complicated logical relations proofs, e.g. for equivalence of programs. When considering program equivalence, you need a binary relation relating values to values, saying when two values are equal. This can be stated very naturally inductively:

    data V'⟦_⟧ : (τ : Tp) → [] ⊢ τ → [] ⊢ τ → Set where
       V/bool-#t : V'⟦ bool ⟧ #t #t
       V/bool-#f : V'⟦ bool ⟧ #f #f
       V/⇒ : {τ₁ τ₂ : Tp} {e₁ e₂ : [] ,, τ₁ ⊢ τ₂}
         → ((v₁ v₂ : [] ⊢ τ₁) → V'⟦ τ₁ ⟧ v₁ v₂ → E⟦ τ₂ ⟧ (subst1 v₁ e₁) (subst1 v₂ e₂))
         → V'⟦ τ₁ ⇒ τ₂ ⟧ (lam e₁) (lam e₂)

We define the relation by type. If a value is a boolean, then we say that `#t` (true) is related to itself, and `#f` is related to itself. If the value is a function, then we say that a lambda term is related to another lambda term if, when applied to two related values, the results are also related. This "function" is directly analogous to the extra stuff we added for the normalization proof. (If you like, you can mentally replace "related" with "equal", but this is misleading since it doesn't capture what is going on in the function case). But this fails the strict positivity check, so we have to define it recursively instead:

    V⟦_⟧ : (τ : Tp) → [] ⊢ τ → [] ⊢ τ → Set
    V⟦ bool ⟧    #t #t = Unit
    V⟦ bool ⟧    #f #f = Unit
    V⟦ bool ⟧ _ _  = Void
    V⟦ τ₁ ⇒ τ₂ ⟧ (lam e) (lam e') = (v v' : [] ⊢ τ₁) → V⟦ τ₁ ⟧ v v' → E⟦ τ₂ ⟧ (subst1 v e) (subst1 v' e')
    V⟦ τ₁ ⇒ τ₂ ⟧ _ _ = Void

Notice that the definition here is much less nice than the inductive definition: we need two fall through cases which assert a contradiction when two things could not possibly be equal, e.g. `#t` could not possibly be equal to `#f`. Furthermore, supposing that we are given V as a hypothesis while performing a proof, we can no longer just case-split on it to find out what kind of information we have; we have to laboriously first case split over the type and expression, at which point the function reduces. To give you a sense of how horrible this is, consider this function which converts from a inductive definition to the recursive definitions:

    pV : {τ : Tp} → {e e' : [] ⊢ τ} → V⟦ τ ⟧ e e' → V'⟦ τ ⟧ e e'
    pV {bool} {#t} {#t} V = V/bool-#t
    pV {bool} {#t} {#f} ()
    pV {bool} {#t} {if _ then _ else _} ()
    pV {bool} {#t} {var _} ()
    pV {bool} {#t} {app _ _} ()
    pV {bool} {#f} {#t} ()
    pV {bool} {#f} {#f} V = V/bool-#f
    pV {bool} {#f} {if _ then _ else _} ()
    pV {bool} {#f} {var _} ()
    pV {bool} {#f} {app _ _} ()
    pV {bool} {if _ then _ else _} ()
    pV {bool} {var _} ()
    pV {bool} {app _ _} ()
    pV {_ ⇒ _} {if _ then _ else _} ()
    pV {_ ⇒ _} {var _} ()
    pV {_ ⇒ _} {lam _} {if _ then _ else _} ()
    pV {_ ⇒ _} {lam _} {var _} ()
    pV {_ ⇒ _} {lam _} {lam _} f = V/⇒ (\ v v' V → pE (f v v' V))
    pV {_ ⇒ _} {lam _} {app _ _} ()
    pV {_ ⇒ _} {app _ _} ()

Good grief! Perhaps the situation could be improved by improving how Agda handles wildcards in pattern matching, but at the moment, all of this is necessary.

"But wait, Edward!" you might say, "Didn't you just say you couldn't define it inductively?" Indeed, this function does not operate on the inductive definition I presented previously, but a slightly modified one, which banishes the non-strictly-positive occurrence by replacing it with V, the recursive definition:

    V/⇒ : {τ₁ τ₂ : Tp} {e e' : [] ,, τ₁ ⊢ τ₂}
      → (V : (v v' : [] ⊢ τ₁) → V⟦ τ₁ ⟧ v v' {- the critical position! -} → E'⟦ τ₂ ⟧ (subst1 v e) (subst1 v' e'))
      → V'⟦ τ₁ ⇒ τ₂ ⟧ (lam e) (lam e')

This conversion function helps a lot, because agda-mode interacts a lot more nicely with inductive definitions (`C-c C-c` works!) than with recursive definitions in cases like this.

------------------------------------------------------------------------

Why do logical relations in Agda? (or any proof assistant, for that matter?) Proofs using logical relations often follow the pattern of defining an appropriate logical relation for your problem, and then a lot of book-keeping to actually push the relation through a proof. Computers are great for doing book-keeping, and I think it is hugely informative to work through a logical relation proof in a proof assistant. An interesting challenge would be to extend this framework to a non-terminating language (adding step-indexes to the relation: the very pinnacle of book-keeping) or extending the lambda calculus with polymorphism (which requires some other interesting techniques for logical relations).
