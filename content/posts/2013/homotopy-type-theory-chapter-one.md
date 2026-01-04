---
title: "(Homotopy) Type Theory: Chapter One"
date: 2013-06-24 18:56:27
slug: homotopy-type-theory-chapter-one
categories: [Type Theory]
comments:
    - id: 6156
      author: Michael Maloney
      date: "2013-06-26 13:58:18"
      content: |
        Hey Edward. (I love your blog).
        
        I'm not sure why they chose such a terrible, academic name like "Indiscernable of Indentities" for this property. Especially since, (unless I am mistaken) it is called "transport" later in the book (and subst in Agda and replace in Idris). 
        
        Take a look at the *based* path induction. You need to fix A : U and a : A. Then if you can provide:
        
        C : Πx:A. a = x -&gt; U and c : C(a, refl_a)
        
        You are given f : Πx:A. Πp : a = x. C(x, p).
        
        Now, if we assume UIP (uniqueness of identity proofs, not the default in Coq, but it is the default in Agda), our identity types are Props. This means that the second argument (the path) to our predicate C is irrelevant. (Since C(x, p) = C(x, q) for any p q : a = x). We might as well drop it from the parameter list, and we're left with C : A -&gt; U. 
        
        If we apply this same argument across the definition, we're left with this modified rule:
        
        C : A -&gt; U and c : C(a)
        
        You are given f : Πx:A. C(x).
        
        Which is, of course, the rule you're familiar with in Coq. 
        
        To put it more simply, in HoTT, the path might matter, and it complicates the path induction law.
        
        The same applies to the one-sided induction law (they called it the Martin-Lof rule in the pre-release of the book, but it appears they changed it):
        
        Given C : Πx y:A. x = y -&gt; U and c : Πx. C(x, x, refl_x), 
        
        inductive gives you: f : Πx y: A. Πp : x = y. C(x, y, p).
        
        Again, it's easier to "see" why this law is true when working with sets (types with UIP). The law, rewritten for sets, says:
        
        Given C : A -&gt; A -&gt; U and c : Πx. C(x, x), 
        
        You have f : Πx y: A. x = y -&gt; C(x, y).
        
        In English. C is a binary relation over A, and you prove it holds over the diagonal, making it a reflexive relation. The conclusion of the rule says then that if x and y are equal, then *of course* C(x, y)!
    - id: 6157
      author: Michael Maloney
      date: "2013-06-26 14:04:53"
      content: |
        Whoops. I should have re-read. I found at least one typo:
        
        "You are given f : Πx:A. Πp : a = x. C(x, a)." (the 'C(x, a)' at the end should be 'C(x, p)' instead)
        
        <strong>Editor: Went ahead and fixed this.</strong>
    - id: 6158
      author: Edward Z. Yang
      date: "2013-06-26 19:36:55"
      content: "Hello Michael! Thanks for the attempt to explain what is going on with path induction. I agree, with UIP, the induction is quite intuitive. But now let us suppose that the path does matter, as it does in HoTT. <em>Even</em> then, we can forget about all that and pretend the path is refl, and that will get the job done. I think that captures what I find unintuitive: that in HoTT, the path could matter, but actually path induction (when there is a free base point) says that it doesn't."
    - id: 6160
      author: Michael Maloney
      date: "2013-06-27 11:22:43"
      content: |
        Ah. I see your concern. 
        
        The visual intuition for the path induction is that both ends are "free floating". If you can prove a proposition is true of any reflexive path, then you are justified in "stretching" out that path between any two connected points. This corresponds directly to the notion of a free homotopy of paths (as opposed to homotopy with endpoints fixed).  
        
        The based path induction, on the other hand, works like a homotopy with just one of the endpoints fixed. You are freely allowed to deform the other end.
        
        So, let's suppose I want to prove a fact about the loop in Circle. First, I prove the fact about refl at base. Then, I deform the path, winding it around the Circle as many times as I want. Since any two paths on the Circle are (freely) homotopic, the two paths are equal in HoTT. 
        
        The above may seem a bit off if you're thinking about the fundamental group of the Circle. However, the fundamental group is always done with endpoints fixed. If you want to reformulate the fundamental group with free homotopies instead, you use loops (embeddings of Circles), not paths.
    - id: 6161
      author: Edward Z. Yang
      date: "2013-06-27 19:19:32"
      content: |
        "Since any two paths on the Circle are (freely) homotopic, the two paths are equal in HoTT." That is a good way of putting it! I would add that in the non-based inductive case, the points can be disconnected, so the induction requires me to provide a proof in the case of refl for all of the points under question, so that I have the right thing to deform in the first place.
        
        When we speak of "proving a property about a path where both endpoints are not fixed", this says something about quantifier placement. '(Πxy. C x y) -> Πxy. P (x = y)' would seem to allow both endpoints to move, 'Πx. (Πy. C x y) -> Πy. P (x = y)' would seem to only allow one endpoint to move, and 'Πxy. C x y -> P (x = y)' has both fixed. Put another way, when endpoints move freely, we are not permitted to appeal to the specific choice of an endpoint in the induction. This is perhaps clearly when you replace '(Πxy. C x y)' with 'D' (importantly, with no free variables): providing such a proof is a harder task.
        
        This seems related to the problem one has when doing ordinary induction where sometimes one needs to strengthen the induction hypothesis by generalizing the statement under question. But it seems to me that usually one has some sort of theorem of the form 'C x y -> P x y' (x and y free, i.e. determined), which generalizes to 'Πy. C x y -> P x y' (only x free). So it is only perhaps different in the sense of the inductive argument.
        
        I wonder if there is a clear explanation of this in game semantic terms which clears things up.
    - id: 6164
      author: Dan Piponi
      date: "2013-07-03 13:42:53"
      content: "I was attempting to code up the proof that based path induction follows from path induction in Agda but was having some difficulty. Do you know how to do this?"
    - id: 6165
      author: Dan Piponi
      date: "2013-07-03 18:00:34"
      content: "OK, managed to prove it, though now I have to untangle my code to figure out what it all means, and remove the --type-in-type cheat I used."
    - id: 6166
      author: Andrej Bauer
      date: "2013-07-03 18:23:52"
      content: |
        As you have already noticed, the important bit about path induction is that it only works for general paths, i.e. ones whose endpoints are not restricted in any way. Another thing worth mentioning is that all constructions in type theory are homotopy invariant, and so we can transport constructions along homotopies or paths. Speaking technically, this is so because type families are interpreted as fibrations (which means that they allow path lifting) as opposed to arbitrary collections of things. So in order to really understand path induction you should stare at the definition of a fibration (starting with Hurewicz fibrations as they have the most obvious geometric meaning).
        
        So, if you showed something about the reflexivity path, then you can transport it to any other path along a homotopy from the reflexivity path to the other path. The transport is possible because whatever you showed was homotopy invariant.
    - id: 6167
      author: Edward Z. Yang
      date: "2013-07-04 12:17:12"
      content: "Eureka! I forgot that everything we're proving is homotopy invariant. It's too bad the book doesn't give a definition of fibration... it seems important."
---

In what is old news by now, the folks at the Institute for Advanced Study have released [Homotopy Type Theory: Univalent Foundations of Mathematics](http://homotopytypetheory.org/book/). There has been some (meta)commentary ([Dan Piponi](https://plus.google.com/107913314994758123748/posts/VzWAsojiifE), [Bob Harper](http://existentialtype.wordpress.com/2013/06/22/whats-the-big-deal-with-hott/), [Andrej Bauer](http://math.andrej.com/2013/06/20/the-hott-book/), [François G. Dorais](http://dorais.org/archives/1425), [Steve Awodey](http://homotopytypetheory.org/2013/06/20/the-hott-book/), [Carlo Angiuli](http://www.carloangiuli.com/blog/homotopy-type-theory-univalent-foundations-of-mathematics/), [Mike Shulman](http://golem.ph.utexas.edu/category/2013/06/the_hott_book.html), [John Baez](https://plus.google.com/117663015413546257905/posts/cm1sKge8qxX)) on the Internet, though, of course, it takes time to read a math textbook, so don’t expect detailed technical commentary from non-authors for a while.

Of course, being a puny grad student, I was, of course, most interested in the book’s contribution of *yet another Martin-Löf intuitionistic type theory introduction*, e.g. chapter one. The classic introduction is, of course, the papers that Martin Löf wrote (nota bene: there were many iterations of this paper, so it’s a little hard to find the right one, though it seems Giovanni Sambin’s notes are the easiest to find), but an introduction of type theory for *homotopy type theory* has to make certain adjustments, and this makes for some novel presentation. In particular, the chapter’s discussion of *identity types* is considerably more detailed than I have seen elsewhere (this is not surprising, since identity is of central importance to homotopy type theory). There is also a considerable bit of pedantry/structure in the discussion of the types that make up the theory, reminiscent of the [PFPL](http://existentialtype.wordpress.com/2012/12/03/pfpl-is-out/) (though I believe that this particular chapter was mostly written by others). And, of course, there are many little variations in how the theory is actually put together, expounded upon in some detail in the chapter notes.

In more detail:

**Definitional and propositional equality.** The chapter spends a little bit of time carefully distinguishing between definitional equality (a purely syntactic notion up to computation) and propositional equality (which involves evidence), which I appreciated. The difference between connectives which show up inside and outside the deductive system was a major point of confusion for me when I was originally learning logic.

**The general pattern of the introduction of a new kind of type.** The modern style for introducing logical connectives is to classify the rules into various kinds, such as introduction rules and elimination rules, and then hew to this regularity in the presentation. Often, readers are expected to “see it”, but this book makes a helpful remark laying out the style. I found a useful exercise was to take the rules and reorganize them so that, for example, all of the elimination rules are together and compare them.

**Recursion and induction.** [I’ve written about this subject before](http://blog.ezyang.com/2013/04/the-difference-between-recursion-induction/), arguing that recursion and induction aren’t the same thing, since induction needs to work over indexed types. This is true, but there is an important point I did not make: *induction is generalized recursion*. This is because when you specify your type family *P* to be the *constant type family* which ignores its index, the dependence is erased and you have an ordinary recursor. In fact, this is a [CPDT exercise](http://adam.chlipala.net/cpdt/html/InductiveTypes.html); I think it clarifies things to see this in both Coq and informal mathematics, as the informal presentation makes the dimension of generalization clearer.

**Identity types.** I won’t lie: I had a difficult time with this section, and I don’t think I fully understand why path induction works, even after a very long remark at the end of the section. (Additionally, while the notes point to some prior literature about the subject, I took a look at the papers and I did not see anything that resembled their presentation of path induction.) By default, Coq thinks the inductive principle for equality types should be what is referred to in this book as the indiscernability of identicals:

    > Check eq_rect.
    eq_rect
         : forall (A : Type) (x : A) (P : A -> Type),
           P x -> forall y : A, x = y -> P y

(As a tangent, the use of family *C* is confusingly overloaded; when discussing the generalization of the previous principlem the reader is required to imagine `C(x) -> C(y)  ===  C(x, y)`—the C’s of course being distinct.) Path induction asks for more:

    eq_ind
         : forall (A : Type), forall (C : forall (x y : A), x = y -> Type),
           (forall (x : A), C x x (eq_refl x)) -> forall (x y : A), forall (p : x = y), C x y p

This is perhaps not too surprising, since this machinery is principally motivated by homotopy type theory. Additionally, the inductive principle follows the same pattern as the other inductive principles defined for the other types. The trouble is a frustrating discussion of why this inductive principle valid, even when you might expect, in a HoTT setting, that not all equality was proven using reflexivity. My understanding of the matter is that is has to do with the placement of the `forall (x : A)` quantifier. It is permissible to move one of the x's to the top level (based path induction), but not *both*. (This is somewhat obscured by the reuse of variable names.) There is also a geometric intuition, which is that when both or one endpoints of the path are free (inner-quantification), then I can contract the path into nothingness. But I have a difficult time mapping this onto any sort of rigorous argument. Perhaps you can help me out.

> As an aside, I have some general remarks about learning type theory from a functional programming background. I have noticed that it is not too hard to use Coq without knowing much type theory, and even easier to miss the point of why the type theory might be helpful. But in the end, it is really useful to understand what is going on, and so it’s well worth studying *why* dependent products and sums generalize the way they do. It also seems that people find the pi and sigma notation confusing: it helps if you realize that they are algebraic puns. Don’t skip the definition of the inductive principles.

I apologize if any of this post has been inaccurate or misleadingly skewed. My overall impression is that this first chapter is a very crisp introduction to type theory, but that the segments on identity types may be a little difficult to understand. Now, onwards to chapter two!
