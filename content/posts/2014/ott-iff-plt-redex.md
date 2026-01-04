---
title: "Ott ⇔ PLT Redex"
date: 2014-01-13 19:11:39
slug: ott-iff-plt-redex
categories: [Programming Languages]
comments:
    - id: 6350
      author: Robby
      date: "2014-01-14 13:19:12"
      content: |
        Thanks for taking the time to write about Redex! 
        
        One thing I notice you omit is Redex's support for testing. Beyond the manual, you can also find more information about it here: http://www.eecs.northwestern.edu/~robby/lightweight-metatheory/ and the next release will contain yet better support testing.
        
        Speaking of the manual, tho, I'd be delighted if you were to share more specific criticism of it.
    - id: 6351
      author: "Sam Tobin-Hochstadt"
      date: "2014-01-14 16:21:33"
      content: "Redex's typesetting support, while I've certainly complained to Robby about it at times, is certainly nice enough to use to write papers about formalisms.  For example, [Takikawa et al, ESOP 2013]; [Strickland et al, OOPSLA 2012] and [Klein et al, POPL 2012] all have their formalism typeset in Redex."
    - id: 6352
      author: Edward Z. Yang
      date: "2014-01-14 17:00:04"
      content: |
        Robby: It's omitted because Ott doesn't support testing at all, so there'd be no comparison to make. Testing is definitely one of the prime reasons why you'd want to use Redex. As for the manual, I'll collect my thoughts and send you some feedback.
        
        Sam: Well, I suppose it gets the job done, which is perhaps all one could ask. (Though the output definitely looks very lispy :-)
    - id: 6556
      author: Grigore Rosu
      date: "2014-03-17 17:51:30"
      content: "The \"cough K Framework cough\" should probably also be dated, because these days K should be very easy to install.  Yes, it was a time when installing K was a serious task, but not anymore."
    - id: 6557
      author: Edward Z. Yang
      date: "2014-03-17 18:07:27"
      content: "Grigore: Yeah, looks like it's gotten much better. I've removed the jab."
---

[Ott](http://www.cl.cam.ac.uk/~pes20/ott/) and [PLT Redex](http://redex.racket-lang.org/) are a pair of complimentary tools for the working semanticist. Ott is a tool for writing definitions of programming languages in a nice ASCII notation, which then can be typeset in LaTeX or used to generate definitions for a theorem prover (e.g. Coq). PLT Redex is a tool for specifying and debugging operational semantics. Both tools are easy to install, which is a big plus. Since the tools are quite similar, I thought it might be interesting to do a comparison of how various common tasks are done in both languages. (Also, I think the Redex manual is pretty terrible.)

**Variables.** In Ott, variables are defined by way of metavariables (`metavar x`), which then serve as variable (by either using the metavariable alone, or suffixing it with a number, index variable or tick).

In Redex, there is no notion of a metavariable; a variable is just another production. There are a few different ways say that a production is a variable: the simplest method is to use `variable-not-otherwise-mentioned`, which automatically prevents keywords from acting as variables. There are also several other variable patterns `variable`, `variable-except` and `variable-prefix`, which afford more control over what symbols are considered variables. `side-condition` may also be useful if you have a function which classifies variables.

**Grammar.** Both Ott and Redex can identify ambiguous matches. Ott will error when it encounters an ambiguous parse. Redex, on the other hand, will produce all valid parses; while this is not so useful when parsing terms, it is quite useful when specifying non-deterministic operational semantics (although this can have bad performance implications). `check-redundancy` may be useful to identify ambiguous patterns.

**Binders.** In Ott, binders are explicitly declared in the grammar using `bind x in t`; there is also a binding language for collecting binders for pattern-matching. Ott can also generate substitution/free variable functions for the semantics. In Redex, binders are not stated in the grammar; instead, they are implemented solely in the reduction language, usually using substitution (Redex provides a workhorse substitution function for this purpose), and explicitly requiring a variable to be fresh. Redex does have a special-form in the metalanguage for doing let-binding (`term-let`), which substitutes immediately.

**Lists.** Ott supports two forms of lists: dot forms and list comprehensions. A dot form looks like `x1 , .. , xn` and requires an upper bound. A list comprehension looks like `</ xi // i IN 1 .. n />`; the bounds can be omitted. A current limitation of Ott is that it doesn’t understand how to deal with nested dot forms, this can be worked around by doing a comprension over a production, and then elsewhere stating the appropriate equalities the production satisfies.

Redex supports lists using ellipsis patterns, which looks like `(e ...)`. There is no semantic content here: the ellipses simply matches zero or more copies of `e`, which can lead to nondeterministic matches when there are multiple ellipses. Nested ellipses are supported, and simply result in nested lists. Bounds can be specified using side-conditions; however, Redex supports a limited form of bounding using named ellipses (e.g. `..._1`), where all ellipses with the same name must have the same length.

**Semantics.** Ott is agnostic to whatever semantics you want to define; arbitrary judgments can be specified. One can also define judgments as usual in Redex, but Redex provides special support for *evaluation semantics*, in which a semantics is given in terms of evaluation contexts, thus allowing you to avoid the use of structural rules. So a usual use-case is to define a normal expression language, extend the language to have evaluation contexts, and then define a `reduction-relation` using `in-hole` to do context decomposition. The limitation is that if you need to do anything fancy (e.g. [multi-hole evaluation contexts](https://github.com/iu-parfunc/lvars/tree/master/redex/lambdaLVar)), you will have to fall back to judgment forms.

**Type-setting.** Ott supports type-setting by translation into LaTeX. Productions can have custom LaTeX associated with them, which is used to generate their output. Redex has a `pict` library for directly typesetting into PDF or Postscript; it doesn’t seem like customized typesetting is an intended use-case for PLT Redex, though it can generate reasonable Lisp-like output.

**Conclusion.** If I had to say what the biggest difference between Ott and PLT Redex was, it is that Ott is primarily concerned with the abstract semantic meaning of your definitions, whereas PLT Redex is primarily concerned with how you would go about *matching* against syntax (running it). One way to see this is in the fact that in Ott, your grammar is a BNF, which is fed into a CFG parser; whereas in PLT Redex, your grammar is a pattern language for the pattern-matching machine. This should not be surprising: one would expect each tool’s design philosophy to hew towards their intended usage.
