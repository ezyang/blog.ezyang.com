---
title: "coqtop -emacs-U"
date: 2012-04-06 00:33:18
slug: coqtop-emacs-u
draft: true
categories: [Miscellaneous]
---

Because [Coq](http://coq.inria.fr/) has a rather nice interface in [ProofGeneral](http://proofgeneral.inf.ed.ac.uk/), one might labor under the false presumption that there is a clean, well-documented interchange format by which ProofGeneral (written in Emacs Lisp) and Coq (written in OCaml) communicate. You couldn't be more wrong.

In fact, Coq, like most research software, offers exceedingly little in the way of support for machine interchangeable formats. You're expected to understand Coq syntax yourself. However, it does give a teensy bit of support for ProofGeneral, however, in the form of the `-emacs` and `--emacs-U` flags (the latter denoting “Unicode friendliness”). The changes to the format are minimal, and the objective of this blog post is to document what these changes are.

The Coq interactive loop is one where you transmit it Vernacular commands (this includes tactics, when in proof editing mode),
