---
title: "The difference between intensional and extensional type theory"
date: 2011-10-19 12:53:29
slug: difference-between-intensional-extensional
draft: true
categories: [Computer Science]
---

The words *intensional* and *extensional* sound very daunting. They’re not really.

The usual insight is that in extensional type theory, *type checking is undecidable*.

    Fixpoint get n (ls : ilist n) : fin n -> A :=
      match ls with
        | Nil => fun idx =>
          match idx in fin n' return (match n' with
                                          | O => A
                                          | S _ => unit
                                        end) with
            | First _ => tt
            | Next _ _ => tt
          end
        | Cons _ x ls' => fun idx =>
          match idx in fin n' return (fin (pred n') -> A) -> A with
            | First _ => fun _ => x
            | Next _ idx' => fun get_ls' => get_ls' idx'
          end (get ls')
      end.
