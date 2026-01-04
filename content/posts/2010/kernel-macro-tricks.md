---
title: "Cute macro tricks in the kernel"
date: 2010-02-01 12:00:16
slug: kernel-macro-tricks
categories: [C, Hack]
comments:
    - id: 61
      author: nelhage
      date: "2010-02-01 14:12:41"
      content: |
        """This advice stems from the fact that a macro and an inline function can achieve the same effect, but the inline function also gets type checking."""
        
        There are other good reasons to prefer inline functions, including the issue with order of evaluations of parameters, and name leakage issues between the macro and its caller. Also, GCC is probably better at you than deciding whether a piece of code was worth inlining anyways.
        
        Also, that cute trick has limitations, such as probably only really working at top-level.
    - id: 62
      author: davidben
      date: "2010-02-01 16:20:40"
      content: "To say nothing about more coherent error messages. This reminds me a lot about similar tricks C++ template coders use, even though those tend to strangely be met with more scorn than C's version of the same sorts of tricks."
    - id: 63
      author: Edward Z. Yang
      date: "2010-02-01 16:33:50"
      content: |
        nelhage: All excellent reasons to opt for inline functions instead of macros.
        
        davidben: I've basically given up on getting coherent error messages from C compilers; as long as the error message puts me near the right line, I'm cool with that.
    - id: 2374
      author: Anonymous Cowherd
      date: "2011-05-06 14:36:08"
      content: |
        ezyang: A throw-away inline *NESTED* function named __check_##name enforces that p is the same type as type.
        
        nelhage: Nested functions are a GNU C extension. You're correct that this little trick wouldn't work in standard C.
---

A classic stylistic tip given to C programmers is that inline functions should be preferred over macros, when possible. This advice stems from the fact that a macro and an inline function can achieve the same effect, but the inline function also gets type checking.

As it turns out, you *can* achieve static type checking with macros, if you're willing to resort to the same cute trick that this following snippet from the Linux kernel uses:

    #define module_param_named(name, value, type, perm) \
            param_check_##type(name, &(value)); \
            module_param_call(name, param_set_##type, param_get_##type, &value, perm); \
            __MODULE_PARM_TYPE(name, #type)

Hmm... I wonder what that `param_check_##type` call is all about. Digging through a few more macro definitions, we see:

    #define __param_check(name, p, type) \
            static inline type *__check_##name(void) { return(p); }

So there you go. A throw-away inline function named `__check_##name` enforces that `p` is the same type as `type`. A comment is also given, explaining what's going on:

    /* The macros to do compile-time type checking stolen from Jakub
       Jelinek, who IIRC came up with this idea for the 2.4 module init code. */
