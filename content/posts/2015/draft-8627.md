---
title: "Rust linked lists"
date: 2015-03-02 17:26:40
slug: 
draft: true
categories: [Rust]
---

The most distinctive feature of the [Rust programming language](http://www.rust-lang.org/) is perhaps its capacity for *safe manual memory management.* (Here is a good [blog post](http://pcwalton.github.io/blog/2013/05/20/safe-manual-memory-management/) by Patrick Walton on the subject.) From my perspective, the promise of Rust is this: if you are willing to work with owned pointers and the borrow checker, then you will get proofs of memory safety for free. The analysis Rust does may not be as sophisticated as separation logic or a [dependently-typed low-level language](http://plv.csail.mit.edu/bedrock/), but that's fine: Haskell still manages to give useful guarantees without requiring dependent types.

What is important to remember, however, is that you have to *adjust the way you program*, in order to accomodate the static analysis. In some cases, these adjustments increase the clarity of the code, since they encourage code to be written in a way that’s obviously correct; in other cases, one has to fight with the type/borrow-checker in order to get the results you want, often because the borrow-checker is simply not smart enough to realize that something is safe. In this post, I want to talk about how to implement *mutable singly-linked lists* in Rust. I’ve written this post as a continuation of the section [Implementing a linked list](http://static.rust-lang.org/doc/master/tutorial.html#implementing-a-linked-list) in the Rust tutorial, so go read that first (along with the section on [Ownership](http://static.rust-lang.org/doc/master/tutorial.html#ownership)) if you haven’t already.

# Iterating over lists

In the tutorial, we’re given one example of how to compute equality over lists, which is presented recursively. An alternative is to consider how to perform this *iteratively*. This is not an academic exercise for a recursion-hater: Rust [does not support tail call optimization](https://github.com/mozilla/rust/issues/217) (although simple examples can get properly optimized by LLVM) and the iterator interface, utilized by for-loops, proceeds iteratively. First, we’ll code the loop by hand, and then show how we can use the iterator interface to factor out the boilerplate.

Intuitively, the iterative version of equality has two loop pointers (one for the first list and one for the second); the standard C code would look something like:

    // xs, ys input lists
    for (; xs = xs->link, ys = ys->link; xs != NULL && ys != NULL) {
        if (xs->elem != ys->elem) return false;
    }
    return (xs == ys);

# Conclusion

As a C programmer, I need to implement linked lists all the time: in Rust, this situation is a bit more unusual, since you’re expected to use the built-in containers that the standard library provides you (furthermore, in many situations, you actually wanted a vector, not a linked list.)
