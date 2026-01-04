---
title: "Why your sandbox needs monads"
date: 2013-04-07 03:24:44
slug: why-your-sandbox-needs-monads
draft: true
categories: [Haskell]
---

There are not many programming languages which support sandboxed execution of untrusted code: JavaScript, Java and Haskell are perhaps the most prominent examples of languages which sport these capabilities out of the box. These languages provide for quite an interesting contrast, since the way Java goes about implementing sandboxing is remarkably different from how Haskell goes about it: Java uses *stack inspection* while Haskell uses *restricted IO monads*. In this post, I’d like to compare these two systems and show how they use different mechanisms to achieve similar effects. From this comparison, I argue that restricted IO monads are in fact a *subsuming* security mechanism over stack inspection. No prior knowledge about monads or Haskell is necessary—in fact, if you’re just learning about monads, I hope this post will give you some useful insight about monads.

# Java stack inspection

Could you have invented stack inspection? Maybe... on a good day.

The basic goal of a sandbox is this: given a method such as `fireMissiles()`, we would like to allow trusted code to invoke this method while forbidding untrusted code from doing so. One obvious way to implement this is to have some sort of “trusted/untrusted flag” which indicates whether or not we are running trusted code or not. However, implementing this as a flag directly is a bad idea, for multiple reasons. Consider this strawman API:

    function setTrusted() { trusted = true; }
    function setUntrusted() { trusted = false; }

The very first problem is that if these are implemented as ordinary functions, anyone can call `setTrusted` and break out of the sandbox. Nor can we restrict calling `setTrusted` to only trusted code: running untrusted code would then be a one-way trip, with no way of becoming trusted again!

Suppose that we had some extralingual mechanism for controlling whether or not you could call `setTrusted` or `setUntrusted`—in Java, all code can potentially be signed by their author, and we might decide only to allow calls to these functions from particular authors. The next problem would be a subtle bug (familiar to anyone who ever written the former code) when `doSomething()` also fiddled with the trust flag (suppose that it is in a trusted module) in the following example:

    setUntrusted();
    doSomething();
    doSomethingElse();
    setTrusted();

`doSomethingElse()` would run as trusted code, since the nested `setTrusted` flipped the flag prematurely!

This issue is relatively simple to patch up, by recording the previous trusted/untrusted state:

    function setUntrusted() {
      var old = trusted;
      trusted = false;
      return old;
    }
    function restoreTrust(old) { trusted = old; }

    var old = setUntrusted();
    doSomething();
    doSomethingElse();
    restoreTrust(old);

With *higher-order functions*, we can clean this up quite nicely:

    function sandbox(callback) {
      var old = trusted;
      trusted = false;
      var r = callback();
      trusted = old;
      return r;
    }
    sandbox(function() {
      doSomething();
      doSomethingElse();
    });

In this form, it is clear to see: when this code is run, the function `withExceptionEnabled` remains as a *stack frame* on the stack while `doSomething` and `doSomethingElse` are executing. This stack frame is responsible for restoring the original value of the flag once the scope exits. Now, suppose you had the ability to look at the current stack frames and pick out this return frame: in such a case, it wouldn’t even be necessary to maintain the global variable `exceptionsEnabled`; as long as there is a `withExceptionsEnabled` frame on the stack, they are enabled; otherwise, they are not. This *stack walking* is the essence of Java’s stack inspection algorithm.

However, things become a little more complicated when we add indirection: suppose we have some “trusted” method `checkDefense()` which ends up calling `fireMissiles()`—if untrusted code calls `checkDefense()`, should we allow the indirect call to `fireMissiles()`? If the answer is always no, then once we are inside an untrusted code region, there is no longer any way to perform any sort of privileged operation. If the answer is always yes, then it would be relatively simple to accidentally leak the ability to delete files: all that is necessary is one extra utility function that was not access controlled to .
