---
title: "How to implement a memo table with key-value weak pointers"
date: 2017-07-29 11:55:23
slug: 
draft: true
categories: [Haskell]
---

A weak pointer is a pointer which does not retain the object it refers to.

In the paper, they say the reason for this extra complexity with weak pointers of introducing a key and value, is due to the key-in-value problem. That is, the normal memo approach would be to have a weak pointer to the key, have a finalizer on the key to delete the value, and a normal pointer to the value.

The key-in-value problem is, when the value itself contains a pointer to the key, then we have a strong reference chain (memo tbl -\> value -\> key) preventing the key ever being collected.

This doesn't make sense to me, I feel like I'm being dumb and missing something. The paper gives the following definition for Haskell weak pointers:

- (3 quoted lines)

To me this directly contradicts the motivation and example they gave. I was thinking that somehow, Haskell weak pointers did an auto conversion of any pointer from the value to the key into a weak pointer. But basically it's saying, we don't do anything. So if the value is reachable "some other way" (e.g., the memo table has a strong reference as the motivating example just said), then the value is live and keeps the key live.... ???
