---
title: "What is a membrane?"
date: 2013-03-15 03:49:08
slug: what-is-a-membran
categories: [Programming Languages]
comments:
    - id: 5976
      author: Brian
      date: "2013-03-15 21:46:05"
      content: "Are you sure Miller wasn't drawing on Salzer's use of the term membrane forty years earlier?"
    - id: 5977
      author: Edward Z. Yang
      date: "2013-03-15 21:57:14"
      content: "Hard to say! It seems certainly plausible that the term membrane was already floating around in the folklore prior to the thesis; I haven’t been able to identify any earlier uses of the term in this context, but I’d be quite interested to hear about them!"
    - id: 5979
      author: Adam Foltzer
      date: "2013-03-18 23:17:16"
      content: |
        This reminds me quite a bit of Matthews &amp; Findler's work on the semantics of multi-language programs: http://www.cs.uchicago.edu/files/tr_authentic/TR-2006-10.fdf
        
        In this application, it's less about providing extra information along with the foreign object than it is about inserting necessary runtime checks for conversion between dynamic and static values. 
        
        This gets particularly interesting when converting arguments and return values, as you have to account for contravariance (in fact, I think this paper was my first exposure to contravariance). I am curious how this is implemented in an untyped setting; lots of dynamic checks?
    - id: 5980
      author: Edward Z. Yang
      date: "2013-03-18 23:38:36"
      content: "Interesting paper! It is hard for me to tell if there is a relationship here; however, I can say that at least for Mozilla’s membranes, wrapped objects can perform any assortment of dynamic checks before forwarding the request along."
    - id: 5995
      author: "Sam Tobin-Hochstadt"
      date: "2013-04-03 02:31:28"
      content: |
        [Belatedly]
        
        All of these systems are related.  Jacob's multi-language systems are really contract systems, and contract systems are very closely related to membranes.  The runtime tools used to implement one can implement the other -- see the paper on Virtual Values, or Tim Disney's contracts.coffee for contracts via proxies, and our paper on Chaperones and Impersonators (in Racket) for membranes via chaperones (designed originally for contracts).
    - id: 25189
      author: Mark S. Miller
      date: "2021-01-27 19:23:33"
      content: |
        &gt; Are you sure Miller wasn’t drawing on Salzer’s use of the term membrane forty years earlier?
        
        I was not. I don't know about Salzer's membranes. Where would I find it?
        
        Expanding on my note at https://twitter.com/marksammiller/status/1354577956253753344
        
        I coined the term and first use of it in a programming language. But I did not invent the concept.
        
        &gt; "[Membranes and more] presented in this chapter are an idealization of the relevant aspects of the KeySAFE design."
        
        which cites
        
        Susan A. Rajunas. The KeyKOS/KeySAFE System Design. Technical Report SEC009-01, Key Logic, Inc., March 1989.
        
        So first written about 1989. I don't know how much earlier was the actual invention.
    - id: 25190
      author: Mark S. Miller
      date: "2021-01-27 19:25:43"
      content: "And Sam is correct. The higher-order contract work is closely related. Both were happening at around the same time before we discovered each other. So perhaps I should retract \"First language-based use of the concept\"."
    - id: 25191
      author: Mark S. Miller
      date: "2021-01-27 19:27:12"
      content: "(Did you get my earlier comment? I don't see it above but I do see my second comment.)"
    - id: 25885
      author: Edward Z. Yang
      date: "2021-04-24 23:23:42"
      content: "Oops, I rarely check my comment queue, I let it through!"
---

If you hang out long enough with a certain crowd (in my case, it was the [ECMAScript TC39 committee](http://wiki.ecmascript.org/doku.php)), you will probably hear the term **membrane** tossed around. And eventually, you will start to wonder, “Well, what *is* a membrane, anyway?”

As is the case with many clever but simple ideas, membranes were first introduced as a footnote \[1\] in [a PhD thesis.](http://www.erights.org/talks/thesis/) Suppose that you are building distributed system, in which you pass references to objects between two separate nodes. If I want to pass a reference to `foo` in process `A` to process `B`, I can hardly just hand over an address—the memory spaces are not the same! So instead, I need to create a wrapper object `wrappedFoo` representing `foo` in `B`, which knows how to access the original object in `A`. So far so good.

Now here’s the catch: what if I pass a reference to `wrappedFoo` back to process `A`? If I were not very clever, I’d do the same thing as I did originally: create a new wrapper object `wrappedWrappedFoo` in `A` which knows how to access `wrappedFoo` in `B`. But this is silly; really, when I cross back over to `A`, I want to get back the original `foo` object.

This wrap-unwrap behavior is *precisely* what a membrane is. We consider the original object `foo` to be “inside” the membrane (a so-called wet object), and as it exits the membrane, it is wrapped with its own little membrane. However, when the object returns to its original membrane, the wrapper goes away. Just like in biology!

![image](http://upload.wikimedia.org/wikipedia/commons/thumb/1/1a/Endocytosis_types.svg/400px-Endocytosis_types.svg.png)

There is one last operation, called a “gate”: this occurs when you invoke a method on a wrapped object. Since the wrapper cannot actually perform the method, it has to forward the request to the original object. However, the *arguments* of the method need to get wrapped (or unwrapped) as they get forwarded; as you might expect.

While I used an RPC-like system to demonstrate the basic principle of membranes, a more conventional use is to enforce access control. Membranes are quite important; [Mozilla](https://developer.mozilla.org/en-US/docs/XPConnect_security_membranes) relies on them extensively in order to enforce access restriction between objects from different websites which may interact with each other, but need security checks. (Actually, did you know that Mozilla is using a capability-based system for their security? Kind of neat!) It’s important to notice that when we unwrap, we are skipping security checks—the only reason this is acceptable is because the only objects that will be able to access the unwrapped object are precisely those objects in the same domain. For a more modern treatment of the subject, check out a more recent paper, [Trustworthy Proxies: Virtualizing Objects with Invariants](http://research.google.com/pubs/pub40736.html), which includes a lucid explanation of membranes.

\[1\] Well, actually it was a figure; figure 9.3 on page 71, to be precise!
