---
title: "The cost of weak pointers and finalizers in GHC"
date: 2014-05-04 04:55:36
slug: the-cost-of-weak-pointers-and-finalizers-in-ghc
categories: [GHC]
comments:
    - id: 6590
      author: Edward Kmett
      date: "2014-05-04 14:04:55"
      content: |
        I for one would love to see the global lock situation at last alleviated.
        
        I hadn't realized the cost of long chains of weak pointers though! That is rather troubling. I have some version control code in the style of https://www.google.com/search?q=revision+control+leijen&amp;oq=revision+control+leijen&amp;aqs=chrome..69i57.3815j0j7&amp;sourceid=chrome&amp;es_sm=91&amp;ie=UTF-8 that uses long chains of weak pointers.
    - id: 6591
      author: Edward Kmett
      date: "2014-05-04 14:05:22"
      content: "Er, I mean't to offer http://research.microsoft.com/en-us/projects/revisions as the link."
    - id: 6592
      author: Edward Z. Yang
      date: "2014-05-04 16:47:27"
      content: |
        Reading your comment, I realized there is probably a misunderstanding about chains of weak pointers that my post may have caused. If you have a chain of "simple" weak pointers (where the key and value are the same), the pessimal situation I describe cannot exist, because the destination of the weak pointer will always have a direct reference from a root (otherwise it'd be dead!) So the "chain of weak pointers" only applies when the key and value are disjoint, and the value of a weak pointer repeatedly causes the key of another to become live.
        
        I'll update the post with some pictures to make this clearer.
---

[Weak pointers and finalizers](http://community.haskell.org/~simonmar/papers/weak.pdf) are a very convenient feature for many types of programs. Weak pointers are useful for implementing memotables and solving certain classes of memory leaks, while finalizers are useful for fitting "allocate/deallocate" memory models into a garbage-collected language. Of course, these features don’t come for free, and so one might wonder what the *cost* of utilizing these two (closely related) features are in GHC. In this blog post, I want to explain how weak pointers and finalizers are implemented in the GHC runtime system and characterize what extra overheads you incur by using them. These post assumes some basic knowledge about how the runtime system and copying garbage collection work.

# The userland API

The API for weak pointers is in [System.Mem.Weak](http://hackage.haskell.org/package/base-4.7.0.0/docs/System-Mem-Weak.html); in its full generality, a weak pointer consists of a key and a value, with the property that if the key is alive, then the value is considered alive. (A "simple" weak reference is simply one where the key and value are the same.) A weak pointer can also optionally be associated with a finalizer, which is run when the object is garbage collected. Haskell finalizers are not guaranteed to run.

Foreign pointers in [Foreign.ForeignPtr](http://hackage.haskell.org/package/base-4.7.0.0/docs/Foreign-ForeignPtr-Safe.html) also have a the capability to attach a C finalizer; i.e. a function pointer that might get run during garbage collection. As it turns out, these finalizers are also implemented using weak pointers, but C finalizers are treated differently from Haskell finalizers.

# Representation of weak pointers

A weak pointer is a special type of object with the following layout:

    typedef struct _StgWeak {   /* Weak v */
      StgHeader header;
      StgClosure *cfinalizers;
      StgClosure *key;
      StgClosure *value;        /* v */
      StgClosure *finalizer;
      struct _StgWeak *link;
    } StgWeak;

As we can see, we have pointers to the key and value, as well as separate pointers for a single Haskell finalizer (just a normal closure) and C finalizers (which have the type `StgCFinalizerList`). There is also a link field for linking weak pointers together. In fact, when the weak pointer is created, it is added to the nursery's list of weak pointers (aptly named `weak_ptr_list`). As of GHC 7.8, this list is global, so we do have to [take out a global lock](https://ghc.haskell.org/trac/ghc/ticket/9075#ticket) when a new weak pointer is allocated; however, the lock has been removed in HEAD.

# Garbage collecting weak pointers

Pop quiz! When we do a (minor) garbage collection on weak pointers, which of the fields in `StgWeak` are considered pointers, and which fields are considered non-pointers? The correct answer is: only the first field is considered a “pointer”; the rest are treated as non-pointers by normal GC. This is actually what you would expect: if we handled the key and value fields as normal pointer fields during GC, then they wouldn’t be weak at all.

Once garbage collection has been completed (modulo all of the weak references), we then go through the weak pointer list and check if the keys are alive. If they are, then the values and finalizers should be considered alive, so we mark them as live, and head back and do more garbage collection. This process will continue as long as we keep discovering new weak pointers to process; however, this will only occur when the key and the value are different (if they are the same, then the key must have already been processed by the GC). Live weak pointers are removed from the "old" list and placed into the new list of live weak pointers, for the next time.

Once there are no more newly discovered live pointers, the list of dead pointers is collected together, and the finalizers are scheduled (`scheduleFinalizers`). C finalizers are run on the spot during GC, while Haskell finalizers are batched together into a list and then shunted off to a freshly created thread to be run.

That's it! There are some details for how to handle liveness of finalizers (which are heap objects too, so even if an object is dead we have to keep the finalizer alive for one more GC) and threads (a finalizer for a weak pointer can keep a thread alive).

# Tallying up the costs

To summarize, here are the extra costs of a weak pointer:

1.  Allocating a weak pointer requires taking a global lock (will be fixed in GHC 7.10) and costs six words (fairly hefty as far as Haskell heap objects tend to go.)
2.  During each minor GC, processing weak pointers takes time linear to the size of the weak pointer lists for all of the generations being collected. Furthermore, this process involves traversing a linked list, so data locality will not be very good. This process may happen more than once, although once it is determined that a weak pointer is live, it is not processed again. The cost of redoing GC when a weak pointer is found to be live is simply the cost of synchronizing all parallel GC threads together.
3.  The number of times you have to switch between GC'ing and processing weak pointers depends on the structure of the heap. Take a heap and add a special "weak link" from a key to its dependent weak value. Then we can classify objects by the minimum number of weak links we must traverse from a root to reach the object: call this the "weak distance". Supposing that a given weak pointer's weak distance is n, then we spend O(n) time processing that weak pointer during minor GC. The maximum weak distance constitutes how many times we need to redo the GC.

In short, weak pointers are reasonably cheap when they are not deeply nested: you only pay the cost of traversing a linked list of all of the pointers you have allocated once per garbage collection. In the pessimal case (a chain of weak links, where the value of each weak pointer was not considered reachable until we discovered its key is live in the previous iteration), we can spend quadratic time processing weak pointers.
