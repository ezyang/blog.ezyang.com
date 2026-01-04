---
title: "Space leak zoo"
date: 2011-05-16 09:00:28
slug: space-leak-zoo
categories: [Space Leak]
comments:
    - id: 2432
      author: Anonymous
      date: "2011-05-16 11:10:08"
      content: |
        Very nice post. I'm awaiting for the sequel.
        
        Only this part wasn't clear to me: 
        
        "These leaks thrive on laziness and intermediate data structures, but, unlike the case of thunk leaks, thunks are good and should be preserved. You can fix them by duplicating work and carefully tracking data dependencies."
        
        Could you clarify?
    - id: 2433
      author: Simon Michael
      date: "2011-05-16 11:16:53"
      content: "Yes I did enjoy this post. Very useful, I look forward to more zoology!"
    - id: 2434
      author: Nick Frisby
      date: "2011-05-16 11:26:49"
      content: |
        I was expecting your hints about fixing stack overflows to mention thunks. The traditional example I'm aware of (i.e. main2 from the previous post) is essentially solved by the strictness annotation in foldl'.
        
        Did you omit that on purpose?
    - id: 2435
      author: Hai
      date: "2011-05-16 12:31:04"
      content: "WHERE'S THE PICTURES?"
    - id: 2436
      author: Anonymous
      date: "2011-05-16 12:50:30"
      content: "Less pretentious prose next time, please."
    - id: 2437
      author: Edward Z. Yang
      date: "2011-05-16 16:26:09"
      content: |
        Anonymous 1: Thunks are good not in the sense that it's fine if they show up in the heap profile, but good in the sense that when we're streaming data, we should only have the current bit of data we're working with, and the thunk to produce the next bit. Hopefully the next post will make it a bit clearer.
        
        Nick: Yes; I'd like to distinguish between thunk leaks and stack overflow leaks. I've clarified the text a little. You can stack overflow a strict functional language too, but thunk leaks are a special kind of stack overflow that can show up even if your code is written iteratively.
        
        Hai: In the future.
        
        Anonymous 2: In Soviet Russia, pretentious prose writes you! (I'll try.)
    - id: 2440
      author: Alessandro Stamatto
      date: "2011-05-16 21:02:27"
      content: "I did not find the prose pretentious, i found it fun! ; )"
    - id: 2448
      author: Rafael
      date: "2011-05-18 01:51:08"
      content: |
        Thanks for the clarification (I'm ex-Anonymous 1), though I'll still need to check next post for an example. 
        
        By the way, your prose isn't pretentious.
    - id: 2476
      author: Paul Liu
      date: "2011-05-20 12:34:46"
      content: |
        I am surprised that you didn't mention loss of sharing of function application as being one, if not the most common, source of memory leaks. It's also the entire reason for memoization to exist, just consider the general type for memo :: (a -&gt; b) -&gt; a -&gt; b, and the many cases where memoization (in the form of using a memo function or knot tying) helps to seal  leaks in recursive functions.
        
        Or if you do, which category does it belong?
    - id: 2478
      author: Edward Z. Yang
      date: "2011-05-20 13:14:55"
      content: "Loss, or gain? I suspect you may be talking about time performance; memoization usually causes greater space usage."
    - id: 2479
      author: Paul Liu
      date: "2011-05-20 15:31:48"
      content: |
        Memoization does not imply great space usage when used properly. The classic example:
        
        fib n = case n of { 0 -&gt; 0; 1 -&gt; 1; _ -&gt; fib (n - 1) + fib (n - 2) }
        
        fib' n = let l = 0 : 1 : zipWith (+) l (tail l) in l !! n
        
        The above fib' is less leaky than fib since it uses a list to memoize previous computation steps.
        
        Not all leaks are as obvious as in fib, where work is apparently not shared. Spotting them may get really tricky in nested or mutual recusions.
        
        On the other hand, not all leaks due to loss of sharing can be easily solved by memoization. As a shameless self-plug, see the paper Plugging a Space Leak with Arrows and Ode to Arrows for more examples.
    - id: 2480
      author: Paul Liu
      date: "2011-05-20 15:41:15"
      content: "Hmm, let me re-think your statement. Maybe you only meant space leaks without time leaks? Usually they come hand in hand though."
    - id: 2482
      author: Edward Z. Yang
      date: "2011-05-21 11:53:58"
      content: "Your example will have lots of stack usage. Also, fib' will leak memory if you accidentally retain the whole list (which could happen with a full laziness transformation). This is why I want to avoid talking about stack overflows: while they can be related to space leaks, they really are different beasts."
    - id: 2483
      author: Paul Liu
      date: "2011-05-21 18:29:06"
      content: |
        Yes, full laziness, or more precisely, the MFE (Maximum Free Expression) transformation does not preserve the space behavior of the original program, and thus can be dangerous. 
        
        fib is bad not because of stack overflow. But never the less, I agree that it is more of a time leak in this case.
    - id: 2585
      author: Lemming
      date: "2011-06-04 19:05:25"
      content: "You define \"memory leak\" and say its not a problem with modern garbage collectors. As far as I have observed, GHC's runtime system never gives freed heap memory back to OS. Am I right?"
    - id: 2587
      author: Edward Z. Yang
      date: "2011-06-04 19:12:07"
      content: "GHC will hold onto memory for a little bit, even if it's not being used, so that it doesn't rapidly release and then re-request memory if the heap grows again. Eventually, it will release memory to the OS. This was not the case for Windows, but was fixed recently: http://hackage.haskell.org/trac/ghc/ticket/698"
    - id: 4008
      author: "Space leaks, and Writers, and Sums (oh my!)"
      date: "2012-08-22 06:15:42"
      content: "[...] may have my terminology wrong here. According to Space leak zoo, my problem would be classified as a &#8216;stack overflow&#8217;, and if that&#8217;s the case, [...]"
    - id: 4067
      author: Quora
      date: "2012-09-03 07:41:26"
      content: |
        <strong>Erlang, Haskell, OCaml: screw one, marry one, kill one. Which and why?...</strong>
        
        I've been screwing Erlang daily for about 4.5 months now, built in applications, compiler tool-chain, binary handling, and all, after stalking its profiles for over a year. After figuring out how an OTP application works, I took a break. I've been ch...
    - id: 21995
      author: "Lazy evaluation | 江苏荣华投资公司"
      date: "2017-05-24 22:32:26"
      content: "[&#8230;] Jump up^ Edward Z. Yang. &#8220;Space leak zoo&#8221;. [&#8230;]"
---

A big thanks to everyone who everyone who [sent in space leak specimens](http://blog.ezyang.com/2011/05/calling-all-space-leaks/). All of the leaks have been inspected and cataloged by our experts, and we are quite pleased to open the doors of the space leak zoo to the public!

There are a few different types of space leak here, but they are quite different and a visitor would do well not to confuse them (the methods for handling them if encountered in the wild vary, and using the wrong technique could exacerbate the situation).

- A *memory leak* is when a program is unable to release memory back to the operating system. It's a rare beast, since it has been mostly eliminated by modern garbage collection. We won’t see any examples of it in this series, though it is strictly possible for Haskell programs to exhibit this type of leak if they use non-bracketed low-level FFI allocation functions.
- A *strong reference leak* is when a program keeps around a reference to memory that in principle could be used but actually will never be used anymore. A confluence of purity and laziness make these types of leaks uncommon in idiomatic Haskell programs. Purity sidesteps these leaks by discouraging the use of mutable references, which can leak memory if they are not cleared when appropriate. Laziness sidesteps these leaks by making it difficult to accidentally generate too much of a data structure when you only want parts of it: we just use less memory to begin with. Of course, using mutable references or strictness in Haskell can reintroduce these errors (sometimes you can fix instances of the former with weak references—thus the name, “strong reference” leak), and live variable leaks (described below) are a type of strong reference leak that catch people unfamiliar with closures by surprise.
- A *thunk leak* is when a program builds up a lot of unevaluated thunks in memory which intrinsically take up a lot of space. It can be observed when a heap profile shows a large number of `THUNK` objects, or when you stack overflow from evaluating a chain of these thunks. These leaks thrive on lazy evaluation, and as such are relatively rare in the imperative world. You can fix them by introducing appropriate strictness.
- A *live variable leak* is when some closure (either a thunk or a lambda) contains a reference to memory that a programmer expects to have been freed already. They arise because references to memory in thunks and functions tend to be implicit (via live variables) as opposed to explicit (as is the case for a data record.) In instances where the result of the function is substantially smaller, these leaks can be fixed by introducing strictness. However, these are not as easy to fix as thunk leaks, as you must ensure *all* of the references are dropped. Furthermore, the presence of a large chunk of evaluated memory may not necessarily indicate a live variable leak; rather, it may mean that streaming has failed. See below.
- A *streaming leak* is when a program should only need a small amount of the input to produce a small amount of output, thus using only a small amount of memory, but it doesn’t. Instead, large amounts of the input are forced and kept in memory. These leaks thrive on laziness and intermediate data structures, but, unlike the case of thunk leaks, introducing strictness can exacerbate the situation. You can fix them by duplicating work and carefully tracking data dependencies.
- A *stack overflow* is when a program builds up a lot of pending operations that need to performed after the current execution. It can be observed when your program runs out of stack space. It is, strictly speaking, not a space leak, but an improper fix to a thunk leak or a streaming leak can convert it into a stack overflow, so we include it here. (We also emphasize these are not the same as thunk leaks, although sometimes they look the same.) These leaks thrive on recursion. You can fix them by converting recursive code to iterative style, which can be tail-call optimized, or using a better data structure. Turning on optimization also tends to help.
- A *selector leak* is a sub-species of a *thunk leak* when a thunk that only uses a subset of a record, but because it hasn't been evaluated it causes the entire record to be retained. These have mostly been killed off by the treatment of GHC’s *selector thunks* by the RTS, but they also occasionally show due to optimizations. (See below.)
- An *optimization induced leak* is a camouflaged version of any of the leaks here, where the source code claims there is no leak, but an optimization by the compiler introduces the space leak. These are very tricky to identify; we would not put these in a petting zoo! (You can fix them by posting a bug to GHC Trac.)
- A *thread leak* is when too many Haskell threads are lying around. You can identify this by a contingent of TSO on the heap profile: TSO stands for thread-state object. These are interesting to debug, because there are a variety of reasons why a thread may not dying.

In the next post, we’ll draw some pictures and give examples of each of these leaks. As an exercise, I invite interested readers to categorize the [leaks we saw last time](http://blog.ezyang.com/2011/05/calling-all-space-leaks/).

*Update.* I’ve separated thunk leaks from what I will now call “live variable leaks,” and re-clarified some other points, especially with respect to strong references. I’ll expand on what I think is the crucial conceptual difference between them in later posts.
