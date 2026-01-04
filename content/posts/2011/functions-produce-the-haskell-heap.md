---
title: "Functions produce the Haskell Heap"
date: 2011-04-27 11:18:38
slug: functions-produce-the-haskell-heap
categories: [Haskell Heap]
comments:
    - id: 2193
      author: Anonymous
      date: "2011-04-27 12:26:50"
      content: |
        Please keep doing this. It helps somewhat. But your text doesn't always explain the graphics. And the graphics are often indecipherable. The meaning of the pictures you are drawing may be obvious to you, but I rarely know what you are talking about.
        
        Examples:
        
        "Standard operating habits for the Strict-o-matic can cause death and/or serious injury if applied to the Ghost-o-matic." I guess I don't understand how you apply these "machines" to one another.
        
        "The curious case of seq x x"  I'm not getting the important ah ha! moment from the graphic.
        
        "Why do former users of the Strict-o-matic ask the seq ghost to do such silly things? This is because when you used the Strict-o-matic, the machine itself was haunted."   Oh no, now I'm completely lost.
        
        I've only been working with haskell for 2 or 3 weeks. Perhaps the target audience is someone more familiar with it.
        
        Thanks for taking the time to do this however.
    - id: 2197
      author: Edward Z. Yang
      date: "2011-04-27 12:37:08"
      content: |
        I've reworded the text to help out. If you have more specific examples, please do tell.
        
        > I guess I don’t understand how you apply these “machines” to one another.
        
        Bad word choice. I don't mean applied in the sense of function application, I meant as in "using your same mental model in one for the other."
        
        > I’m not getting the important ah ha! moment from the graphic.
        
        If you don't really see why you would want to use seq x x, I suppose there's not really an ah-ha moment. I've added an explanation why users are tempted to write seq x x.
        
        > Oh no, now I’m completely lost.
        
        I've explained precisely what it means for the Strict-o-matic to be haunted. "To be haunted" means the act of running the machine causes an effect. In Haskell, function application never causes effects. In a strict language, function application can cause effects.
        
        Someone who has been working with Haskell for 2 or 3 weeks is a great use-case. I hope I can keep this series useful even as we go into the deep end.
    - id: 2207
      author: yachris
      date: "2011-04-27 17:29:49"
      content: |
        Also a newby to Haskell, and enjoying this a lot; I think I'll have to come back once I've done (a lot) more Haskell to really get it all.
        
        Importantly: Is the strict-o-matic a reference to imperative languages, or to something in Haskell?
        
        And...
        
        You missed a great chance to do a "Ghost in the Shell" reference :-)
    - id: 2208
      author: Edward Z. Yang
      date: "2011-04-27 17:31:31"
      content: |
        Strict-o-matic refers to strict functions in most traditional languages, yep.
        
        I know! I’ve also missed a Serial Experiments Lain reference: “Present Day, Present Time!”
    - id: 2209
      author: djahandarie
      date: "2011-04-27 19:36:24"
      content: |
        It's okay, you can make it up by ending your series with a reference to Marcel Proust. That's a good enough homage to Lain. 
        
        Great work, by the way. :) A little extra text wouldn't hurt in certain cases. I really like the "(this is the actual name of what we're talking about)" labels.
    - id: 2223
      author: Andrew Pennebaker
      date: "2011-04-28 05:02:51"
      content: "This one almost lost me. Is the Ghost-o-matic IO, and is Strict-o-matic ordinary Haskell forms?"
    - id: 2226
      author: Edward Z. Yang
      date: "2011-04-28 10:19:32"
      content: "No. Ghost-o-matic is all functions in Haskell, and Strict-o-matic is functions in *other* programming languages. The question of IO is a knotty question that I'm not bothering with at the moment."
    - id: 2231
      author: Anonymous Cowherd
      date: "2011-04-28 17:23:12"
      content: |
        I'm not a Haskell user, but I have a CS degree. ;)  I think a lot of the confusion over your "seq x x" digression is coming from the fact that you use the word "seq" to refer both to the Ghost-o-matic (the seq function) and the ghost itself (the thunk associated with (seq x x)).  It's also unclear what you mean by "User asks the seq ghost to do something".  I assume that's a euphemism for "User opens a package haunted by the seq ghost", but it's definitely unclear. Ghosts only ever do one preprogrammed thing, right, so you can never "ask" a ghost to do anything in the natural sense. Either the ghost ignores you, or it wakes up and does its preprogrammed thing; you can't really talk to these ghosts.
        
        IMO, this paragraph --- "Seq is a Ghost-o-matic machine [...] would have opened it anyway!" --- should read more like this:
        
        Some Haskell beginners are uncomfortable with the whole idea of unopened presents. They want to be able to write "x" and know that x has now been opened. By reading the documentation, they discover that one particular Ghost-o-matic machine, "seq", produces presents haunted by a ghost who makes sure both presents it knows about are opened (and then rewards you with the contents of the second present).  So the beginner naively thinks that writing "seq x x" will make sure that x is really opened.  But it doesn't do that at all!  You see, "seq" is simply a Ghost-o-matic machine that produces a haunted present!  You still need to open *that* present yourself (or get a ghost to do it), before the seq ghost will go and open your original present x.  The seq ghost, like all other ghosts in Haskell, is very lazy and doesn't open anything until you yourself open the present it's haunting.
    - id: 2233
      author: Edward Z. Yang
      date: "2011-04-28 20:38:56"
      content: "Thanks for all the comments. It seems clear that the attempt at a whimsical, \"operational manual\" style presentation was more confusing than amusing. As such, I've rewritten substantial bits of the post and added some more pictures. Hope it's better now!"
    - id: 2240
      author: Jason Dusek
      date: "2011-04-29 00:47:32"
      content: |
        These are so amusing. I've used Haskell for many years and yet I find these posts really help me to understand and articulate laziness better than I have before.
        
        Maybe it's simply too early in your series; but I notice you mention the Ghost-o-matic as strictly more powerful than the Strict-o-matic  but don't go so far as to talk about control flow...
    - id: 2244
      author: gasche
      date: "2011-04-29 03:07:07"
      content: |
        &gt; The lazy Ghost-o-matic machine is split into two discrete phases:
        &gt; the function application, which doesn’t actually do anything, just 
        &gt; creates the present, and the actual opening of the present. The 
        &gt; Strict-o-matic does it all in one bang. This is one reason why the 
        &gt; Ghost-o-matic machine is more powerful than the Strict-o-matic 
        &gt; machine.
        
        That's not true. That may be true if you consider Strict-of-matic some second-class citizen of an ambiant lazy language. In strict languages with some support for lazyness, the machine returns a value right away, but that value may be a present itself. With this point of view, the Strict-o-matic is actually "more expressive" than the Lazy-o-matic, as the Lazy-o-matic can be decomposed in two atomic steps (as you explained), one of them being just a strict-o-matic.
        
        This is in particular how the "strict Core" language works. The whole explanation of why this is a more flexible choice counters you powerfulness argument.
        
        I don't think one can say that one choice is "strictly more powerful" than the other, but I would claim there is a consensus that building lazyness on top of strictness is more practical (easier to understand, implement and manipulate) than the other way around.
    - id: 2245
      author: Edward Z. Yang
      date: "2011-04-29 04:34:37"
      content: |
        Jason: Ah yes. Control operators will have to be done some time.
        
        gasche: Yes, that was a little ill-founded—I was thinking of the case when a strict machine didn’t return a think, but of course, there’s no reason why it can’t. I’ve reworded it.
    - id: 2255
      author: Anthony
      date: "2011-04-29 21:46:30"
      content: "Will you please implement this in Haskell (with the same cool terminology) later?"
    - id: 2337
      author: gasche
      date: "2011-05-04 09:41:53"
      content: |
        Well, sorry to come a bit late, but I have read your modified version and it still isn't satisfying. I don't know if it's right or wrong, because I no longer understand what it means.
        
        &gt; The Strict-o-matic does it all in one bang—although it could output a present (that’s what happens when you implement laziness inside a strict language). But in a strict language, you have to do it all yourself.
        
        (It's funny how tedious repetitions can sneak in otherwise well-written pieces when you start patching them pieces by pieces instead of writing them in one go.)
        
        I re-read the whole Strict-o-machine thing and I'm not sure anymore this paragraph makes sense. What is the strict-o-machine meant to represent? Is there any difference between the "strict-o-machines" of strict languages and Haskell's one?
        
        We usually separate call-by-value and call-by-name by what's going *in* the machine, not the output. Strict-o-machine takes opened present (evaluated values) (which of course may be thunks themselves if explicitly requested), while lazy machines accept any present (thunk). Before giving a present to a strict-o-machine, you have to open it.
        
        Does the lazy machine wraps its output as a present, or is the output *already* wrapped because it's described, in the machine plans, as a present? I don't know how Haskell does that. I certainly don't believe that the strict-o-machine first builds a present, then opens it, which is how your explanation might be interpreted.
    - id: 2339
      author: Edward Z. Yang
      date: "2011-05-04 11:10:04"
      content: |
        > What is the strict-o-machine meant to represent?
        
        It is a function in a strict programming language. I think part of the conclusion here is I've never explicitly presented what the mental model of the execution of a strict language is: go through each statement step by step, whenever you see a function run the machine, etc.
        
        > Is there any difference between the “strict-o-machines” of strict languages and Haskell’s one?
        
        Yes. If I'm executing a strict program and I reach a function, when I operate the function, side effects happen. When I'm executing a Haskell program, and I encounter a 'let', I run all of the functions but everything I get back are thunks, to be used later. This is a lot clearer when you have a bunch of case-analyses, and then a big "where" section whose variables are used in various places but not all at once.
        
        > We usually separate call-by-value and call-by-name by what’s going *in* the machine, not the output.
        
        Agreed. The failure to get that across is mine alone.
        
        > Does the lazy machine wraps its output as a present, or is the output *already* wrapped because it’s described, in the machine plans, as a present?
        
        Lazy machines only ever output presents. Indeed, there's never any output to "wrap", since it's the ghosts job to go figure that out.
        
        > I certainly don’t believe that the strict-o-machine first builds a present, then opens it, which is how your explanation might be interpreted.
        
        Right, it skips the present creation process, which makes it more efficient.
    - id: 2463
      author: Joao
      date: "2011-05-19 07:18:52"
      content: |
        Thanks for taking the time to write this! I've just found the series; I'll read it from the first post on.
        
        The drawings look great, too; are you using your X61 tablet? Which app. do you use?
        
        Thanks,
        Joao
    - id: 2470
      author: Edward Z. Yang
      date: "2011-05-19 12:27:24"
      content: "Joao: Yep, with Xournal."
    - id: 2472
      author: Anonymous
      date: "2011-05-20 00:58:52"
      content: |
        Are you still taking space leaks for the zoo?
        How about a thread leak? (a fork bomb)
        Something like
        
        leakySam=do
         forkIO leakSam
         leakySam
    - id: 2473
      author: Edward Z. Yang
      date: "2011-05-20 04:53:39"
      content: "Ah yes, thread leaks are a good one. I wasn't even thinking about that."
---

<div class="container center">

New to the series? Go [to the beginning.](http://blog.ezyang.com/2011/04/the-haskell-heap/)

</div>

We’ve talked about how we open (evaluate) presents (thunks) in the Haskell Heap: we use IO. But where do all of these presents come from? Today we introduce where all these presents come from, the Ghost-o-matic machine (a function in a Haskell program).

![image](/img/heap/function.png)

Using a function involves three steps.

![image](/img/heap/function-howto.png)

We can treat the machine as a black box that takes present labels and pops out presents, but you can imagine the inside as having an unlimited supply of identical ghosts and empty present boxes: when you run the machine, it puts a copy of the ghost in the box.

![image](/img/heap/function-assembly-line.png)

If the ghosts we put into the presents are identical, do they all behave the same way? Yes, but with one caveat: the actions of the ghost are determined by a script (the original source code), but inside the script there are holes that are filled in by the labels you inserted into the machine.

![image](/img/heap/hamlet-ghost.png)

Since there’s not actually anything in the boxes, we can precisely characterize a present by the ghost that haunts it.

![image](/img/heap/ghost-anatomy.png)

A frequent problem that people who use the Ghost-o-matic run into is that they expect it to work the same way as the Strict-o-matic (a function in a traditional, strictly evaluated language.) They don’t even take the same inputs: the Strict-o-matic takes unwrapped, unhaunted (unlifted) objects and gift cards, and outputs other unhaunted presents and gift cards.

![image](/img/heap/strict-function.png)

But it’s really easy to forget, because the source-level syntax for strict function application and lazy function application are very similar.

![image](/img/heap/function-danger.png)

This is a point that must be thoroughly emphasized. In fact, in order to emphasize it, I’ve drawn two more pictures to reiterate what the permitted inputs and outputs for a Ghost-o-matic machine are.

Ghost-o-matics take labels of presents, not the actual presents themselves. This importantly means that the Ghost-o-matic doesn’t open any presents: after all, it only has labels, not the actual present. This stands in contrast to a Strict-o-matic machine which takes presents as inputs and opens them: one might call this machine the `force` function, of type `Thunk a -> a`. In Haskell, there is no such thing.

![image](/img/heap/function-allowed-input.png)

The Ghost-o-matic always creates a wrapped present. It will never produce an unwrapped present, even if there is no ghost haunting the present (the function was a constant).

![image](/img/heap/function-allowed-output.png)

We state previously that there is no `force` function in Haskell. But the function `seq` seems to do something very like forcing a thunk. A present haunted by a seq ghost, when opened, will cause two other presents to be opened (even if the first one is unnecessary). It seems like the first argument is forced; and so `seq x x` might be some reasonable approximation of `force` in an imperative language. But what happens when we actually open up a present haunted by the `seq` ghost?

![image](/img/heap/seq-x-x.png)

Although the ghost ends up opening the present rather than us, it’s too late for it to do any good: immediately after the ghost opens the present, we would have gone to open it (which it already is). The key observation is that the `seq x x` ghost only opens the present `x` when the present `seq x x` is opened, and immediately after `seq x x` is opened we have to go open `x` by means of an indirection. The strictness of the seq ghost is defeated by the fact that it’s put in a present, not to be opened until `x` is desired.

One interesting observation is that the Strict-o-matic machine does things when its run. It can open presents, fire missiles or do other side effects.

![image](/img/heap/strict-seq.png)

But the Ghost-o-matic machine doesn’t do any of that. It’s completely pure.

To prevent confusion, users of the Strict-o-matic and Ghost-o-matic machines may find it useful to compare the the present creation life-cycle for each of the machines.

![image](/img/heap/creation-cycle.png)

The lazy Ghost-o-matic machine is split into two discrete phases: the function application, which doesn’t actually do anything, just creates the present, and the actual opening of the present. The Strict-o-matic does it all in one bang—although it could output a present (that’s what happens when you implement laziness inside a strict language). But in a strict language, you have to do it all yourself.

The Ghost-o-matic is approved for use by both humans and ghosts.

![image](/img/heap/equal-opportunity.png)

This does mean that opening a haunted present may produces more presents. For example, if the present produces a gift card for presents that don’t already live on the heap.

![image](/img/heap/more-presents.png)

For a spine-strict data structure, it can produce a *lot* more presents.

![image](/img/heap/too-many-presents.png)

Oh, and one more thing: the Ghost-o-matic makes a great gift for ghosts and family. They can be gift-wrapped in presents too. After all, everything in Haskell is a present.

![image](/img/heap/ghost-o-matic-present.png)

*Technical notes.* With optimizations, a function may not necessarily allocate on the heap. The only way to be sure is to check out what optimized Core the program produces. It’s also not actually true that traditional, strict functions don’t exist in Haskell: unboxed primitives can be used to write traditional imperative code. It may look scary, but it’s not much different than writing your program in ML.

I’ve completely ignored partial application, which ought to be the topic of a later post, but I will note that, internally speaking, GHC does try its very best to pass all of the arguments a function wants at application time; if all the arguments are available, it won’t bother creating a partially application (PAP). But these can be thought of modified Ghost-o-matics, whose ghost already has some (but not all) of its arguments. Gifted ghost-o-matics (functions in the heap) can also be viewed this way: but rather than pre-emptively giving the ghost some arguments, the ghost is instead given its free variables (closure).

Last time: [Implementing the Haskell Heap in Python, v1](http://blog.ezyang.com/2011/04/implementing-the-haskell-heap-in-python-v1/)

Next time: [How the Grinch stole the Haskell Heap](http://blog.ezyang.com/2011/04/how-the-grinch-stole-the-haskell-heap/)

![image](http://i.creativecommons.org/l/by-sa/3.0/88x31.png)

This work is licensed under a [Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).
