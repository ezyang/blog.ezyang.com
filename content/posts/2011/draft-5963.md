---
title: "Programming with story boards"
date: 2011-10-23 20:20:39
slug: 
draft: true
categories: [Miscellaneous]
---

“Why did you do this research?”

Armando Solar-Lezama

"Mental pictures": program declaratively

Program synthesis

Scenarios: - more powerful than example, as infinite set of examples - partial specification, ambiguous

"x could be pointing to NULL" "the rest of the structure stays unmodified"

IDEA: Graphics (Storyboard) -\> Semantics (Storyboard Framework) -\> Synthesis IDEA: Combine many simple specifications to constrain behavior (it's like types! or contracts! good reason to believe that this is /different/ from 'checking'; true by construction)

"What does correct mean? Needs to obey all of the storyboards: so there are MULTIPLE possible codes: programmer can ask for SHORTEST code" "Memory safety? Baked into framework"

Example: linked list reversal

Often need to specify CORNER CASES. Use INDUCTION with unfoldable '...' which turn into longer things. (So predicates are defined this way)

TVLA: huge blow up of cases (more freedom in code? less precise analysis?) <http://www.cs.tau.ac.il/~tvla/>

Solver can PICK the code. (KEY!) We exploit structure. (they have more primitive analysis)

What do we give up? We can't do anything where we have nodes are coreferent (these are all disjoint memory locations). We can't deal with graphs. "What about doubly linked lists?" Aliasing must SHOW in the graph. "If I look at a node, I see all nodes going to that arrow, that's ALL the possible aliases." "Can you do btrees?" Yeah, but not generally. When you want to represent aliasing, the hidden assumption is every node represents an separate location. "Btree + list of all the nodes". You do this using an unfold: it represents a list AND the btree. (Confusion about arbitrary fanout, as opposed to leaf-linked tree.)

Is this harder if we're synthesizing programs with manual memory management? (malloc and free). We're exploiting the fact that the heap is a constant, so it's not increasing/decreasing size. Interesting further work. (Does that make a difference?) Maybe definite pattern of allocation would work. (Maybe if we color the graph, then the notation is expressive enough. It's finite? "This is a new node") New nodes in input/output state, we can represent, but the problem is: intermediate states allocation/deallocation. (Maybe we can think of this as a list of free nodes: it's an input.)

Me: Why isn't this equivalent to SAT?

Contrast synthesis (known: input and output, not functions) with verification (known: functions and input, not output) (but maybe you need to compute outputs too to figure out if it's correct.)

Add choice variables for transfer functions: "it's a big bag of statements we can have"

Can't give to solver: least fixed-point; and set based reasoning in a constraint reasoner is hard. (Me: wait, where did the set come from) (Adequacy theorems? "If your skeleton is explicit enough, we'll find it". So the weakness in the language is the expressiveness of language? Bounded space of programs, going to search them for.) (Structural info: can we say how many statements allowed in loop body? Yes. So hopefully the skeleton is obviously finite.)

What do we mean by sets? Because every time we run a statement again, we have more possible shapes, so we need a set of all of the possible shapes.

Fundamentally confusing: I assumed the roles of the variables was a RELATIONAL semantics. But it's not a relational semantics; it's a set of pre-states to a set of post-states, and it doesn't say which states are associated with them.

SHOWING THE "RECAP: STORYBOARD CONSTRAINTS DIAGRAM"

has the Abstract Scenarios (begin/end flip; and the unfolding on the right side for summary nodes that we need to reason about) and Concrete scenarios (the edge cases x3)

The induction on the right is a TYPING constraint, not a CORRECTNESS constraint.

"LOL: least fixed point gets you the right answer"

Pragmatically, if you specify the base cases, we HAPPEN to get a list reversal algorithm. Right! So we want to see how far we can get with a simple constraint language, and still have the nice property that we get desired code. As long as the corner cases are good enough, we'll get what we want.

(It seems to me that this will cause problems.)

The top is the "crucial hint". So why do we need the right side? So we NEED this right constraint. If we don't give the RIGHT constraint, it's just an arbitrary set of nodes. But now, we're over constraining the algorithm. It doesn't matter if it's completely broken!

Can we extend it so that it generates loop invariant, so we can use it to verify later on? Maybe you have enough information for that anyway. (Implicit loop invariants, from our states.) Would it be useful for a sound verification tool? (heehee Chlipala)

Approach from traditional programming languages perspective: the way is we would come up with a language that would completely identify the soundness constraints for algorithm, we generate it, and we're sound. 'Now, the uncharitable approach, is we throw in these piece, we squirrel it all together, and happen to get what we want.' circa 1995. OR, this whole agenda of doing soundness is a failure, and we need to do something different. "You're incompetent, or the old way is broken."

At the beginning, you need to write a lot of specifications. And that is fine if you know what you're doing. "This is a tool for people who don't know what they are doing." (Martin Rinard's question). So do you think programmers don't know what they're doing? "We're giving them a tool where they can provide their insights. They don't have to be sound or precise, but they'll get something out of it, and if they keep doing it, they'll reach towards something." So they kind of know what they're doing.

THIS IS A TOOL FOR SHARPENING SEMANTICS.

So why do you like this approach, as opposed to the prevailing approach? "I think this will help normal programmers make less mistakes." So you think the other route is infeasible for other reasons. "It might be feasible, but we think this is more direct." So you don't really have a philosophy. It's like this research. Toss it, and see if something good happens. Do you think this is an accurate portrayal of what your research is doing? I think this is fascinating because it's against a 'rational' approach in computer science. And the attitude is like that too for research! I think these program example systems are only as good as the spec you provide.

Why did you do this research? "It sounded fascinating. This is how teachers teach, they're not going to say exactly what they're doing, how many quantities, but they'll give some of the insights. Replicate INSIGHT retrieval." (back to the AI world!)

Here's what I like: you're doing the eat your own dogfood thing. The systems guys build their own system, and eat it. You ask anyone in 1990? They say it's a skanky hacked up system. But it's the way forward. You're just going to do it. (I think you're completely mischaracterizing this. "...") This is NOT an engineering research project; it's a SCIENTIFIC research project, the hypothesis is that if you generate implementations from partial specification, we'll explore this and see how often we get a solution that works.

If he THOUGHT this way, he never would have done this. "Hey I'm playing around in a lab." Armando: underlying hypothesis: if you look at something complicated. any complete specification is a pain to write and a pain to understand. Higher-order logic monstrosity. If we can say "here's something that I know about it", etc, if you have the machinery to go and combine the facts \[...\] and you realize some extra things that need to be added, so the ability to just go in and here's the "other fact".

Important hypothesis: the solutions generated to these partial programs, are likely to be progress to the full articulated specification. But I am SURE Rushav is not thinking about it. "They hack something together " "Please say NO."

The point I'm trying to make is, if you think too much, you don't get anywhere. "That seems to be the lesson of formal verification." (You mean you're complimenting him? Yes! Of course!)

Chlipala: Traditional specs are too hard to write, so let's write by programs. Well, I think these examples are more complicated than my specs. (Maybe it's different problems?) Consider insertion in lists. I want my trees to be balanced. \[blue screen, computer turns off\] But that's not the specification: it's the asymptotic performance and the behavior he cares about! Everything else is implementation details!

We don't care about what the program looks like in the end, as long as it's correct. "What about space?" Should be in the spec. "Well, you could write a spec. But now, the machine is in the unenviable position of needing to rediscover 50 years of computer science to figure out how to do this. That's the point. Then synthesis walks into the science fiction realm. The machine shouldn't have to come up with the clever insights.

So maybe verification is more important for data structures? Depends on what you call a data structure. If at the level of trees and lists, there are relatively few of them. But pretty much any system of reasonable complexity is going to have some data organization that uses these same principles. If you squint, it's a data structure.

But the same argument says that SQL could never work. But everyone uses that and forgets about the data structures!

Sure, but lots of people have tried to use SQL for things like organizing file system in OS, and found it wanting. Because it didn't have sufficient control.

And people needed to invent languages to express integrity constraints on top of the relational model.

"The ideal synthesizer is the grad student."
