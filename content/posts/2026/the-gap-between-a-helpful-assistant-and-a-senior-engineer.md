---
title: "The gap between a Helpful Assistant and a Senior Engineer"
date: 2026-01-03 13:25:36
slug: the-gap-between-a-helpful-assistant-and-a-senior-engineer
draft: true
categories: [Miscellaneous]
---

Let's suppose you asked an AI coding agent to "implement a CLI calculator".
Imagine if, instead of only writing short Python script, it also started
building an automated test suite, a crash reporting mechanism and a telemetry
subsystem.  You'd be like, "What the fuck is this?"

But now let's say that you were planning to release this project to users. It
would be clearly negligent to not have an automated test suite. A crash
reporting mechanism might be overkill for a simple calculator, but for more
complicated CLIs interacting with the real world, it may not always be
feasible to have reproducer, in which case crash logs are essential.
Similarly, a telemetry subsystem would be wildly inappropriate for an open
source local-only calculator, but it could make sense for a networked
application or a corporate tool of all consenting users.  One of the important
functions of a senior engineer is to be able to evaluate the context a
software project lives in and figure out if we need to do something, even if
it isn't explicitly asked for.  This is contrast to a helpful assistant, who
is first and foremost obligated to *follow the user's instructions.*  This
leads to a gap between a Helpful Assistant and a Senior Engineer.

In principle, you could prompt the LLM agent to act like a Senior Engineer.
In fact, why stop at Senior, let's tell the LLM to be a Staff Engineer!
Imagine that scaling continues: what would you expect the LLM to do when
instructed to act in this way?  Well, imagine a human L7 engineer who has just
been hired by a big tech company to head up some big, new, multi-year
initiative.  Will they say, "Sure, I can help with that!" and start busily
coding away?  Of course not: they will go out and start reviewing code,
reading docs, talking to people, asking questions, shadowing oncalls, doing
small starter tasks--they will start by going out and building context. Here,
the "helpful assistant" frame for LLMs is limiting: sure, Claude might ask you
a few questions to clarify the task upfront, but if your coding agent starts
asking you about "relevant partner teams" and "org-wide priorities for this
half" you are definitely going to raise an eyebrow.

What does this mean in practice? In particular, as a person whose day job
involves working on PyTorch, I want to know what exactly should we be doing
differently in this age of LLMs.  The path of least resistance is to just slot
LLMs in anywhere in the traditional software development pipeline where they
substitute only small steps out of a fundamentally human-oriented process.
But doing this quickly causes a bottleneck on TL reviewer bandwidth, where
everyone is now able to generate code much more quickly, but LLMs haven't
really improved the sense making / consensus-building / overall system design
parts of the process (and arguably, the TLs had always been overloaded, and AI
coding has only made it worse.)  And the argument above, suggests that without
the major unlock of continual learning or persistent context, in the short
term LLMs aren't going to make a dent on this: they can help you brainstorm
things to think about, they can rubber duck with you, they can investigate the
codebase; but at the end of the day, the "helpful assistant" frame means that
a human has to actually operate the thing.

So now we have a fork in the road: we can assume that the human will always be
setting the context, and look for smaller interventions in the role of "senior
engineer" where LLMs can obviously help speed things up.  Or, we can think
about what it would take for an LLM to be able to act like a Senior Engineer,
including improvements to the models:

* Perhaps all it takes is a sufficiently long document, explaining what it means to have



P.S. Not good for safety.


The billion dollar question is 

I have been trying to understand what, today, I should be doing differently in the age of LLM-enabled software engineering. 



Much ink has been spilled about the context problem for agents--the problem of how to create an AI that can continually learn.  The miracle of modern coding agents is that it turns out that by simply grepping around and reading a codebase, an agent can gain enough context to do useful tasks.


Even if you asked an LLM to think about these things (and it certainly has heard of all of these things, and will happy add tests and log lines if you ask it to), without a nuanced enough understanding of the context of the system, we can't trust the LLM to make the right decisions (after all, a decision that is right in one context, can be wrong in another! Extensive testing is counterproductive if you are rapidly prototyping or doing investigatory work, but it is table stakes for a widely used production system.)

Even in humans, building up all of this context is expensive.




Top line: whatever is I'm hypothesizing about here, should lead ACTIONABLE things to do today

Context is expensive to build
Context is location dependent.


Examples: is where depending on context, doing X is exactly the right thing or the wrong thing. What the right thing is depend on context.

Rapid prototyping context versus the production engineering context.  (Example: traditional infra versus ML infra)



Context evolution




There are lots of reasons why LLMs aren't (currently) replacing human programmers.  But a lot of discussion about scaling

What do I mean by 'Helpful'?  HHH.

What do I mean by 'Staff Engineer'?  https://staffeng.com/



