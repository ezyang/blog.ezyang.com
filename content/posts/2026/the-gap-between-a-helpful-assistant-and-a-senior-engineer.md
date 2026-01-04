---
title: "The gap between a Helpful Assistant and a Senior Engineer"
date: 2026-01-04 05:41:00
slug: the-gap-between-a-helpful-assistant-and-a-senior-engineer
categories: [AI Coding]
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

What would take for an LLM to be able to act like a Senior Engineer?

* Perhaps prompting is all you need, and you just need to write enough
  information about the surrounding context for a project, and once you feed
  in enough tokens, a smart model can infer the rest of the details you didn't
  explicitly right down.  This context would be bespoke for every project; you
  would have to redo this exercise every time you had a new project!

* Perhaps you can instead prompt a model on *how* to operate agentically to get
  the context it needs.  This prompt here might be more reusable.
  But the model may need to actually do wetwork (e.g., talk to humans) to get
  all of the information it needs.  And remember the old saying: the more generic
  the advice is, the less useful it is.  Specificity is king, which leads to...

* Let's say we solve continual learning.  Instead of crafting the perfect
  prompt upfront; you could just drop the model as an "embodied" software
  developer.  It reads code, talks to people, does projects, and in doing so
  slowly develops its latent context, in the same way a human engineer does.
  Building context will often be bottlenecked in the same way humans are: you
  can't get experience related to pushing a feature to production, until
  you've actually pushed the feature to production (however long that takes).

But just like how you shouldn't micromanage a Senior Engineer, all of these
approaches involve fundamentally different expectations about what an AI
coding agent should *do*, and so even if a model and scaffold are capable of
doing these things, it is altogether another question if it will be asked to
behave in this way.  So let's not take it as a foregone conclusion that METR
task times will keep following the empirical trendline: I expect a phase
transition when the context an LLM needs to do a good job exceeds the
capability of scaffolding to provide on the fly.
