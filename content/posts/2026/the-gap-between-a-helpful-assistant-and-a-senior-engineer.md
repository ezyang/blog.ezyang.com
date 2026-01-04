---
title: "The gap between a Helpful Assistant and a Senior Engineer"
date: 2026-01-03 13:25:36
slug: the-gap-between-a-helpful-assistant-and-a-senior-engineer
draft: true
categories: [Miscellaneous]
---

Let's suppose you asked an AI coding agent to "implement a CLI calculator". Imagine if, instead of only writing short Python script, it also started building an automated test suite, a crash reporting mechanism and a telemetry subsystem. You'd be like, "What the fuck is this?"

But now let's say that you were planning to release this project to users. It would be clearly negligent to not have an automated test suite. A crash reporting mechanism might be overkill for a simple calculator, but for more complicated CLIs interacting with the real world, it may not always be feasible to have reproducer, in which case crash logs are essential. Similarly, a telemetry subsystem would be wildly inappropriate for an open source local-only calculator, but it could make sense for a networked application or a corporate tool of all consenting users. One of the important functions of a senior engineer is to be able to evaluate the context a software project lives in and figure out if we need to do something, even if it isn't explicitly asked for. This is contrast to a helpful assistant, who is first and foremost obligated to *follow the user's instructions.* This leads to a gap between a Helpful Assistant and a Senior Engineer.

How can we cross this gap? If you are tired of having to explicitly prompt the model to add tests for every change it makes, the current meta is to add instructions about tests to AGENTS.md. You could expand this to a list of "senior engineer" behaviors to copy-paste

**Explicitly ask the LLM to do these things.** If we ask the LLM to

ALso, green field, CLI is the EASIEST configuration

> would quickly want an automated test suite. If the code was likely to be used in

A senior engineer would know at what scale, in what context, it makes sense not to just directly follow instructions and go beyond what has been asked, even if we didn't explicitly say there should be observability in the prompt.

The prevailing practice today is to add considerations like this to AGENTS.md; especially for things like testing, which the models are heavily post-trained to do.

But the point of a senior engineer is *you don't have to micromanage features like this*.

Even if you asked an LLM to think about these things (and it certainly has heard of all of these things, and will happy add tests and log lines if you ask it to), without a nuanced enough understanding of the context of the system, we can't trust the LLM to make the right decisions (after all, a decision that is right in one context, can be wrong in another! Extensive testing is counterproductive if you are rapidly prototyping or doing investigatory work, but it is table stakes for a widely used production system.)

Even in humans, building up all of this context is expensive.

Top line: whatever is I'm hypothesizing about here, should lead ACTIONABLE things to do today

Context is expensive to build Context is location dependent.

Examples: is where depending on context, doing X is exactly the right thing or the wrong thing. What the right thing is depend on context.

Rapid prototyping context versus the production engineering context. (Example: traditional infra versus ML infra)

Context evolution

There are lots of reasons why LLMs aren't (currently) replacing human programmers. But a lot of discussion about scaling

What do I mean by 'Helpful'? HHH.

What do I mean by 'Staff Engineer'? <https://staffeng.com/>
