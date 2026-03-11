---
title: "AI-assisted programming for spmd_types"
date: 2026-03-10T21:11:43-04:00
slug: "ai-assisted-programming-for-spmd-types"
categories: ["PyTorch", "AI Coding"]
---

[spmd_types](https://github.com/ezyang/spmd_types/tree/main/sixlib/spmd_types) is a type checker for distributed PyTorch programs.  I'll write a blog post about the system itself as it gets closer to completion, but in this post I want to talk about my workflow for AI-assisted programming in the project.  In particular, `spmd_types` is my first greenfield project that both:

1. I have written it entirely with AI, and
2. I am comfortable being accountable for it (in the same way I am accountable for my handwritten code).

I have encouraged others to also use AI as the primary mechanism of writing code for the codebase, but I have noticed that people get different outcomes (in terms of diff volume and number of review cycles), so I wanted to write down my workflow in case it was helpful for somebody.

This is NOT a workflow for letting the AI autonomously write tons of code. You're going to be reading every single line it produces.

# Pre-requisites

My workflow relies on a few important pre-requisites.  If you don't have these, this workflow may work less well:

1. **I could have written the code from scratch.** The LLM isn't writing code in a language or domain that I don't have extensive experience in.  Importantly, this means I can review the code quickly, because I know how I "would have written it" and can compare the LLM code against this mental model.

2. **I can hold the entire design in my head.** I have a pretty good conceptual understanding of how `spmd_types` is supposed to work and I can reliably judge if LLM output is aligned with the design or not.  There were many aspects of this type system that were non-obvious to us (even though we are shamelessly copying JAX's design), so there was quite a lot of pre-work that happened before even a single line of code was written.  I wrote the entire first [DESIGN.md](https://github.com/ezyang/spmd_types/blob/main/sixlib/spmd_types/DESIGN.md) draft by hand with no LLM assistance, which served as the initial prompt.

3. **I can multi-task work with the AI.** Claude Opus is not fast enough for single session interactive.  While it's generating tokens, there need to be other things to do, e.g., simultaneously working on another orthogonal feature in the codebase or reviewing diffs.  I personally find multi-tasking very strenuous, but if the tasks are related I find it easier to avoid getting sidetracked.

4. **I am not price sensitive regarding token use.** I am not using a rate limited subscription plan, and because my development is going at human speed (not agent swarms), the cost of the tokens is low enough that I don't need to be penny-pinching token use all the time.

# The workflow

I don't use a Claude Code orchestrator.  I have one window per work tree, each a single tab that is just a regular shell, and then as many tabs as I think I need for concurrent Claude Code instances on that worktree.  Usually, only one is actually doing work, and the others are just if I want to ask questions without interrupting the main agent.  Because I am typically doing work on a GPU system, these sessions are remote, so I use iTerm2 + `tmux -CC` (tmux control mode) to conveniently have multiple windows and tabs in native UI.  It is so incredibly good to type ⌘T and get a fresh tab on the remote machine.

It is helpful to have multiple worktrees.  This is not because you *want* to multitask, but because maybe one worktree is tied up with a debugging session, and you have some coding tasks on the TODO list to fry while it is going.  I currently maintain three worktrees, but usually only one of them is running.  You want true worktrees, not independent clones, because you are going to be passing commits between the trees.  I have been especially enjoying the [smartlog mechanism](https://sapling-scm.com/docs/overview/smartlog/) in [Sapling](https://sapling-scm.com/), which makes it easy to keep track of all the patches flying around from the agents.  I miss this UX a lot when working in Git.

Here is the central loop:

1. (Optional) If you're not sure exactly how the feature should go, or you are worried about the LLM misunderstanding the prompt, go to plan mode and get the LLM to give you a satisfactory plan.  You are reviewing the plan to understand if the LLM's intent matches yours: the plan is shorter than the output code, so it's quicker to review.  Don't blindly accept the plan, you should be able to evaluate it personally!

2. Prompt the LLM to make the change you want.  While it's going, maybe review some code or start another task in a parallel worktree.  Check in regularly, including seeing what the LLM is up to.

3. Sometimes, the LLM will get stuck with some problem.  Understand why the LLM is stuck: did it make a wrong turn somewhere?  Did you misunderstand the problem statement?  Is there a latent bug?  This is one of the most common situations to multitask, since you may identify follow ups that you can kick off in parallel while you keep working on the original task.

4. Commit it.  (I like LLM for this, since it can usually write something pretty good given my original prompt + the trajectory of actually implementing it.)

5. Read through every single line of code produced.  Imagine you are instead writing the code.  You are looking for cases you are surprised.  Any time you are surprised, feed your comment to the LLM, either as a question ("Why did you do it this way?") or a requested edit ("Instead of X, do Y", copying in the diff to help the LLM localize the change).  Claude Code supports queuing edits, so you can keep feeding them in and then continue reading.  Be lazy: if something doesn't make sense, don't try to puzzle it out, just send it straight to the LLM.  Do NOT multitask this part, it needs your undivided attention (unsurprisingly, this steps ends up being the bottleneck!)  Repeat as many times necessary.

6. Submit the diff.

7. Clear the context, start your next change.

For the code review cycle, I like responding to comments in your review system, and then feeding them to the LLM to amend the commits.  Those amendments need to be reviewed in the same way.

# Expectations

What level of quality can we hope to get out of the LLM generated code in a workflow like this?  Let's be clear: with this workflow, there are going to be some aspects of the code that you do not fully understand.  This is fine, because handwritten code is often also the same (e.g., with respect to whether or not they pass tests, or have problems in production!)

But in some sense, whether or not you fully understand a given line of code function implementation is immaterial: if there's a bug in a function, yes... bugs are bad, but you can just fix it.  (It's not like human code doesn't also have bugs as well!  And frontier LLMs are pretty good at not making small localized mistakes--arguably, they're better than humans at this already!  And we didn't pick a language you are unfamiliar with--so many basic problems you should be able to pick out already.)  There are other things that are harder to fix: public APIs, overall architecture of the system, module boundary choices--these are the important things to evaluate.  But you don't really need to be reading every single line of code to evaluate these: most of the design intent shines through in the prompts you write (where you explain what it is you want out of the system.)

The biggest goal of a setup like this is continuously maintaining *human comprehension* of the AI authored codebase.  You can't offload understanding to an AI.  But you can force the AI to write the code in a way that is easy for you, personally, to comprehend.  You can force the AI to answer all your questions and then tell it, "That doesn't seem very convincing."  One of the beautiful things about AI-assisted programming is how quickly you can invalidate your own mental model and rebuild a better, more accurate one.

As an aside, the other thing is that it's (relatively) easy to make architectural changes: just ask the LLM to do it.  If you notice that some downstream usage is struggling, and you come up with a better way to deal with it, just spin up an agent in a worktree to make the change!  No more spending an entire day refactoring, you can be out in half an hour.
