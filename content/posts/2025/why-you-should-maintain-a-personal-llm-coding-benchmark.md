---
title: "Why you should maintain a personal LLM coding benchmark"
date: 2025-04-04 03:05:46
slug: why-you-should-maintain-a-personal-llm-coding-benchmark
categories: [AI Coding]
comments:
    - id: 35156
      author: vieplivee
      date: "2025-04-05 01:33:03"
      content: |
        I've probably been doing this on non-coding thingies for a while.
        
        Up till GPT-o1, no chatbot can solve the problem "how to make 24 out of 5 5 5 1".
        
        Then up till Claude 3.7, no chatbot can solve sudoku using an uploaded image.
        
        Quite recently, Gemini 2.5 pro failed a question about "six little dragons" that Grok 3 answered perfectly.
        
        I generally open 2+ chatbots when asking a question that's potentially non-trivial, and get a sense about who's good on what accordingly. This feels more useful than chatbot arena, where I can't control the models.
    - id: 35723
      author: Venkatesh (Rahul Shetty)
      date: "2025-06-28 00:51:41"
      content: "Great post! Thanks for sharing this."
---

Do you use an LLM for coding? Do you maintain a personal benchmark based on problems you have posed the LLM? The purpose of this blog post is to convince you *should* do this: that you can do so with *marginal* effort on top of your day-to-day vibe coding and that you will get both *short and long term benefits* from making your own personal benchmark exist.

------------------------------------------------------------------------

I started thinking about benchmarks for coding in part with my frustration with the discourse around LLMs in the public squares I frequent (Reddit and Twitter). People often want to know "what's the best model" or "what's the best coding IDE"? One might imagine that the way to answer this question would be to test the models on a variety of problems from real world uses of the LLM for coding, and then compare how well various systems do on this. Indeed, whenever a new SOTA model releases, the lab will usually tell you about the model's performance against a few well known coding benchmarks. Problem solved?

![image](/uploads/2025/03/Screenshot-2025-03-31-at-10.10.14%E2%80%AFAM.png)

Of course not! In fact, for the most part, no one really talks about benchmarks when comparing models. Why? I argue the most popular benchmarks measure tasks that are largely different from what a user wants out of an LLM. For example, take the recent [Gemini 2.5 Pro](https://blog.google/technology/google-deepmind/gemini-model-thinking-updates-march-2025/#gemini-2-5-pro) release. In their headline table, they test against LiveCodeBench, Aider Polyglot and SWE-bench Verified. Both LiveCodeBench and Aider Polyglot derive their problems from contest programming and pedagogical exercises (respectively), while SWE-bench assesses bug fixes to preexisting codebases. While useful, this is only a small slice things people want to do with LLMs.

Wouldn't it be great if you had your own, personal benchmark, based on problems you actually care about? If you are tweaking your `.cursorrules`, you could run your benchmark to see if a change you made helped or not. When a new model comes out, you could spend a few bucks to run your eval and make a decision if you should switch your daily driver. And then on social media, if you wanted to stan the new model, instead of asking the model to drop a ball inside a rotating hexagon or vagueposting about how the new model is *incredible*, you could just post your benchmark results.

------------------------------------------------------------------------

Nicholas Carlini's [Yet Another Applied LLM Benchmark](https://github.com/carlini/yet-another-applied-llm-benchmark/) is an existence proof that this playbook can work. As Nicholas describes it:

> It's a collection of nearly 100 tests I've extracted from my actual conversation history with various LLMs.
>
> There are two defining features of this benchmark that make it interesting. Most importantly, I've implemented a simple dataflow domain specific language to make it easy for me (or anyone else!) to add new tests that realistically evaluate model capabilities. This DSL allows for specifying both how the question should be asked and also how the answer should be evaluated. Most questions are evaluated by actually running the code the model writes but the framework supports a bunch of other evaluation methods as well. And then, directly as a result of this, I've written nearly 100 tests for different situations I've actually encountered when working with LLMs as assistants.

I have been working on [my own benchmark](https://github.com/ezyang/ezbench) based off of Carlini's benchmark, and I can confirm that this works well for the traditional style of coding eval, where you have a one-shot task that generates and executes the code against some test cases. My basic strategy is to vibe code as usual, but whenever I give an LLM a task that it isn't able to one shot, I consider adding it to the benchmark. In more detail:

- I only add a task if a SOTA LLM failed it. This ensures the benchmark consists of all appropriate difficulty problems: easy enough that I thought an LLM should be able to do it, but hard enough that a SOTA model failed on it. I don't need problems that are too hard (this is already well covered by well known benchmarks like SWE-Bench or SWE-Lancer), and I don't mind if my problems saturate because, hey, that means the models are that much better for my use cases!
- After I have added the task to the benchmark, I can use the benchmark runner to tell if changing the model, tweaking the prompt, or even just running the prompt again at nonzero temperature can make it pass. Indeed, it's helpful to find some configuration that makes the eval pass, as this is good for debugging issues in the evaluation function itself... also it means you have working code for whatever task you were working on. Conversely, you can make the task harder by leaving things out from the prompt.
- Writing the test is the labor intensive part, but you can always vibe code a test. Importantly, you have a failing implementation (your initial generation) and some way you (manually?) determined that the implementation was wrong, so just turn this into your evaluation function! (And for all you yak shaving aficionados, if the model fails to vibe code your test, well, you have another task for your benchmark!)

For example, the other day I needed to take an asciinema recording and convert it into a sequence of frames rendered as plain text. However, the only project for doing these conversations was [agg](https://github.com/asciinema/agg), which converts recordings into animated gifs. In [agg_to_text](https://github.com/ezyang/ezbench/blob/main/long25/agg_to_text.py), I ask an LLM to take agg's source code and create a new program which dumps the frames as plain text rather than gif images. The reason why this task is difficult, is because there is some discretion in deciding when to emit a frame, and with my original prompt the LLM didn't precisely replicate the original behavior in agg. While working on the benchmark, I realized that instructing the model specifically about how frame batching worked was enough to get it to preserve the original behavior. But I don't think I should need to do this: thus this task. (P.S. If this test saturates, well, I can always make it harder by removing the agg source code from the prompt.)

------------------------------------------------------------------------

The ability to benchmark one shot tasks is here today, but I would like to speculate a bit about what lies beyond them. In particular, most of my LLM coding activity involves asking the LLM to make changes to a pre-existing project, which makes it less amenable to "single prompt creates self contained program". (Also, I usually only ask one-shot questions that the LLM can answer, so most of them would never go in my benchmark.)

In short, how can I extract tasks from my day-to-day work? There seems to be two big extra levers we have:

- **Codebase tasks.** This is the heavy-weight approach: you record the Git commit of your codebase at the time you prompted for some new feature to be added, and then when you want to run an eval on a new model you just check out the codebase at that commit and let the end-to-end system go. You'll typically want to execute the modified code, which means you'll also need a way to reliably setup the runtime environment for the code; things like lockfiles can help a lot here.
- **Transcript tasks.** You don't actually need the entire codebase to be available to ask an LLM for a completion; you only need the conversation transcript up to the point of the critical generation. If the transcript is mostly your agent system reading in files for context, you can end up with a relatively system generic prompt that can tell you something about other systems. Of course, if you want to actually run the change, you still need the full codebase, which is why this approach is much more amenable if you're going to do some **static analysis** on the output. For example, if a model keeps adding `try: ... except: ...` blocks that are suppressing errors, you can take some transcripts where you've caught the model red-handed doing this and make an eval that checks if the model is still doing this. I suspect testing on transcripts works best for testing if changing prompts or rules improves performance, since the transcript itself will put the model into some particular latent space and if it were a different model they might have made different choices leading to a different latent space. Transcripts from thinking models are especially susceptible to this!

I have started adapting Carlini's framework to work better for these cases, although I would love to be told someone has already solved this problem for me. In particular, I am very excited about using transcript tasks to evaluate whether or not things I add to my prompts / triggered rules are helping or not. Current SOTA model instruction following isn't great and I regularly catch models doing behaviors that I explicitly told them not to in the system prompt. I have started some initial analysis over all of my chat logs to find cases where the model misbehaved, although I haven't quite worked out how I want to build an eval out of it.

One word of warning: to make transcript tasks, you need an AI coding system that doesn't obscure how it assembles its underlying prompts (which rules out most of the popular closed source AI code editors.)

------------------------------------------------------------------------

I started building evals for a selfish reason: I wanted to be able to tell if modifications to my prompts were doing anything. But I also think there is a broader opportunity that arises if we also publish these benchmarks to the world.

For one, building a real world benchmark on use cases we care about is a way to communicate to the people training AI models whether or not they are doing well or not. Historical evals have focused on LeetCoding, and consequently we have models that would ace any big tech interview and yet on real world tasks will drive you off a cliff at the first opportunity. And this is not just free labor for the top labs: if you believe in open source models, one of the biggest barriers to good small models is having really high quality data. We, the OSS vibe coding community, can directly help here.

I think there is a tremendous opportunity for the open source community to really push the state of the art in coding evaluations. There's only so many benchmarks that I, personally, can create, but if *everyone* is making benchmarks I could eventually imagine a universe of benchmarks where you could curate the problems that are relevant to your work and quickly and cheaply judge models in this way: a Wikipedia of Coding Benchmarks.

To summarize: every time an LLM fails to solve a problem you ask it for, this is a potential new benchmark. As long as there is a way to automate testing if the LLM has solved the problem, you can turn this into a benchmark. Do this for yourself, and you can quickly have a personal benchmark with which to evaluate new models. Do this at scale, and you can help push the frontier in coding models.
