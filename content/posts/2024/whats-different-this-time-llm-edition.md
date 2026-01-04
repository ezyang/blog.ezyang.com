---
title: "What's different this time? LLM edition"
date: 2024-10-04 00:30:09
slug: whats-different-this-time-llm-edition
categories: [Academia]
comments:
    - id: 33898
      author: Ji
      date: "2024-10-31 00:01:19"
      content: |
        lol on "SQL - Its strange syntax purportedly makes it easier for non-software engineers to understand, whereas many (myself included) would often prefer a more functional syntax ala LINQ/list comprehensions. It's pretty hard to make an alternate SQL syntax take off though, because SQL is not one language, but many many dialects everywhere with no obvious leverage point."
        
        During my very short time (&lt; 6 months) trying to code I loved functional stuff like list comprehension, lambda, writing one line to calculate Fibonacci sequence using scala. Tail recursions in python fascinated me quite a bit.
        
        SQL is just a simple way to turn raw data (a lot of random-ish data) into usable data (a mini data warehouse that can be used to generate results). Majority of things that need to happen in SQL is to examine in many ways how this data could be wrong, find ways to fix/patch that in SQL.
---

One of the things that I learned in grad school is that even if you've picked an important and unsolved problem, you need *some* reason to believe it is solvable--especially if people have tried to solve it before! In other words, "What's different this time?" This is perhaps a dreary way of shooting down otherwise promising research directions, but you can flip it around: when the world changes, you can ask, "What can I do now that I couldn't do before?"

This post is a list of problems in areas that I care about (half of this is PL flavor, since that's what I did my PhD in), where I suspect something has changed with the advent of LLMs. It's not a list of recipes; there is still hard work to figure out *how* exactly an LLM can be useful (for most of these, just feeding the entire problem into ChatGPT usually doesn't work). But I often talk to people want to get started on something, anything, but have no idea to start. Try here!

**Static analysis.** The chasm between academic static analysis work and real world practice is the scaling problems that come with trying to apply the technique to a full size codebase. Asymptotics strike as LOC goes up, language focused techniques flounder in polyglot codebases, and "Does anyone know how to write cmake?" But this is predicated on the idea that static analysis has to operate on a whole program. It doesn't; humans can do perfectly good static analysis on fragments of code without having to hold the entire codebase in their head, without needing access to a build system. They make assumptions about APIs and can do local reasoning. LLMs can play a key role in drafting these assumptions so that local reasoning can occur. What if the LLM gets it wrong? Well, if an LLM could get it wrong, an inattentive junior developer might get it wrong too--maybe there is a problem in the API design. LLMs already do surprisingly well if you one-shot prompt them to find bugs in code; with more traditional static analysis support, maybe they can do even better.

**DSL purgatory.** Consider a problem that can be solved with code in a procedural way, but only by writing lots of tedious, error prone boilerplate (some examples: drawing diagrams, writing GUIs, SQL queries, building visualizations, scripting website/mobile app interactions, end to end testing). The PL dream is to design a sweet compositional DSL that raises the level of abstraction so that you can [render a Hilbert curve in seven lines of code](https://diagrams.github.io/gallery/Hilbert.html). But history is also abound with cases where the DSL did not solve the problems, or maybe it did solve the problem but only after years of grueling work, and so there are still many problems that feel like there ought to be a DSL that should solve them but there isn't. The promise of LLMs is that they are extremely good at regurgitating low level procedural actions that could conceivably be put together in a DSL. A lot of the best successes of LLMs today is putting coding powers in the hands of domain experts that otherwise do not how to code; could it also help in putting domain expertise in the hands of people who can code?

I am especially interested in these domains:

- SQL - Its strange syntax purportedly makes it easier for non-software engineers to understand, whereas many (myself included) would often prefer a more functional syntax ala LINQ/list comprehensions. It's pretty hard to make an alternate SQL syntax take off though, because SQL is not one language, but many many dialects everywhere with no obvious leverage point. That sounds like an LLM opportunity. Or heck, just give me one of those AI editor environments but specifically fine tuned for SQL/data visualization, don't even bother with general coding.
- End to end testing - This is <https://momentic.ai/> but personally I'm not going to rely on a proprietary product for testing in my OSS projects. There's definitely an OSS opportunity here.
- Scripting website/mobile app interactions - The website scraping version of this is <https://reworkd.ai/> but I am also pretty interested in this from the browser extension angle: to some extent I can take back control of my frontend experience with browser extensions; can I go further with LLMs? And we typically don't imagine that I can do the same with a mobile app... but maybe I can??

**OSS bread and butter.** Why is Tesseract still the number one OSS library for OCR? Why is smooth and beautiful text to voice not ubiquitous? Why is the voice control on my Tesla so bad? Why is the wake word on my Android device so unreliable? Why doesn't the screenshot parser on a fansite for my favorite mobage not able to parse out icons? The future has arrived, but it is not uniformly distributed.

**Improving the pipeline from ephemeral to durable stores of knowledge.** Many important sources of knowledge are trapped in "ephemeral" stores, like Discord servers, private chat conversations, Reddit posts, Twitter threads, blog posts, etc. In an ideal world, there would be a pipeline of this knowledge into more durable, indexable forms for the benefit of all, but actually doing this is time consuming. Can LLMs help? Note that the dream of LLMs is you can just feed all of this data into the model and just ask questions to it. I'm OK with something a little bit more manual, we don't have to solve RAG first.
