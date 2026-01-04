---
title: "The future of software development in the age of AI coding"
date: 2026-01-01 00:38:28
slug: the-future-of-software-development-in-the-age-of-ai-coding
draft: true
categories: [Miscellaneous]
---

- Green field, it doesn't matter if it's production ready, system is setup so that failure is not a big deal / obvious when failure has happened
  - Random UIs for making things easier, visualizations, personalization features
  - Less obvious: one off investigations (this is troublesome, because if the LLM steers you to the wrong conclusion, this can be harmful)
- Surgical fixes
  - Especially in codebases you don't know well
  - Or in languages you don't know well

Why code review? \* Compliance \<--\> voodoo \* At least one other person understand the code \* Prevent malicious code (mostly from OSS people) \* Catch bugs \* Human alignment, esp design (code is just a way of communicating the design)

Very worried about increasing code review cost \* Social norms problem: It is unacceptable to send code in you haven't reviewed yourself? (Is it?) Junior vs senior. \* Problem: SUPER inefficient ping ponging the review back this way. (This is the no more junior hires problem.) \* LLM learning problem (learning as a struggle) \* You want to align frequently (the claude code lesson) \*

Tests accumulating indefinitely / system invariants  
- The system invariant is wrong / there is no system invariant -\> senior engineer is about adding the system invariant
- With LLMs, can write a lot MORE tests
  - Overtesting is a big problem (Twitter conversations)

Senior software engineer --\> AI coding really hasn't changed the job at all. What is the job normally? \* Reviewing other people's code (code review!) \* Working on the system design / understanding system invariants \* LLMs are pretty good (esp re comprehensiveness) \* You want to iterate on design faster, and LLM / LLM coding spikes speed up this dramatically \* What did I miss? \* Communicating with team and other people \* (??? What happens if you fire all the junior engineers) \* You're not going fire all junior engineers, because even if you're maintaining all the agents, there is a limit to your scaling attention \* Judgment ~ wisdom - yes you COULD do it, but better NOT to \* Tradeoffs (you could do it this way, you could do that way, there's a tradeoff, WHICH tradeoff do you make?) \* Unknown tradeoffs \* Intent (prompts are reified intent) \* What is intuition? Intuition is the distillation of an extremely high dimensional space into a feeling "oh we should do X"

- Things they are looking for
  - Making sure you can't do bad things with APIs (make illegal states unrepresentable)

  - Understand the implications (how will it be used? how can it be misused)?  
    - For these, a reasonable prompt can work, mostly it's a false positive issue (LLM flagging way too much stuff and reducing signal)

Alignment problem? \* What does it mean for a software project to be successful? \* Probably not just making money? \* What does it even mean for a software project to last ten years?

Embodied AI / continual learning problem / long context (what do you expect to see that makes it closer to solving this problem) (the problem can be solved in a one off way from domain to domain) ^--- scaling is all about compute MIT mystery hunt

What about PyTorch? (What about OSS?) (What about superintelligence research?)

What does it mean for PyTorch to be AI first? \* What does it mean for a library to be AI first? \* API design \* Luatorch API is very historical! \* But why is the compiler API so terrible? \* Distributed API design problem, quite difficult (DTensor) \* Compositionality (first autograd, but then vmap, torch dispatch, etc.) \* What did PyTorch spend sweat on? \* Compilers \* Autograd \* C++ library (ATen) (and with it codegen, open registration, custom ops) \* C++/Python divide (tradeoff between overhead and ease of changes) \* Crossing all of the hardware updates \* Extensibility \* Distributed! (the eager APIs especially)

What does it mean for OSS to be AI first? \* Historically, being OSS is good because \* You're in the pretrain set \* We get lots of issue reports / community / self-support \* OSS pull requests! (usually small targeted, "fix this particular thing that is affecting me") --\> facilitating lots of PRs (torch function -- done externally, huge impact)

Question: can you have a library setup where there isn't code review? \* APIs are signed up to be maintained forever - You know how Go has that very ugly versioning scheme, where you never break BC -- maybe this is a better idea <https://blog.ezyang.com/2016/12/thoughts-about-spec-ulation-rich-hickey/> \* Hey, if there was an easy way to offload features into third party repos for best effort maintenance \* Don't have to run CI \* Main problem is dependency tangles (!!) \* Can we debundle PyTorch so that it is a very small core \* C++ ABI \* Monorepo / CI

Worried: \* Breaking downstream users / SEVs \* Permanently having to maintain things \* Ability to continue making changes (irreversible changes) \* Maintaining a coherent design Smaller worries: \* Dumb mechanical problems that are difficult to debug (e.g., refcounts/locking/etc)

Debuggability / observability / understandability \* Especially if you're having a production incident (e.g., why NOT to rewrite the code into assembly) \* A well trusted test suite (e.g., the simonw post about that HTML parser in Python) \* The thing about having debuggable code, is I AM PRETTY SURE YOU CAN PROMPT \* Good example of embodied intelligence: because it's hard to tell people how to write "observable code", instead, it works a lot better to force people to try to look into the code in the large / production incidents, and THAT is the most reliable reward signal for getting people to do better when writing their code (but maybe you can write some prompts that will get the right idea to the LLM) \* Accountability (can't ask the LLM to join debugging a SEV) \* It's not that valuable to have human who is deeply familiar with the exact source code in a SEV, because \* That person might have left the company! \* Most of the problem is GETTING to the root cause of the problem \* What is a SEV: a way to pull the right people together

The million dollar questions: \* Can I fire all the engineers? \* Can I stop code reviewing code? \* How should I rearchitect PyTorch so things are better? (Or, easier: how would I do it from scratch?) \* What should I tell people to do? \* What should be in my prompt? \* What tests should I have?
