---
title: "Ways to use torch.compile"
date: 2024-11-05 10:11:52
slug: ways-to-use-torch-compile
categories: [PyTorch]
comments:
    - id: 34122
      author: Satan
      date: "2024-12-10 06:30:22"
      content: |
        ```torch.compile``` speeds the flame,
        Trade-offs linger, but worth the game.
        Train or infer, it cuts the line,
        With care and craft, its power’s thine.
    - id: 35060
      author: Skv
      date: "2025-03-23 19:12:22"
      content: "Are there any graph-level optimizations passes that are applied with torch.compile? What is the location of those passes? (Is it the same as torch/jit/passes)"
    - id: 36157
      author: Daisie
      date: "2025-09-06 04:34:50"
      content: |
        "overselling the feature a teensy bit"
        
        Not a tiny bit. It is a lightyear away.
        I have NEVER seen a model that fit into category 1 and 2.
        I think torch.compile is a design mistake. 
        Writing the computation process in DSL, compile it and be able to reuse the compiled artifacts, is the correct way of speeding up pytorch.
---

On the surface, the value proposition of torch.compile is simple: compile your PyTorch model and it runs X% faster. But after having spent a lot of time helping users from all walks of life use torch.compile, I have found that actually understanding how this value proposition applies to your situation can be quite subtle! In this post, I want to walk through the ways to use torch.compile, and within these use cases, what works and what doesn't. By the way, some of these gaps are either served by export, or by missing features we are actively working on, those will be some other posts!

# Improve training efficiency on a small-medium scale

**Scenario:** You have a model in PyTorch that you want to train at a small-medium scale (e.g., below 1K GPUs--at the 1K point there is a phase change in behavior that deserves its own section). You would like it to train faster. Locally, it's nice to get a trained model faster than you would have otherwise. But globally, the faster everyone's models train, the less GPU hours they use, which means you can run more jobs in a given time window with a fixed cluster. If your supply of GPUs is inelastic (lol), efficiency improvement means you can support more teams and use cases for the same amount of available GPUs. At a capacity planning level, this can be a pretty big deal even if you are GPU rich.

**What to do:** In some sense, this is *the* reason we built torch.compile. (When we were initially planning torch.compile, we were trying to assess if we were going after inference; but inference compilers are a much more crowded space than training compilers, and we reasoned that if we did a good job building a training compiler, inference would work too--which it did!) The dream which we sold with torch.compile is that you could slap it on the top of your model and get a speed up. This turns out to... not quite be true? But the fact remains that if you're willing to put in some work, there is almost always performance waiting at the end of the road for you. Some tips:

- *Compile only the modules you need.* You don't have to compile the entire model; there might be specific modules which are easy to compile which will give you the most of the benefit. For example, in recommendation systems, there is not much compute improvement to be had from optimizing the embedding lookups, and their model parallelism is often quite hard to handle in the compiler, so `torch.compiler.disable` them. NB: This doesn't apply if you want to do some global graph optimization which needs the whole model: in that case, pass `fullgraph=True` to torch.compile and ganbatte!
- *Read the missing manual.* The [missing manual](https://docs.google.com/document/d/1y5CRfMLdwEoF1nTk9q8qEu1mgMUuUtvhklPKJ2emLU8/edit?tab=t.0#heading=h.ivdr7fmrbeab) is full of guidance on working with the compiler, with a particular emphasis on working on training.

**Open source examples:** [torchtune](https://github.com/pytorch/torchtune) and [torchtitan](https://github.com/pytorch/torchtitan) are two first party libraries which are intended to showcase modern PyTorch using torch.compile in a training context. There's also some training in [torchao](https://github.com/pytorch/ao).

**Downsides:**

- *The compiler is complicated.* One of the things we've slowly been coming to terms with is that, uh, maybe promising you could just slap torch.compile on a model and have it run faster was overselling the feature a teensy bit? There seems to be some irreducible complexity with compilers that any user bringing their own model to torch.compile has to grapple with. So yes, you are going to spend some of your complexity budget on torch.compile, in hopes that the payoff is worth it (we think it is!) One ameliorating factor is that the design of torch.compile (graph breaks) means it is very easy to incrementally introduce torch.compile into a codebase, without having to do a ton of upfront investment.
- *Compile time can be long.* The compiler is not a straightforward unconditional win. Even if the compiler doesn't slow down your code (which it can, in pathological cases), you have to spend some amount of time compiling your model (investment), which you then have to make back by training the model more quickly (return). For very small experimentation jobs, or jobs that are simply crashing, the time spent compiling is just dead weight, increasing the overall time your job takes to run. (teaser: async compilation aims to solve this.) To make matters worse, if you are scheduling your job on systems that have preemption, you might end up repeatedly compiling over and over again every time your job gets rescheduled (teaser: caching aims to solve this.) But even when you do spend some time training, it is not obvious without an A/B test whether or not you are actually getting a good ROI. In an ideal world, everyone using torch.compile would actually verify this ROI calculation, but it doesn't happen automatically (teaser: automatic ROI calculation) and in large organizations we see people running training runs without even realizing torch.compile is enabled.
- *Numerics divergence from eager.* Unfortunately, the compiler does not guarantee exact bitwise equivalence with eager code; we reserve the right to do things like select different matrix multiply algorithms with different numerics or eliminate unnecessary downcast/upcasts when fusing half precision compute together. The compiler is also complicated and can have bugs that can cause loss not to converge. Expect to also have to evaluate whether or not application of torch.compile affects accuracy. Fortunately, for most uses of compiler for training efficiency, the baseline is the eager model, so you can just run an ablation to figure out who is actually causing the accuracy problem. (This won't be true in a later use case when the compiler is load bearing, see below!)

# Improve Python inference efficiency

**Scenario:** You've finished training your model and you want to deploy it for inference. Here, you want to improve the efficiency of inference to improve response latency or reduce the overall resource requirements of the system, so you can use less GPUs to serve the traffic you are receiving. Admittedly, it is fairly common to just use some other, more inference friendly systems (which I will decline to name by name lol) to serve the model. But let's say you can't rewrite the model in a more serving friendly language (e.g., because the model authors are researchers and they keep changing the model, or there's a firehose of models and you don't have the money to keep continuously porting each of them, or you depend on an ecosystem of libraries that are only available in CPython).

**What to do:** If Python can keep up with the CPU-side QPS requirements, a way of getting good performance without very much work is taking the Python model, applying torch.compile on it in the same way as you did in training and directly using this as your inference solution. Some tips that go beyond training:

- *Autotuning makes the most sense for inference.* In training runs, you have a limited window (the lifetime of the training job) to get return on the investment you spent optimizing the model. In the serving regime, you can amortize over the entire lifetime of your model in inference, which is typically much longer. Therefore, expensive optimization modes like `mode="max-autotune"` are more likely to pay off!
- *Warmup inference processes before serving traffic to them.* Because torch.compile is a just-in-time compiler, you will spend quite a bit of time compiling (even if you cache hit) at startup. If you have latency requirements, you will want to warmup a fresh process with a representative set of inputs so that you can make sure you trigger all of the compilation paths you need to hit. Caching will reduce compile time but not eliminate it.
- *Try skip_guard_eval_unsafe to reduce guard overhead.* Dynamo guard overhead can be material in the inference case. If this is a problem, get a nightly and try [skip_guard_eval_unsafe](https://github.com/pytorch/pytorch/pull/140251).

**Open source examples:** LLM serving on torch.compile is quite popular: [vllm](https://github.com/vllm-project/vllm/issues/6378), [sglang](https://lmsys.org/blog/2024-09-04-sglang-v0-3/), [tensorrt-llm](https://pytorch.org/TensorRT/user_guide/torch_compile.html), [gpt-fast](https://github.com/pytorch-labs/gpt-fast) (this is technically not an E2E serving solution, but one of its primary reasons for existing is to serve as a starting point so you can build your own torch.compile based LLM inference stack on top of it). Stable diffusion models are also notable beneficiaries of torch.compile, e.g., [diffusers](https://huggingface.co/docs/diffusers/en/optimization/torch2.0).

**Downsides:**

- *Just in time compilation is a more complicated operational model.* It would be better if you didn't have to warmup inference processes before serving traffic to them. Here, torch.compile has traded operational simplicity for ease of getting started. If you wanted to guarantee that compilation had already happened ahead of time, you have to instead commit to some sort of export-based flow (e.g., C++ GPU/CPU inference) below.
- *Model and dependency packaging in Python is unaddressed.* You need to somehow package and deploy the actual Python code (and all its dependencies) which constitute the model; torch.compile doesn't address this problem at all (while torch.export does). If you are running a monorepo and do continuous pushes of your infra code, it can be organizationally complicated to ensure people don't accidentally break model code that is being shipped to production--it's very common to be asked if there's a way to "freeze" your model code so that the monorepo can move on. But with Python inference you have to solve this problem yourself, whether the solution is torch.package, Docker images, or something else.
- *Caches are not guaranteed to hit.* Do you have to recompile the model every time you restart the inference process? Well, no, we have an Inductor and Triton (and an in-progress AOTAutograd) cache which in principle can cache all of the cubin's that are generated by torch.compile. Most of the time, you can rely on this to reduce startup cost to Dynamo tracing the model only. However, the caches are not guaranteed to hit: there are rarer cases where we don't know how to compute the cache key for some feature a model is using, or the compiler is nondeterministic in a way that means the cache doesn't hit. You should file bugs for all of these issues as we are interested in fixing them, but we don't give a categorical guarantee that after you've compiled your inference program once, you won't have to compile it again. (And indeed, under torch.compile's user model, we can't, because the user code might be the root cause of the nondeterminism--imagine a model that is randomly sampling to decide what version of a model to run.)
- *Multithreading is currently buggy.* It should, in principle, be possible to run torch.compile'd code from multiple threads in Python and get a speedup, especially when CUDA graphs or CPP wrapper is used. (Aside: Inductor's default compile target is "Python wrapper", where Inductor's individually generated Triton kernels are called from Python. In this regime, you may get in trouble due to the GIL; CUDA graphs and CPP wrapper, however, can release the GIL when the expensive work is being done.) However, it doesn't work. Track the issue at <https://github.com/pytorch/pytorch/issues/136833>

# Like above, but the compiler is load bearing

**Scenario:** In both the cases above, we assumed that we had a preexisting eager model that worked, and we just wanted to make it faster. But you can also use the compiler in a load bearing way, where the model does not work without the compiler. Here are two common cases where this can occur:

1.  *Performance:* A compiler optimization results in an asymptotic or large constant factor improvement in performance can make a naive eager implementation that would have otherwise been hopelessly slow have good performance. For example, [SimpleFSDP](https://arxiv.org/abs/2411.00284) chooses to apply *no* optimizations to the distributed collectives it issues, instead relying on the compiler to bucket and prefetch them for acceptable performance.
2.  *Memory:* A compiler optimization reduces the memory usage of a model, can allow you to fit a model or batch size that would otherwise OOM. Although we don't publicly expose APIs for doing so, you can potentially use the compiler to do things like force a certain memory budget when doing activation checkpointing, without requiring the user to manually specify what needs to be checkpointed.

**What to do:** Unlike in the previous cases where you took a preexisting model and slap torch.compile, this sort of use of the compiler is more likely to arise from a codevelopment approach, where you use torch.compile while you build your model, and are constantly checking what the compiler does to the code you write. Some tips:

- *Don't be afraid to write your own optimization pass.* Inductor supports custom FX optimization passes. torch.compile has done the work of getting your model into an optimizable form; you can take advantage of this to apply domain specific optimizations that Inductor may not support natively.

**Open source examples.** SimpleFSDP as mentioned above. VLLM uses torch.compile to apply custom optimization passes. Although its implementation is considerably more involved than what you might reasonable expect a third party to implement, [FlexAttention](https://pytorch.org/blog/flexattention/) is a good example of a non-compiler feature that relies on the compiler in a load-bearing way for performance.

**Downsides:** Beyond the ones mentioned above:

- *You can no longer (easily) use eager as a baseline.* This is not always true; for example, FlexAttention has an eager mode that runs everything unfused which can still be fast enough for small experiments. But if you have an accuracy problem, it may be hard to compare against an eager baseline if you OOM in that case! It turns out that it's really, really useful to have access to an eager implementation, so it's worth working harder to make sure that the eager implementation works, even if it is slow. (It's less clear how to do that with, e.g., a fancy global optimization based activation checkpointing strategy.)

------------------------------------------------------------------------

**Next time:** [Ways to use torch.export](http://blog.ezyang.com/2024/12/ways-to-use-torch-export/)
