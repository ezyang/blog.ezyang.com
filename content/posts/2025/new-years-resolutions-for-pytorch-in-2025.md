---
title: "New Years resolutions for PyTorch in 2025"
date: 2025-01-09 15:50:08
slug: new-years-resolutions-for-pytorch-in-2025
categories: [PyTorch]
comments:
    - id: 34436
      author: Anonymous
      date: "2025-01-11 23:37:13"
      content: Nice
    - id: 34448
      author: LP
      date: "2025-01-13 04:57:43"
      content: "Highly excited for the ABI stable PyTorch extension! I think this will enable lots of great (ML-) systems research, and I cannot thank you guys enough for your work."
    - id: 35432
      author: Anonymous
      date: "2025-05-20 01:34:18"
      content: |
        Great insights shared in this blog—PyTorch continues to empower developers and researchers with its flexibility and performance. We’ve recently put together a YouTube playlist covering hands-on PyTorch tutorials, ideal for both beginners and intermediate users. Feel free to explore and learn along with us:
         PyTorch YouTube Playlist_https://www.youtube.com/watch?v=38mfRY2_AV0&amp;list=PL7CWBDRZZ_QfvjuE02voEH7O3f_iHDrWi&amp;pp=gAQB
        
        Looking forward to more valuable content from the community!
---

In my previous two posts "\`Ways to use torch.compile \<http://blog.ezyang.com/2024/11/ways-to-use-torch-compile/\>\`\_" and "\`Ways to use torch.export \<http://blog.ezyang.com/2024/12/ways-to-use-torch-export/\>\`\_", I often said that PyTorch would be good for a use case, but there might be some downsides. Some of the downsides are foundational and difficult to remove. But some... just seem like a little something is missing from PyTorch. In this post, here are some things I hope we will end up shipping in 2025!

# Improving torch.compile

**A programming model for PT2.** A programming model is a an abstract description of the system that is both simple (so anyone can understand it and keep it in their head all at once) and can be used to predict the system's behavior. The [torch.export programming model](https://docs-preview.pytorch.org/pytorch/pytorch/143546/export.programming_model.html) is an example of such a description. Beyond export, we would like to help users understand why all aspects of PT2 behave the way it does (e.g., via improved error messages), and give simple, predictable tools for working around problems when they arise. The programming model helps us clearly define the intrinsic complexity of our compiler, which we must educate users about. This is a big effort involving many folks on the PyTorch team and I hope we can share more about this effort soon.

**Pre-compilation: beyond single graph export.** Whenever someone realizes that torch.compile compilation is taking a substantial amount of time on expensive cluster machines, the first thing they ask is, "Why don't we just compile it in advance?" To support precompiling the torch.compile API exactly as is not so easy; unlike a traditional compiler which gets the source program directly as input, users of torch.compile must actually run their Python program to hit the regions of code that are intended to be compiled. Nor can these regions be trivially enumerated and then compiled: not only must know all the metadata input tensors flowing into a region, a user might not even *know* what the compiled graphs are if a model has graph breaks.

OK, but why not just run the model, dump all the compiled products, and then reuse them later? This works! Here is [a POC from Nikita Shulga](https://github.com/lianakoleva/no-libtorch-compile/blob/master/sticky_cache.py) where a special decorator `aot_compile_sticky_cache` swaps between exporting a graph and running the exported product. [Zhengxu Chen used a similar idea](https://github.com/zhxchen17/torchnative/blob/main/whisper_aoti/export.py) to export Whisper as a few distinct graphs, which he then manually stitched together in C++ to get a Python-free version of Whisper. If you want training to work, you can more directly integrate AOTInductor as an Inductor backend, e.g., as seen in [this POC.](https://github.com/pytorch/pytorch/pull/141700). We are a stones throw away from working precompilation, which can guarantee no compilation at runtime, we just need to put the pieces together!

**Improving caching further.** There are some gaps with caching which we hope to address in the near future: (1) loading Triton cache artifacts takes a long time because we still re-parse the Triton code before doing a cache lookup (James Wu is on this), (2) if you have a lot of small graphs, remote cache ends up having to do lots of small network requests, instead of one batched network request at the beginning (Oguz Ulgen recently landed this), (3) AOTAutograd cache is not fully rolled out yet (James Wu again). These collectively should be worth a 2x speedup or even more on warm cache time.

**Fix multithreading.** We should just make sure multithreading works, doing the testing and fiddly thread safety auditing needed to make it work. Here's [a list of multithreading related issues.](https://github.com/pytorch/pytorch/issues?q=sort%3Aupdated-desc+is%3Aopen+label%3A%22module%3A+multithreading%22)

# Improving torch.export

**Draft mode export.** Export requires a lot of upfront work to even get an exported artifact in the first place. Draft mode export capitalizes on the idea that it's OK to generate an unsound "draft" graph early in the export, because even an incorrect graph is useful for kicking the tires on the downstream processing that happens after export. A draft export gives you a graph, and it also gives you a report describing what potential problems need to be fixed to get some guarantees about the correctness of the export. You can then chip away on the problems in the report until everything is green. One of the biggest innovations of draft-mode export is pervasive use of real tensor propagation when doing export: you run the export with actual tensors, so you can always trace through code, even if it is doing spicy things like data-dependent control flow.

**Libtorch-free AOTInductor.** AOTInductor generated binaries have a relatively small ABI surface that needs to be implemented. [This hack from the most recent CUDA Mode meetup](https://github.com/lianakoleva/no-libtorch-compile) shows that you can just create an alternate implementation of the ABI that has no dependence on libtorch. This makes your deployed binary size much smaller!

**Support for bundling CUDA kernels into AOTInductor.** AOTInductor already supports directly bundling Triton kernels into the generated binary, but traditional CUDA kernels cannot be bundled in this way. There's no reason this has to be the case though: all we're doing is bundling cubins in both case. If we have the ability to bundle traditional CUDA kernels into AOTInductor, this means you could potentially directly embed custom operators into AOTInductor binaries, which is nice because then those operators no longer have to be offered on the runtime (especially if you're commonly iterating on these kernels!)

**Export multigraphs.** Export's standard model is to give you a single graph that you call unconditionally. But it's easy to imagine a level of indirection on top of these graphs, where we can dispatch between multiple graphs depending on some arguments to the model. For example, if you have a model that optionally takes an extra Tensor argument, you can simply have two graphs, one for when the Tensor is absent, and one for when it is present.

**ABI stable PyTorch extensions.** It's hard work being a third-party PyTorch extension with native code, because whenever there's a new release of Python or PyTorch you have to rebuild all of your wheels. If there was a limited ABI that you could build your extension against that didn't expose CPython and only relied on a small, stable ABI of PyTorch functions, your binary packaging situation would be much simpler! And if an extension relied on a small ABI, it could even be bundled with AOTInductor binary, letting these export products be truly package agnostic (one of our lessons we learned with torch.package is picking the split between "what is packaged" and "what is not" is very difficult, and people would much rather just have everything be packaged.) Jane Xu is investigating how to do this, and separately, Scott Wolchok has been refactoring headers in libtorch so that a small set of headers can be used independently of the rest of libtorch.
