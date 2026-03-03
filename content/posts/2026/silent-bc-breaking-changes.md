---
title: "Silent BC Breaking Changes"
date: 2026-03-02T22:45:08-05:00
slug: "silent-bc-breaking-changes"
categories: ["Software Engineering"]
---

A **silent BC breaking change** is a change of semantics in an API that is not immediately obvious; e.g., the old usages of the broken behavior won't raise a compile or runtime error on upgrade.  If you are a project that cares about backwards compatibility, it's generally a bad idea to ship silent BC breaking changes, because they manifest as users getting silently incorrect results and having to painfully root cause the difference to a particular version upgrade.  However, many bug fixes technically are silent BC breaking changes, especially when the old buggy behavior could conceivably be useful in certain circumstances.  Load bearing bugs are often the reason why sometimes bug-for-bug compatibility is required.

For example, in [PyTorch PR #167598](https://github.com/pytorch/pytorch/pull/167598), we are addressing a bug where calling `item()` on a partial `DTensor` (a tensor which is pending a summation across the nodes in your cluster) produces the *partial* value, rather than the *full* value (after having done the summation).  This is a bug, because the existing behavior is inconsistent with how DTensor generally handles operations in [global SPMD semantics]({{<relref "global-vs-local-spmd.md">}}).  However, the current behavior is potentially *useful*, because computing the full value requires actually issuing a collective across the nodes, which is expensive, and maybe you actually did want to just look at the local partial value.

So let's say we want to fix this bug and make this "BC breaking" change.  What are our options?

**YOLO fix.**  If we strongly believe that no one possibly was using the old semantics, or that the software is young and unstable enough that breaking changes are OK, the simplest and easiest thing to do is just change the behavior and write a note about it in the release notes.  Anyone who was previously triggering this behavior will silently get opted into the new behavior, for better (they didn't realize their code was buggy and was expecting the correct semantics) or worse (they were intentionally relying on the old semantics and now their code is broken).

When an API like this has a plausible alternate semantics (return the partial value), it's important in the release notes to describe how you can recover the old semantics.  In this particular case, you would rewrite:

```
partial_dtensor.item()
```

into

```
partial_dtensor.to_local().item()
```

which unambiguously indicates that you wanted the partial value without doing any communications.  Conversely, it's also helpful to indicate to users how to get the new semantics in older versions of your software.  In this case:

```
partial_dtensor.redistribute(placements=[Replicate()]).item()
```

**Turn a silent break into a noisy one.**  A general rule for diagnosable systems is that you should endeavor to fail noisily and as quickly as possible.  So in the case above, we can instead make `partial_dtensor.item()` directly raise an error, instructing users to switch to either the `to_local` or `redistribute` forms to disambiguate which semantics they wanted.  If it is desirable to reclaim the original API form `partial_dtensor.item()` (which it is here), after a suitable amount of time, you can make the original API stop erroring and now produce the new behavior: the point is to force everyone to hit the error and disambiguate their call sites before you (safely) turn a previously erroring API into a usable one.

If you expect users of the old behavior to be rare, or in situations where it's easy to fix the problem in userland, this two-step process is all you need.

**Making a BC break gentler.**  Now, to be clear, making a function that used to work raise an error is still a BC breaking change, and you can potentially break people by doing so!  In a situation where the BC breaking API to be widely used, you can soften the blow by adding more steps:

1. First, make the deprecated API raise a warning.
2. Next, make the deprecated API raise an error.
3. Finally, switch the deprecated API to the new semantics.

If you are rolling out your change to a bigco style monorepo, it is better to replace a "warning" with "some sort of telemetry where you can identify all users of the deprecated API."  Then you can roll out (1) to the fleet, look for breakages and proactively fix them before moving on to step (2).  However, sometimes release cycles can be very slow (from personal experience, it can take more than a month for new code to be taken up in some systems), so you can't rely on this catching everything.

When you actually make the API raise an error, it is helpful to have a circuit breaker or feature flag that lets you easily revert back to the old (non-erroring) behavior.  This is helpful if someone is trying to take a version update of your software, and everything is fine *except* for this BC breakage.  The feature flag lets you temporarily turn off the BC breakage until you're able to go fix the code, because sometimes it is inconvenient to rollback all of the code.

**Miscellanea.**  Sometimes, it is difficult to tell if a deprecated usage of an API occurred.  For example, in the past I worked on an ill-fated attempt to make `reshape()` always return semantically a clone of a tensor (using copy-on-write when a clone was not actually needed).  It is difficult to tell if a `reshape()` usage here is deprecated, because to observe it you have to do a observable mutation on the input/output of reshape and observe this mutation in the output/input; it's not as simple as raising an error when `reshape()` is called.  In a situation like this, you may need to build an apparatus to simply *detect* if you should error or not in the first place.
