---
title: "OSS code review, in the era of LLMs"
date: 2026-04-17T10:31:05-04:00
slug: "oss-code-review-in-the-era-of-llms"
categories: ["AI Coding"]
---

In [Code review as human alignment, in the era of LLMs]({{<relref "../2025/code-review-as-human-alignment-in-the-era-of-llms.md">}}), I talked about how we should approach code review with other team members, where you have a pre-existing relationship and care about building human-to-human alignment over time.  However, popular open source projects also need to account for drive-by contributions from external contributors, where it is unclear if you want to invest in a long term relationship with these people.  In the era of LLMs, it has never been easier to submit a pull request to a project, and consequently, many OSS projects have described being overwhelmed by a wave of slop AI pull requests.  Some projects have responded by [simply refusing all external contributions](https://github.com/tldraw/tldraw/issues/7695), while others have resorted to [banning bad actors](https://github.com/ghostty-org/ghostty/blob/main/AI_POLICY.md) with a quick trigger finger.

Dealing with external contributions has always been a problem for overworked OSS maintainers, but the current volume I think warrants a discontinuous change in behavior.  Having spent a lot of my career helping shepherd OSS PRs into PyTorch, for codebases where we still care about human understanding, I think there are three primary problems to solve:

* Don't let randos hijack your attention. (GitHub notifications is broken.)
* Reduce round trips as much as possible. (You can use LLMs too.)
* Offload work to the contributor. (Act like a short on time exec.)

# Don't let randos hijack your attention

An average person can easily afford to have open DMs, because for the most part, no one is going to send you a direct message, and when it does occur it is likely to be very high signal.  Compare this with the president, a celebrity or a CEO: the volume of inbound messages means that it absolutely makes sense to have a secretary make a determination about whether or not a message should actually make it to you or not.  Even if the president does occasionally write response letters to random citizens who send him correspondence, who to correspond to is picked by his staff and clearly timeboxed.

GitHub notifications... is not this.  It is the equivalent of open DMs.  If you are relying on GitHub notifications to figure out what work you need to do, you're going to have a bad time.  You cannot let randos hijack your attention simply by the fact that GitHub has no way of letting you reject notifications you don't care about.

I don't have a mechanical prescription for to fix this; in particular, I wouldn't rely on GitHub actually adding direct UX for this any time soon.  You could try delegating to email notifications, or vibe code an alternative notifications UI like in [ghinbox](https://github.com/ezyang/ghinbox/) (but I will be first to admit that this particular UI is very specific to my preferences, and I am still working out what exactly I want.)

# Reduce round trips as much as possible

The worst case scenario for a PR review is you put in the time and effort into a review... and then nothing.  All of that work is wasted.  The second worst case scenario is you put time and effort into a review, and then nothing happens for a few weeks and it drops from your context window, and then it pops up again and you have to page everything back in again (every PR is quadratic effort! Yay!)

If your review is purely mechanical changes and the core spirit of the change is good, don't bother waiting for the original author to get back to you.  Take over the PR, have a coding agent do the rest of the needed changes and just land it.  No round trips and a predictable latency to completion.  If you're token limited, set a dollar budget on how much money you're willing to spend shepherding, and then prioritize contributions accordingly.

There are some process problems in GitHub that make the "take over a contributed PR" more difficult than it should be.  For example, users can choose not to allow maintainers to push to their branch.  My main recommendation is to make it frictionless to create a new PR that is based off an OSS PR, but assigned to your username.  For security reasons, once you take over a PR, the original author should not be able to make changes to it.  If you had a policy where only a single maintainer was necessary to approve an external PR to land, you shouldn't require another stamp on these PRs.  The new PR should credit the original author as much as possible, but GitHub has some technical limitations that make full credit difficult; we should all decide that this is socially acceptable.

Because you are going to be shipping a lot more PRs, it is also important to make sure the friction for shepherding a PR to completion is minimized.  If your project has a CI that takes a long time, use agents to automatically fix CI problems and retrigger CI, so that you never have "dead time" where there is an easy to fix CI failure that isn't being actioned on.  Use agents to do mechanical reviews that don't have any cleverness but just require methodological diligence.  Make sure that your limiting factor is human review bandwidth.

# Offload work to the contributor

There will still be a decent number of PRs where it doesn't make sense to take over and ship it.  Here, my advice is that *you are the bottleneck*, and you should shed load to parts of the system with the capacity to absorb it.  In particular, you can (imperfectly) shed load to the external contributor.  Obviously, you cannot abdicate all responsibility to the contributor.  But there are many things you can do that, in the previous era, might have been considered rude or antisocial:

* One bite at the apple: a given contributor gets one chance, and after that, that's it, no more "free attention" budget for you.  As an external contributor, you would then have to demonstrate some more costly signal that you are worth engaging with.
* Don't make me think: if there's something funny going on and maybe you could figure it out if you sat down and deeply engaged with the problem, but the author hasn't explained it clearly, don't think. Just push it back to the author. Or if you think the PR is AI coded, ask an AI agent directly (but this is much more in the "you are planning to take it over" regime).
* If you don't like it, just close it, with a brief explanation why.  If you don't care about solving the root problem, don't spend time trying to figure out what the right way to do it is.  That's the contributor's responsibility.

I know this sucks for people who are just starting out and trying to climb the ladder by starting out with open source.  Standards are higher now.  But it's also never been such a wonderful time to engage with an unfamiliar project, with an AI agent who can write code and is infinitely patient when you ask questions of it.  I'm pulling up the ladder... but you also have stilts now.  You can manage.  As a maintainer, if you *do* care about helping people get their start (and I know there are plenty of you that do), I think it's fine to spend time on this, just remember... self care comes first!

# Conclusion

There's no doubt about it: OSS contribution is going to look a little different in the LLM universe.  But I think it is still possible to maintain a productive relationship with people who send in PRs, potentially even easier if you can spend tokens; the goal of this post is to describe what I see as some of the technical and social barriers to doing so.

Oh, and this is just about code that humans care about understanding.  I think the rules are a bit different for entirely vibe slop codebase!  Intended in the most non-judgmental way possible--in this regime, I think you want to invest in *verifiers*.
