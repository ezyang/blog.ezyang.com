---
title: "Vibe Coding Design Study: tlparse"
date: 2026-02-03T22:35:59-05:00
slug: "vibe-coding-design-study-tlparse"
categories: [AI Coding]
---

I recently received the following question about vibe-coding for [tlparse](https://github.com/meta-pytorch/tlparse), a structured log parser for `torch.compile` (slightly edited for ease of reading):

> Hi Ed, I have some thoughts on vibe-coding tlparse and I'm curious what your opinions are. I think it's fairly easy to vibe code and improve tlparse, but it's hard to find people who know Rust or HTML or JavaScript well to properly review the vibe-coded stuff.  The Rust PRs are not impossible to review, but the JavaScript ones are certainly hard...  YOLO-landing PRs that we can't review is certainly bad, I guess one option is just not landing any changes, and tell people to vibe-code themselves...?

> I wonder if you have any opinion on this? I saw one of your BE week proposals is for a more vibe-coding friendly tlparse. Do you think we should just not attempt to review or land any front-end features (which most likely we cannot review), and all-in on the "custom vibe-coded" frontend route?

Oh boy, do I have opinions!

# High stakes, low stakes

When is it acceptable not to review code?  I find this is the wrong question to ask, because code review is only a proxy for the underlying measure we actually care about: *does it matter if this code is good?*  And to answer this question, we have to understand whether or not whether or not we are in a high stakes or low stakes situation.

Here are some things that suggest high stakes:

* It does destructive/irreversible actions
* It used by many users (human or computer)
* It covers a large surface area
* It constitutes BC surface
* It handles money, private data, secrets, lives
* It runs automatically
* There are no automated tests / it is tested in prod

Here are some things that suggest low stakes:

* It is personal use only
* It is disposable or exploratory
* The output is easy to verify
* It is trivial to rollback to old versions (or better yet, you can run as many versions as you want at the same time)
* It comes with no warranty
* It is a starting point, rather than a complete product
* No side effects, no persistent data

Many problems won't neatly fall into one bucket or another, but it's still helpful to know what the high stakes aspects are, because you can spend more effort (e.g., doing code review) on those aspects and less on the low stakes things.  Also, being aware of why something is high stakes can push you towards restructuring things so that a problem becomes lower stakes.

Let's run this exercise for tlparse.  tlparse is generally a low stakes project: it takes a structured logs and generates HTML files for viewing it.  You can (in principle) run newer or older versions of it, and as a program with no side effects it is basically as simple as these things get.

However, there are some things that are high stakes about it.  It *is* used extremely widely internally at Meta; if it was broken, we would likely get a message within a day from someone who was relying on it to diagnose problems in training jobs (including production SEVs.)  The way it is deployed internally is as a classic executable which is automatically rolled out as new versions are published; most users don't know how to run an old version of it--so while *in principle* it can be rolled back trivially, in practice you would need to instruct users on how to do so.  Finally, although it doesn't do that much (just generate HTML from a structured log), there is a large amount of variety of actual logs you see in production, which makes it difficult to comprehensively test it.  A memorable example from the past is someone adding syntax highlighting to the tool.  I reverted this because it caused extremely long files to take a long time to parse, and it also made it more difficult to grep for specific lines in the generated folder.

How can you lower the stakes?  In the case of tlparse, here are some ideas:

* Have a separation between prod (the stable stuff that is shown by default) and experimental (the wacky untested stuff that needs some dogfooding to see if it works)
* Make it easy to have multiple versions of tlparse; update broke something, just go back to your favorite version
* Don't deploy tlparse via a single rollout mechanism; have it as a local app that people can run / vibe code on without a deployment step
* Improve testing to ensure features don't break
* Keep it simple, don't add lots of features to keep the surface area of things to test simpler

# Evaluating LLM generated code

Another trap is to think of code review as an all or nothing option: "if I haven't done a careful line-by-line review of this code, I might as well just not review it at all."  There are lots of ways to evaluate LLM generated code, and some of these don't involve reviewing the code at all.

First, let's talk about evaluating LLM generated code in high stakes situations.  The bar here is that your LLM generated code should be indistinguishable from the code you would have written yourself: the LLM just helped you type faster.  This is a very high bar: imagine a mathematical proof written by someone not you.  Can you just read the proof and understand it?  Typically not: the only way to a real, durable understanding is to work through the steps of the proof yourself.  It is difficult, intellectually taxing work--and you cannot offload this to the LLM, because the LLM isn't the owner of the code, you are the owner of the code!  I find in a situation like this it is best to steer the LLM very closely during authoring: I should have a clear idea of what I want written, and the LLM is just typing, and I am pausing and correcting it when it does things I don't want.  If you have the LLM run by itself for an hour, you'll end up with code that is not yours, and you will have to spend the effort to make it yours.  It is much easier, with both LLMs and regular colleagues, to own code if you're involved every step in its conception.

A lower stakes situation is when the exact details of the code don't matter, but you're still thinking architecturally about the problem.  I often am in this mode when I'm doing exploration: I don't care about the exact things the LLM is typing, but I do care that it roughly has the right shape.  After you've used LLMs a bunch, you get a sense for what kinds of mistakes they do and don't make.  You can just skip reviewing all the things that you know LLMs generally won't mess up, and just look for the big strokes.  To do this, however, you need to have a good, high level understanding of how things should work.  If there's a big pile of JavaScript code and you know absolutely nothing about how DOMs work, this isn't going to work!  And if there is a way to do something without having reams of JavaScript, you should absolutely steer the LLM to not do that.

In a situation where you are vibe coding and not looking at the generated code at all, you still have a very important job.  You need to actually Q&A the feature and see if it actually works.  I find for my pure vibe coding projects, this is the most time consuming part: I can ask the LLM to do anything, but then actually checking if it works (and transmitting feedback to the LLM) takes up most of my time.  You can ask the LLM to write tests, but then you have to check if the tests are actually testing the important thing (to be clear, reviewing LLM generated tests is very high leverage.)  The golden city in the distance is the LLM being able to Q&A the tool for yourself, but for something like tlparse HTML, we are still very early days in this kind of sophisticated browser use.  (Claude has come a long way here, though, and I expect it to get better.)

# So, what should you do?

Let's look at some concrete PRs:

* [Add create symbols logs to compilation metrics artifact](https://github.com/meta-pytorch/tlparse/pull/168) - I agree with the outcome (an approval) here.  It is not super intrusive, the new indexes are a little messy but this is unlikely to cause problems with other parts of the system.
* [Add style to provenance tracking](https://github.com/meta-pytorch/tlparse/pull/166) - Big JavaScript changes, likely all vibe coded. To get out of draft, there needs to be evidence of dog fooding to show it actually works. There will likely be bugs. It might be worth getting this into a lower stakes setting to iterate more quickly on real data.

It is perhaps unsatisfying that you have to evaluate things on a case-by-case basis.  But hopefully this post gives you some framework to think about it.
