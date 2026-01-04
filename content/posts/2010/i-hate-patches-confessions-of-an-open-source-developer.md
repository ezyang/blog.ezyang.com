---
title: "I Hate Patches: <br>Confessions of an Open-Source Developer"
date: 2010-05-17 09:00:11
slug: i-hate-patches-confessions-of-an-open-source-developer
categories: [Personal, Teaching]
comments:
    - id: 419
      author: Juho Vepsäläinen
      date: "2010-05-17 13:01:27"
      content: |
        It would be cool if more projects used brainstorm approach (see http://brainstorm.ubuntu.com/ for reference). I think it would help development cycle quite a bit.
        
        Patch represents an implementation of an idea. It's better if the idea is good in the first place. If you can prune bad ideas (this is subjective and up to the project/module owner to decide :) ) early on, it saves some cycles for real, productive development work.
        
        It also helps in avoiding frustration. Nothing's more painful than having to wait for a long time for a review. It isn't good for the project either as it's quite possible to alienate potential committers this way.
    - id: 421
      author: Edward Z. Yang
      date: "2010-05-17 16:05:47"
      content: |
        I won't argue with the benefits of up front interface and usability design; sometimes a rough mockup is much better at making people ask tough questions than something relatively close to the final product would be (which is more prone to bikeshedding.)
        
        However, there is an extremely large amount of effort that needs to be put into engineering what may seem like a conceptually simple change, and often an end user, even a developer-savvy one, isn't in the place to dictate the overall design. For example, the idea of networked whiteboarding came up recently on the Xournal development list, and unsurprisingly the lead developer Denis Auroux had the most coherent and cohesive view of what would need to happen to make such functionality happen. Features in particular are suspect to iceberg perception: simple on the surface, but so deep that even with three months of dramatically changing the code base the lead developer, who wrote all of the original code, couldn't manage to cook the change thoroughly.
    - id: 422
      author: Juho Vepsäläinen
      date: "2010-05-18 00:33:18"
      content: |
        Note that I agree on your point that generally patches tend to suck, one way or another. This is more of a process issue for me. What can be done to make work more effective and focused better?
        
        In case of Xournal it seems that the idea helped to refocus the project goal. As you mentioned, it ended up causing a lot of work. I don't think there's an easy way around that. Possibly some external solution could be used, perhaps not. Open source projects are suspectible to scope creep, no denying that.
        
        Scope creep may be avoided by aiming as lean integration as possible. A new feature should provide just the bare essentials. It can be enhanced further later. I admit keeping it lean may not be always easy. Particularly if the feature spans throughout the architecture of the program.
        
        It may even cause major re-engineering to make it happen. In this case it may be better to put the feature in the back burner and postpone it till these changes may done properly. Of course work may be aligned so that these changes happen gradually instead in one big bang that fixes it all.
        
        Sometimes patches function as proofs of concept. They provide more insight to the new feature more than a technical description ever could. It is just so much easier to see something in action than to read and examine mockups.
        
        Of course technically the code may be flawed but that's not the point. In some cases patches may act as a throwaway commodity that help to illustrate ideas better. I suppose in open source community it's more fun just to code than write specifications but I digress.
    - id: 423
      author: Magnus
      date: "2010-05-18 04:44:22"
      content: "Hear! Hear!"
    - id: 424
      author: Felipe Contreras
      date: "2010-05-18 16:37:14"
      content: "There are other projects with good patch review processes. Basically, all the ones related to the linux kernel."
---

It is a truth universally acknowledged that if you *really* want a change put into an open source project, you submit a patch along with the bug report. Sure, you might complain that the *average* user doesn't have any programming experience and that it's *unreasonable* to expect them to learn some complex system and then figure out how to make the change they're looking for, but you're just not in the secret society of hacker enthusiasts who fix their own damn software and give back to the projects they use.

I hate patches. I feel *morally obligated* to review and patch them in, and it usually ends up taking more than just "a few cycles."

Not all patches are created equal. I group them into this hierarchy:

- *Cosmetically deficient.* The developer doesn't know anything about submitting patches; perhaps they haven't even discovered version control yet. They're apt to sending the entire modified file along with their changes. Those who do use `diff -u` don't bother looking at the output of the patch; they submit patches that swap spaces with tabs, random whitespace changes and gratuitous cosmetic changes. Many developers simply reject these patches.
- *Semantically deficient.* For some developers, the act of crafting a patch is taking a source file, trying a vaguely plausible change, seeing if the change had the desired effect, and if not, try something else. In the degenerate case, the patch is nonsense, and in no way correct. More frequently, the submitted patch fails to account for common edge-cases in the application, appropriate error handling or interactions with other parts of the system. Many developers will reply nicely to patches like this and ask for a hunk to be done another way.
- *Engineering deficient.* The patch is well-written, it looks good and does the right things. But... they didn't add tests to test the new changes, they didn't fix old unit tests changed by the functionality difference and they didn't add documentation in the appropriate places for the fix. Many developers will buckle down and make the engineering extras for the patch. Some developers don't have such tests (cough Linux kernel cough). Even more rarely, some projects can afford to make the patch submitter add the tests; usually this only occurs in projects that are aimed towards a fairly literate programming end-user community.

The Git mailing list can and does expect excellent patch submissions from its community; it's a version control system, that's the point! A library written in PHP used primarily by developers who have never written a unit test or submitted a unified diff for upstream review has much less flexibility. Most patches I receive for HTML Purifier never make it past the cosmetic deficiencies. And worse, the developers simply don't have the time to interactively improve the patch to the end: if I reply with a patch review, they never manage to get their patch to the point where it's acceptable for submission without excessive tooth pulling. But I feel guilty that my software is wrong, and so when I get the patch, I go and clean it up, incorporate it in, rewrite half of it, add the tests and then ship the change.

So, in the end, the software is improved by the submission by the patch, even if it didn't save the maintainer any time. So yeah, I hate patches. Maybe I should stop being *grumpy* and go back to *improving* my open source projects.
