---
title: "Same repository access control for Git"
date: 2012-10-06 02:17:10
slug: 
draft: true
categories: [Git]
---

A common question asked by users of Git is how to enforce access control on repositories; for example, Alice should be allowed to write to the `images` folder, but not to the entire repository. The most common answer is “Don’t do that,” with a side dish of “Split it up into separate repositories and use an out-of-band mechanism to enforce access control.”

Of course, these mechanisms rely on some centralized authority to actually do the checking, so our *decentralized* version control system doesn’t live up to its name, at least when it comes to access control! It also means if I can break into your server and push some malicious commits, if people aren’t looking closely these commits could propagate (until a rebase was performed). So it’s worth pondering, how might we build access control in a decentralized way?

For simplicity’s sake, let’s just consider the case where the access control list is completely static and known by everyone pushing and pulling. Each member of the access control list has a public key associated with their name. The naïve solution would then involve having every commit be signed by the committer, and during a pull, every commit would be checked to see if it had a valid signature and if the commit only touched files which the corresponding public key had rights to.
