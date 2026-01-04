---
title: "Recovering from upstream interactive rebases"
date: 2011-07-17 07:53:48
slug: recover-interactive-git-rebase
draft: true
categories: [Miscellaneous]
---

We here at the Ministry of Truth understand that history is mutable: that is, we can and should rewrite history to suit our aesthetic desires and practical needs. The tool by which we achieve this trick is `git rebase -i`, which permits us to amend, add or delete commits. Unfortunately, if we had already published this history and `git push -f`, downstream users will now see diverging lines of history. In this blog post, we describe one way of automatically updating such downstream repositories to be using the new history. We’ll also describe the particular circumstance which prompted a tool like this.

# Assumptions
