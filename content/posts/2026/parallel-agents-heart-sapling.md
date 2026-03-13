---
title: "Parallel Agents ❤️ Sapling"
date: 2026-03-13T11:00:47-04:00
slug: "parallel-agents-heart-sapling"
categories: ["AI Coding"]
---

Previously in [AI-assisted programming for spmd_types]({{<relref "ai-assisted-programming-for-spmd-types.md">}}), I mentioned that I have been enjoying using Sapling (the version control system Meta uses internally) to manage parallel agents on worktrees.  In this post, I want to describe this workflow in more detail.  The basic development flow here is I have three work streams going in parallel:

1. I am working on enabling an end-to-end use case; in this case, enabling strict type-checking on a realistic codebase.
2. I am adding new features and fixing bugs to the core library itself, as identified by (1).
3. I am addressing code review comments and CI failures for the diffs produced by (2).

All of this is in the context of a single stack, the bottom of which is the feature diffs, and the top of which is the enablement diffs.  I have multiple worktrees: one at the top for E2E enablement, one or more at the top of the feature diffs to add new features, and one or more inside the stack to address code review and fix CI.  All of these are going in parallel, and so the problem is how to coordinate these parallel changes on a single source-of-truth stack.  Git does terribly at this, but if you know the trick, Sapling does really well!

# Sapling primer

*You can skip this section if you're familiar with Sapling.  Disclosure: this section is AI generated and then heavily edited by me, all mistakes are mine.*

If you've used Git, you're used to branches. Sapling replaces branches with
**stacks**: linear chains of draft commits sitting on top of public (landed)
commits. There are no branch names to manage. You just make commits, and they
form a stack.

## Smartlog: your best friend

The first thing to internalize is `sl` (short for `sl smartlog`). Run it with
no arguments and you get a view of all your in-flight work:

```
$ sl

  o  00030000  39 minutes ago  ezyang
  │  Wire up the new endpoint
  │
  @  00020000  39 minutes ago  ezyang
  │  Add request validation
  │
  o  00010000  39 minutes ago  ezyang
╭─╯  Define the schema
│
o  00000000  45 minutes ago  remote/master
```

In the example logs I'm showing, I use fake hashes of the form `000X000Y`, where `X` identifies the logical commit and `Y` is the amend/restack version. So `00020000` and `00020001` are two versions of the same commit (before and after an amend).

A few things to notice about the smartlog:

- **`@`** marks where you are (your working directory parent).
- **The kink** (`╭─╯`) is how you can tell where the public/draft boundary is.
  Everything above the kink is your draft stack; below is public.
- **Public commits** are the landed/pulled ones at the base. Sapling hides
  the thousands of other public commits—you only see what's relevant.
- **No branch names.** The stack *is* the branch. If you have two independent
  pieces of work, you'll see two separate stacks in smartlog.

You can have all sorts of stacks running around for parallel work, but in the flow we're going to describe there will be one main stack we're worrying about, with temporary branches.

## Navigating the stack

Sapling gives you commands to move up and down without remembering hashes:

```
sl prev       # move down one commit
sl next       # move up one commit
sl top        # jump to the top of the stack
sl bottom     # jump to the bottom of the stack
```

These are relative to the current stack. `sl top` doesn't take you to some
other stack—it goes to the tip of whichever stack you're currently in.
You can also directly `sl checkout HASH` to jump to a particular commit.

## Working on a stack

When you `sl commit` a change, it makes a new commit in your stack.  If you're in the middle of your stack, commiting will create a fork, like this:

```
  o  00030000  ezyang
  │  Wire up the new endpoint
  │
  │ @  00040000  ezyang
  ├─╯  Fix validation bug
  │
  o  00020000  ezyang
  │  Add request validation
  │
  o  00010000  ezyang
╭─╯  Define the schema
│
o  00000000  remote/master
```

If you `sl amend` a change, by default Sapling will modify the change and then `sl restack` all the downstream commits so they are on top of the amended commit.

```
  o  00030001  ezyang
  │  Wire up the new endpoint
  │
  @  00020001  ezyang
  │  Add request validation (amended)
  │
  o  00010000  ezyang
╭─╯  Define the schema
│
o  00000000  remote/master
```

If there's a merge conflict, Sapling will bail out and leave the commits at the old spot.  You can explicitly run `sl restack` to do the restack and address the merge conflicts at your convenience.

```
  @  00020001  ezyang
  │  Add request validation (amended)
  │
  │ o  00030000  ezyang
  │ ╷  Wire up the new endpoint
  │ ╷
  │ x  00020000  [Rewritten into 00020001]  ezyang
  ├─╯  Add request validation
  │
  o  00010000  ezyang
╭─╯  Define the schema
│
o  00000000  remote/master
```

When you amend a commit, any other worktrees will stay at their old commits.  So you might have another worktree that was on `00020000`, and if you want to get to the updated copy, you would want to checkout `00020001`.

# Two aliases to rule the world

Remember that our desired workflow is that we have multiple agents working all over the stack.  There are two aliases that handle the majority of common situations you will run into:

```
[alias]
follow = goto last(successors(.))
adopt = rebase -s 'children(parents(.)) - .' -d .
```

## sl follow: Someone else amended and restacked a parent diff

When you amend a diff early in the stack, all the children diffs will automatically get restacked.  In particular, all of your other worktrees later in the stack will now be on stale, "rewritten" diffs.  This is actually a good thing: if you have an agent working on the working copy, you would prefer the working tree to not suddenly change out under them.  So you are now in the following state:

1. The diff you are on has been rewritten, and
2. You have some uncommitted changes on your worktree.

Before you amend or commit the uncommitted changes, simply run `sl follow`.  This will switch you to the newly amended commit, keeping your uncommitted changes, as long as the amended files are disjoint from your local changes.  If there is a potential merge conflict, it will ask you to run it again with `sl goto --merge`, in which case it will try to merge changes in the normal way.

## sl adopt: You added another diff in the middle of the stack

When you commit a new diff in the middle of a stack, the old children diffs won't get restacked, because restack only occurs for amend.  To do the equivalent of a restack, run `sl adopt`, which will rebase all of the old children onto your new commit.

I find these two commands cover the majority of situations that show up when you are working with parallel agents!

# A worked example

*Disclosure: this section is AI generated and lightly edited by me, all mistakes are mine.*

Let's walk through a concrete scenario.  I have a stack of three diffs: a schema definition, a validation layer, and an E2E integration test.

```
  o  00030000  ezyang
  │  E2E integration test
  │
  o  00020000  ezyang
  │  Add validation layer
  │
  o  00010000  ezyang
╭─╯  Define schema
│
o  00000000  remote/master
```

I set up three worktrees:

- **worktree-e2e** is checked out at `00030000` (E2E test), where an agent is working on expanding test coverage.
- **worktree-feature** is checked out at `00020000` (validation), where an agent is adding a new validation rule.
- **worktree-review** is checked out at `00010000` (schema), where an agent is fixing a code review comment on the schema diff.

All three agents are running in parallel.

## Step 1: The review agent finishes first

The agent on **worktree-review** amends the schema diff to address a reviewer's comment.  It runs `sl amend`.  Sapling automatically restacks the children, so the stack now looks like:

```
  o  00030001  ezyang
  │  E2E integration test
  │
  o  00020001  ezyang
  │  Add validation layer
  │
  @  00010001  ezyang
╭─╯  Define schema (amended)
│
o  00000000  remote/master
```

But **worktree-e2e** is still on `00030000` and **worktree-feature** is still on `00020000`—the old, rewritten versions.  That's fine.  The agents are still working on their uncommitted changes, and we don't want the rug pulled out from under them.

## Step 2: The feature agent finishes next

The agent on **worktree-feature** is done.  It has uncommitted changes on `00020000`, which has been rewritten to `00020001`.  Before committing, we run:

```
$ sl follow
```

This moves us from the stale `00020000` to the current `00020001`, carrying our uncommitted changes along.  Now we commit the new validation rule:

```
$ sl commit -m "Add email format validation"
```

This creates a fork in the stack, because we committed on top of `00020001` but the E2E test is also a child of `00020001`:

```
  o  00030001  ezyang
  │  E2E integration test
  │
  │ @  00040000  ezyang
  ├─╯  Add email format validation
  │
  o  00020001  ezyang
  │  Add validation layer
  │
  o  00010001  ezyang
╭─╯  Define schema (amended)
│
o  00000000  remote/master
```

We want the E2E test on top of our new commit, so we run:

```
$ sl adopt
```

This rebases `00030001` (the E2E test, which was a child of `00020001`) on top of `00040000` (our new commit):

```
  o  00030002  ezyang
  │  E2E integration test
  │
  @  00040000  ezyang
  │  Add email format validation
  │
  o  00020001  ezyang
  │  Add validation layer
  │
  o  00010001  ezyang
╭─╯  Define schema (amended)
│
o  00000000  remote/master
```

The stack is linear again, with our new diff slotted in.

## Step 3: The E2E agent finishes last

The agent on **worktree-e2e** is still on `00030000`, which has been rewritten twice—first to `00030001` by the review agent's restack, then to `00030002` by the adopt.  No matter: `sl follow` chases the full successor chain.

```
$ sl follow
$ sl amend
```

And we're done.  The final stack:

```
  @  00030003  ezyang
  │  E2E integration test (expanded coverage)
  │
  o  00040000  ezyang
  │  Add email format validation
  │
  o  00020001  ezyang
  │  Add validation layer
  │
  o  00010001  ezyang
╭─╯  Define schema (amended)
│
o  00000000  remote/master
```

All done!

# Comparison with other VCS

I have tried out [git-branchless](https://github.com/arxanas/git-branchless) in the past, because I was looking for a Sapling style stack workflow natively in Git. However, I found branchless buggy and failed to reliably restack commits.  I feel Sapling's tracking of successors is important for a good stack flow.

I think [Jujutsu](https://github.com/jj-vcs/jj) is a very similar workflow to Sapling.  I did do some basic research and it seems that Jujutsu will transparently update worktrees when a separate worktree is amended?  This seems bad: I want to delay worktree updates until I am ready (e.g., the agent finishes work.)
