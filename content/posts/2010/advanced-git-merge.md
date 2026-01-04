---
title: "Five advanced Git merge techniques"
date: 2010-01-18 12:00:17
slug: advanced-git-merge
categories: [Git]
comments:
    - id: 1143
      author: cbemerine
      date: "2010-09-11 18:37:04"
      content: "Excellent article and a good refresher!"
    - id: 1668
      author: tedm
      date: "2010-12-18 18:40:25"
      content: "Thanks! I didn't know about the diff3 option. That's made my life a lot easier!"
    - id: 1855
      author: Fredrik
      date: "2011-03-04 02:50:56"
      content: "Rebase is awesome, but beware that it does rewrite your history. You should only rebase stuff you haven't pushed anywhere."
    - id: 2001
      author: "Using GIT with SVN | Justin Carmony"
      date: "2011-04-07 17:34:33"
      content: "[...] Five advanced Git merge techniques - Five more advanced tricks with Git and merging, not sure if I&#8217;ll actually use all of them. [...]"
    - id: 3623
      author: MrBandersnatch
      date: "2012-04-04 09:52:15"
      content: "One note on Number 5. Be very careful with this. Especially when dealing with remote tracking branches, as a rewrite of history can be dreadfully painful when things get out of sync. [DETACHED HEAD] [DIVERGING BRANCHES] If your going to do a rebase merge, Do it before you push. You've been warned."
    - id: 5987
      author: Robert B
      date: "2013-03-31 01:29:17"
      content: "Regarding MrBandersnatch's comment: In other words, never rebase anything anyone else is working on unless they're there with you. Rebasing is an awesomely powerful tool, but should never be used on a collaborative branch unless there's clear communication across the team about the timeline magic you're doing,"
    - id: 5991
      author: Jonathan Beerhalter
      date: "2013-04-01 07:55:22"
      content: "There are a couple comments here that make rebasing seem extremely scary when working collaboratively. Personally, I think they're erroneous. Rebasing does not rewrite history, at least not what you're rebasing --onto. The collaborative branches history remains fixed when working collaboratively, and it is up to the contributor to ensure that their changes work within the current history of the code base. Additionally, most shared repositories can be configured to only allow fast-forward commits, thereby preventing the possibility of rewriting the common history."
    - id: 20914
      author: "Advanced merging on file level &#8211; Chiel ten Brinke"
      date: "2016-06-27 06:14:34"
      content: "[&#8230;] git allows that. This page and this page are a helpful read when it comes to merging files &#8220;manually&#8221;. The following python [&#8230;]"
---

Have you ever performed a merge in Git and not have it quite turn out the way you wanted it to? For example, you accidentally converted all of your UNIX line endings to DOS line endings, and now the entire file reports a conflict? Maybe you see a conflict that you don't really care about resolving, and want to resolve as theirs? Or perhaps the conflicted file is empty and you can't figure out just what happened there?

Here are some advanced techniques you can apply to your conflicted merges to make things go a little easier. Many of them utilize Git plumbing; that is, the internal commands that interface directly with the bare metal Git abstractions: the index, the tree, the commit graph. Others are as simple as flipping a configuration switch.

1.  Turn `diff3` conflicts using `git config --global merge.conflictstyle diff3`. The `diff3` conflict style adds an extra section between the new `|||||||` marker and `=======` markers, which indicates the original contents of the section, with your changes above and their (the branch that is being merged in's) changes below. `diff3` is a powerful way of reestablishing context of a change you made several months ago (to see the changes you made, compare the middle section with the upper section; for the changes they made, compare the middle section with the lower section), and there is really no good reason not to have this on by default.

2.  If you've come in from Subversion, you may be familiar with the `FILE.mine`, `FILE.r2` (the original you worked with) and `FILE.r3` (the latest version checked in) files, as well as the ability to run `svn resolve --accept theirs-full` or `mine-full`, which says "I don't care about the other changes, just use this version of the file." Git offers similar facilities utilizing the merge parents, although they're a little more hidden.

    You may be already familiar with the `git show` command, which lets you view commits as well as arbitrary blobs inside the tree of any given commit. When you are inside a merge, you can use a special `:N:` syntax, where `N` is a number, to automatically select one of the merge parents. `1` selects the common base commit (the lower revision), `2` selects your version ("mine"), and `3` selects their version (the higher revision). So `git show :3:foobar.txt` shows the upstream version of `foobar.txt`.

    To actually use one of these versions as the merge resolution, use `git checkout {--ours|--theirs} filename.txt.`

3.  When you're in a conflict, `git diff` will give you the deep and dirty of all the conflicts that occurred, sometimes this is too much information. In that case, you can run `git ls-files -u` to view all of the unmerged files (this is also a lot faster than `git status`, and will omit all of the files that *were* merged properly.)

    You may notice that there as many as three copies of a file inside the list; this tells you the state of the "common", "ours" and "theirs" copies mentioned previously. If 1 (common) is missing, that means that the file appeared at the same time in our branch and their branch. If 2 (ours) is missing, it means we deleted the file, but it got a change upstream. If 3 (theirs) is missing, it means we made some changes, but upstream deleted the file. This is especially useful if a file is conflicted, but you can't figure out why (since there are no conflict markers.)

4.  Sometimes life gives you lemons. Many people suggest you make lemon juice. However, if Git gives you a really bad set of conflict markers, for example, you accidentally flipped the newline style for one of the files, so now the entire file conflicts, don't settle for that: redo the merge for that file. You can do this with the handy `git merge-file` command. This runs a three-way file merge, and takes three arguments: the current file, the common file, and the upstream file, and writes out the merge into the current file (first argument). Use `git show` to dump out your file, the common file and upstream file, do whatever changes to those files you need (for example, run `dos2unix`), run `git merge-file mine common theirs`, and then copy the `mine` over the old conflicted file. Voila, instant new set of conflict markers.

    If you discover a global conflict relatively early in the merge process, and it was your fault, it might be easier to back out of the merge `git reset --hard`, fix the mistake, and try merging again. However, if you've already made substantial progress merging a copy, re-merging just a single file can be a lifesaver.

5.  Don't merge, rebase! Instead of running `git pull`, run `git pull --rebase`. Instead of running `git merge master`, run `git rebase master`. Your history will be much cleaner as a result, and you want have to go on a massive rebase marathon later if you want to submit your patches upstream.

Now, go forth and merge to thy hearts content!
