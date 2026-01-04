---
title: "How to maintain a pristine copy of your configuration files"
date: 2014-01-20 19:02:31
slug: how-to-maintain-a-pristine-copy-of-your-configuration-files
categories: [Git, Toolbox]
comments:
    - id: 6356
      author: farseerfc
      date: "2014-01-20 20:37:18"
      content: "Archlinux's pacman avoided this problem by introducing *.pacnew and *.pacsave files. It's fairly easy to use git or some other VCS on top of that."
    - id: 6357
      author: Ondra
      date: "2014-01-20 21:08:29"
      content: "can Nix do this?"
    - id: 6358
      author: Edward Z. Yang
      date: "2014-01-21 00:21:33"
      content: "farseerfc: I'm not terribly fond of this approach, since it's basically version control as per the bad old days (when people just made copies of old files when they needed backups.) Though, I'm curious: *.pacorig files don't actually get created normally, so how do you diff your current /etc against the pristine version?"
    - id: 6359
      author: Edward Z. Yang
      date: "2014-01-21 00:24:03"
      content: "Ondra: If I understand correctly, Nix can do this, but only if you manage your configuration as Nix packages."
    - id: 6365
      author: Paolo G. Giarrusso
      date: "2014-01-27 13:22:05"
      content: |
        &gt; Archlinux’s pacman avoided this problem by introducing *.pacnew and *.pacsave files. It’s fairly easy to use git or some other VCS on top of that.
        
        rpm and dpkg have such files as well, but at least rpm and dpkg will create those files only upon conflicts between a package upgrade and local changes to the configuration files.
        
        Both rpm and dpkg also allow to check if the configuration files are pristine (with rpm -V or installing debsums), and for non-pristine files you can always get the original package (with yum/apt-get), unpack it, and commit it to the pristine branch. But scripting all that, while not challenging, takes more than 5 minutes, otherwise I'd post a bash solution for apt-get.
    - id: 22117
      author: Spheniscus
      date: "2017-10-06 14:54:59"
      content: |
        Keeping a pristine branch is a good idea, but I don't understand how your method is supposed to work robustly.  I don't even understand what guarantees that the pristine branch is really keeping what it says it does, namely pristine config files as shipped with the packages.
        
        The main advantage of having a pristine branch would be to have a possibility to merge pristine into master after an upgrade.  But you seem to be doing things the other way around: first you let apt-get apply the changes to the files in /etc, then the user resolves conflicts, and etckeeper makes its commit.  Only then that commit gets cherry-picked on top of the pristine branch, master gets rolled back and pristine merged-in.
        
        IMHO a symptom of this conceptual problem is that the cherry-pick onto the pristine branch can fail.  The evolution of the pristine branch is determined by what is shipped with the packages, there should be no conflicts there, ever.
        
        Do I misunderstand your idea?
    - id: 22119
      author: Edward Z. Yang
      date: "2017-10-06 22:47:26"
      content: "It's not guaranteed to succeed. But it seems to be OK for most updates."
---

[etckeeper](http://joeyh.name/code/etckeeper/) is a pretty good tool for keeping your /etc under version control, but one thing that it won’t tell you is what the diff between your configuration and a pristine version of your configuration (if you installed the same packages on the system, but didn’t change any configuration). [People have wanted this](https://blueprints.launchpad.net/ubuntu/+spec/foundations-q-dpkg-pristine-conffiles), but I couldn’t find anything that actually did this. A month ago, I figured out a nice, easy way to achieve this under etckeeper with a Git repository. The idea is to maintain a pristine branch, and when an upgrade occurs, automatically apply the patch (automatically generated) to a pristine branch. This procedure works best on a fresh install, since I don’t have a good way of reconstructing history if you haven’t been tracking the pristine from the start.

Here’s how it goes:

1.  Install etckeeper. It is best if you are using etckeeper 1.10 or later, but if not, you should replace [30store-metadata](https://github.com/joeyh/etckeeper/blob/master/pre-commit.d/30store-metadata) with a copy from the latest version. This is important, because pre-1.10, the metadata store included files that were ignored, which means you’ll get lots of spurious conflicts.

2.  Initialize the Git repository using `etckeeper init` and make an initial commit `git commit`.

3.  Create a pristine branch: `git branch pristine` (but stay on the master branch)

4.  Modify the etckeeper configuration so that `VCS="git"`, `AVOID_DAILY_AUTOCOMMITS=1` and `AVOID_COMMIT_BEFORE_INSTALL=1`:

        diff --git a/etckeeper/etckeeper.conf b/etckeeper/etckeeper.conf
        index aedf20b..99b4e43 100644
        --- a/etckeeper/etckeeper.conf
        +++ b/etckeeper/etckeeper.conf
        @@ -1,7 +1,7 @@
         # The VCS to use.
         #VCS="hg"
        -#VCS="git"
        -VCS="bzr"
        +VCS="git"
        +#VCS="bzr"
         #VCS="darcs"

         # Options passed to git commit when run by etckeeper.
        @@ -18,7 +18,7 @@ DARCS_COMMIT_OPTIONS="-a"

         # Uncomment to avoid etckeeper committing existing changes
         # to /etc automatically once per day.
        -#AVOID_DAILY_AUTOCOMMITS=1
        +AVOID_DAILY_AUTOCOMMITS=1

         # Uncomment the following to avoid special file warning
         # (the option is enabled automatically by cronjob regardless).
        @@ -27,7 +27,7 @@ DARCS_COMMIT_OPTIONS="-a"
         # Uncomment to avoid etckeeper committing existing changes to 
         # /etc before installation. It will cancel the installation,
         # so you can commit the changes by hand.
        -#AVOID_COMMIT_BEFORE_INSTALL=1
        +AVOID_COMMIT_BEFORE_INSTALL=1

         # The high-level package manager that's being used.
         # (apt, pacman-g2, yum, zypper etc)

5.  Apply [this patch to etckeeper/commit.d/50vcs-commit](http://web.mit.edu/~ezyang/Public/etckeeper-pristine.patch). This patch is responsible for keeping the pristine branch up-to-date (more explanation below).

6.  Create a `.gitattributes` file with contents `.etckeeper merge=union`. This makes merges on the metadata file use the union strategy, which reduces spurious conflicts dramatically:

        diff --git a/.gitattributes b/.gitattributes
        new file mode 100644
        index 0000000..b7a1f4d
        --- /dev/null
        +++ b/.gitattributes
        @@ -0,0 +1 @@
        +.etckeeper merge=union

7.  Commit these changes.

8.  Permit pushes to the checked out `/etc` by running `git config receive.denyCurrentBranch warn`

9.  All done! Try installing a package that has some configuration and then running `sudo gitk` in `/etc` to view the results. You can run a diff by running `sudo git diff pristine master`.

So, what’s going on under the hood? The big problem that blocked me from a setup like this in the past is that you would like the package manager to apply its changes into the pristine etc, so that you can merge in the changes yourself on the production version, but it’s not obvious how to convince dpkg that `/etc` lives somewhere else. Nor do you want to revert your system configuration to pristine version, apply the update, and then revert back: this is just asking for trouble. So the idea is to apply the (generated) patch as normal, but then *reapply* the patch (using a cherry-pick) to the pristine branch, and then rewrite history so the parent pointers are correct. All of this happens outside of `/etc`, so the production copy of the configuration files never gets touched.

Of course, sometimes the cherry-pick might fail. In that case, you’ll get an error like this:

    Branch pristine set up to track remote branch pristine from origin.
    Switched to a new branch 'pristine'
    error: could not apply 4fed9ce... committing changes in /etc after apt run
    hint: after resolving the conflicts, mark the corrected paths
    hint: with 'git add <paths>' or 'git rm <paths>'
    hint: and commit the result with 'git commit'
    Failed to import changes to pristine
    TMPREPO = /tmp/etckeeper-gitrepo.CUCpBEuVXg
    TREEID = 8c2fbef8a8f3a4bcc4d66d996c5362c7ba8b17df
    PARENTID = 94037457fa47eb130d8adfbb4d67a80232ddd214

Do not fret: all that has happened is that the `pristine` branch is not up-to-date. You can resolve this problem by looking at `$TMPREPO/etc`, where you will see some sort of merge conflict. Resolve the conflict and commit. Now you will need to manually complete the rest of the script, this can be done with:

    git checkout master
    git reset --hard HEAD~ # this is the commit we're discarding
    git merge -s ours pristine
    git push -f origin master
    git push origin pristine

To make sure you did it right, go back to `/etc` and run `git status`: it should report the working directory as clean. Otherwise, there are discrepancies and you may not have done the merges correctly.

I’ve been testing this setup for a month now, and it has proceeded very smoothly (though I’ve never attempted to do a full release upgrade with this setup). Unfortunately, as I’ve said previously, I don’t have a method for constructing a pristine branch from scratch, if you have an existing system you’d like to apply this trick to. There’s nothing stopping you, though: you can always decide to start, in which case you will record just the diffs from the time you started recording pristine. Give it a spin!
