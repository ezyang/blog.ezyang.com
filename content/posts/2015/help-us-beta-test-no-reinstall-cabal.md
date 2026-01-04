---
title: "Help us beta test \"no-reinstall Cabal\""
date: 2015-08-29 00:31:15
slug: help-us-beta-test-no-reinstall-cabal
categories: [Haskell]
comments:
    - id: 16339
      author: dmwit
      date: "2015-08-30 22:22:32"
      content: "Using commit 209147f, I find that build errors end with \"ExitFailure 1ExitFailure1\" (and no newline). It's pretty minor compared to the feature you're pushing, but... anyway it's probably not intended. =)"
    - id: 16412
      author: Edward Z. Yang
      date: "2015-09-03 17:20:25"
      content: "OK reported here: https://github.com/haskell/cabal/issues/2807"
    - id: 16452
      author: gregnwosu
      date: "2015-09-05 13:38:53"
      content: "amazing , very excited will try out on my arm laptops"
    - id: 16765
      author: dgpratt
      date: "2015-09-10 14:35:37"
      content: |
        I decided to give this a try. Installed yesod and setup a project, all went well. A bit later I tried to install Snap and this was the result:
        
        http://lpaste.net/140612
        
        I say "a bit later" because I can't recall if I installed any other packages in between.
        
        I'm running this on Windows 10, btw.
    - id: 16771
      author: Edward Z. Yang
      date: "2015-09-10 16:42:40"
      content: "Hey dgpratt, thanks for the report. Can you pastebin the output of 'ghc-pkg dump' (warning, will be long!) Also what version of GHC?"
    - id: 16772
      author: dgpratt
      date: "2015-09-10 20:41:32"
      content: |
        Indeed, large enough that lpaste seems not to like it :)
        
        https://onedrive.live.com/redir?resid=C3B85758C5D07CE2!44413&amp;authkey=!AD3S72tv4z6ApEM&amp;ithint=file%2czip
        
        ghc --version reports:
        
        The Glorious Glasgow Haskell Compilation System, version 7.10.2
    - id: 16773
      author: Edward Z. Yang
      date: "2015-09-10 20:49:28"
      content: "Oh ugh this is related to how lens synthesizes package keys for Template Haskell; I guess Cabal is putting the wrong thing in the macro. Thanks, I should be able to fix this."
    - id: 16838
      author: dgpratt
      date: "2015-09-11 08:23:34"
      content: "By the way, I'm extremely excited about the prospects for this project. As such, I really appreciate all the work you and others are putting into it. As much as I love Haskell, there are issues that make me feel that I cannot recommend it to anyone for serious development. I feel that you're on the cusp of solving perhaps the biggest of those issues."
    - id: 16843
      author: Edward Z. Yang
      date: "2015-09-11 20:04:40"
      content: "dgpratt: OK, I pushed a commit that should fix it. You'll need to rebuild lens with the new Cabal for this to work; the most precise way would be to remove it and anything that recursively depends on it from the package database, but it might be easier to just blow the database away."
    - id: 17048
      author: dgpratt
      date: "2015-09-14 14:04:09"
      content: |
        Hmm. Probably doing something wrong, but I'm getting the same result as before.
        
            1. ghc-pkg unregister lens.
            2. pulled latest from https://github.com/ezyang/cabal.git (cabal-no-pks branch).
            3. built/installed Cabal from source.
            4. built/installed cabal-install from source.
            5. attempted to "cabal install snap".
        
        The package keys referenced in the errors from the last command appear to be identical to the previous attempt.
    - id: 17158
      author: Edward Z. Yang
      date: "2015-09-16 14:57:04"
      content: |
        Hello dgpratt,
        
        This was a bit tricky to debug, but I think the problem is that you actually have the old buggy version of Cabal installed parallel with the good version (thanks to no-reinstall) and lens is picking the old version to compile against (I guess this is a bug; I think we have a plan to solve this actually). You can verify by checking: ghc-pkg list Cabal; there will be two Cabal-1.23.0.0 entries. So just 'ghc-pkg unregister Cabal-1.23.0.0' and then reinstall Cabal and cabal-install, and then reinstall lens and try again. (Or if you can figure out which one is new, unregister that specific one with 'ghc-pkg unregister --ipid IPID'; no need to reinstall in that case.)
        
        To test the fix, you can do as before; a faster method, however, is to 'cabal unpack lens', cd'ing to the directory, 'cabal configure' and then look at 'dist/build/autogen/cabal_macros.h'; scroll to the end and look to see that current package key is "3n5vL4MDitwxi3zZCJ0Kys" and not "lens-4.13-3n5vL4MDitwxi3zZCJ0Kys" (or similar).
    - id: 17161
      author: dgpratt
      date: "2015-09-16 16:01:57"
      content: |
        You already know this, but for anyone else who happens upon this, you were right. The proposed fix did indeed fix it.
        
        Thanks!
    - id: 20719
      author: Ben F.
      date: "2016-04-11 03:29:54"
      content: |
        I have cloned the repo and did
        
        &gt; ./setup-dev.sh
        
        but it fails with
        
        [...]
        [75 of 82] Compiling Distribution.Simple.Build ( Distribution/Simple/Build.hs, Distribution/Simple/Build.o )
        
        Distribution/Simple/Build.hs:408:58:
            Not in scope: ‘configFlags’
            Perhaps you meant data constructor ‘ConfigFlags’ (imported from Distribution.Simple.Setup)
        + die 'Cabal: Could not create '\''Setup'\'' executable'
        + echo 'Cabal: Could not create '\''Setup'\'' executable'
        Cabal: Could not create 'Setup' executable
        + exit 1
        + die 'Failed to build Cabal'
        + echo 'Failed to build Cabal'
        Failed to build Cabal
        + exit 1
        
        PS using
        
        &gt; cabal --version
        cabal-install version 1.22.9.0
        using version 1.22.5.0 of the Cabal library 
        &gt; ghc --version
        The Glorious Glasgow Haskell Compilation System, version 7.10.3
    - id: 20733
      author: Edward Z. Yang
      date: "2016-04-12 22:34:46"
      content: "Thanks for trying. Actually, Duncan's patchset has merged and will be shipping with 1.24, so you try that out instead! The draft announcement post is here: https://github.com/ezyang/cabal/blob/pr/nix-announce/nix-announce.rst there will be a more official announcement coming soon."
    - id: 30134
      author: "stack: more binary package sharing &#8211; FPComplete"
      date: "2023-07-26 18:38:12"
      content: "[&#8230;] was first released. It&#8217;s not coincidental that this support is being added not long after  a similar project completed for Cabal. Ryan Trinkle- Vishal&#8217;s mentor on the project- described the work to me a few months back, [&#8230;]"
---

Over this summer, Vishal Agrawal has been working on a GSoC project to [move Cabal to more Nix-like package management system](https://ghc.haskell.org/trac/ghc/wiki/Commentary/GSoC_Cabal_nix). More simply, he is working to make it so that you'll never get one of these errors from <span class="title-ref">cabal-install</span> again:

    Resolving dependencies...
    In order, the following would be installed:
    directory-1.2.1.0 (reinstall) changes: time-1.4.2 -> 1.5
    process-1.2.1.0 (reinstall)
    extra-1.0 (new package)
    cabal: The following packages are likely to be broken by the reinstalls:
    process-1.2.0.0
    hoogle-4.2.35
    haskell98-2.0.0.3
    ghc-7.8.3
    Cabal-1.22.0.0
    ...

However, these patches change a nontrivial number of moving parts in Cabal and cabal-install, so it would be very helpful to have willing guinea pigs to help us iron out some bugs before we merge it into Cabal HEAD. As your prize, you'll get to run "no-reinstall Cabal": Cabal should **never** tell you it can't install a package because some reinstalls would be necessary.

Here's how you can help:

1.  Make sure you're running GHC 7.10. Earlier versions of GHC have a hard limitation that doesn't allow you to reinstall a package multiple times against different dependencies. (Actually, it would be useful if you test with older versions of GHC 7.8, but only mostly to make sure we haven't introduced any regressions here.)
2.  `git clone https://github.com/ezyang/cabal.git` (I've added some extra corrective patches on top of Vishal's version in the course of my testing) and `git checkout cabal-no-pks`.
3.  In the `Cabal` and `cabal-install` directories, run `cabal install`.
4.  Try building things without a sandbox and see what happens! (When I test, I've tried installing multiple version of Yesod at the same time.)

It is NOT necessary to clear your package database before testing. If you completely break your Haskell installation (unlikely, but could happen), you can do the old trick of clearing out your `.ghc` and `.cabal` directories (don't forget to save your `.cabal/config` file) and rebootstrapping with an old `cabal-install`.

Please report problems here, or to [this PR in the Cabal tracker](https://github.com/haskell/cabal/pull/2752). Or chat with me in person next week at ICFP. :)
