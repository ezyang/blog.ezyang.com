---
title: "Xmonad and media keys on Saucy"
date: 2013-10-27 16:30:39
slug: xmonad-and-media-keys-on-saucy
categories: [Toolbox]
comments:
    - id: 6268
      author: thouters
      date: "2013-10-27 18:30:12"
      content: "Why is this Ubuntu's fault exactly?"
    - id: 6269
      author: Edward Z. Yang
      date: "2013-10-27 18:44:35"
      content: "It might not be; I've just gotten in the habit of blaming all problems related to window managers to Ubuntu, since they're in the habit of making lots of changes there ^^."
    - id: 6278
      author: nonesuch
      date: "2013-10-27 23:00:18"
      content: |
        haskell planet feedback.
        why are U using Ubuntu or 'garbage software' and NOT
        FreeBSD?
        
        the tests
        1.)closed
        2.)developer code review
        3.)much too widespread and bloated - debian
        4.)code copying and code smells
        5.)
    - id: 6271
      author: Anonymous
      date: "2013-10-28 00:06:59"
      content: "Why don't you submit a patch? It works under gnome panel, which is an Ubuntu fix."
    - id: 6272
      author: Edward Z. Yang
      date: "2013-10-28 00:10:10"
      content: |
        Well, it's not a case where the problem is "patchable." Gnome has specifically decided that the functionality does not belong in gnome-session-daemon. I should probably file a feature request though.
        
        gnome-panel is a way to get things to work, but there's no way to run gnome-panel, without the panel, if you know what I mean ;)
    - id: 6273
      author: nagisa
      date: "2013-10-28 03:50:01"
      content: "You might want to look at how I put MPRIS into my XMonad (https://github.com/nagisa/sysconf/blob/master/home/config/xmonad/lib/MPRIS.hs) and possibly do the same."
    - id: 6274
      author: Chris Warburton
      date: "2013-10-28 06:33:19"
      content: |
        Meh, gnome-settings-daemon requires all kinds of bloated gnome stuff to be installed as dependencies. I map my volume keys to amixer using xbindkeys and once it was set up I've never had to tweak it.
        
        Maybe "x-settings-daemon" will do what you want?
    - id: 6275
      author: Edward Z. Yang
      date: "2013-10-28 06:34:52"
      content: "Well, I've found the display configuration and power management to also be pretty useful, which is why I’ve kept it around. What is \"x-settings-daemon\"?"
    - id: 6276
      author: Anonymous
      date: "2013-10-28 14:59:00"
      content: "I'm saying that Ubuntu has brought back the media keys plugin. For one, it's required by Unity. gnome-panel was an example of something which doesn't work in upstream g-s-d, but does in Ubuntu."
    - id: 6277
      author: Edward Z. Yang
      date: "2013-10-28 17:58:20"
      content: "Oh, now that is confusing. So is this something like the fix_media_keys_on_unity.patch they're applying?"
    - id: 6280
      author: Ben Gamari
      date: "2013-10-30 10:18:41"
      content: "I took some time yesterday to add MPRIS and PulseAudio volume support to my XMonad configuration. So far things seem to work well, https://github.com/bgamari/xmonad-config"
    - id: 6284
      author: Anonymous
      date: "2013-10-31 14:32:03"
      content: |
        Try the trusty version of gnome-settings-daemon. It sounds like it fixes exactly your problem.
        
        It should be updated in 13.10 soon, too
    - id: 6285
      author: Edward Z. Yang
      date: "2013-10-31 14:35:25"
      content: |
        <pre>gnome-settings-daemon (3.8.5-0ubuntu12) trusty; urgency=low
        
          * debian/patches/fix_media_keys_on_unity.patch: Another reworking. Try to
            start the legacy keygrabber all the time. Only don't if we're running
            shell. Should fix keygrabber in environments without panel or Unity.
         -- Iain Lane <iain.lane@canonical.com>   Tue, 29 Oct 2013 17:03:35 +0000</pre>
        
        Oh sweet!
    - id: 6300
      author: Simao
      date: "2013-11-18 05:58:35"
      content: "Anyone managed to fix this without installed those packages??"
    - id: 6302
      author: Thiago Negri
      date: "2013-11-21 09:42:22"
      content: "I don't know exactly how, but I'm using Arch Linux + Xmonad without spawning anything of gnome-* and my media keys works correctly."
    - id: 6303
      author: Edward Z. Yang
      date: "2013-11-22 22:29:55"
      content: "Verified that the trusty package gets suspend and all that working. I'll upload a backport to my PPA shortly."
    - id: 6310
      author: Arash Rouhani
      date: "2013-12-01 05:39:37"
      content: |
        I just upgraded to 13.10. However the fn keys are still working for me. It might be because I login with "Xmonad with Gnome" in the login screen.
        
        Oh, and thanks for this amazing blog! Never stop posting! :)
    - id: 6318
      author: Arash Rouhani
      date: "2013-12-06 06:21:31"
      content: |
        Nevermind. I realized it was just `gnome-panel` making the keys working.
        
        Tough, I just realized that if you launch `gnome-panel` and then `taffybar` (in that order). You'll be running `gnome-panel` without the actual panel! :)
    - id: 6319
      author: Anonymous
      date: "2013-12-07 19:30:56"
      content: "Yep, that's right: gnome-panel will handle media keys. The taffybar trick is a neat one"
    - id: 6366
      author: eythian
      date: "2014-01-28 20:55:39"
      content: |
        This no longer seems to work:
        
        dpkg: dependency problems prevent configuration of gnome-settings-daemon:
         gnome-settings-daemon depends on libgnome-desktop-3-4 (&gt;= 3.5.3); however:
          Package libgnome-desktop-3-4 is not installed.
    - id: 6367
      author: Edward Z. Yang
      date: "2014-01-28 21:25:40"
      content: Hey eythian; the new improved workaround is to install a backport from Trusty. I have one in my personal PPA; see the EDIT on the post.
    - id: 6368
      author: eythian
      date: "2014-01-29 16:27:11"
      content: |
        Actually, it's not needed any more. After posting that I discovered that there was a version of gnome-settings in saucy-proposed that fixes it.
        
        https://bugs.launchpad.net/ubuntu/saucy/+source/gnome-settings-daemon/+bug/1235625
        
        Hopefully it'll get released properly soon.
    - id: 6369
      author: Edward Z. Yang
      date: "2014-01-29 17:55:28"
      content: Great news; if you could write another comment when it gets released I can update the post again.
    - id: 6378
      author: LaunchpadJanitor
      date: "2014-02-17 14:33:18"
      content: "Fix released a day after your last comment - https://bugs.launchpad.net/ubuntu/saucy/+source/gnome-settings-daemon/+bug/1235625"
    - id: 13869
      author: Anonymous
      date: "2015-05-06 06:12:55"
      content: "It seems like Ubuntu vivid breaks media keys again.  gnome-panel does not seem to support media keys, the Trusty-backported gnome-settings-daemon now conflicts with gsettings-desktop-schemas, and unity-settings-daemon (suggested by a recent comment in LP bug 1235625) doesn't seem to handle media keys either.  Have you managed to find any workaround?"
    - id: 13870
      author: Anonymous
      date: "2015-05-06 06:27:49"
      content: |
        After some futzing around, here's an awkward workaround that gets media keys working (using stock packages from Ubuntu vivid):
        
          add gnome-settings-daemon to RequiredComponents in xmonad.session
          after login, run gnome-panel
          after login, run "unity-settings-daemon --replace"
        
        It seems that the combination of gnome-panel and unity-settings-daemon are necessary to handle media keys, and furthermore, for some reason adding unity-settings-daemon to RequiredComponents does not properly launch unity-settings-daemon.
    - id: 13873
      author: Edward Z. Yang
      date: "2015-05-06 12:36:41"
      content: "That is bonkers! Thanks for the note, I'll have to give it a try. (I really don't like gnome-panel because there's no way to get rid of the bottom window bar, but it would be good to know if this method works reliably.)"
    - id: 14219
      author: Martyn Smith
      date: "2015-05-22 02:26:44"
      content: |
        Wow, I've been searching for ages to figure this out (I'm using i3 as a window manager) and yes, running gnome-panel makes everything work.
        
        It'd be awesome if anyone could figure out how to have it working without splatting gnome headers/footers to the screen :-)
    - id: 14221
      author: Martyn Smith
      date: "2015-05-22 02:46:41"
      content: |
        Further update, I've worked out (since I run a nested X server for some selenium stuff I do) I can run gnome-panel in there (using DISPLAY=:1 gnome-panel &amp;), then the stupid top/bottom bars are out of sight, and global media keys still works.
        
        Seriously, that's _very_ ugly though, hopefully someone can improve on that :-)
    - id: 14222
      author: Edward Z. Yang
      date: "2015-05-22 02:56:03"
      content: "I suppose that would work! But then how do you get your tray icons?"
    - id: 14226
      author: Martyn Smith
      date: "2015-05-22 06:33:22"
      content: |
        Oh, i3 has a doofer that renders tray icons for you ... I wonder if gnome panel is eating them though, might have to check that tomorrow :-)
        
        Either way, it'd be nice if I didn't have to run gnome-panel at all ...
    - id: 17176
      author: Michael J. Sullivan
      date: "2015-09-16 23:21:08"
      content: |
        Looked into this a bit while I should have been working on something more productive, and it looks like all gnome-panel was doing to cause this to work was requesting the name (whatever that means) "org.gnome.Panel" on the "org.freedesktop.DBus" interface. Of course.
        
        So I hacked together a script that does that and then goes to sleep: https://gist.github.com/msullivan/d3bca6ed2907ee5d8d16
        
        I could probably package this up more nicely if that would be useful to people.
---

Ubuntu continues on its rampage of breaking perfectly good software, and on my most recent upgrade to Saucy Salamander, I discovered to my dismay that my media keys (e.g. volume keys, fn (function) keys, suspend button, etc) had stopped working. Of course, it worked fine if I logged into my user using Unity, but who wants to use a silly window manager like that...

The root problem, according to [these Arch Linux forum posts](https://bbs.archlinux.org/viewtopic.php?pid=1262471) is that Gnome has moved media-key support out of `gnome-settings-daemon` (which any self-respecting Xmonad user is sure to spawn) and into their window manager proper. Which, of course, is no good because I don’t want to use their window manager!

For now, it seems the simplest method of bringing back this functionality is to run a 3.6 version of gnome-settings-daemon. Fortunately, at least for Saucy, there are a few builds of 3.6 available before they upgraded to 3.8. So, all you need to do is grab these two deb files appropriate for your architecture (you need gnome-control-center too, because it has a dependency on gnome-settings-daemon):

- [gnome-control-center 1:3.6.3-0ubuntu34](https://launchpad.net/ubuntu/+source/gnome-control-center/1:3.6.3-0ubuntu34)
- [gnome-settings-daemon 3.6.4-0ubuntu19](https://launchpad.net/ubuntu/+source/gnome-settings-daemon/3.6.4-0ubuntu19)

Once you've downloaded the appropriate deb files, a `dpkg -i $DEBFILE` and then `apt-mark hold gnome-control-center gnome-settings-daemon` should do the trick. You should run an `aptitude upgrade` to make sure you haven't broken any other dependencies (for example, `gnome-shell`). (Power-users can add the debs to a local repo and then downgrade explicitly from `apt-get`.)

Moving forward, we will probably be forced to reimplement media key bindings in some other software package, and it would be nice if this could be standardized in some way. Linux Mint has already forked gnome-settings-daemon, with their [cinnamon-settings-daemon](https://github.com/linuxmint/cinnamon-settings-daemon), but I've not tried it, and have no idea how well it works.

**Update.** Trusty has an updated version of this package which restores support, so I am providing backports [via my PPA.](https://launchpad.net/~ezyang/+archive/ppa)
