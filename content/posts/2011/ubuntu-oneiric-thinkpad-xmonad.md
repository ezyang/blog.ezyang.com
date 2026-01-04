---
title: "Ubuntu Oneiric upgrade (Thinkpad/Xmonad)"
date: 2011-11-24 03:59:50
slug: ubuntu-oneiric-thinkpad-xmonad
categories: [Toolbox]
comments:
    - id: 3153
      author: paurullan
      date: "2011-11-24 05:29:53"
      content: "I know Ubuntu reaches the sweet spot between Debian stable and unstable but I am almost scared to recommend anybody upgrade and Ubuntu release!"
    - id: 3154
      author: Nicolas Wu
      date: "2011-11-24 05:53:11"
      content: "I also found that it broke Skype: I can no longer receive video."
    - id: 3155
      author: yachris
      date: "2011-11-24 09:50:55"
      content: |
        This is why I have two partitions: one for root, one for user.  I keep everything important (programs I'm working on, music, photos, etc.) on the user partition, and install as little as possible on root.
        
        Then when a new distribution comes out, I do a reformat and clean install.  This post is why.
    - id: 3156
      author: Anonymous
      date: "2011-11-24 10:39:14"
      content: "Switch to Arch.  I switched after a similar episode and I cannot recommend it enough!"
    - id: 3157
      author: Edward Z. Yang
      date: "2011-11-24 11:00:27"
      content: "Yeah, honestly, Ubuntu pulls these shenanigans every major release, and I just deal with it by never upgrading until, e.g. major holiday. To be fair though, except for the DBUS problem, most of these problems came from the rampant customization that I make on my install (this is also why I'd be terrified of a fresh install: with an upgrade, I know I'll do a bounded amount of work to make things work again; with a fresh install, who knows how much configuration I'll lose?)"
    - id: 3158
      author: Tristram Brelstaff
      date: "2011-11-24 11:01:15"
      content: I also do a clean install of Ubuntu each time instead of upgrading.  I found out a while back that it was actually faster to do things that way (as well as being simpler).
    - id: 3159
      author: Edward Z. Yang
      date: "2011-11-24 11:05:42"
      content: "Sigh. Remember when we had to do this “clean install” stuf for Windows? Boy times have change."
    - id: 3160
      author: agumonkey
      date: "2011-11-24 12:42:27"
      content: |
        welcome to 1995.
        *sad*
        
        sent from xp
    - id: 3161
      author: refold
      date: "2011-11-24 13:13:30"
      content: |
        &gt; Default GHC is 7.0.3!
        
        This is why I always install GHC &amp; cabal-install manually and don't use distro packages.
    - id: 3162
      author: Anonymous
      date: "2011-11-24 16:56:51"
      content: |
        i'm new at functional programming (and lambda calculus) so excusse me if my question is silly, or the answer too obvious :D
        
        Why does f (g x (h y (a + b))) translates into f(g(x,h(y,a + b)) and not to f(g,x(h,y(a+b))). I understood the syntax from the article this way: after the '$' symbol we have a parenthesis, where the variables (right "operators" of the '$' symbol) that follow, are the arguments to the left "operator" of the '$' (the function).
        
        where am i wrong? thnks in advance for your answers :D
    - id: 3163
      author: Qi Qi
      date: "2011-11-24 23:06:32"
      content: "That's why I had my thinkpad settled with Debian a few years ago because of its rolling upgrade. And also I have Xmonad coupled with Gnome 3's fallback."
    - id: 3203
      author: Evan
      date: "2011-12-06 01:46:05"
      content: |
        Just remembered that I meant to comment about this, in the interests of trying to spread good information.
        
        NetworkManager's managed=true setting in the ifupdown plugin is connected to how it handles /etc/network/interfaces. The assumption in the default install is that if you configured a device in /etc/network/interfaces, you want it managed by ifupdown and not NM, so NM does not try to configure those interfaces itself. This is a reasonable default setting because NM doesn't yet support the full range of capabilities that /etc/network/interfaces has (e.g. obscure things like interface bonding and bridge interfaces)
        
        The generally correct way to configure a modern Debian/Ubuntu desktop is to remove all non-loopback interfaces from /etc/network/interfaces and let NM take over. This likely would have also kept you from triggering the /etc/init/failsafe.conf Upstart job (which was put in place because Ubuntu server systems were booting fast enough that traditional init scripts were running before networking was up - http://pad.lv/580319)
        
        Also, in defense of Ubuntu, almost all of the problems you describe sound to me like they are connected to external upgrades - the GNOME 3 transition in particular - which means that you likely would have had many of those issues upgrading from any GNOME 2 distribution to its GNOME 3 release.
    - id: 3455
      author: "ubuntu的apt-get upgrade命令停在read changlog不工作的处理办法ubuntu的apt-get upgrade命令停在read changlog不工作了。 | Changblog"
      date: "2012-02-20 17:28:52"
      content: "[...] 重试，问题依旧。后来搜索“Reading changelogs”发现这篇文章中提到apt-listchanges “Reading changelogs.” apt-listchanges isn’t particularly useful, and [...]"
---

I upgraded from Ubuntu Natty Narwhal to Oneiric Ocelot (11.10) today. Lots of things broke. In order:

- “Could not calculate the upgrade.” No indication of what the error might be; in my case, the error ended up being old orphan OpenAFS kernel modules (for whom no kernel modules existed). I also took the opportunity to clean up my PPAs.
- “Reading changelogs.” `apt-listchanges` isn’t particularly useful, and I don’t know why I installed it. But it’s really painful when it’s taking more time to read changelogs than to install your software. Geoffrey suggested `` gdb -p `pgrep apt-listchanges ``<span class="title-ref"> and then forcing it to call </span><span class="title-ref">exit(0)</span>\`, which worked like a charm. Had to do this several times; thought it was infinitely looping.
- Icons didn’t work, menus ugly. Go to “System Settings \> Appearance” and go set a new theme; in all likelihood your old theme went away. This [AskUbuntu](http://askubuntu.com/questions/59791/how-do-i-fix-my-theme) question gave a clue.
- Network Manager stopped working. For some inscrutable reason the default NetworkManager config file `/etc/NetworkManager/NetworkManager.conf` has `managed=false` for `ifupdown`. Flip back to true.
- New window manager, new defaults to dunk you in Unity at least once. Just make sure you pick the right window manager from the little gear icon.
- `gnome-power-manager` went away. If you fix icons a not-so-useful icon will show up anyway when you load `gnome-settings-daemon`.
- “Waiting for network configuration.” There were lots of suggestions here. My `/var/run` and `/var/lock` were borked so I [did these instructions](http://uksysadmin.wordpress.com/2011/10/14/upgrade-to-ubuntu-11-10-problem-waiting-for-network-configuration-then-black-screen-solution/), I also hear that you should punt `wlan0` from `/etc/network/interfaces` and remove it from `/etc/udev/rules.d70-persistent-net.rules`. I also commented out the sleeps in `/init/failsafe.conf` for good measure.
- Default GHC is 7.0.3! Blow away your `.cabal` (but hold onto `.cabal/config`) and go reinstall Haskell Platform. Don’t forget to make sure you install profiling libraries, and grab `xmonad` and `xmonad-contrib`. Note that previous haskell-platform installs will be rather broken, on account of missing GHC 6 binaries (you can reinstall them, but it looks like they get replaced.)
- ACPI stopped knowing about X, so if you have scripts for handling rotation, source `/usr/share/acpi-support/power-funcs` and run `getXuser` and `getXconsole`
- DBUS didn’t start. This is due to leftover pid and socket files, see [this bug](https://bugs.launchpad.net/ubuntu/+source/dbus/+bug/811441)
- Was mysteriously fscking my root drive on every boot. Check your `pass` param in `/etc/fstab`; should be `0`.
- Redshift mysteriously was being reset by xrandr calls; worked around by calling it oneshot immediately after running xrandr.
- Not sure if this was related to the upgrade, but fixed an annoyance where suspend-checking (in case you are coming out of hibernate) was taking a really long time in boot. Set `resume` to right swap in `/etc/initramfs-tools/conf.d/resume` and `update-initramfs -u` with great prejudice).

Unresolved annoyances: [X11 autolaunching in DBUS](https://bugs.launchpad.net/ubuntu/+source/dbus/+bug/812940), the power icon doesn’t always properly show AC information and is too small in stalonetray, xmobar doesn’t support percentage battery and AC coloring simultaneously (I have a patch), a totem built from scratch segfaults.
