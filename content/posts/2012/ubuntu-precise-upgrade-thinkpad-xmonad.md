---
title: "Ubuntu Precise upgrade (Thinkpad/Xmonad)"
date: 2012-05-18 20:51:12
slug: ubuntu-precise-upgrade-thinkpad-xmonad
categories: [Toolbox]
comments:
    - id: 3879
      author: Nicolás Wolovick
      date: "2012-07-25 13:17:04"
      content: "Did you recovered the battery icon under xmonad? I also miss it a lot."
    - id: 3881
      author: Edward Z. Yang
      date: "2012-07-25 18:53:49"
      content: "Alas, I never did figure out which daemon was responsible for it. Let us know if you figure it out!"
    - id: 4101
      author: Steve Levine
      date: "2012-09-13 19:23:07"
      content: "Agreed that the old monospace font is quite nice - switching back as well. Thanks for the pointer :-)"
---

It is once again time for Ubuntu upgrades. I upgraded from Ubuntu Oneiric Ocelot to Ubuntu Precise Pangolin (12.04), which is an LTS release. Very few things broke (hooray!)

- The Monospace font changed to something new, with very wide glyph size. The old font was DejaVuSansMono, which I switched back to.
- Xournal stopped compiling; somehow the linker behavior changed and you need to specify the linker flags manually.
- [gnome-keyring](https://bugs.launchpad.net/ubuntu/+source/gnome-keyring/+bug/932177) isn't properly starting up for us non-Unity folks. The underlying problem appears to be [packaging errors by Gnome](http://lists.debian.org/debian-lint-maint/2009/07/msg00129.html), but adding `` eval `gnome-keyring-daemon -s ``<span class="title-ref"> to my </span><span class="title-ref">.xsession</span>\` cleared things up.
- The battery icon went away! I assume some daemon is failing to get run, but since I have a very nice xmobar display I'm not mourning its loss.
- Default GHC is GHC 7.4.1! Time to rebuild; no Haskell Platform yet. (Note that GHC 7.4.1 doesn't support the gold linker; this is the `chunk-size` error.)

I also upgraded my desktop from the previous LTS Lucid Lynx.

- I had a lot of invalid signature errors, which prevented the release upgrade script from running. I fixed it by uninstalling almost all of my PPAs.
- Offlineimap needed to be updated because some Python libraries it depended on had backwards incompatible changes (namely, the imap library.)
- VirtualBox messed up its revision numbers, which contained an [underscore which is forbidden](https://bugs.launchpad.net/ubuntu/+source/dpkg/+bug/613018). Manually editing it out of the file seems to fix it.
