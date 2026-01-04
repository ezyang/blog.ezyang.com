---
title: "Third-party unattended upgrades in three steps"
date: 2010-03-03 12:00:59
slug: third-party-unattended-upgrade
categories: [Toolbox]
comments:
    - id: 128
      author: Ken Takusagawa
      date: "2010-03-03 16:35:56"
      content: |
        One should also verify that
        
        APT::Periodic::Unattended-Upgrade "1";
        
        is in /etc/apt/apt.conf.d/10periodic
        
        I think I had to add it manually for Debian Lenny.
    - id: 129
      author: Edward Z. Yang
      date: "2010-03-03 16:45:43"
      content: Updated post to include this vital bit of information.
    - id: 395
      author: James Revillini
      date: "2010-05-07 11:38:37"
      content: |
        Great post!  Question - you said:
        
        This translates into the following configuration:
        
        Unattended-Upgrade::Allowed-Origins {
               "Ksplice karmic";
        };
        
        Should "Unattended-Upgrade::Allowed-Origins {" be "APT::Periodic::Unattended-Upgrade::Allowed-Origins {"?
        
        Maybe you assumed we'd know that, but I wasn't sure.
    - id: 398
      author: Edward Z. Yang
      date: "2010-05-07 14:34:50"
      content: It worked for me without the namespace qualifier.
    - id: 534
      author: syaman
      date: "2010-06-13 19:36:36"
      content: |
        Is it also possible to do apt-pinning for unattended upgrades? For instance if several updated packages appear in both the security repository and a third party one, but I only want a particular package to always be updated from the third party repository?
        
        Thanks in advance
    - id: 536
      author: Edward Z. Yang
      date: "2010-06-13 23:35:21"
      content: "My understanding is that apt-pinning works regardless of how the update gets taken (whether it's unattended or not). You should try and see how it goes."
    - id: 1733
      author: Andres Cimmarusti
      date: "2011-01-13 22:53:30"
      content: |
        Thanks for this post, it's just what I was looking for... Well, almost.
        I run Debian Squeeze on several computers and I use some packages from Debian Backports and Debian Multimedia repositories (I have my apt preferences setup so that only the packages already installed from these repos become upgradeable).
        
        Naturally I want to add these repos to the list of Allowed origins. Following your tutorial I get to, for example the Debian multimedia release file: http://www.debian-multimedia.org/dists/squeeze/Release
        But you can see that in the field ORIGINS there are the 3 words: Unofficial Multimedia Packages.
        I'm hesitant to put these 3 words in my 50unattended-upgrades followed by the SUITE. 
        Is this going to work at all? what should I do?
    - id: 1734
      author: Edward Z. Yang
      date: "2011-01-13 23:08:39"
      content: "There might be some quoting mumble going on, but I suspect the obvious thing probably will work. Try it and report back?"
    - id: 1997
      author: "Dotdeb packages unattended upgrade | blog.erben.sk"
      date: "2011-04-07 03:39:23"
      content: "[...] this page how to enable other repositories.  Filed under: uncategorized Leave a comment     Comments (0) [...]"
    - id: 2887
      author: "Things I learned today: apt, cron, and unattended-upgrades &laquo; Notatypewriter&#039;s Blog"
      date: "2011-08-04 23:36:43"
      content: "[...] Here are some instructions on adding third party repositories to the unattended-upgrades list. But rather than pointing your browser at the Release file at each repository (Google&#8217;s repository actually seems to disallow this, possibly through some user agent magic), you can look at your apt cache for the cached versions of these files. These are located in /var/lib/apt/lists. The files you want are the ones ending in *_Release. [...]"
    - id: 8379
      author: "ste-fan"
      date: "2014-09-30 15:41:52"
      content: |
        You can also find the needed info concerning the repository in the output of `apt-cache policy`, e.g.:
        
             500 http://deb.torproject.org/torproject.org/ trusty/main amd64 Packages
                 release o=TorProject,a=trusty,n=trusty,c=main
                 origin deb.torproject.org
        
        (`o=`)`TorProject` is the origin and (`a=`)`trusty` is the suite.
        
        Another side note - the correct syntax in `50unattended-upgrades` (now?) seems to be to seperate origin and suite by colon:
        
            Unattended-Upgrade::Allowed-Origins {
                "TorProject:trusty";
            };
    - id: 17714
      author: iynque
      date: "2015-09-29 13:50:03"
      content: |
        This is just what I needed. :D
        I had no idea how to figure out what the right origin:archive should be, and this method worked perfectly.
        
        I'm on Lubuntu 14.04, which notes in the file that the syntax is (origin:archive):
        "// Automatically upgrade packages from these (origin:archive) pairs"
        So, as ste-fan said, the syntax is slightly different (now?).
        
        Also, it's possible (at least in 14.04) to replace an explicit suite name—as suggested in this post—with a variable so you always grab the archive for your version, as in:
        "LP-PPA-teward-znc:${distro_codename}";
        
        This way, if I were to upgrade from 14.04 "trusty" to 15.04 "vivid," then my unattended upgrades would still be working and still be installing the correct code for me, rather than grabbing from the wrong version until I remember to edit the allowed sources again.
    - id: 22685
      author: "Roo&#039;s View &raquo; Blog Archive &raquo; Ubuntu 16.04 to 18.04 rebuild with new SSD"
      date: "2019-03-05 10:18:10"
      content: "[&#8230;] first found this post which provides a path to figuring it out, but I found a better description which I used to come up [&#8230;]"
---

[unattended-upgrades](http://packages.ubuntu.com/karmic/unattended-upgrades) is a nifty little package that will go ahead and automatically install updates for you as they become enabled. No serious system administrator should use this (you *are* testing updates before pushing them to the servers, right?) but for many personal uses automatic updates are really what you want; if you run `sudo aptitude full-upgrade` and don't read the changelog, you might as well turn on unattended upgrades. You can do this by adding the line `APT::Periodic::Unattended-Upgrade "1"` to `/etc/apt/apt.conf.d/10periodic` (thanks Ken!)

Of course, the default configuration they give you in `/etc/apt/apt.conf.d/50unattended-upgrades` only pulls updates from their security repository, and they only give you a commented out line for normal updates. People [have asked](http://ubuntuforums.org/showthread.php?t=1401845), "well, how do I pull automatic updates from other repositories?" Maybe you have installed Chromium dailies; seeing the "you have updates" icon every day can be kind of tiresome.

Well, here's how you do it:

1.  Find out what URL the PPA you're interested in points to. You can dig this up by looking at `/etc/apt/sources.list` or `/etc/apt/sources.list.d/` (the former is if you manually added a PPA at some point; the latter is likely if you used `add-apt-repository`).
2.  Navigate to that URL in your browser. Navigate to `dists`, and then navigate to the name of the distribution you're running (for me, it was `karmic`). Finally, click `Release`. (For those who want to just enter the whole URL, it's <http://example.com/apt/dists/karmic/Release>).
3.  You will see a number of fields `Fieldname: Value`. Find the field `Origin` and the field `Suite`. The two values are the ones to put in Allowed-Origins.

For example, the [Ksplice repository](http://www.ksplice.com/apt/dists/karmic/Release) has the following `Release` file:

    Origin: Ksplice
    Label: Ksplice
    Suite: karmic
    Codename: karmic
    Version: 9.10
    Date: Sun, 07 Feb 2010 20:51:12 +0000
    Architectures: amd64 i386
    Components: ksplice
    Description: Ksplice packages for Ubuntu 9.10 karmic

This translates into the following configuration:

    Unattended-Upgrade::Allowed-Origins {
           "Ksplice karmic";
    };

And that's it! Go forth and make your systems more secure through more timely updates.

*Bonus tip.* You can turn on unattended [kernel updates](http://www.ksplice.com/) via Ksplice by editing `/etc/uptrack/uptrack.conf` and setting `autoinstall = yes`.
