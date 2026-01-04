---
title: "Reduce Ubuntu latency by disabling mDNS"
date: 2012-03-24 15:56:04
slug: reduce-ubuntu-latency-by-disabling-mdns
categories: [Toolbox]
comments:
    - id: 4094
      author: LJ
      date: "2012-09-12 16:14:11"
      content: "ohmygodIcouldkissyou.  thank you so much - this un-broke my entire testbed.  ;-)"
    - id: 5164
      author: Dave
      date: "2012-12-05 02:52:01"
      content: "OMG3someIcomingWithMoreKisses, lol Thank you so much!"
    - id: 20667
      author: ViiJay
      date: "2016-04-05 13:15:39"
      content: |
        Holy. Shit.
        Seriously I have searched for this issue a long time and this is it. Just uninstall one simple package which messes up my whole DNS setup under linux.
        
        My issue was that could ping my local server with just the name like: "ping bob" and I got a reply. If I would use instead "ping bob.mydomain.local" it just didn't work. Within Linux I just couldn't get it to work. Within a Windows machine I had no trouble at all. First I thought there was my pfsense firewall acting up in some way but after a long period of time and countless internet searches I found this simply article and there it is. The solution for my problem.
        
        Thank you very much. Very. Very. Very. Much.
        
        VJ
---

This is a very quick and easy fix that has made latency on Ubuntu servers I maintain go from *three to four seconds* to instantaneous. If you've noticed that you have high latency on ssh or scp (or even other software like remctl), and you have control over your server, try this on the server: `aptitude remove libnss-mdns`. It turns out that multicast DNS on Ubuntu has a [longstanding bug](https://bugs.launchpad.net/ubuntu/+source/nss-mdns/+bug/94940) on Ubuntu where they didn't correctly tune the timeouts, which results in extremely bad performance on reverse DNS lookups when an IP has no name.

Removing multicast DNS will break some applications which rely on multicast DNS; however, if you're running Linux you *probably* won't notice. There are a number of other solutions listed on the bug I linked above which you're also welcome to try.
