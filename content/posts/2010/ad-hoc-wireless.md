---
title: "Ad hoc wireless"
date: 2010-03-24 09:00:05
slug: ad-hoc-wireless
categories: [Personal, Toolbox]
comments:
    - id: 238
      author: Gabriel
      date: "2010-03-26 04:12:50"
      content: "I don't know what kind of hardware you own, but Linux is certainly able to set up ad-hoc networks (given the right drivers). Look at http://wifi.pps.jussieu.fr (using Babel : http://www.pps.jussieu.fr/~jch/software/babel/ on OpenWrt) or http://www.funkfeuer.at/ (using OLSR), it's all made of Linux-based routers. And I can confirm that OS X was able to connect to the Babel network, for instance (didn't try with Windows though). Of course, YMMV."
    - id: 241
      author: Edward Z. Yang
      date: "2010-03-26 16:36:09"
      content: "Right; I imagine that for some permutation of hardware and Linux drivers ad-hoc networks would work on Linux. The key is having the drivers. :-)"
    - id: 246
      author: carmen
      date: "2010-03-27 19:06:16"
      content: |
        linux would time out if it didnt find an ad hoc neighbour MAC address soon, then take down the entire essid/net, in my attempt
        
        basically if i lined up the laptops and pressed enter on all 3 at once, it worked!
    - id: 267
      author: milosh
      date: "2010-04-05 05:03:23"
      content: |
        I don't know what dnsmasq is for. I use my linux desktop as a wifi router (ad-hoc as my card cannot do better), connecting to it through linux, nintendo, mac osx works. It is just a matter of 1) configuring network interfaces (and declare essid, channel,...), 2) enabling ip_forwarding*, 3) add iptable rules to route things**. Optionnally, 1bis, configuring a dhcp server for the clients not to have to manually configure ip addresses and routes.
        *: means: 'echo 1 &gt; /proc/sys/net/ipv4/ip_forward'
        **: for example 'iptables -t nat -A POSTROUTING -s "192.168.0.0/255.255.255.0" -j MASQUERADE'
        Or else, use a graphical prog for that. Firestarter for example does it.
---

Hello from Montreal! I'm writing this from a wireless connection up on the thirty-ninth floor of La Cité. Unfortunately, when we reading the lease, the only thing we checked was that it had "Internet"... not "Wireless." So what's a troop of MIT students with an arsenal of laptops and no wireless router to do? Set up wireless ad hoc networking.

Except it doesn't actually work. Mostly. It took us a bit of fiddling and attempts on multiple laptops to finally find a configuration that worked. First, the ones that didn't work:

- *Windows,* as Daniel Gray tells me, has two standard methods for creating ad hoc networks: bridging two networks or .... We tried both of them, and with ... we were able to connect other Windows laptops and Mac OS X laptops... but no luck with the Linux laptops. As three of us are Linux users, we were quite unhappy with this state of affairs.
- *Linux* theoretically has support for ad hoc networks using dnsmasq; however, we tried two separate laptops and neither of them were able to set up an ad hoc network that any of the other laptops were able to use. We did discover some hilarious uninitialized field bugs for ESSIDs.
- *Mac OS X.* At this point, we were seriously considering going out, finding a wireless hardware store, and buying a router for the apartment. However, someone realized that there was one operating system we hadn't tried yet. A few minutes of fiddling... and yes! Ad hoc network that worked on all three operating systems!

Ending score: Apple +1, Microsoft 0, Linux -1. Although, it's hard to be surprised that no one actually is paying the attention necessary to the wireless drivers.
