---
title: "How to package Coq as a PPA"
date: 2013-08-01 14:14:52
slug: 
draft: true
categories: [Miscellaneous]
---

Patched versions of Coq seem to be all the rage these days ([mtac](http://plv.mpi-sws.org/mtac/) and [HoTT](https://github.com/HoTT/coq)), and distributing this software can be somewhat annoying, since on cannot simply tell a colleague to `aptitude install coq` to get the latest software.

I decided to

mk-sbuild (used to need LVM, no longer; lower level sbuild-createchroot)

dget URL-OF-DSC-FILE (find it on Launchpad; make sure this is in a new directory)

for your own non-confusion, make a new changelog entry with ~raring1

debuild -S

apt-get build-dep coq

sbuild -d raring foo.dsc

DEB_BUILD_OPTIONS=nocheck

> To CHANGE the golden image: sudo schroot -c source:raring-amd64 -u root To ENTER an image snapshot: schroot -c raring-amd64 To BUILD within a snapshot: sbuild -A -d raring-amd64 PACKAGE\*.dsc
