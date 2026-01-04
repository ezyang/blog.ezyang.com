---
title: "How to build i686 glibc on Ubuntu"
date: 2011-12-18 18:03:44
slug: how-to-build-i686-glibc-on-ubuntu
categories: [Toolbox]
---

An “easy”, two-step process:

1.  [Apply this patch for i686](http://www.eglibc.org/archives/patches/msg00073.html). (Why they haven't fixed this in the trunk, I have no idea.)
2.  Configure with `CFLAGS="-U_FORTIFY_SOURCE -fno-stack-protector -O2"` (this disables fortify source and stack protection which Ubuntu enables by default but interferes with glibc. You need to keep optimizations on, because glibc won't build without it.) You’ll need to do the usual extra dance of creating a separate build directory and specifying a prefix.

Hope this helps someone else. In case you were wondering why I was building glibc, it's because I was reporting these two bugs in iconv:

- [Bug 13517: iconv generates spurious warnings even with //IGNORE](http://sources.redhat.com/bugzilla/show_bug.cgi?id=13517)
- [Bug 13518: iconv truncates input with //IGNORE](http://sources.redhat.com/bugzilla/show_bug.cgi?id=13518)
