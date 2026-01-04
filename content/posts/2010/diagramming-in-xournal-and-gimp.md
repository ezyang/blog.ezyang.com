---
title: "Diagramming in Xournal and Gimp"
date: 2010-04-09 09:00:35
slug: diagramming-in-xournal-and-gimp
categories: [Toolbox]
comments:
    - id: 290
      author: Anonymous
      date: "2010-04-09 13:22:44"
      content: "Dia (for Linux) is much better than OmniGraffle. But hand-drawn are better than both. I showed of your post in large part because the pictures were great!"
    - id: 291
      author: ifp5
      date: "2010-04-09 14:48:00"
      content: "I'd advise to take a look at SDraw (part of OpenOffice package). It lacks stencil library mechanism, but except for that lacking SDraw is quite good enough. Also it supports grouping and copy-pasting which allows to do most Visio jobs."
    - id: 298
      author: Edward Z. Yang
      date: "2010-04-11 22:22:42"
      content: "Anonymous and ifp5, thanks for the suggestions. I've also received a recommendation for Krita, the KDE based drawing app."
    - id: 1775
      author: David Gibb
      date: "2011-02-16 17:46:25"
      content: "You should really check out tikz. Sure, it's not as friendly as some of the gui apps, but you can create some really nice diagrams"
    - id: 3644
      author: Alex
      date: "2012-04-16 22:35:16"
      content: |
        Hi Edward,
        
        I wanted to check one more thing about this post -- you mentioned word tablet up there. Is it actually useable on the tablet (as in android touchscreen)? I can't see it mentioning android anywhere on Xournal home page.
    - id: 3652
      author: Edward Z. Yang
      date: "2012-04-17 10:14:03"
      content: "I have never used Xournal on an iPad style tablet. I'm not sure if it is a good idea, since those tends of tablets tend not to have stylus."
    - id: 6179
      author: Anonymous
      date: "2013-07-18 07:21:08"
      content: "i know this blog is a bit old but i'm looking around for ways to draw nice pictures just like you and could not find good recommendations. do you use a particular digital pen (for Linux)?"
    - id: 6181
      author: Edward Z. Yang
      date: "2013-07-18 12:13:40"
      content: "Well, a digital pen is not enough if you don't have a tablet.  The X61 comes with a Wacom stylus, which I quite like."
---

Two people have asked me how drew the diagrams for my previous post [You Could Have Invented Zippers](http://blog.ezyang.com/2010/04/you-could-have-invented-zippers/), and I figured I'd share it with a little more elaboration to the world, since it's certainly been a bit of experimentation before I found a way that worked for me.

Diagramming software for Linux sucks. Those of you on Mac OS X can churn out eye-poppingly beautiful diagrams using [OmniGraffle](http://www.omnigroup.com/products/OmniGraffle/); the best we can do is some dinky GraphViz output, or maybe if we have a lot of time, a painstakingly crafted SVG file from Inkscape. This takes too long for my taste.

So, it's hand-drawn diagrams for me! The first thing I do is open my trusty [Xournal](http://xournal.sourceforge.net/), a high-quality GTK-based note-taking application written by [Denis Auroux](http://www-math.mit.edu/~auroux/) (my former multivariable calculus professor). And then I start drawing.

![image](/img/diagrams/xournal.png)

Actually, that's not *quite* true; by this time I've spent some time with pencil and paper scribbling diagrams and figuring out the layout I want. So when I'm on the tablet, I have a clear picture in my head and carefully draw the diagram in black. If I need multiple versions of the diagram, I copy paste and tweak the colors as I see fit (one of the great things about doing the drawing electronically!) I also shade in areas with the highlighter tool. When I'm done, I'll have a few pages of diagrams that I may or may not use.

From there, it's "File \> Export to PDF", and then opening the resulting PDF in Gimp. For a while, I didn't realize you could do this, and muddled by using `scrot` to take screen-shots of my screen. Gimp will ask you which pages you want to import; I import all of them.

![image](/img/diagrams/gimp.png)

Each page resides on a separate "layer" (which is mildly useless, but not too harmful). I then crop a logical diagram, save-as the result (asking Gimp to merge visible layers), and then undo to get back to the full screen (and crop another selection). When I'm done with a page, I remove it from the visible layers, and move on to the next one.

When it's all done, I have a directory of labeled images. I resize them as necessary using `convert -resize XX% ORIG NEW` and then dump them in a public folder to link to.

*Postscript.* Kevin Riggle reminds me not to mix green and red in the same figure, unless I want to confuse my color blind friends. Xournal has a palette of black, blue, red, green, gray, cyan, lime, pink, orange, yellow and white, which is a tad limiting. I bet you can switch them around, however, by mucking with `predef_colors_rgba` in *src/xo-misc.c*
