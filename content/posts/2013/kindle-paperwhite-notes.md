---
title: "Kindle Paperwhite notes"
date: 2013-01-30 09:00:17
slug: kindle-paperwhite-notes
categories: [Toolbox]
comments:
    - id: 5951
      author: Bill Casarin
      date: "2013-01-30 09:45:29"
      content: "For reading PDFs the kindle DX is still the only way to go. As far as I know there are no plans for making a paperwhite version of it though :("
    - id: 5952
      author: Mikhail Glushenkov
      date: "2013-01-30 14:51:06"
      content: |
        Thanks for the link to cut2col!
        
        On the topic of interesting software, there's https://github.com/hwhw/kindlepdfviewer which adds support for DjVu and ePub.
    - id: 5953
      author: Edward Z. Yang
      date: "2013-01-30 14:54:16"
      content: "Oh sweet! But since it adds PDF support, I bet it's not Paperwhite targeted (which features native PDF support)."
    - id: 5954
      author: Mikhail Glushenkov
      date: "2013-01-30 17:33:34"
      content: Wikipedia says that native PDF support was introduced in a Nov 2009 firmware update to Kindle 2.
    - id: 5959
      author: Maciek
      date: "2013-01-31 16:18:43"
      content: |
        If you like cut2col, then check out also k2pdfopt (http://www.willus.com/k2pdfopt/). 
        It is the best tool for converting PDFs for Kindle I've seen so far.
    - id: 5961
      author: Edward Z. Yang
      date: "2013-01-31 16:24:49"
      content: "Nice, I'll give it a try."
    - id: 5962
      author: illissius
      date: "2013-01-31 16:44:45"
      content: |
        Thanks, for klip.me especially! I've been searching for something similar. Lack of image support is a bummer. But I'll definitely give it a try.
        
        I haven't had much trouble with academic papers. The Kindle zooms in 50% increments, so with a two column layout I zoom in and then read the four quadrants sequentially, which is good enough. It seems like cut2col pregenerates the same thing, which might be more convenient. For single column layouts landscape mode usually works. The problematic case (Kleisli arrows of outrageous fortune was the recent example) is when it uses a single column layout but with huge margins on either side, but less than half the page width, so if you zoom in it doesn't fit, and in landscape mode it's still tiny. So the options are to strain your eyes (surprisingly it /is/ still barely readable, so you can if you want to, and I did), or read it somewhere else.
        
        My biggest annoyance has been how often it does a page flip when I had intended to pan.
        
        And I've likewise found that the inefficiency of random access is the big drawback relative to physical paper.
    - id: 5972
      author: Oleksandr Manzyuk
      date: "2013-02-05 15:52:49"
      content: "Thanks for cut2col!  I use briss (http://briss.sourceforge.net/) to crop PDF scans of books, so that they can be comfortably read on my Kindle 4 in the landscape mode."
---

Along with a [Nexus 7](http://blog.ezyang.com/2012/12/googl-nexus-7-setup-notes/), I also acquired a [Kindle Paperwhite](http://www.amazon.com/Kindle-Paperwhite-Touch-light/dp/B007OZNZG0) over winter break. (Wi-Fi only) I have been quite pleased by this purchase, though in an unexpected way: while I have not increased the number of books I read, the Kindle has materially changed how I read *articles on the Internet.* Not via their web browser, which is essentially unusable except for the simplest tasks, but via tools which take articles on the Internet and convert them into ebook form.

For **blog posts** I use [Calibre](http://calibre-ebook.com/) with the Google Reader source. This has been revolutionary: I *no longer read blog posts in my browser*; instead, I bundle them up on my Kindle and read them at my leisure. This change has also meant that I read blog posts much more carefully ([interfluidity](http://www.interfluidity.com/) used to only get a skim; now I can actually make it through the posts). This setup is a little nontrivial so I’ll probably describe it in a later blog post. (I used to use [Klip.me](http://www.klip.me/googlereader/), which had easy setup, but (1) it could not handle images (so equations and graphs are right out), and (2) it botched formatting of `pre` formatted text.)

For **longform articles** I use [Longform](http://longform.org/), which serves up a interesting mix of in-depth reporting and nonfiction articles. They have a very convenient “Send to Kindle” button which I’m told is served by Readability; I wonder if I should add a button like that to my blog. I am also using Amazon’s [Send to Kindle](http://www.amazon.com/gp/sendtokindle/firefox) Firefox extension for my paywalled articles (primarily [LWN.net](http://lwn.net/)), although its results seem a bit spottier than Readability.

For **academic papers**, the going has been a bit rough, but I have gotten decent results on two-column papers with [cut2col](http://www.cp.eng.chula.ac.th/~somchai/cut2col/), which manages to make text large enough to be readable in Kindle’s PDF reader. Landscape mode also helps a bit with reading PDFs. Generally, however, managing which PDFs are on my device is a bit of an open problem. I haven’t started using Calibre yet for this purpose, although I have used it to perform some conversions between ebook formats. These conversions don’t work particularly well, although they are usable. It’s well worth getting the latest version of Calibre, since there are some bugs in the current Quantal version; I use [ppa:n-muench/calibre](https://launchpad.net/~n-muench/+archive/calibre).

For **textbooks**, ebook editions are still extremely expensive. I would love to carry around all of the famous computer science textbooks in my pocket, but to date I don’t have any good way of doing so without breaking the bank. **Books** suffer similarly: it turns out I did most of my pre-Kindle reading borrowing books from libraries; however, I hear the [Palo Alto public library](http://www.cityofpaloalto.org/gov/depts/lib/default.asp) does Kindle lending, and I intend to check them out at some point in time. The typesetting for public domain books put out by places like Project Gutenberg are notoriously spotty; Readability and similar services seem to do a much better job! Alas, the Stanford library system does not appear to carry ebooks. The Amazon free samples of books have also been good fun, and I’ve collected quite a few of them.

Some annoyances with the Kindle itself:

- Amazon Special Offers, which I cannot seem to get rid of (apparently you can only pay the \$20 to remove them if the Kindle is associated with the original Amazon account that purchased the device; which is not me)
- Re-sorting behavior of the *Recent* view: if you have gone home after reading a book, the Kindle will briefly flash the old order of items before it reshuffles and moves the item you just finished reading to the front. If you attempt to click on a different item during this period, the click will not register until the new reshuffle has happened; this has frustratingly caused me to accidently click on the wrong item multiple times!
- With hypertext articles, they tend to contain links, which means that if you are using a "tap" to advance to the next page, and a link appears under your finger and you will accidentally bring up your browser. This is quite annoying and I would just like to turn off links entirely. (Yes, I know you can just swipe, but that is annoying and I do not want to bother retraining myself.)
- For certain formats, especially PDFs, page refresh speed is rather slow; this makes it difficult to rapidly flip through pages like you might in a real book. This is probably the primary downside of a Kindle as opposed to a traditional book; the other being the inability to see and rapidly flip to bookmarks (it takes more than one tap to move to a bookmark on a Kindle).
- I have also jailbroken my Kindle, but there does not seem to be any interesting software to run with it.

All-in-all, I am quite pleased with this device, and I would recommend it to anyone interested in reducing the amount of time they spend staring at a computer monitor.
