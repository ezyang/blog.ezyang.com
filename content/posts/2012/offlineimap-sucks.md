---
title: "OfflineIMAP sucks"
date: 2012-08-30 01:07:58
slug: offlineimap-sucks
categories: [Toolbox]
comments:
    - id: 4045
      author: Mathnerd314
      date: "2012-08-30 11:32:40"
      content: |
        Well, how hard could it be to reimplement it in Haskell? It's only 6k lines of Python code, which since Haskell is a more terse language amounts to only 5k lines of Haskell code. You could write that much code in a GSOC project. Plus there's the HaskellNet library (itself another GSOC project), which might or might not have done half the work already.
        
        Does this mean you should immediately fork OfflineIMAP and start working on the port? Probably not. But it's something to consider.
    - id: 4046
      author: Edward Z. Yang
      date: "2012-08-30 11:34:17"
      content: "Yes, I think rewriting OfflineIMAP in Haskell would be a good project. Not one that I would do, but probably tractable for GSOC (as long as you're not trying to solve the test infrastructure problem; that's another GSOC in and of itself). It is, however, *hard*, and Haskell will not necessarily help some of the hard bits."
    - id: 4088
      author: Ivan Miljenovic
      date: "2012-09-10 03:51:27"
      content: "So do you recommend any of the alternatives (such as mbsync)?"
    - id: 4089
      author: Edward Z. Yang
      date: "2012-09-10 09:24:47"
      content: "No. As I mentioned, I still use offlineimap. If anyone wants to plug their personal favorite, please do."
    - id: 4090
      author: Dave Abrahams
      date: "2012-09-10 12:35:21"
      content: "In an attempt to escape suckiness, I recently did some extensive research on OfflineIMAP alternatives recently.  I'm still using OfflineIMAP :-(."
    - id: 4243
      author: Jan Niggemann
      date: "2012-09-25 16:03:38"
      content: |
        Perhaps it sucks, but I found no alternative when I searched 2 years ago...
        If Dave can't find any alternatives in 2012, I'll take it that there aren't any and I'll continue using OfflineIMAP...
    - id: 4379
      author: Dave Abrahams
      date: "2012-10-12 22:13:24"
      content: "I think I may have finally found one I like: mbsync from http://isync.sourceforge.net/.  Use the Git master branch.  Appears very impressive."
    - id: 5975
      author: Tim Smart
      date: "2013-02-25 05:05:51"
      content: |
        I have tried offlineimap and concur that it sucks. Slow, crashes and consumes way too much resources just for e-mail.
        
        I am now using mbsync which is *much* more efficient, has a nice low footprint and plenty of configuration options. I use it in tandum with mutt and https://github.com/tim-smart/node-mbsync-watcher , which syncs folders whenever anything changes. Much better than my previous setup of plain mutt with its built-in imap support. For large mailboxes that thing was slow.
    - id: 6360
      author: Marcus
      date: "2014-01-23 07:38:13"
      content: "It has been almost one and a half year now, do the points made here still apply?"
    - id: 6364
      author: Edward Z. Yang
      date: "2014-01-26 21:29:52"
      content: "Development seems to be marginally less moribund since I last looked at it, and I think sqlite status is default now. I don't know what the rest of the status is."
    - id: 6386
      author: Anonymous
      date: "2014-03-01 15:23:33"
      content: "isync (mbsync) seems like a perfect replacement and seems to be really reliable ;-)"
    - id: 12154
      author: worksforme
      date: "2015-01-17 13:28:07"
      content: no issues synchronizing imap account
    - id: 14970
      author: Gustavo Fring
      date: "2015-07-07 07:10:46"
      content: there is no excuse for not using mbsync instead. mbsync has easier configuration and is much faster
    - id: 16753
      author: Lluís
      date: "2015-09-10 08:38:32"
      content: |
        The main problem I've found with mbsync is that it does not support IMAP's custom flags, which I use through gnus (a mail client for Emacs):
          http://sourceforge.net/p/isync/mailman/message/32518610/
    - id: 21783
      author: Jose
      date: "2017-01-13 15:25:20"
      content: |
        Hello.
        
        Did things change now in 2017? What's your impression? I want to use offlineimap or isync, but not sure what one to use.
        
        Kind regards.
    - id: 21816
      author: Thomas Tuegel
      date: "2017-02-15 17:27:44"
      content: |
        Jose,
        
        I am a long-time user of OfflineIMAP and a recent convert to mbsync, so I can comment a little on this. I find mbsync to be significantly faster than OfflineIMAP; I don't have quantitative data, but re-downloading an entire account with ~7000 messages with mbsync is faster than checking the same account with no new messages over OfflineIMAP. I also find that mbsync is much more robust. If your connection is interrupted while syncing, OfflineIMAP will corrupt its cache (even if there is no new mail!) and you will have to download the entire mailbox again. This isn't a bug in the implementation of OfflineIMAP; it's an essential feature of how it tracks messages.
        
        The main problem with mbsync is that it seems unable to download more than ~200 messages without freezing. (It's a good thing it's robust to interruptions!) It's also not very smart; I had to manually configure the authentication mechanisms for one account because it insisted on using a mechanism I don't have installed. OfflineIMAP is significantly more configurable than mbsync, but that is only good to a point: OfflineIMAP has so many configuration options, there is no central documentation that lists all of them. (The best, maybe only, way to write your OfflineIMAP configuration is to search the Web for configurations by other people who use the same provider.) OfflineIMAP also has the ability to include arbitrary Python code in your configuration file; again this is a double-edged sword: the _ability_ to include arbitrary Python code in the configuration file has led to a state of affairs where it is almost _required_ that you do so. On the other hand, mbsync has a single man page describing every command-line and configuration file option; you can sit down and read the whole thing in less than ten minutes.
        
        In short, I would strongly recommend that you use mbsync over OfflineIMAP.
    - id: 21984
      author: Ryan
      date: "2017-05-15 23:50:53"
      content: |
        In case anyone's interested, I just wrote a daemon for mbsync:
        
        It listens for changes to any IMAP IDLE mailboxes, as well as for local changes, and syncs when they occur. I hope someone here finds it useful!
        
        https://github.com/rlue/little_red_flag
---

I am going to share a dirty little secret with you, a secret that only someone who uses and hacks on OfflineIMAP could reasonably know: OfflineIMAP sucks. Of course, you can still use software that sucks (I do all the time), but it’s useful to know what some of its deficiencies are, so that you can decide if you’re willing to put up with the suckage. So why does OfflineIMAP suck?

> This is not really a constructive post. If I were actually trying to be constructive, I would go and fix all of these problems. But all of these are big problems that require substantial amounts of effort to fix... and unfortunately I don’t care about this software enough.

# Project health is anemic

The original author, [John Goerzen](http://www.complete.org/JohnGoerzen), has moved on to greener, more Haskell-like pastures, and the current maintainership is having difficulty finding the time and expertise to do proper upkeep on the software. Here is [one of the most recent calls for maintainers](http://comments.gmane.org/gmane.mail.imap.offlineimap.general/5754), as both of the two co-maintainers who were maintaining OfflineIMAP have failed to have enough free time to properly keep track of all submitted patches. There still seem to be enough people with a vested interest in seeing OfflineIMAP not bitrot that the project should continue to keep working for the foreseeable future, but one should not expect any dramatic new features or intensive work to be carried out on the codebase.

# Nearly no tests

For most of OfflineIMAP’s history, there were no tests. While there is now a dinky little test suite, it has nowhere near the coverage that you would want out of such a data-critical program. Developers are not in the habit of adding new regression tests when they fix bugs in OfflineIMAP. But perhaps most perniciously, there is no infrastructure for testing OfflineIMAP against as wide a range of IMAP servers as possible. Here are where the really *bad* bugs can show up, and the project has none of the relevant infrastructure.

# Over-reliance on UIDs

OfflineIMAP uses UIDs as its sole basis for determining whether or not two messages correspond to each other. This works almost most of the time, except when it doesn’t. When it doesn’t, you’re in for a world of hurt. OfflineIMAP does not support doing consistency checks with the `Message-Id` header or the checksum of the file, and it’s `X-OfflineIMAP` hack for servers that don’t support `UIDPLUS` ought to be taken out back and shot. To it’s credit, however, it has accreted most of the special casing that makes it work properly in all of the weird cases that show up when you have UIDs.

# Poor space complexity

The memory usage of OfflineIMAP is linear with the number of messages in your inbox. For large mailboxes, this effectively means loading hundreds of thousands of elements into a set and doing expensive operations on it (OfflineIMAP consistently pegs my CPU when I run it). OfflineIMAP should be able to run in constant space, but zero algorithmic thought has been put into this problem space. It also has an extremely stupid default status folder implementation (think repeatedly writing 100MB to disk for *every single file you upload*), though you can fix that fairly easily by setting `status_backend = sqlite`. Why is it not default? Because it’s still experimental. Hm...

# Unoptimized critical path

OfflineIMAP was never really designed for speed. This shows up in the synchronization time it takes, even in the common cases of no changes or just downloading a few messages. If one’s goal is to download your new messages as quickly as possible, a lot of adjustments could be made, including reducing the number of IMAP commands (esp. redundant selects and expunges), reducing the number of times we touch the filesystem, asynchronous filesystem access, not loading the entirety of a downloaded message in memory, etc. A corollary is that OfflineIMAP doesn’t really seem to understand what data it is allowed to lose, and what data it must fsync before carrying on to the next operation: “safety” operations are merely sprinkled through the code without any well-defined discipline. Oh, and how about some inotify?

# Brain-dead IMAP library

OK, this one is not really OfflineIMAP’s fault, but `imaplib2` really doesn’t protect you from the pointy-edged bits of the IMAP protocol (and how it is implemented in the real-world) at all. You have to do it all yourself. This is dumb, and a recipe for disaster when you forget to check UIDVALIDITY in that new IMAP code you were writing. Additionally, it encodes almost no knowledge of the IMAP RFC, with respect to responses to commands. Here is one place where some more type safety would really come in handy: it would help force people to think about all of the error cases and all of the data that could occur when handling any given command.

# Algorithmica obscura

OfflineIMAP has a fair bit of debugging output and UI updating code interspersed throughout its core algorithms, and the overall effect is that it’s really hard to tell what the overall shape of the algorithm being employed is. This is not good if the algorithm is kind of subtle, and relies on some global properties of the entire execution to ensure its correctness. There is far too much boilerplate.

# Conclusion

In conclusion, if you would like to use OfflineIMAP on a well-behaved, popular, open-source IMAP server which a maintainer also happens to use with a relatively small number of messages in your INBOX and are willing to put up with OfflineIMAP being an immutable black box that consumes some non-zero amount of time synchronizing in a wholly mysterious way, and never want to hack on OfflineIMAP, there is no finer choice. For everyone else, well, good luck! Maybe it will work out for you! (It mostly does for me.)
