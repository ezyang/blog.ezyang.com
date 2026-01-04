---
title: "How OfflineIMAP works"
date: 2012-08-27 10:00:33
slug: how-offlineimap-works
categories: [Toolbox]
comments:
    - id: 4021
      author: Nicolas Wu
      date: "2012-08-27 11:42:21"
      content: "Wonderful! It just so happens that I was looking at OfflineIMAP yesterday, and wondering what was meant by \"an algorithm designed to prevent mail loss at all costs\": thanks for taking the time to look into this and reporting to the rest of us!"
    - id: 4027
      author: Brian Sniffen
      date: "2012-08-28 00:37:00"
      content: "Pity you didn't test it.  OfflineImap has lost mail for me.  I believe the reasons have to do with its actually using lists of message IDs, not the message contents---necessary for reasonable performance, but prone to loops of message copying when used against Exchange.  Lots of copying, lots of deleting, and plenty of chance to screw up and drop something."
    - id: 4032
      author: Edward Z. Yang
      date: "2012-08-28 09:17:09"
      content: |
        Brian: I've been using OfflineIMAP for the better part of three years. I've seen a lot of boneheaded behavior from it, some of it my fault, some of it OfflineIMAP's fault. That being said, I've never used OfflineIMAP with Exchange, and I suppose there is no good reason to believe Exchange has implemented UID correctly.
        
        Actually, the correctness of the above algorithm is entirely predicated on UIDs being handled correctly by the server. If we used message IDs, we'd get more stability, and if we used hashes of the messages, even better. OfflineIMAP doesn't do those checks (and it probably should.) I have a lot of rags against OfflineIMAP; maybe I should save those for another post :-)
    - id: 4036
      author: gasche
      date: "2012-08-29 03:24:19"
      content: "Given that your post made me want to use OfflineIMAP again, it's your responsibility to disclose any rags you have!"
    - id: 4042
      author: Tracy Reed
      date: "2012-08-29 21:13:32"
      content: "I've been trying to use offlineimap for a number of months. While I haven't noticed any lost mail, it has been very unreliable. I just can't run it from a cron job or continuously in a terminal and not have it jam up somewhere. Deadlocks and other issues seem quite common. Unfortunately I haven't had the time to run it in debug mode and see just what is going on but it looks rather complicated and I'm not sure I will ever bother. Other people have reported it being very reliable so I have no idea what is going on. I am synchronizing four different email accounts with it (two dovecot imap, one exchange, one gmail) so maybe that is a source of problems. I'm planning on switching back to direct IMAP access."
    - id: 5967
      author: Daniel
      date: "2013-02-01 03:33:02"
      content: "Interestingly, my first experience running OfflineImap, following the configuration instructions on the arch linux wiki verbatim, lead to mass irrecoverable deletion of the email in several of my gmail folders, including \"drafts\" and \"starred\". Even if there was some user error (I don't really see how), I don't see how this could possibly be an algorithm that avoids mail loss \"at all costs\"."
    - id: 6198
      author: Anonymous
      date: "2013-08-22 06:56:28"
      content: "Awesome post, thanks a lot for the work!"
    - id: 6231
      author: "Use read-only Mutt IMAP for shared and subscribed folders | Living {values, technology, spirit}"
      date: "2013-09-11 05:07:21"
      content: "[&#8230;] subscribed read-only IMAP folders, you can use offlineimap with a local retention policy. It&#039;s a sophisticated tool. In this case it&#039;s 35d and subscribed folders only. You can switch Mutt onto the offlineimap [&#8230;]"
---

As software engineers, we are trained to be a little distrustful of marketing copy like this:

> OfflineIMAP is SAFE; it uses an algorithm designed to prevent mail loss at all costs. Because of the design of this algorithm, even programming errors should not result in loss of mail. I am so confident in the algorithm that I use my own personal and work accounts for testing of OfflineIMAP pre-release, development, and beta releases.

What is this algorithm? Why does it work? Where is the correctness proof? Unfortunately, no where in OfflineIMAP’s end user documentation is the algorithm described in any detail that would permit a software engineer to convince himself of OfflineIMAP’s correctness. Fortunately for us, OfflineIMAP is open source, so we can find out what this mysterious algorithm is. In fact, OfflineIMAP's synchronization algorithm is very simple and elegant. (Nota bene: for simplicity’s sake, we don’t consider message flag synchronization.)

# Preliminaries

Define our local and remote repositories (Maildir and IMAP, respectively) to consist of sets over messages L and R. In a no-delete synchronization scheme, we would like to perform some set of operations such that end states of the repositories L' and R' are L ∪ R.

However, no-delete synchronization schemes work poorly for email, where we would like the ability to delete messages and have those changes be propagated too. To this end, OfflineIMAP defines a third repository called the status repository, also a set over messages, which says whether or not a message has been synchronized in the past without an intervening synchronized delete. There are now seven possible states for a message to have, based on which repositories it is a member:

![image](/img/offlineimap/state-space.png)

Considering all possible combinations:

- **Synchronized** (L,R,S): The message is fully synchronized and needs no further processing.
- **New Local** (L): The message was newly added to the local repository and needs to be uploaded.
- **New Remote** (R): The message was newly added to the remote repository and needs to be downloaded.
- **Status Missing** (L,R): The message is synchronized but our status is out-of-date.
- **Remote Removed** (L,S): The message was synchronized, but since then was removed from the remote; it should now be removed from local.
- **Local Removed** (R,S): The message was synchronized, but since then was removed from the local; it should now be removed from remote.
- **Missing** (S): The message has been deleted everywhere and our status has a stale entry for it.

The green-shaded region of the Venn diagram is what we would like L, R and S to cover at the end of synchronization.

# Algorithm

Define a synchronization operation on a source, destination and status repository `syncto(src, dst, status)` to be these two steps:

1.  Calculate the set difference `src - status`, and copy these messages to `dst` and `status`.
2.  Calculate the set difference `status - src`, and delete these messages from `dst` and `status`.

The full synchronization algorithm is then:

1.  `syncto(R, L, S)` (download changes)
2.  `syncto(L, R, S)` (upload changes)

# How it works

In the absence of crashes, the correctness proof only involves verifying that the status repository invariant (that messages in status have been synchronized in the past without an intervening synchronized delete) is preserved over all four operations, and that the set differences are, in fact, precisely the sets of messages we want to copy and delete. However, we can also try and look at how the local, remote and status repositories change as the algorithm progresses. In particular, the contents of the status repository in the first `syncto` is slightly surprising as it evolves differently from `local`, despite having the same operations applied to it (it then evolves in lockstep with `remote`).

![image](/img/offlineimap/normal.png)

Another important correctness claim is that OfflineIMAP never “loses mail”. Under what conditions is mail deleted? When it is present in status repository, but not in the local or remote repository. So it is easy to see that when the status repository is “lost” (either corrupted, or deleted as the instructions tell you to if you delete the contents of your local folders), OfflineIMAP will conservatively perform a full, no-delete synchronization between the two sources. So long as the status repository never contains data for more messages than it ought to, OfflineIMAP will not delete your mail.

# Variations

Suppose that I have more disk space available on local disk for Maildir than my remote IMAP server. Eventually, you will end up in the awkward position of wanting to delete messages from your remote IMAP server without correspondingly nuking them from your local mail store. OfflineIMAP provides the `maxage` option (in which OfflineIMAP refuses to acknowledge the existence of messages older than some sliding window), but what if we *really* wanted to be sure that OfflineIMAP would never ever delete messages from my local repository?

Simple: Skip step 1-2.

![image](/img/offlineimap/asymmetric.png)

# Conclusion

By utilizing a third repository, for which data loss results in a *conservative* action on the part of the program, OfflineIMAP achieves its claims of *an algorithm designed to prevent mail loss at all costs*. It is also a simple algorithm, and I hope that any computer scientist or software engineer using this software will take the time to convince themselves of its correctness, rather than relying on the hearsay of some marketing material.
