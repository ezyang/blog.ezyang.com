---
title: "Maildir synchronizing Sup"
date: 2012-12-01 18:33:38
slug: maildir-synchronizing-sup
categories: [Toolbox]
comments:
    - id: 5091
      author: Sagi
      date: "2012-12-02 09:29:15"
      content: |
        This is great, for me personally this opens up the possibility to try sup without disrupting my normal workflow too much, thank you! Can you perhaps provide some more information on your workflow, e.g. do you use multiple sup instances at home/work synced by this approach or is your use-case IMAP access on top of one sup instance?
        
        
        Regarding the offline-imap patch: are you planning on upstreaming your changes?
    - id: 5098
      author: Edward Z. Yang
      date: "2012-12-02 12:43:23"
      content: |
        Since keeping multiple sup instances synced was a problem heliotrope was addressing; I sidestep the issue by only having one sup instance; so if I don't have my laptop I use conventional access methods which are slower, of course. Probably the most annoying thing is I do a bit of filtering by Sup and so when my Sup instance isn't running I don't get any filters running, but this is not unsurmountable...
        
        Unfortunately, the OfflineIMAP patch defines a new file format on top of Maildir and thus has a low chance of getting upstreamed absent a sudden and mysterious uptake in adoption of this format.
    - id: 5100
      author: herpderp
      date: "2012-12-02 13:37:17"
      content: "Any thoughts on publishing your thoughts how to set up a reasonable X60 tablet configuration in recent Ubuntu? :)"
    - id: 5102
      author: Edward Z. Yang
      date: "2012-12-02 13:48:14"
      content: "Eh, well, as far as I'm concerned, it should just work :)"
    - id: 5269
      author: Supper
      date: "2012-12-08 16:41:53"
      content: |
        Hi,
        
        I'm a little confused about the maildir_labels config. Could you elaborate what exactly "standford" in your example stands for?
        
        Thanks!
    - id: 5270
      author: Supper
      date: "2012-12-08 16:44:11"
      content: |
        Arg, me again:
        
        I'd like to archive the following: When I have archived a message in sup ("a" key) it should show up in my archive maildir, so how would the maildir_labels config look like?
        
        :foo: [[:archive, 2]] 
        
        I guess, but's not working.
        
        Thanks again!
    - id: 5273
      author: Edward Z. Yang
      date: "2012-12-08 17:20:42"
      content: |
        The "stanford" in the example refers to the name of the account you're going to apply the moving to.  You can find out what labels your accounts have by looking in config.yaml
        
        You can't do ':archive' because it's not a real label--an archived message is defined by its lack of an inbox label. So the way I tend to do it is "archived messages" are 'null' (that is, if no other labels apply to them, in particular, inbox, then put the message there.)
    - id: 5274
      author: Supper
      date: "2012-12-08 17:41:35"
      content: "Thanks for your input! \"null\" works like a charm."
---

On the prompting of Steven Hum, I've put some finishing touches on my Sup patchset and am “releasing” it to the world (more on what I mean by “release” shortly.) The overall theme of this patchset is that it integrates as much Sup metadata it can with Maildir data. In particular:

- It merges Damien Leone’s sync-back patchset with the latest Sup mainline. The sync-back patchset synchronizes flags such as “Read” or “Trashed” to the Maildir, which can then be propagated back to your IMAP server using OfflineIMAP.
- Furthermore, this patchset has the ability to synchronize arbitrary labels, with a simple set of rules of what folder a message should be moved to depending on what labels it has. For example, inbox and archived messages can be kept in separate folders, so that non-Sup clients can usefully access mail you care about. (Trust me: this is really awesome.) This is coupled with a bonus OfflineIMAP patch which implements fast remote message moving.
- It implements inotify on Maildir, so a full directory scan is no longer necessary to retrieve new messages. The bottleneck for polling is now strictly OfflineIMAP.
- It implements the ability to save sent and draft messages to Maildir, so they show up in third-party clients.
- Finally, it has a number of miscellaneous bugfixes and extra hooks which I have personally found useful.

There is at least a high probability the patchset will work for you, since I’ve been using it actively for a while. Sup will sometimes crash; if it doesn't happen reproduceably or cause data loss, I probably won’t investigate too hard. Some of my patches are a bit sketchy (especially those labeled `HACK`: I’ve attempted to document all the skeevy bits in commit messages and code comments.) So, how supported is this version of Sup? Well:

1.  I am using this patchset, therefore, for all use-cases and environments I care about, it will stay working;
2.  I will probably not fix problems I am not affected by, and definitely not problems I cannot reproduce;
3.  I do not promise a stable commit history: I’ve rebased the patchset multiple times and will continue to do so.

Some of the early patches are pretty uncontroversial though, and I’d like to see them get into mainline eventually. You can get the code here: <http://gitorious.org/~ezyang/sup/ezyang/commits/maildir-sync/>

# New hooks

    sent-save-to
      Configures where to save sent mail to. If this hook doesn't exist,
      the global sent setting will be used (possibly defaulting to sup://sent)
      Variables:
          message: RMail::Message instance of the mail to send.
          account: Account instance matching the From address
      Return value:
           Source to save mail to, nil to use default

    compose-from
      Selects a default address for the From: header of a new message
      being composed.
      Variables:
        opts: a dictionary of ComposeMode options, including :from, :to,
          :cc, :bcc, :subject, :refs and :replytos
      Return value:
        A Person to be used as the default for the From: header

    draft-save-to
      Selects a source to save a draft to.
      Variables:
        from_email: the email part of the From: line, or nil if empty
      Return value:
        A source to save the draft to.

# Label synchronization

To use this functionality, in `config.yaml`, you need a new option `:maildir_labels`:

    :maildir_labels:
      :stanford: [[:inbox, 4], [null, 6]]

The value of this option is a dictionary of "accounts" to lists of precedences. (The account label `stanford` doesn’t actually mean anything; it's just for documentation.) Read it as follows:

> For messages belonging in source 4 or source 6 (consult `sources.yaml`), if the message has the `:inbox` tag, move it to source 4, otherwise move it to source 6.

This will automatically start working for any new mail you change the labels of. In order to apply this to old mail, you need to run `sup-sync-back-maildir`. If you're going to move a lot of mail, you probably want to run this version of OfflineIMAP: <https://github.com/ezyang/offlineimap>
