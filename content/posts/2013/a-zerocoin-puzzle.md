---
title: "A Zerocoin puzzle"
date: 2013-04-11 18:54:02
slug: a-zerocoin-puzzle
categories: [Cryptography, Math]
comments:
    - id: 6030
      author: Mihai Maruseac
      date: "2013-04-12 06:31:10"
      content: "This seems a little similar to the El Farol puzzle. http://en.wikipedia.org/wiki/El_Farol_Bar_problem"
    - id: 6055
      author: Wouter Schut
      date: "2013-04-24 06:44:30"
      content: |
        Given that there is already 1 coin in the pool: practically zero. You would still have plausible deniability because other users could time their withdrawal with your deposit.
        
        This is probably not the answer you where looking for :P
---

I very rarely post linkspam, but given that I’ve written on the subject of [anonymizing Bitcoins](http://blog.ezyang.com/2012/07/secure-multiparty-bitcoin-anonymization/) in the past, this link seems relevant: [Zerocoin: making Bitcoin anonymous](http://blog.cryptographyengineering.com/2013/04/zerocoin-making-bitcoin-anonymous.html). Their essential innovation is to have a *continuously operating* mixing pool built into the block chain itself; they pull this off using zero-knowledge proofs. Nifty!

Here is a puzzle for the readers of this blog. Suppose that I am a user who wants to anonymize some Bitcoins, and I am willing to wait expected time *N* before redeeming my Zerocoins. What is the correct probability distribution for me to pick my wait time from? Furthermore, suppose a population of Zerocoin participants, all of which are using this probability distribution. Furthermore, suppose that each participant has some utility function trading off anonymity and expected wait time (feel free to make assumptions that make the analysis easy). Is this population in Nash equilibrium?
