---
title: "Rafting away from the island of Paxos"
date: 2012-10-13 03:07:58
slug: 
draft: true
categories: [Miscellaneous]
---

XXX

# Paxos

For reference, I’ve replicated the explanation of Paxos from *Paxos Made Simple*:

> Phase 1. (a) A proposer selects a proposal number n and sends a prepare request with number n to a majority of acceptors.
>
> \(b\) If an acceptor receives a prepare request with number n greater than that of any prepare request to which it has already responded, then it responds to the request with a promise not to accept any more proposals numbered less than n and with the highest numbered proposal (if any) that it has accepted.
>
> Phase 2. (a) If a proposer receives a response to its prepare requests (numbered n) from a majority of acceptors, then it sends an accept request to each of the acceptors for a proposal numbered n with a value v, where v is the value of the highest numbered proposal among the responses, or is any value if the responses reported no proposals.
>
> \(b\) If an acceptor receives an accept request for a proposal numbered n, it accepts the proposal unless it has already responded to a prepare request having a number greater than n.

The standard invariants that are derived from this protocol are (that aren’t directly encoded in the algorithm):

- P1. An acceptor must accept the first proposal it receives.
- P2. If a proposal with value v is chosen, then every higher numbered proposal that is chosen has value v.
- P2a. If a proposal with value v is chosen, then every higher numbered proposal accepted by any acceptor has value v.
- P2b. If a proposal with value v is chosen, then every higher-numbered proposal issued by any proposer has value v.

# Raft

So how does Raft differ from Paxos? First, we have to reduce it to the single consensus case (since Raft is roughly equivalent to Multi Paxos, as opposed single instance Paxos.)
