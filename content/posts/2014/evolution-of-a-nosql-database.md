---
title: "You could have invented Spanner!"
date: 2014-05-28 18:10:32
slug: evolution-of-a-nosql-database
draft: true
categories: [Miscellaneous]
---

...it just might have taken you a long time.

Simple NoSQL: key to string Added timestamp: versioned database Added directories: DISTRIBUTION problem, control placement (geographic replication DSL) Added fragments: directories sometimes are big

Not really NoSQL: semi-relational tables (require primary key), query language, transactions. Hierarchichal database for locality (key interleave).

Directory: Bag of key/timestamp to string mappings, where keys are contiguous and have common prefix Tablet: Bag of directories, lock table and a Paxos instance. Server: Spanserver containing 100-1000 tablets Datacenter (single administrative deployment; possibly unit of physical isolation, so multiple zones in datacenter): Zone containing zonemaster, a few location proxies and 100-1000 spanmasters Multiple datacenters over many geographic locations: Universe containing multiple zones. Singleton universe master and placement driver.

Paxos groups span across datacenters (i.e. zones), I guess. Leader of this is called "participant leader" Additionally, one of these Paxos groups is special (transaction manager),

What is meant by prefix? Directories seem to be disjoint. GOOD IDEA: keep frequently accessed together directories in same Paxos group (avoid multi-group transaction)

Does movedir need to lock the transaction manager?

How are the Paxos groups organized?

Pipelined Paxos: allow initiating new instances of ordering protocol before previous one has completed.

TrueTime: TT.after(t) = t \< TT.now().earliest; TT.before(t) = TT.now().latest \< t

What kind of atomic clock did Google use? Seems to cost on order of \$1k

Concurrency control

What is a "snapshot read"? Snapshot relaxes conflict detection.

Confusing: "commit is inevitable" (what is commit?) "by repeating...the current read position" (what is the current read position?)
