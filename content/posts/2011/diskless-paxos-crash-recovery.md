---
title: "Diskless Paxos crash recovery"
date: 2011-08-14 00:41:30
slug: diskless-paxos-crash-recovery
categories: [Distributed Systems]
comments:
    - id: 2902
      author: Gregory Collins
      date: "2011-08-14 04:51:40"
      content: |
        &gt; we have other convenient sources of monotonic numbers: synchronized clocks (which are useful for Paxos in other contexts)
        
        Is this necessarily true, Edward? I'm thinking of instances like "negative leap seconds" and clock skew. I know proposed fixes for these problems exist (e.g. http://www.ucolick.org/~sla/leapsecs/torino/kuhn.pdf) but I'm not sure how widespread they are, and in practice implementors spend a lot more time worrying this issue then they would like to.
    - id: 2903
      author: Edward Z. Yang
      date: "2011-08-14 10:00:42"
      content: "Ooh, that's nasty. You might be able to solve the clock skew problem by refusing to start up if you haven't synchronized your network time, but I don't know what to do about negative leap seconds..."
    - id: 3599
      author: Xiaodong Huang
      date: "2012-03-28 23:53:43"
      content: |
        Hey, Edward. I'm afraid that your implementation is still problematic.
        
        The recovered acceptor can't remember the maximum proposal number it had ever responded before its crash.
        
        As you figured out.
        " P1b. An acceptor can accept proposal numbered (e, n) iff e* ≤ e and it has not responded to a prepare request (_, n') with n' &gt; n."
        
        P1b is correct, but the problem is: how can a recovered acceptor know whether "it has not responded to a prepare request (_, n') with n' &gt; n"? 
        
        Am I right? Anyway, I've fixed it by introducing a lease for proposer, and I'm writing my thesis on it.
    - id: 3600
      author: Xiaodong Huang
      date: "2012-03-29 05:11:23"
      content: |
        Well, forget the lease, it's incorrect.
        
        Actually, the maximum responded proposal number can be determined by collecting messages from a good quorum. Each of the messages must contain the sender's maximum local reponded proposal number and have an epoch vector greater than the recovered acceptor's. It is easy to prove that the recovered acceptor can't have responded to a higher proposal number before its crash.
    - id: 3601
      author: Edward Z. Yang
      date: "2012-03-29 12:14:18"
      content: |
        I'm not sure I see the problem. If e* ≤ e is not true, then the acceptor may *not* accept a message with epoch e. So it doesn't need to know what's going on with those prepare requests. If the condition is true, you apply the KNOWN-UNKNOWNS lemma to show that the acceptor does know whether or not the latter condition is true.
        
        Certainly requiring a quorum to determine the epoch vector doesn't hurt, but in fact this quorum is already established by the leader, so it is strictly unnecessary.
    - id: 3602
      author: Xiaodong Huang
      date: "2012-03-30 00:28:49"
      content: |
        Ok, let me show it in a concrete run.
        Consider a three-node system, in which the nodes are P1, P2, P3.
        
        P2 sent a prepare request ((0,0,0), 2) to P3.
        Since P3 had never received a prepare request before, it responded the request. Thus, P2 collected responses from a quorum (P2, P3) for proposal number 2. 
        P2 and P3 continued to finish the accept phase, and finally decided a value V2 without the participation of P1. 
        Then, P3 crashed. After recovery, P3 set its vector to (0,0,1).
        
        P1 had no idea about what's happened above, so it just sent a prepare request ((0,0,0), 1) to the recovered P3.
        P1 was then rejected by P3, P3 would then synchronize its vector to (0,0,1) and resend the prepare quest ((0,0,1), 1) to P3. This time, P1 got responses from P3. Then, P1 and P3 continued to finish the accept phase, and decided a new value V1.
        
        Thus this process did not violate KNOW-UNKNOWNS, but has decided two different values, V1 and V2.
    - id: 3603
      author: Xiaodong Huang
      date: "2012-03-30 00:37:36"
      content: |
        "P1 was then rejected by P3, P3 would then synchronize its vector to (0,0,1) and .."
        Sorry, here should be "P1 would then ...", not P3.
        
        As long as more than one leader possibly exists in the system simultaneously, the recovered node must retrieve the maximum proposal number.
        
        Since strong leader election is itself a consensus problem. If you can guaranteen the uniqueness of leader, the consensus problem has already been solved.
    - id: 3604
      author: Edward Z. Yang
      date: "2012-03-30 01:05:18"
      content: |
        The problem with your example is that you have lost your majority of nodes who know some fact, which means the behavior is undefined (effectively, nodes 1 and 3 have crashed, in your example.) It's well known that vanilla Paxos cannot recover from such an event, and you need to pull some fancy tricks to manage it. (In my counterexample for the Paxos made Live paper, I only need one node who did not participate in the quorum to die.)
        
        I do agree that it is poor that safety is violated in this case (especially since in general it's hard how well distributed information is in the network), and it might make sense to guarantee a stronger guarantee but I am not at all convinced you can use this proof structure. If everyone except one person dies, they have to find that person, but if that person is behind a network partition this case is indistinguishable from a reboot.
    - id: 3605
      author: Xiaodong Huang
      date: "2012-03-30 03:46:34"
      content: |
        &gt;&gt;" The problem with your example is that you have lost your majority of nodes who know some fact, which means the behavior is undefined (effectively, nodes 1 and 3 have crashed, in your example.) "
        
        I don't think the behavior in my example is undefined. Although P1 is a lagging node at the beginning, you've got at least a quorum of nodes (P1 and P2) that have never crashed during this run, which is sufficient to achieve consensus. 
        
        What I want to say is, P1b is correct as well as your proof based on P1b. If you can ensure P1b, the protocol is without problem.
         
        The problem lies in your KNOWN-UNKNOWNS lemma, i.e. it's insufficient to ensure P1b because P1b requires the acceptor to check its local proposal number. Following KNOWN-UNKOWNS will still lead to violating P1b.
        
        &gt;&gt;P1b. An acceptor can accept proposal numbered (e, n) iff e* ≤ e and it has not responded to a prepare request (_, n') with n' &gt; n.
        
        In my example above, the safety is violated because P3 violates P1b. 
        When P3 received ((0,0,1), 1)， the condition "e* ≤ e" holds(i.e., (0,0,1) ≤ (0,0,1)), but the condintion "it has not responded to a prepare request (_, n') with n' &gt; n." does not hold( i.e. P3 has already responded a n' = 2, and n' is greater than the incoming proposal number n = 1.
    - id: 3606
      author: Edward Z. Yang
      date: "2012-03-30 04:04:23"
      content: |
        I think you've misdiagnosed why your example fails, which can be illustrated in a simple example without crash recovery. Consider the same setup, where 1 is a lagging node. However, nodes 2 and 3 only extracts a "prepare" from 3; the "accept" message is lost. Then 1 attempts to negotiate with 3; 3 will reply that it the proposal number is invalid (just like in the epoch case)--however, critically, it will also reply with the value that node 2 originally asked it to prepare (a value which, unbeknownst to 3, has been "chosen", although only node 2 knows about it), so 1 may in fact do redundant work, but for the "right" value.
        
        Obviously, this invariant fails if 2 has crashed in the interim; it doesn't "know" what the right value to reply with is. But if it is not the case that a majority of the nodes "know" what the right value is, it's always possible for 1 to get unlucky with the quorum it has mustered.
        
        I would like to restate that I *agree* that this behavior is suboptimal. But I think there are good reasons for it. In particular, suppose I have a 1000 nodes, and 1 node "knows all", but 999 of the nodes crash and come back alive. They somehow need to know to talk to the 1 surviving node; this requires far more than a majority poll.
    - id: 3607
      author: Xiaodong Huang
      date: "2012-03-30 05:03:15"
      content: |
        Actually you can't avoid comparing the proposal number if you want to ensure P1b. 
        The epoch vector condition and the proposal number condition are independent. In my example above, it's still possible to reserve safety by ensuring P1b in a correct way.
        
        We have to help P3 figure out its maximum reponded proposal number. Let this number be MRPN. To ensure P1b, actually you don't have to figure out the accurate MRPN, instead, finding a  number no smaller than MRPN is enough.
        
        So, how can we get such a proposal number?
        
        First, we use a weak leader election (or a pre-prepare phase actually).
        A leader is allowed to send its Prepare and Accept requests only if it has sent its proposal number to and get reponses from a quorum of correct nodes ( similar with the prepare phase, the responded nodes keep promises). 
        Note that a node is correct if it has never crashed or has completely recovered from a previous crash. Note that correct node is possibly a lagging node( e.g. P1).
        
        Second, P3 can find a number not smaller than its MRPN by collecting messages from a quorum of correct nodes.
        The collected messages contain senders' MRPN, and P3 will pick the maximum one among these numbers. But you need to ensure that the collected messages are sent after the crash of P3, and the epoch vector will help this.
        
        Given the leader election above, it's easy to prove that the picked number is no smaller than P3's MRPN: the message sender quorum and the supporter quorum of the latest leader have at least one common member.
        
        Unless P3 has established a proposal number according to protocol above, it is not allowed to respond in the system. 
        Once establishing the proposal number, P3 manages to compare the proposal number to ensure P1b. 
        Furthermore, once P3 knows the proposed value for the established proposal number, it has finished the complete recovery, and is correct again because it has been updated to a state newer than its state before crash (newer because proposal number and proposed value are newer or at least the same, and the epoch vector has been advanced).
        
        The protocol above must statisfy two requirements to ensure correctness.
        1. There must be always a quorum of correct nodes in the system.
        2. The recovered node has to know itself is a crashed and recovering node, so it will not participate in the protocol (namely, becomes a correct node again) before it completes the full recovery procedure.
    - id: 3609
      author: Xiaodong Huang
      date: "2012-03-30 05:21:15"
      content: |
        &gt;&gt;I would like to restate that I *agree* that this behavior is suboptimal. But I think there are good reasons for it. In particular, suppose I have a 1000 nodes, and 1 node “knows all”, but 999 of the nodes crash and come back alive. They somehow need to know to talk to the 1 surviving node; this requires far more than a majority poll.
        
        This counter-example may not be appropriate. If 999 out of 1000 nodes crash, consensus is impossible to be achieved according to the conclusion given in "Failure Detection and Consensus in the Crash-Recovery Model". 
        
        But in my example, 2 out of 3 nodes have never crashed, the consensus can be easily achieved. One second thought, if your 3-node system can't tolerate even a single failure (P3), it's obviously useless, isn't it?
    - id: 3610
      author: Xiaodong Huang
      date: "2012-03-30 05:54:15"
      content: |
        Let's talk about the solution in the "Paxos Made Live" paper.
        
        The original description in the paper is  as follows:
        &gt;&gt; A replica with a corrupted disk rebuilds its state as follows. It participates in Paxos as a non-voting member; meaning that it uses the catch-up mechanism to catch up but does not respond with promise or acknowledgment messages. It remains in this state until it observes one complete instance of Paxos that was started after the replica started rebuilding its state. By waiting for the extra instance of Paxos, we ensure that this replica could not have reneged on an earlier promise.
        
        Note the sentence: &gt;&gt; "It remains in this state until it observes one complete instance of Paxos that was started after the replica started rebuilding its state. "
        
        This solution is correct literally. The recovering node just ignores any consensus instance which may has happened before its crash, and only choose to participate in consensus instances it has never participated before. This solution is actually stronger than my solution above.
        
        But the paper did not spell out how to check an instance was started after the crashed replica started recovery. 
        Because in an asynchonous network without synchronized clocks, the message the recovering node received is possibly sent before its crash.
        
        That's what epoch vector can help. The epoch vector is a type of vector clock mentioned in some other papers. It's a extended logical clock to figure out the casual order between messages or events. For example, here it's used to figure out the order of node-crash event and protocol messages.
        
        In practice, real-world networks are not asynchronous. By waiting for a sufficient long time, the recovering node can actually ensure that the messages received are sent after its crash, so it may be not practical to use epoch-vector for this reason.
    - id: 3611
      author: Edward Z. Yang
      date: "2012-03-30 14:50:00"
      content: |
        Just to confirm: we are in agreement that the protocol described in this blog post is correct so long as some hard to state condition about a majority of nodes knowing some fact is preserved. (The condition is hard to state! Definitely a black mark.) You are arguing that there exists a simple extension to this protocol which is correct on a simpler condition (that is, there is always a quorum of "correct" nodes at any time), and I am arguing that this problem is hard.
        
        I'm going to comment on your second comment first. We don't even need a majority of the nodes to crash in order to get into a pickle. Suppose we have 99 nodes, 50 of the nodes achieve quorum, and then 49 of those nodes crash. When they come back, only 1 node knows what the true value was, but less than half of the nodes crashed.
        
        Jumping to your first comment, I think you would still not worry about this in your modified scheme, because "get a quorum of correct" rules out any of the other 48 as a valid participant in the recovery protocol.
        
        I'd like you to flesh out this argument in more detail. In your scheme, you require the leader to somehow know whether or not a node is correct or not. I believe that as described, a node knows that it is "correct" when it has concluded the recovery protocol, which tells it an upper bound on a proposal number it may have responded to. You claim that it is sufficient to poll a majority of correct nodes to determine this proposal number. But this is not sufficient: our node may have made promises with the crashed nodes that are higher than any of the correct node promises. The obvious thing to do is modify the invariant: you need a lemma saying that these promises don't matter (since both participants have "forgotten".) Furthermore, consider the case in a five-node instance where P1 makes a high-numbered promise to P5, crashes, and then polls P2, P3 and P4 for information. However, P5 was behind a network partition and never extracted promises from these nodes; so P1 doesn't find out about its promise to P5. So you will need another lemma stating that this case doesn't matter either. In the face of lagging messages, achieving this may be tricky.
        
        This is in fact the counterexample I gave for the original algorithm. Consider a 5 node cluster, where P1 and P2 are separated by a network partition from P3, P4 and P5. P1 has promised P2 not to accept ballots lower than #1000, but then crashes, and comes back in the P3, P4, P5 partition. Now P1 knows nothing about its promise, and it does the recovery protocol with P3, P4 and P5, and starts accepting ballots lower than what it promised P2. Things could get bad quick if P2's accept messages hits P3 after the recovery protocol, both pairs P4-P5 and P2-P3 could think that P1 is part of their quorum.
    - id: 3612
      author: Edward Z. Yang
      date: "2012-03-30 14:59:40"
      content: "I also want to remark that your solution is better is some sense, in that it halts progress if we lose quorum of correct nodes."
    - id: 3613
      author: Edward Z. Yang
      date: "2012-03-30 23:01:20"
      content: |
        I chatted with someone about this issue, and we found a counterexample for your protocol (if we understood how it worked properly, of course!)
        
        The actors in our play are L, A1, A2, B1 and X. We start off with L, B1 and X doing a Paxos instance on ballot 1 with value v1, but the accept message to B1 is delayed. Then X crashes, and polls A1, A2 and B1 for its max proposal number (according to B1, it is 1); then A1, A2 and X run a Paxos instance on ballot 2 with value v2. In the meantime, B1 receives the delayed accept message, and sends the accept-ok, and L assumes that v1 was chosen. However, A1, A2 and X can run a protocol, and furthermore they can be using a different value of v2.
        
        I've also discovered the correct formulation for the safety condition in my original protocol: the condition is that once a value has been globally chosen (a majority of nodes have received the accept message), a majority of nodes must continue to know about the value.
    - id: 3616
      author: Xiaodong Huang
      date: "2012-03-31 03:36:14"
      content: |
        I'm working with the complete proof, it takes time, and I'll show you later. 
        
        I'd like to first show that your counter-example for my protocol is incorrect.
        Because the delayed accept message can be checked with epoch vector. After X recovered and polled to B1, B1 had updated its epoch vector, and would ignore the delayed message.
    - id: 3617
      author: Xiaodong Huang
      date: "2012-03-31 03:45:20"
      content: |
        You have figured out your safety condition correctly. But how can you ensure this condition in a real system? 
        
        Monitoring the number of correct nodes is easy, but monitoring the number of correct nodes which have kept the chosen value is not trivial. Moreover, this condition is too strict for a practical system.
    - id: 3688
      author: Marton Trencseni
      date: "2012-04-24 10:28:40"
      content: "I'm wondering whether you're familiar with PaxosLease, it's a Paxos variation for leases (Google it). Also, our ScalienDB product has an implementation of both Paxos and PaxosLease, check out https://github.com/scalien/scaliendb/tree/master/src/Framework/Replication"
    - id: 3691
      author: Edward Z. Yang
      date: "2012-04-25 22:54:24"
      content: "Only peripherally. My understanding is that PaxosLease is a bit simpler in some ways, which makes recovery easier."
    - id: 3692
      author: Xiaodong Huang
      date: "2012-04-26 04:46:08"
      content: |
        @Marton Trencseni
        
        In fact, PaxosLease does not solve the same problem.
        
        PaxosLease works because it's not a  typical state machine replication setting in which chosen values of consensus instances are update operations applied to state machines. In PaxosLease, the chosen values are the states directly!  
        
        So your can simply forget any previous consensus instance as long as you have attended in a latest instance and get a new state!
        
        By waiting for a maximal lease, the acceptor can guarantee that the global state has been changed, so it can simply forget any previous promise.
        
        But in typical state machine replication, to reach the latest state, you have to remember the results of all consensus instances, which the diskless acceptors in PaxosLease can't help.
    - id: 13766
      author: David Turner
      date: "2015-05-01 08:25:11"
      content: |
        Hi Edward,
        
        Sorry for resurrecting an ancient thread, but I've been thinking about this recently.
        
        In my understanding, the safety proof for Paxos seems to rely on the proposal numbers being totally ordered. I've tried to see if I can remove this restriction but have so far failed, although I've also failed to find a counterexample.
        
        Are you sure the proof still holds even if the proposal numbers are only partially ordered as in your article above? Do you have an intuition for why this might be so?
        
        Many thanks,
        
        David
---

*This is an edited version of an email I sent last week. Unfortunately, it does require you to be familiar with the original Paxos correctness proof, so I haven’t even tried to expand it into something appropriate for a lay audience. The algorithm is probably too simple to be in the literature, except maybe informally mentioned—however, if it is wrong, I would love to know, since real code depends on it.*

I would like to describe an algorithm for [Paxos](http://en.wikipedia.org/wiki/Paxos_algorithm) crash-recovery that does not require persistent storage, by utilizing synchronized clocks and a lattice-based epoch numbering. The basic idea is to increase the ballot/proposal number to one for which it is impossible for the crashed node to have made any promises for it. Such an algorithm, as noted in [Paxos made Live](http://labs.google.com/papers/paxos_made_live.html), is useful in the case of disk corruption, where persistent storage is lost. (Unfortunately, the algorithm they describe in the paper for recovering from this situation is incorrect. The reason is left as an exercise for the reader.) It is inspired by [Renesse's](http://www.cs.cornell.edu/home/rvr/) remark about an "epoch-based system", and the epoch-based crash-recovery algorithm described in [JPaxos: State Machine Replication Based on the Paxos Protocol](http://infoscience.epfl.ch/record/167765). However, in correspondence with [Nuno](http://personnes.epfl.ch/nuno.santos), I discovered that proofs for the correctness of their algorithm had not been published, so I took it upon myself to convince myself of its correctness, and in the process discovered a simpler version. It may be the case that this algorithm is already in the community folklore, in which case all the better, since my primary interest is implementation.

First, let's extend proposal numbers from a single, namespaced value n to a tuple `(e, n)`, where `n` is a namespaced proposal number as before, and `e` is an epoch vector, with length equal to the number of nodes in the Paxos cluster, and the usual Cartesian product lattice structure imposed upon it.

Let's establish what behavior we'd like from a node during a crash:

**KNOWN-UNKNOWNS.** An acceptor knows a value `e*`, for which for all e where `e* ≤ e` (using lattice ordering), the acceptor knows if it has responded to prepare requests of form `(e, n)` (for all `n`).

That is to say, the acceptor knows what set of proposal numbers he is guaranteed not to have made any promises for.

How can we establish this invariant? We might write a value to persistent storage, and then incrementing it upon a crash; this behavior is then established by monotonicity. It turns out we have other convenient sources of monotonic numbers: synchronized clocks (which are useful for Paxos in other contexts) have this behavior. So instead of using a vector of integers, we use a vector of timestamps. Upon a crash, a process sets its epoch to be the zero vector, except for its own entry, which is set to his current timestamp.

In [Paxos made Simple](http://academic.research.microsoft.com/Publication/12945610/paxos-made-simple), Lamport presents the following invariant on the operation of acceptors:

**P1a.** An acceptor can accept proposal numbered `n` iff it has not responded to a prepare request greater than `n`.

We can modify this invariant to the following:

**P1b.** An acceptor can accept proposal numbered `(e, n)` iff `e* ≤ e` and it has not responded to a prepare request `(_, n')` with `n' > n`.

Notice that this invariant "strengthens" **P1a** in the sense that an acceptor accepts a proposal in strictly less cases (namely, it refuses proposals when `e* ≰ e`). Thus, safety is preserved, but progress is now suspect.

When establishing progress of Paxos, we require that there exist a stable leader, and that this leader eventually pick a proposal number that is "high enough". So the question is, can the leader eventually pick a proposal number that is "high enough"? Yes, define this number to be `(lub{e}, max{n} + 1)`. Does this epoch violate **KNOWN-UNKNOWNS**? No, as a zero vector with a single later timestamp for that node is always incomparable with any epoch the existing system may have converged upon.

Thus, the modifications to the Paxos algorithm are as follows:

- Extend ballot numbers to include epoch numbers;
- On initial startup, set `e*` to be the zero vector, with the current timestamp in this node's entry;
- Additionally reject accept requests whose epoch numbers are not greater-than or equal to `e*`;
- When selecting a new proposal number to propose, take the least upper bound of all epoch numbers.

An optimization is on non-crash start, initialize `e*` to be just the zero vector; this eliminates the need to establish an epoch in the first round of prepare requests. Cloning state from a snapshot is an orthogonal problem, and can be addressed using the same mechanisms that fix lagging replicas. We recommend also implementing the optimization in which a leader only send accept messages to a known good quorum, so a recovered node does not immediately force a view change.

I would be remiss if I did not mention some prior work in this area. In particular, in [Failure Detection and Consensus in the Crash-Recovery Model](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.17.5958), the authors present a remarkable algorithm that, without stable storage, can handle more than the majority of nodes failing simultaneously (under some conditions, which you can find in the paper). Unfortunately, their solution is dramatically more complicated than solution I have described above, and I do not know of any implementations of it. Additionally, an alternate mechanism for handling crashed nodes with no memory is a group membership mechanism. However, group membership is notoriously subtle to implement correctly.
