---
title: "Calculating Shanten in Mahjong"
date: 2014-04-01 04:20:46
slug: calculating-shanten-in-mahjong
categories: [Computer Science, Frivolity]
math: true
comments:
    - id: 6577
      author: Alain
      date: "2014-04-01 07:36:35"
      content: |
        You really fooled me.
        I'm a faithfull follower or your blog, so I hoped for some categery theoretic formalization...
        Well done !
    - id: 6579
      author: yachris
      date: "2014-04-05 09:40:49"
      content: |
        Great writeup!  Good luck with your thesis.
        
        Two minor things -- first, in the "...choices of new tiles times choices of discard", shouldn't it be 14 for discards?  I presume you can discard the tile you drew.  If not, please ignore this :-)
        
        Second, "the various suits are no longer symmetric, so we have to do three times as much work" -- shouldn't this be four times as much work?
    - id: 6580
      author: Edward Z. Yang
      date: "2014-04-05 16:55:46"
      content: "(yachris: Since it may not be so clear post-April 1st, the parts about my thesis being about this are April Fools. The rest of the content is true.) On the first comment, you are right. On the second comment, there are actually only three suits in Mahjong, as opposed to four in poker."
    - id: 6582
      author: Anonymous
      date: "2014-04-08 12:05:55"
      content: "on my win7, IE and chrome are not showing the formula correctly, in particular chrome."
    - id: 10537
      author: "Useful resources, articles and open source | Mahjong Riichi in code"
      date: "2014-11-28 17:03:52"
      content: "[&#8230;] Calculating Shanten in Mahjong [&#8230;]"
    - id: 11182
      author: Tak Suyu
      date: "2014-12-16 02:30:55"
      content: |
        Though this might not be your thesis I noticed an interesting trend since I've been working on this problem for awhile.
        
        You'll always be &lt;= 7 shanten with any hand you are given (Chiitoitsu is great).
        
        So if we were to judge it by pairs.
        at 0 pairs you can have 1-7 shanten
        at 1, 0-6
        at 2, 1-5
        at 3, 2-4
        at 4, 3
        at 5, 2
        at 6, 1
        at 7, you win :3
        
        Then just take the max shanten for your pairs and subtract one shanten for each run and set and there is your magical shanten number.
    - id: 11183
      author: Tak Suyu
      date: "2014-12-16 02:37:10"
      content: |
        Sorry noticed a little flaw in my logic, for every pair you remove you have to change which range you are working with.
        
        So if you have a pair and then use one of them to make a set you have -1 pair when calculating your shanten.
    - id: 22253
      author: Assemble final shanten of different suits
      date: "2018-02-23 21:14:18"
      content: "Good job on the lookup table method for calculating the shanten of one suit.  About the  the assemblage of shanten of multiple suits, I have problems  following how the combination works.  Suppose my hand is 12466m66789p235s4s, what do you mean by \"try every distribution of triples and the pairs over the four types of tiles\"? Does it mean that we should try 66m, 66p, 234s and 66m, 678p, 345s and 66m, 789p, 234s,etc?   If that is true, how do we then \"consult the shanten of the requested shape\"?  Thanks."
    - id: 22254
      author: Edward Z. Yang
      date: "2018-02-23 21:30:15"
      content: "The idea is that you have already computed a lookup table which can tell you, given something like 235s, how far from an actual triple like 234s you are. So you do have a lot of splits to test (140), but it's constant time to compute its distance."
    - id: 22257
      author: George G. Hu
      date: "2018-02-24 20:03:16"
      content: |
        Thanks for you quick reply. In my last comment, I mistook the name for the title. In this follow-up comment, I have attached my name here. Recently, I am working on a pilot AI Chinese mahjong project and therefore are very interested in the so-called tile efficiency. Since Chinese materials on the shanten calculation is very limited, I am very glad to find your post here.
        
        I still have two comments:
        
        (1)	Since a winning configuration requires 14 tiles, whereas table A can contain only up to 13 tiles, therefore only tenpai configurations in A can be initialized with a zero of shanten. This is also the widely used definition of shanten. If we consider configurations that are tenpai and contain tiles less than 13, these configurations must contain 1 or 4 or 7 or 10 tiles. Configurations containing other number of tiles cannot be exchanged to be a tenpai and therefore their shanten cannot be computed.  In the breadth-first search, the adjacent node has the same number of tiles as the current node. Therefore, we have to initialize all tenpai configurations of different number of tiles. 
        
        (2)	For the assemblage of the shanten of multiple suits, suppose my hand is 112345p1234567s. If we get splits by each suit, then we have 112345 for pin tiles and 1234567 for sou tiles. Because the number of pin tiles is 6, the shanten cannot be computed. Therefore, this interpretation of the way of splits should be considered to be wrong. If we get splits among all 13 tiles, we may get one possible splits such as 11p, 234p, 5p, 123s, 456s, 7s. In this case, how should we assemble the final shanten?
        
        Thanks!
    - id: 22258
      author: George G. Hu
      date: "2018-02-24 21:53:13"
      content: |
        BTW, I went over the Stars and Bars method and get more comfortable with the formula listed in the blog. However, about the formula,  I am not sure about what the three 4's stand for. It should be clear that one 4 stand for four suits of tiles (including honor tiles).  What is the first 4 in the formula for?  In addition, a hand usually may contain 4 triples (just a collection of any three tiles, not necessarily series or identicals) and one single tile or 2 pairs (collection of any two tiles), 3 triples. In this case, shouldn't we distribute 5 such groups into 4 types of tiles ? 
        
        Your blog mentions that  the suits are symmetric. Maybe, I missed something. If so, please correct me.
        
        Thanks.
---

Move aside, poker! While the probabilities of various poker hands are well understood and tabulated, the Chinese game of chance [Mahjong](http://en.wikipedia.org/wiki/Mahjong) \[1\] enjoys a far more intricate structure of expected values and probabilities. \[2\] This is largely due in part to the much larger variety of tiles available (136 tiles, as opposed to the standard playing card deck size of 52), as well as the turn-by-turn game play, which means there is quite a lot of strategy involved with what is ostensibly a game of chance. In fact, the subject is so intricate, I’ve decided to write my PhD thesis on it. This blog post is a condensed version of one chapter of my thesis, considering the calculation of *shanten*, which we will define below. I’ll be using Japanese terms, since my favorite variant of mahjong is Riichi Mahjong; you can consult the [Wikipedia article](http://en.wikipedia.org/wiki/Japanese_Mahjong) on the subject if you need to translate.

# Calculating Shanten

The basic gameplay of Mahjong involves drawing a tile into a hand of thirteen tiles, and then discarding another tile. The goal is to form a hand of fourteen tiles (that is, after drawing, but before discarding a tile) which is a winning configuration. There are a number of different winning configurations, but most winning configurations share a similar pattern: the fourteen tiles must be grouped into four triples and a single pair. Triples are either three of the same tile, or three tiles in a sequence (there are three “suits” which can be used to form sequences); the pair is two of the same tiles. Here is an example:

<div class="container mahjong">

![image](http://upload.wikimedia.org/wikipedia/commons/1/1c/MJw1.png)

![image](http://upload.wikimedia.org/wikipedia/commons/c/c3/MJw2.png)

![image](http://upload.wikimedia.org/wikipedia/commons/9/9e/MJw3.png)

![image](http://upload.wikimedia.org/wikipedia/commons/e/ed/MJw5.png)

![image](http://upload.wikimedia.org/wikipedia/commons/e/ed/MJw5.png)

![image](http://upload.wikimedia.org/wikipedia/commons/6/61/MJt2.png)

![image](http://upload.wikimedia.org/wikipedia/commons/a/a4/MJt3.png)

![image](http://upload.wikimedia.org/wikipedia/commons/7/72/MJt4.png)

![image](http://upload.wikimedia.org/wikipedia/commons/6/6f/MJt7.png)

![image](http://upload.wikimedia.org/wikipedia/commons/1/18/MJt8.png)

![image](http://upload.wikimedia.org/wikipedia/commons/a/a1/MJt9.png)

![image](http://upload.wikimedia.org/wikipedia/commons/d/df/MJs4.png)

![image](http://upload.wikimedia.org/wikipedia/commons/b/bf/MJs5.png)

![image](http://upload.wikimedia.org/wikipedia/commons/c/cb/MJs6.png)

</div>

Represented numerically, this hand consists of the triples and pairs 123 55 234 789 456.

One interesting quantity that is useful to calculate given a mahjong hand is the *shanten* number, that is, the number of tiles away from winning you are. This can be used to give you the most crude heuristic of how to play: discard tiles that get you closer to tenpai. The most widely known shanten calculator is [this one on Tenhou’s website](http://tenhou.net/2/) \[3\]; unfortunately, the source code for this calculator is not available. There is [another StackOverflow question](http://stackoverflow.com/questions/4239028/how-do-i-calculate-the-shanten-number-in-mahjong) on the subject, but the “best” answer offers only a heuristic approach with no proof of correctness! Can we do better?

Naïvely, the shanten number is a breadth first search on the permutations of a hand. When a winning hand is found, the algorithm terminates and indicates the depth the search had gotten to. Such an algorithm is obviously correct; unfortunately, with 136 tiles, one would have to traverse \$((136-13)\times 14)^n\$ hands (choices of new tiles times choices of discard) while searching for a winning hand that is n-shanten away. If you are four tiles away, you will have to traverse over six trillion hands. We can reduce this number by avoiding redundant work if we memoize the shanten associated with hands: however, the total number of possible hands is roughly \$136 \choose 13\$, or 59 bits. Though we can fit (via a [combinatorial number system](http://en.wikipedia.org/wiki/Combinatorial_number_system)) a hand into a 64-bit integer, the resulting table is still far too large to hope to fit in memory.

The trick is to observe that shanten calculation for each of the suits is symmetric; thus, we can dynamic program over a much smaller space of the tiles 1 through 9 for some generic suit, and then reuse these results when assembling the final calculation. \$9 \times 4 \choose 13\$ is still rather large, so we can take advantage of the fact that because there are four copies of each tile, an equivalent representation is a 9-vector of the numbers zero to four, with the constraint that the sum of these numbers is 13. Even without the constraint, the count \$5^9\$ is only two million, which is quite tractable. At a byte per entry, that’s 2MB of memory; less than your browser is using to view this webpage. (In fact, we want the constraint to actually be that the sum is less than or equal to 13, since not all hands are single-suited, so the number of tiles in a hand is less.

The breadth-first search for solving a single suit proceeds as follows:

1.  Initialize a table A indexed by tile configuration (a 9-vector of 0..4).
2.  Initialize a todo queue Q of tile configurations.
3.  Initialize all winning configurations in table A with shanten zero (this can be done by enumeration), recording these configurations in Q.
4.  While the todo queue Q is not empty, pop the front element, mark the shanten of all adjacent uninitialized nodes as one greater than that node, and push those nodes onto the todo queue.

With this information in hand, we can assemble the overall shanten of a hand. It suffices to try every distribution of triples and the pairs over the four types of tiles (also including null tiles), consulting the shanten of the requested shape, and return the minimum of all these configurations. There are \$4 \times {4 + 4 - 1 \choose 4}\$ (by [stars and bars](http://en.wikipedia.org/wiki/Stars_and_bars_(combinatorics))) combinations, for a total of 140 configurations. Computing the shanten of each configuration is a constant time operation into the lookup table generated by the per-suit calculation. A true shanten calculator must also accomodate the rare other hands which do not follow this configuration, but these winning configurations are usually highly constrained, and quite easily to (separately) compute the shanten of.

With a shanten calculator, there are a number of other quantities which can be calculated. *Uke-ire* refers to the number of possible draws which can reduce the shanten of your hand: one strives for high uke-ire because it means that probability that you will draw a tile which moves your hand closer to winning. Given a hand, it's very easy to calculate its uke-ire: just look at all adjacent hands and count the number of hands which have lower shanten.

# Further extensions

Suppose that you are trying to design an AI which can play Mahjong. Would the above shanten calculator provide a good evaluation metric for your hand? Not really: it has a major drawback, in that it does not consider the fact that some tiles are simply unavailable (they were discarded). For example, if all four “nine stick” tiles are visible on the table, then no hand configuration containing a nine stick is actually reachable. Adjusting for this situation is actually quite difficult, for two reasons: first, we can no longer precompute a shanten table, since we need to adjust at runtime what the reachability metric is; second, the various suits are no longer symmetric, so we have to do three times as much work. (We can avoid an exponential blowup, however, since there is no inter-suit interaction.)

Another downside of the shanten and uke-ire metrics is that they are not direct measures of “tile efficiency”: that is, they do not directly dictate a strategy for discards which minimizes the expected time before you get a winning hand. Consider, for example, a situation where you have the tiles 233, and only need to make another triple in order to win. You have two possible discards: you can discard a 2 or a 3. In both cases, your shanten is zero, but discarding a 2, you can only win by drawing a 3, whereas discarding a 3, you can win by drawing a 1 or a 4. Maximizing efficiency requires considering the lifetime ure-kire of your hands.

Even then, perfect tile efficiency is not enough to see victory: every winning hand is associated with a point-score, and so in many cases it may make sense to go for a lower-probability hand that has higher expected value. Our decomposition method completely falls apart here, as while the space of winning configurations can be partitioned, scoring has nonlocal effects, so the entire hand has to be considered as a whole. In such cases, one might try for a Monte Carlo approach, since the probability space is too difficult to directly characterize. However, in the Japanese Mahjong scoring system, there is yet another difficulty with this approach: the scoring system is *exponential*. Thus, we are in a situation where the majority of samples will be low scoring, but an exponentially few number of samples have exponential payoff. In such cases, it’s difficult to say if random sampling will actually give a good result, since it is likely to miscalculate the payoff, unless exponentially many samples are taken. (On the other hand, because these hands are so rare, an AI might do considerably well simply ignoring them.)

To summarize, Mahjong is a fascinating game, whose large state space makes it difficult to accurately characterize the probabilities involved. In my thesis, I attempt to tackle some of these questions; please [check it out](http://en.wikipedia.org/wiki/April_Fools'_Day) if you are interested in more.

------------------------------------------------------------------------

\[1\] No, I am not talking about the travesty that is mahjong solitaire.

\[2\] To be clear, I am not saying that poker strategy is simple—betting strategy is probably one of the most interesting parts of the game—I am simply saying that the basic game is rather simple, from a probability perspective.

\[3\] Tenhou is a popular Japanese online mahjong client. The input format for the Tenhou calculator is `123m123p123s123z`, where numbers before `m` indicate man tiles, `p` pin tiles, `s` sou tiles, and `z` honors (in order, they are: [east, south, west, north, white, green, red](http://tenhou.net/2/?q=1234567z)). Each entry indicates which tile you can discard to move closer to tenpai; the next list is of ure-kire (and the number of tiles which move the hand further).
