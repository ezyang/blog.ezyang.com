---
title: "Bugs and Battleships"
date: 2011-12-19 11:04:51
slug: bugs-and-battleships
categories: [Computer Science]
comments:
    - id: 3236
      author: paurullan
      date: "2011-12-19 13:45:47"
      content: "Great :D"
    - id: 3237
      author: David Barbour
      date: "2011-12-19 14:52:09"
      content: Excellent article.
    - id: 3239
      author: Joel Wang
      date: "2011-12-20 04:47:44"
      content: "As a novice software tester I find this very intuitive. Thanks!"
    - id: 3240
      author: Maintenance Man
      date: "2011-12-20 15:39:56"
      content: |
        At first I thought your diagrams were battleship layouts. Heheh.
        
        Looking at your last diagram, is the conclusion of the matter to employ types and type checking to increase testing efficiency?
    - id: 3242
      author: Aidan Delaney
      date: "2011-12-20 17:42:33"
      content: "As someone who has taught software testing to undergrads for several years, I commend your insight and choice of metaphor.  This post clarifies exactly what I try and teach.  Would you mind if I used some of the text and images on slides in my class?"
    - id: 3243
      author: John Regehr
      date: "2011-12-20 23:10:07"
      content: "Nice!"
    - id: 3246
      author: mpe
      date: "2011-12-21 07:51:36"
      content: "Great article. Though I think your final picture grossly overstates the space that is taken up by bugs that type checking can find, at least in real programs."
    - id: 3248
      author: Natalia
      date: "2011-12-21 14:02:16"
      content: "Good article! Thank you!"
    - id: 3249
      author: Elliot Smith
      date: "2011-12-21 18:57:02"
      content: Great article. Thanks
    - id: 3251
      author: Anonymous
      date: "2011-12-22 09:57:49"
      content: "make programs single-shot, only. compose them."
    - id: 3257
      author: Pk
      date: "2011-12-25 17:31:13"
      content: "Edward, if you write a book in the same approach as your articles, I would buy it."
    - id: 3258
      author: Critic
      date: "2011-12-25 19:06:49"
      content: "Um .. so what's new? I think Dijkstra has said everything you just did, and much better."
    - id: 3259
      author: Edward Z. Yang
      date: "2011-12-25 19:17:42"
      content: "Are you referring to the very end, where I channel Dijkstra’s essay “On the Cruelty of Really Teaching Computer Science.” Actually, that was never the original intent of the essay (which was this vague idea I had in my head that test suites ought not to be thought of as discrete things, but as continuous distributions.) I may be channeling Dijkstra there, but the relevant essay escapes me."
    - id: 3286
      author: Anonymous
      date: "2011-12-31 14:24:39"
      content: "It occurs to me that modularity is kind of like separating the rows and columns of the Battleship board, in order to reduce the number of dimensions you need to search in."
    - id: 3287
      author: Pelle
      date: "2012-01-01 13:56:55"
      content: "Great article. Also have you looked at QuickCheck type of property based testing?"
    - id: 3301
      author: Tom Crayford
      date: "2012-01-06 18:21:40"
      content: This post dearly needs a diagram about isolated (ie mock/stub based) unit testing.
    - id: 3303
      author: Edward Z. Yang
      date: "2012-01-06 22:44:52"
      content: "Tom: Metaphors should not be over-stretched, and I'm not really sure what mock/stub based testing would look like."
    - id: 3551
      author: rdm
      date: "2012-03-15 12:34:19"
      content: |
        This was a great read.
        
        That said, this essay said almost nothing about "feature testing".  (Feature testing is not focused on "finding bugs", it's about dealing with cost/benefit issues, it's about sketching out the design, and is about tracking who to be discussing priorities with, when things need to be changing.  Features are not uniformly valuable, and have costs.)
    - id: 3555
      author: Edward Z. Yang
      date: "2012-03-15 17:40:17"
      content: "Yes, I'm not really trying to talk about feature testing, because it's a bit different. Bugs tend to be a bit more clear cut (though they're not black and white), cost/benefit of features is much mushier. Certainly you're not going to be able to formally verify something like this."
    - id: 4222
      author: "Illustration in Teaching | My Own Fortune"
      date: "2012-09-22 09:05:00"
      content: "[...] &#8220;Bugs and Battleships&#8221; by Edward Z. Yang [...]"
    - id: 23589
      author: "New top story on Hacker News: Bugs and Battleships (2011) &#8211; protipsss"
      date: "2020-04-06 11:41:20"
      content: "[&#8230;] Bugs and Battleships (2011) 3 by gklitt | 0 comments on Hacker News. [&#8230;]"
    - id: 23590
      author: "New top story on Hacker News: Bugs and Battleships (2011) &#8211; News about world"
      date: "2020-04-06 11:41:53"
      content: "[&#8230;] Bugs and Battleships (2011) 3 by gklitt | 0 comments on Hacker News. [&#8230;]"
    - id: 23591
      author: "New top story on Hacker News: Bugs and Battleships (2011) &#8211; Latest news"
      date: "2020-04-06 11:42:23"
      content: "[&#8230;] Bugs and Battleships (2011) 3 by gklitt | 0 comments on Hacker News. [&#8230;]"
    - id: 23592
      author: "New top story on Hacker News: Bugs and Battleships (2011) &#8211; Hckr News"
      date: "2020-04-06 11:53:32"
      content: "[&#8230;] Bugs and Battleships (2011) 4 by gklitt | 0 comments on Hacker News. [&#8230;]"
    - id: 23593
      author: "New top story on Hacker News: Bugs and Battleships (2011) &#8211; Ultimate News"
      date: "2020-04-06 11:55:18"
      content: "[&#8230;] Bugs and Battleships (2011) 5 by gklitt | 0 comments on Hacker News. [&#8230;]"
    - id: 23991
      author: "Bugs and Battleships (2011) | Hacker News"
      date: "2020-07-20 12:10:51"
      content: "[&#8230;] Bugs and Battleships (2011) (ezyang.com) [&#8230;]"
---

![image](/img/testing/battleship.png)

Do you remember your first computer program? When you had finished writing it, what was the first thing you did? You did the simplest possible test: you ran it.

As programs increase in size, so do the amount of possible tests. It’s worth considering which tests we actually end up running: imagine the children’s game Battleship, where the ocean is the space of all possible program executions, the battleships are the bugs that you are looking for, and each individual missile you fire is a test you run (white if the test passes, red if the test fails.) You don’t have infinite missiles, so you have to decide where you are going to send them.

![image](/img/testing/toy-programs.png)

In the case of “your first computer program,” the answer seems pretty obvious: there’s only one way to run the program, only a few cases to test.

But this fantasy is quickly blown away by an encounter with real software. Even if your program has no inputs, hardware, operating system, development environment, and other environmental factors immediately increase the space of tests. Add explicit inputs and nondeterminism to the application, and you’re looking at the difference between a swimming pool and an ocean.

![image](/img/testing/real-programs.png)

How do we decide what to test? What is our strategy—where do we send more missiles, where do we send less? Different testing strategies result in different distributions of tests on the space of all possible executions. Even though we may not be *thinking* about the distribution of test cases when we write up tests or run the whole system in an integration test, different test strategies result in different coverage.

For example, you might decide not to do any tests, and rely on your users to give you bug reports. The result is that you will end up with high coverage in *frequently used areas* of your application, and much less coverage in the rarely used areas. In some sense, this is an optimal strategy when you have a large user base willing to tolerate failure—though anyone who has run into bugs using software in unusual circumstances might disagree!

![image](/img/testing/field-testing.png)

There is a different idea behind regression testing, where you add an automatic test for any bug that occurred in the past. Instead of focusing coverage on frequently used area, a regression test suite will end up concentrated on “tricky” areas of the application, the areas where the most bugs have been found in the past. The hypothesis behind this strategy is that regions of code that historically had bugs are more likely to have bugs in the future.

![image](/img/testing/regression-testing.png)

You might even have some a priori hypotheses about where bugs in applications occur; maybe you think that boundary cases in the application are most likely to have bugs. Then you might reasonable focus your testing efforts on those areas on the outset.

![image](/img/testing/boundary-testing.png)

Other testing strategies might focus specifically on the distribution of tests. This is especially important when you are concerned about *worst-case* behavior (e.g. security vulnerabilities) as opposed to average-case behavior (ordinary bugs.) Fuzz testing, for example, involves randomly spattering the test space without any regard to such things as usage frequency: the result is that you get a lot more distribution on areas that are rarely used and don’t have many discovered bugs.

![image](/img/testing/fuzz-testing.png)

You might notice, however, that while fuzz testing changes the distribution of tests, it doesn’t give any *guarantees.* In order to guarantee that there aren’t any bugs, you’d have to test every single input, which in modern software engineering practice is impossible. Actually, there is a very neat piece of technology called the *model checker*, designed specifically with all manner of tricks for speed to do this kind of exhaustive testing. For limited state spaces, anyway—there are also more recent research projects (e.g. Alloy) which perform this exhaustive testing, but only up to a certain depth.

![image](/img/testing/model-checking.png)

Model checkers are “dumb” in some sense, in that they don’t really understand what the program is trying to do. Another approach we might take is to take advantage of the fact that we know how our program works, in order to pick a few, very carefully designed test inputs, which “generalize” to cover the entire test space. (We’ll make this more precise shortly.)

![image](/img/testing/equivalence-partitioning.png)

The diagram above is a bit misleading, however: test-cases rarely generalize that readily. One might even say that the ability to generalize behavior of specific tests to the behavior of the program is precisely what distinguishes a good program from a bad one. A bad program is filled with many, many different cases, all of which must be tested individually in order to achieve assurance. A good program is economical in its cases, it tries to be as complex as the problem it tries to solve, and no more.

![image](/img/testing/bad-good-software.png)

What does it mean to say that a test-case generalizes? My personal belief is that chunks of the test input space which are said to be equivalent to each other correspond to a single case, part of a larger mathematical proof, which can be argued in a self-contained fashion. When you decompose a complicated program into parts in order to explain what it does, each of those parts should correspond to an equivalence partition of the program.

The corollary of this belief is that *good programs are easy to prove correct.*

![image](/img/testing/certified-programs.png)

This is a long way from “running the program to see if it works.” But I do think this is a necessary transition for any software engineer interested in making correct and reliable software (regardless of whether or not they use any of the academic tools like model checkers and theorem provers which take advantage of this way of thinking.) At the end of the day, you will still need to write tests. But if you understand the underlying theory behind the distributions of tests you are constructing, you will be much more effective.

*Postscript.* The relationship between type checking and testing is frequently misunderstood. I think this diagram sums up the relationship well:

![image](/img/testing/type-checking.png)

Types eliminate certain regions of bugs and fail to affect others. The idea behind dependent types is to increase these borders until they cover all of the space, but the benefits are very tangible even if you only manage to manage a subset of the test space.

![image](http://i.creativecommons.org/l/by-sa/3.0/88x31.png)

This work is licensed under a [Creative Commons Attribution-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-sa/3.0/).
