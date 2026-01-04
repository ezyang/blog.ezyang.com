---
title: "Interactive Demo of Zero-Knowledge Proofs"
date: 2011-12-17 13:56:39
slug: interactive-demo-of-zero-knowledge-proofs
categories: [Computer Science]
comments:
    - id: 3229
      author: Sami Liedes
      date: "2011-12-17 18:00:43"
      content: "I don't understand. Since the prover is allowed to mix up the colorings between choices, how does this prove anything? What would prevent the prover from cheating by just revealing two random different colors every time?"
    - id: 3230
      author: Edward Z. Yang
      date: "2011-12-17 18:19:13"
      content: "Right. So the key assumption is that in a real instance of the protocol, the prover has to *commit* to some color before the user makes any choice. The usual metaphor is that the prover puts all of his colors in locked boxes which he gives to the user, the user picks two, and then he sends the keys for just those two boxes, which the user can then use to check."
    - id: 3260
      author: Anonymous
      date: "2011-12-25 21:50:29"
      content: |
        Are you sure in the question you correctly gave the equation for confidence?
        It seems that the program is using 1-((E-1)/E)^n, but you put 1-(1/E)^n there.
---

For the final project in our [theoretical computer science and philosophy class](http://stellar.mit.edu/S/course/6/fa11/6.893/index.html) taught by [Scott Aaronson](http://www.scottaaronson.com/blog/), [Karen Sittig](https://plus.google.com/104657582733825681275) and I decided to create an interactive demonstration of zero-knowledge proofs. (Sorry, the picture below is not clickable.)

![image](/img/interactive-zk.png)

For the *actually* interactive demonstration, click here: <http://web.mit.edu/~ezyang/Public/graph/svg.html> (you will need a recent version of Firefox or Chrome, since we did our rendering with SVG.)
