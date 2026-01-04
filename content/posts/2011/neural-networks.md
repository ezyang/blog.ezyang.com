---
title: "Chain Rule + Dynamic Programming <br />= Neural Networks"
date: 2011-05-30 09:00:24
slug: neural-networks
categories: [Math]
comments:
    - id: 2523
      author: yachris
      date: "2011-05-30 09:28:08"
      content: |
        So dynamic programming is also gradient descent?
        
        My AI definition (from back when I worked in AI during the AI summer of the mid-80s) was two-fold.
        
        First, there is the "Magic Circle" of AI problems: when we don't know how to solve a problem (so it's impossible, so we need AI) it's definitely an AI problem.  Once we have some kind of solution, WELL, it's just an ALGORITHM, so it certainly isn't AI any more!
        
        Second, it's a group of techniques that we use where "ordinary" algorithms don't work.  We all agree we don't need AI to add two numbers together.
        
        Anyway, it was fun when I was in that field because people would occasionally ask, "When will computers be able to think?"  I would always just ask in return, "Well, what does it mean to think?"  This kind of derailed the conversation, but I think it got the insight across that it's a bit deeper of a subject that you might first assume.
    - id: 2524
      author: Edward Z. Yang
      date: "2011-05-30 09:33:14"
      content: |
        Hmm, I don't think so, though dynamic programming is frequently used for optimization style problems. In this case, gradient descent requires the computation of a value, which can be found through dynamic programming.
        
        Yes, I think a lot of people who take an AI class without knowing very much about the topic think, "This is just another algorithms class!" But it's wrong to say that these algorithms are not AI, and it took a while for me to put my finger on why these algorithms could be legitimately called "AI".
        
        I think AI researchers get a little annoyed at the philosophical approach to "What is thinking"; but that's just my perception :-)
    - id: 2526
      author: Edward Kmett
      date: "2011-05-30 13:14:41"
      content: "Note: back propagation is a trivial application of automatic differentiation. It always amused me that so many papers get published in this field that are completely mechanical in their derivation -- no pun intended."
    - id: 2530
      author: Danno
      date: "2011-05-31 17:46:52"
      content: "My naive impression of the sorts of algorithms that my undergraduate AI course focused on was that they were broadly search algorithms. Lots of \"finding\" things that were optimal or correct through one approach or another. I think I had been hoping that AI solutions would be more constructive rather than exhaustive."
    - id: 2548
      author: Edward Z. Yang
      date: "2011-06-01 13:06:44"
      content: |
        Edward: I get a similar impression too. :-)
        
        Danno: Probabilistic AI is usually not covered in introductory courses, sadly. Winston threw in some material on SVMs in 6.034, but I don't think anyone came away thinking they understood them.
    - id: 2744
      author: Jules
      date: "2011-06-27 17:04:23"
      content: "An easy way to do neural networks is with a matrix library. A two layer neural network computes the function f(x) = a(A*a(B*x)) where A and B are the weight matrices, and a is the activation function lifted over vectors. The notational compactness saves some work in computing the gradient."
    - id: 6315
      author: JH
      date: "2013-12-01 23:02:59"
      content: "I totally agree with you on the chain rule part! I was searching online for some confirmation, and here goes your post. Unfortunately, as a statistics major, I know nothing about dynamic programming. Therefore, still perplexed by the importance of defining an error term delta."
    - id: 18409
      author: Anonymous
      date: "2015-10-23 23:14:10"
      content: |
        Agreed.
        When I first learnt neural network in grad school (I did undergrad in pure math and took some algo class though), the chapter of the textbook is full of fancy words like back propagation, activate and the derivation of partial derivatives(namely the gradient at a given weight vector) was wordy and lacking of concise. At some point I even thought the author did not really learn that lot of math because he totally missed the points the really important points are no more than just chain rule 
        
        df / dw = df/da * da/dw
        
        and per dynamic programming we need to store the state
         da/dw
        in memory that's it.
    - id: 22470
      author: Jeetendra Dhall
      date: "2018-08-24 02:14:04"
      content: |
        Michael Nielsen says that Neural Networks is a programming paradigm, and Deep Learning (back propagation + gradient descent) is a technique (algorithm). Back Propagation is the calculus (chain rule), and Gradient Descent is the search / optimization approach.
        
        The text above says that Gradient Descent is NOT Dynamic Programming, but, instead, uses the lookup table created using a dynamic programming based approach.
        
        When we compare neural networks as a programming paradigm with other programming paradigms (procedural, object-oriented, functional, etc), we can think along imperative (C++, Java) and declarative paradigms (SQL). In both cases, the programmer is in charge of defining a function (given x, get me y). In fact, declarative also looks like imperative + metadata based convenience. But, this taxonomy doesn't get us to neural networks.
        
        If, instead, we use 'programming paradigm' to cite examples like imperative, fuzzy, genetic, neural, etc,  we can attempt to think of neural networks as a set of a huge number of hyperplanes that form decision boundaries, and that combine with each other in a combinatorially explosive way to form complex decision boundaries doing so progressively layer by layer (going deep) till the boundaries can enclose a class.
        
        The name 'artificial intelligence' may have got stuck for historical reasons, and we call it 'stuck' because of this paradigm not being able to explain biological neurons.
---

(Guess what Edward has in a week: Exams! The theming of these posts might have something to do with that...)

At this point in my life, I’ve taken a course on introductory artificial intelligence twice. (Not my fault: I happened to have taken MIT’s version before going to Cambridge, which also administers this material as part of the year 2 curriculum.) My first spin through 6.034 was a mixture of disbelief at how simple the algorithms were, indignation at their examination methods, and the vague sense at the end that I really should have paid more attention. My second time through, I managed to distill a lot more algorithmic content out of the course, since I wasn’t worrying as much about the details.

The topic of today’s post is one such distillation of algorithmic content from the neural network learning process. Well, at least, for multilayer perceptrons—since that’s what usually gets studied as a case of neural networks. It should be noted that the perceptron is a really simple sort of mathematical function: it’s a multivariable function that takes as arguments a weight vector and an input vector, takes their dot product and runs the result through an activation function (which is usually chosen so that it has nice properties when differentiated.) “Learning” in this case is first-order optimization via gradient descent, and the primarily computational content involves calculating the partial derivative of the function with respect to the weight vector—something that anyone who has taken multivariable calculus ought to be able to do in his sleep.

Note that I say *ought*. Actually, neural networks gave me a pretty horrendous time both times I had to learn it. Part of the trouble is that once you’ve worked out the update formulas, you don’t actually need to understand the derivation: they “just work.” Of course, no self-respecting course would want to quiz you on your ability to memorize the relevant equations, so they’ll usually ask you to write out the derivation. There you run into the second trouble: most presentations of the derivation are quite long and don’t “compress” well.

The first insight into the process, which I (eventually) picked up the first time I took the course, was that these derivations were actually just repeatedly applying the chain rule. Thus, the laborious analysis of all of the partial derivatives can be replaced with the following algorithm: “Chop the perceptron into smaller functions, calculate the derivative of each function, and then multiply the results back together.” Now, this does require a little bit of care: one normally visualizes the perceptron network as a function on the input values, but the derivative is with respect to the weights. Furthermore, the perceptron network is a much more involved partial differentiation problem than one usually finds on a multivariable calculus exam, so if you don’t have your variable indexing sorted out it’s very easy to get confused. (Here, a notion of fresh names and global names comes in handy, because it sets the ground rules for notational sleights of hands that mathematicians do freely and confusingly.) If you have the chain rule in your arsenal, you have a fairly convincing story for the output perceptron, and with a little bit more confusion you might manage the the internal perceptrons too.

The second insight into the process I didn’t pick up until my second time around: it is the resemblance of backpropagation to dynamic programming. This involved the realization that, in principle, I could calculate the partial derivative of the function with respect to any weight simply by tracing out the nodes “downstream” from it, and calculating the (longer) derivative chains manually. I could do this for every node, although it might get a bit tedious: the key idea of “backpropagation” is that you can reuse results for an efficiency increase, just as you do for dynamic programming. It is also gratifying to see that this explains why both treatments I’ve seen of neural nets obsess over δ, a seemingly innocuous derivative that really shouldn’t get its own symbol. The reason is this value is precisely what is stored in the dynamic programming table (in this case, shaped the same way as the input neural net); the actual partial derivative for a weight isn’t actually what we need. This is actually fairly common, as far as contest dynamic programming problems go—part of the trick is figuring out what intermediate calculations you also need to store in your table. Backpropagation is then just filling out the table from the output node to the input nodes.

So there you have it: chain rule + dynamic programming = neural network backpropagation algorithm. Of course, this formulation requires you to know how to do the chain rule, and know how to do dynamic programming, but I find these concepts much easier to keep in my brain, and their combination pleasantly trivial.

*Postscript.* No lecturer can resist the temptation to expound on what they think “artificial intelligence” is. I’ll take this opportunity to chime in: I believe that AI is both a problem and an approach:

- Artificial intelligence is a problem, insofar as asking the question “What can humans do that computers cannot” is a tremendous way of digging up computationally interesting problems, and
- Artificial intelligence is an approach, insofar as instances of intelligence in nature suggest possible solutions to computational problems.

I have tremendous respect for the power of AI to frame what questions researchers should be asking, and if we say an approach is AI because it handles a problem in this domain quite well, AI is everywhere. (It also explains why AI thrives at MIT, a very engineering oriented school.) I am still, however, skeptical about “biological inspiration”, since these approaches doesn’t actually seem to work that well (e.g. the fall of “traditional” AI and the rise of statistical NLP methods), and the fact that the resulting methods are a far cry from their biological counterparts, as any neuroscientist who is familiar with “neural” networks may attest. In some cases, the biological analogies may be actively harmful, obscuring the core mathematical issues.
