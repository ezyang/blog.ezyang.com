---
title: "An Interactive Tutorial of the Sequent Calculus"
date: 2012-05-22 09:00:28
slug: an-interactive-tutorial-of-the-sequent-calculus
categories: [Logic, Teaching]
comments:
    - id: 3748
      author: Rob Simmons
      date: "2012-05-22 09:29:47"
      content: |
        Looks really slick (if I overlook use of classical logic ;-D). It survived a little bit of <a href="http://logitext.ezyang.scripts.mit.edu/logitext.fcgi/proving/.28forall+n.2E+Plus.28z.2Cn.2Cn.29.29+.2D.3E+.28forall+n.2E+forall+m.2E+forall+p.2E+Plus.28n.2Cm.2Cp.29+.2D.3E+Plus.28s.28n.29.2Cm.2Cs.28p.29.29.29+.2D.3EPlus.28s.28s.28z.29.29.2Cs.28z.29.2Cs.28s.28s.28z.29.29.29.29" rel="nofollow">gratuitous abuse</a> by me, though responsiveness was getting noticeably bad by the end; I'm glad I didn't try to add <i>four</i> to something!
        
        I like the choice of forcing contraction on implication left, where you would otherwise have to explain how to split the whole context, but allowing contraction to be user-specified at the quantifiers; it seems like the obviously right choice UI-wise. The other choice would probably be to always force contraction on non-invertible rules, I suppose, but what you did probably leads to better-looking final sequent proofs (my proof was wide enough as it was).
    - id: 3754
      author: Edward Z. Yang
      date: "2012-05-22 15:49:30"
      content: |
        That is pretty gratuitous :-) One reason for the slowdown is that there's a bit of code which is written with the assumption that proof trees are pretty small; so they get traversed and re-traversed quite a bit. I don't think there is any fundamental reason for this limitation, but you're example is clearly not what I had in mind :-)
        
        I think it's the case that some treatments of sequent calculus have auto-contraction in implication left, so it's not entirely my invention. I think there exists a good interface for moving the context between the two goals (click and drag a hypothesis to the other goal), but I was too lazy to code it up, and it's nice not having to tell people what contraction is until later.
    - id: 3755
      author: Chris
      date: "2012-05-22 15:50:48"
      content: "I suppose that one of the features too annoying to implement was backtracking? :)"
    - id: 3756
      author: Edward Z. Yang
      date: "2012-05-22 15:55:25"
      content: "That one is purposely left out :-)"
    - id: 3757
      author: Sean Leather
      date: "2012-05-22 16:15:46"
      content: |
        Fantastic! I love it.
        
        Several comments:
        * Errors could be improved by using English instead of {wAtomicClause} and {wTurnstile}. Also, you could refer to the clause by name.
        * When in the modal quantifier box:
          - Put the focus (i.e. cursor) immediately in the text field.
          - Allow for Esc to leave the box.
          - Don't hide the rest of the derivation.
          - Disallow clicking on other parts of the derivation (thus making it truly modal) as you can do here:
        http://logitext.ezyang.scripts.mit.edu/logitext.fcgi/proving/.28forall+x.2C+P.28x.29.29+.2D.3E+.28exists+x.2C+P.28x.29.29
        
        An alternative to the modal quantifier box is to replace the quantified term with input fields, thus still allowing interactivity with other parts of the derivation.
    - id: 3758
      author: Edward Z. Yang
      date: "2012-05-22 16:21:37"
      content: "The error dialogue and lack of autofocus are known bugs. I'm not really sure how to setup Esc to leave the box but I can look at it. I think there is a CSS bug in some versions of Chrome where the box overlaps with the sequent you're currently interacting with, which is definitely suboptimal. I need to swap out the tooltip library with one that supports HTML tooltips, that should help with some of the other bugs. I like the \"replace the quantified term with input fields\" suggestion, although we definitely still need the help text."
    - id: 3759
      author: Sean Leather
      date: "2012-05-22 16:30:00"
      content: "In Chrome 19.0.1084.46 and Safari 5.0.6, the quantifier dialog covered the term in question. In Firefox 8.0, it did not, though I only tried one example."
    - id: 3761
      author: Slava Kravchenko
      date: "2012-05-23 06:03:02"
      content: |
        Well done and thank you for sharing! I have to admit, this is somewhat unusual experience, but exciting nonetheless ;-)
        I think that increasing the font size (of the examples, at least) would look more appealing for clicking and playing with those interactive elements -- it wouldn't make it look like an article.
    - id: 3764
      author: Edward Z. Yang
      date: "2012-05-23 13:23:29"
      content: |
        OK, quantifier boxes work a /lot/ better now.
        
        Slava: I'm not sure; the examples are supposed too look like real math, and I worked hard to make the illusion that it is an article :-)
    - id: 3766
      author: Alexis Gallagher
      date: "2012-05-24 06:38:04"
      content: "This is handy. I would like it even more if I could use it from an iPad. Currently, once you tap an underlined term, there is no obvious way to dismiss the tooltip."
    - id: 3769
      author: Edward Z. Yang
      date: "2012-05-24 19:25:21"
      content: "Hmm, that is a very interesting UI minefield. Maybe the right thing to do is add an onclick handler for the tooltip which dismisses it? Unfortunately I don't have an iPad so testing that is difficult."
    - id: 3787
      author: Mikhail Glushenkov
      date: "2012-05-29 11:32:30"
      content: "Thank you, playing with the tutorial was really fun, and now I have a better idea of what sequent calculus is."
    - id: 32678
      author: Anonymous
      date: "2024-06-21 09:44:04"
      content: |
        Hi there,
        I search for an exhaustive list of sequents, can you provide me a list?
---

You can view it here: [An Interactive Tutorial of the Sequent Calculus](http://logitext.ezyang.scripts.mit.edu/logitext.fcgi/tutorial). This is the "system in three languages" that I was referring to in [this blog post](http://blog.ezyang.com/2012/05/what-happens-when-you-mix-three-research-programming-languages-together/). You can also use the system in a more open ended fashion from [this page](http://logitext.ezyang.scripts.mit.edu/logitext.fcgi/main). Here's the blurb:

> *This interactive tutorial will teach you how to use the sequent calculus, a simple set of rules with which you can use to show the truth of statements in first order logic. It is geared towards anyone with some background in writing software for computers, with knowledge of basic boolean logic.*

Developing this system has been quite a fascinating foray into user interface and game design. While similar systems have been built in the past, my goal was a little different: I wanted something simple enough and accessible enough that anyone with a vague interest in the topic could pick it up, work through it in an hour, and learn something interesting about formal logic. I don't think this demo will be particularly successful among the math-phobic, but perhaps it will be more successful with people who have an intrinsic interest in this sort of thing. I must have incorporated dozens of comments from my many friends at MIT who put up with my repeated badgering about the system. The first version looked very different. I give my superlative thanks to my beta testers.

There is a lot of hubbub about the next generation of online teaching systems (edX), and this demo (because, really, that's what it is) is intended to explore how to blur the line between textbooks and video games. It doesn't really go far enough: it is still too much like a textbook, there is not enough creative latitude in the early exercises. I don't feel like I have gotten the right feel that a video game which progressively layers concepts (e.g. Portal). On the other hand, I do feel I have done a good job of making the text skimmable, and there are a lot of little touches which I think do enhance the experience. I am embarrassed to admit that there are some features which are not included because they were technically too annoying to implement.

If there is one important design principle behind this demo, it is that there is a difference between giving a person a map and letting a person wander around in a city for a few hours. But, fair reader, you probably don't have a few hours, and I am probably demanding too much of your attention. Nevertheless, forgive my impoliteness and please, [take it out for a spin.](http://logitext.ezyang.scripts.mit.edu/logitext.fcgi/tutorial)
