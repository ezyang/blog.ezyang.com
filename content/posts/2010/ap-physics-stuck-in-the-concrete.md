---
title: "AP Physics: Stuck in the concrete"
date: 2010-06-07 09:00:32
slug: ap-physics-stuck-in-the-concrete
categories: [Teaching]
math: true
comments:
    - id: 505
      author: paurullan
      date: "2010-06-07 11:26:43"
      content: |
        I would love soooooooo much that my teachers would have told me in primary school the true relationship with acceleration and distance that right now I would be studying physics instead of computer engineering.
        
        In the computer world there is the «problem» of how you teach the meaning of a variable and a pointer. Some people cannot understand it! I think that if you taught in a functional style with purism, later explain how you store data and then how to recover it that would be all.
    - id: 506
      author: Quentin
      date: "2010-06-07 11:36:18"
      content: "I'm pretty sure I was introduced to the differential version of these equations in high school, though we didn't know the math needed to apply them except in very simple cases."
    - id: 507
      author: Derek Elkins
      date: "2010-06-07 12:46:17"
      content: |
        My experience is similar to Quentin's.  I certainly was shown that velocity is the derivative of position and acceleration the derivative of velocity, and this was used to derive the equations you've listed both in AP Calculus and AP Physics.  Any attempt to do physics without calculus is doomed to failure.
        
        You might find this TED talk interesting: http://www.ted.com/talks/dan_meyer_math_curriculum_makeover.html
        
        Also, anyone interested in physics should look at geometric algebra.  Geometric algebra makes every area of physics more accessible.  Using geometric algebra it would be quite feasible to do much more complicated mechanics (e.g. complicated rotational mechanics problems) and electromagnetism problems in high school
    - id: 508
      author: Edward Z. Yang
      date: "2010-06-07 12:54:41"
      content: "Interesting! I was going to say to Quentin that he was a special case, but I suppose some High Schools did get their physics curriculum right (while I adored my Physics teacher, this was not the case.) Derek, I do agree that our math curriculum is very much due for dramatic change (though this was not quite the topic of the post :-)"
    - id: 509
      author: Jeffrey Bosboom
      date: "2010-06-07 13:09:38"
      content: "I didn't take AP Physics at my high school for precisely this reason.  In my \"honors\" physics class (a prerequisite for AP), we did work through the derivations of formulas from calculus, but after that we just \"plug-and-chugged\" the numbers into our calculators.  (Similarly for AP Chemistry, which I did take.)  I don't fault my teachers for teaching this way, as it's what the AP tests expected and left us well-prepared to show work in the expected way.  (Besides, I also got to take a multivariable calculus class, taught on my high school campus but through Cal State Fullerton, that did a lot of this stuff as explanations of applications.)"
---

*Attention conservation notice.* The author reminisces about learning physics in high school, and claims that all too often, teaching was focused too much on concrete formulas, and not the unifying theory around them.

In elementary school, you may have learned D=RT (pronounced "dirt"), that is, distance is rate multiplied with time. This was mostly lies, but it was okay, because however tenuous the equation's connection to the real world, teachers could use it to introduce the concept of algebraic manipulation, the idea that with just D=RT, you could also find out R if you knew D and T, and T if you knew D and R. Unless you were unusually bright, you didn't know you were being lied to; you just learned to solve the word problems they'd give you.

Fast forward to high school physics. The lie is still preached, though dressed in slightly different clothes: "Position equals velocity times time," they say. But then the crucial qualifier would be said: "This equation is for *uniform* motion." And then you'd be introduced to your friend uniform acceleration, and there'd be another equation to use, and by the time you'd finish the month-long unit on motion, you'd have a veritable smörgåsbord of equations and variables to keep track of.

CollegeBoard AP Physics continues this fine tradition, as stated by their equation sheet:

<div class="container center">

\$v = v_0 + at\$

\$x = x_0 + v_0t + \frac12at^2\$

</div>

With the implicit expectation that the student knows what each of these equations means, and also the inadvertent effect of training students to pattern match when an equation is appropriate and which values go to which variables.

I much prefer this formulation:

<div class="container center">

\$v = \int a\\ dt\$

\$x = \int v\\ dt\$

</div>

With these two equations, I tap into calculus, and reach the very heart of the relationship between position, velocity and acceleration: one is merely the derivative of the previous. These equations are fully general (not only do they work for non-uniform motion, they work on arbitrary-dimensional vectors too), compact and elegant. They're also not immediately useful from a calculational standpoint.

Is one more valuable than the other? They are good for different things: the first set is more likely to help you out if you want to calculate how long it will take for an apple to fall down from a building, neglecting air resistance. But the second set is more likely to help you really *understand* motion as more than just a set of algebraic manipulations.

I was not taught this until I took the *advanced* Classical Mechanics class at MIT. For some reason it is considered fashionable to stay stuck in concrete formulas than to teach the underlying theory. AP Physics is even worse: even AP Physics C, which purports to be more analytical, fills its formula sheet with the former set of equations.

Students have spent most of grade school learning how to perform algebraic manipulations. After taking their physics course, they will likely go on to occupations that involve no physics at all, all of that drilling on exercises wasted. They deserve better than to be fed more algebraic manipulations; they deserve to know the elegance and simplicity that is Classical Mechanics.

*Postscript.* For programmers reading this blog, feel free to draw your own analogies to your craft; I believe that other fields of science have much to say on the subject of *abstraction*. For prospective MIT students reading this blog, you may have heard rumors that 8.012 and 8.022 are hard. This is true; but what it is also true is that this pair of classes have seduced many undergraduates into the physics department. I cannot recommend the pair of classes more highly.
