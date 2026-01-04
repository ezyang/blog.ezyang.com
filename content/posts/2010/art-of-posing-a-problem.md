---
title: "The Art of Posing a Problem"
date: 2010-02-24 12:00:01
slug: art-of-posing-a-problem
categories: [Personal, SIPB]
comments:
    - id: 118
      author: Kevin Riggle
      date: "2010-02-27 20:48:55"
      content: |
        <i>Many projects are extremely large and complex, and in many cases it's simply not possible to assign someone an interesting, high-level project and expect them to make significant headway.</i>
        
        Thinking about your points in the context of my experience with MITSFS, I find it depends a lot on the person.  There are people to whom I can give a large, complex project, and they'll approach it, break it down, come back to me as they have questions, and eventually bring back an excellent result.  Other people need more hand-holding, and some people are completely incapable of functioning on their own (though at least in the context of MITSFS they tend to go away quickly).  Figuring out where any particular person falls in this is an important skill as a manager.  I'll often start by tossing someone a big, poorly-defined project.  If they can handle it, great; if they're still around but not making any headway after some amount of time, I'll take them off that and give them something better specified to do.  Doing the big complicated projects is also a skill people develop, so I watch people and give them bigger things if I think they've developed the skills to handle them.
        
        <i>No one ever tells you what they're interested in!</i>
        
        I find it really hard to tell someone <i>a priori</i> what I'm interested in, and I think other people do too.  I find it works a lot better for the person posing the project to lay out some of the options and then let me choose or narrow from there.
        
        <i>It's easy to exert too much or too little control over the direction of the project. [...] Too little control and the person can easily get lost or waste hours fighting incidental issues and not the core of the problem.</i>
        
        See above about figuring out how much oversight people need, with the caveat that there's often utility to letting people try something their own way and fail first, if that's what they want to do, and <i>then</i> come to you for guidance.  That's a much more powerful and long-lasting learning experience than doing exactly what you told them.
        
        <i>Being a proper mentor is a time-consuming process, even if you exert minimal control. [...] You might wonder why you didn't just do the damn task yourself.</i>
        
        Yup.  It's hard to avoid.  Ultimately you're trying to <i>build</i> people to whom you can give a poorly-specified problem and who will then go off and solve it, so if you can think of your time as an investment towards that goal, it becomes more worthwhile.
        
        <i>As people refine the art of bootstrapping, the number of possible projects they can work on explode, and what makes you think that they're going to work on your project? [...] if you don't get the person to buy in, you can easily loose them.</i>
        
        Yup.  The best solution to this I've found is to throw as many people as you have at as many kinds of problems as possible, and when somebody gets bored or burned-out, find something else for them to try.  The people who like a project and have bought into it will stick, and your organization as a whole will be a lot happier if everybody is doing more-or-less what they enjoy.
    - id: 21866
      author: "on blog commenting | Free Dissociation"
      date: "2017-03-14 18:59:42"
      content: "[&#8230;] My response to Ed&#039;s post on learning to manage, or &quot;The Art of Posing a Problem&quot;. [&#8230;]"
---

Last week, I was talking with [Alexey Radul](http://web.mit.edu/~axch/www/) to figure out some interesting research problems that I can cut my teeth on. His [PhD thesis](http://web.mit.edu/~axch/www/phd-thesis.pdf) discusses "propagation networks", which he argues is a more general substrate for computation than traditional methods. It's a long work, and it leaves open many questions, both theoretical and practical. I'm now tackling one very small angle with regards to the implementation of the system, but while we were still figuring a problem out, Alexy commented, "the more work I realize it takes to do a good job of giving someone a problem."

I wholeheartedly agree, though my experiences come from a different domain: [SIPB](http://sipb.mit.edu). Some of the key problems with assigning interested prospectives projects to work on include:

- Many projects are extremely large and complex, and in many cases it's simply not possible to assign someone an interesting, high-level project and expect them to make significant headway. They're more likely to progress on a [wax on wax off](http://tvtropes.org/pmwiki/pmwiki.php/Main/WaxOnWaxOff) style training, but that's not interesting.
- No one ever tells you what they're interested in! Even if you ask, you'll probably get the answer, "Eh, I'd be up for anything." As someone who has used this phrase before, I also emphatically understand that this is not true; people have different interests and will enjoy the same task dramatically differently.
- It's easy to exert too much or too little control over the direction of the project. Too much control and you've defined the entire technical specification for the person, taken away their creative input, made them feel bad when they've not managed to get work done, and are bound to be dismayed when they failed to understand your exacting standards in the first place. Too little control and the person can easily get lost or waste hours fighting incidental issues and not the core of the problem.
- Being a proper mentor is a time-consuming process, even if you exert minimal control. Once the person comes back with a set of patches, you still have to read them, make sure they're properly tested, and send back reviews on how the patches need to be reviewed (and for all but the most trivial of changes, this will be inevitable). You might wonder why you didn't just do the damn task yourself. Reframing the problem as a purely educational exercise can also be disappointing, if not done properly.
- As people refine the art of bootstrapping, the number of possible projects they can work on explode, and what makes you think that they're going to work on *your* project? People decide what they want to work on, whether it's because they made it themselves, or it's in a field they're interested in, or it's a tool they use day-by-day, and if you don't get the person to buy in, you can easily loose them.

I imagine similar tensions come up for open-source project maintainers, internship programs and Google Summer of Code organizers. And I still have no feeling for what strategies actually *work* in this space, even though I've certainly been on both sides of the fence. I'd love to hear from people who have tried interesting strategies and had them work!
