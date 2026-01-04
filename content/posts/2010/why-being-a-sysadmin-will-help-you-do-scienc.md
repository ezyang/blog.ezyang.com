---
title: "Why being a sysadmin will help you do Science!"
date: 2010-10-11 09:00:40
slug: why-being-a-sysadmin-will-help-you-do-scienc
categories: [SIPB]
comments:
    - id: 1264
      author: Thomas
      date: "2010-10-12 08:08:53"
      content: "Learning things and gaining experience is all great, but after reading the post I still don't get the title. While I would require that software engineers (especially the kind that talks about big systems or metrics) to get to know the details of real systems by working with them, for (theoretical) computer science I can only say from my own experience that I does not help to know how to set up servers and so on. Maybe I just did not notice it because I was dealing with heterogenous networked systems well before going to uni and subsequently lost interest in tinkering for tinkering's sake while I saw some people just start to get up to speed in those areas during the beginning of their studies."
    - id: 1266
      author: Edward Z. Yang
      date: "2010-10-12 08:44:20"
      content: |
        Thomas, I think you’ve put your finger one of the tremendous inequalities of incoming undergraduate computer scientists, which is that some people have been tinkering since they were in High School, and others have not, and it really is a tremendous difference.
        
        Knowing how to setup a server doesn’t really help you unless you’re going into systems research, I would probably agree. But being able to formulate complex hypotheses about software and then verify them (either by conducting the experiments or reading code) is generally applicable for the more science-y aspects of computer science. (The more mathematical edges of computer science probably don’t benefit as much.)
    - id: 1270
      author: Anonymous
      date: "2010-10-13 06:23:04"
      content: "I wholeheartedly agree. Allow me to add that being an expert in (La)TeX also helps tremendously in doing math. All those old fogeys in yesteryear having to hire secretaries (ugh!) to typeset their handwritten schtuff don't know what they're missing. The pathetic quality of their math is testimony enough that ignorance of Computer Modern is injurious to the intellect."
---

A complaint I once heard about SIPB is that it leans too much towards the system administration side: we proudly display the services we have deployed and neglect to talk very much about actually programming or conducting novel computer science research (despite the fact that we are very much programmers and some of us are quite research oriented.) So if you’re really not at all interested in that sort of thing (like me) you might think to yourself, “That’s very nice” and go and do something else.

I think this is a mistake, and that even a short period of time administrating a system larger than your personal laptop can be tremendously helpful for any work with computers you may do, whether it’s software engineering or computer science. System administration is advanced *computer* literacy, it’s a bit like knowing how to operate a complicated telescope. Sure, it has nothing to do with actually studying the stars or the fundamental nature of computation, but you’ll sure get a lot more done if you’re not fumbling with your equipment. System administration is what we do to get other things done.

“Sure,” you may say, “but any practicing software engineer or computer scientist will pick up the bits and pieces of system administration they need to know.” Yes, but I will argue that unless you actively seek out sysadmin tasks, the set of skills you acquire will be the *bare minimum* to get what you need done. Much like strength training, you are only actively gaining sysadmin skills when you’ve been pushed beyond what you’re ordinarily capable of. But unlike strength training, the benefits don’t go away after you’re done: you keep using the tricks you learned for everyday tasks. And what better way to push yourself further than to administrate a system for others?

To which I say, SIPB is a fantastic opportunity for this sort of work. It’s rare to be able to work on systems of this size and scope as an undergraduate: class projects don’t even come close. And if you’re a graduate student, you may be building complex systems but you may also be the only one using them, and having users is an enlightening experience (despite the fact that we sometimes complain, “Can we deprecate users?”)

Here are some personal perks that I’ve picked up from SIPB:

- I now know how to RAID two disks together, forced to learn how to do so because of standard operating procedure in our project. It’s not something that I would have gone out and done before, but now I consider it essential to any new physical server I setup. Because, for disk failure, it’s not a question of if, it’s a question of when.
- I now know how to effectively source dive code. I’ve used this to help communicate more effectively with upstream, fix bugs, add features, fill in for missing documentation, and more.
- I understand how the MIT Athena infrastructure works at a much more fundamental level.
