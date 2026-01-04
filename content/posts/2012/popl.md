---
title: "POPL"
date: 2012-01-28 08:30:59
slug: popl
categories: [Computer Science]
comments:
    - id: 3383
      author: Justin Bailey
      date: "2012-01-28 12:06:45"
      content: "Sounds really cool. I'm curious about the  \"Run your Research\" talk? Was that in the context of mechanized proofs only? Is there an associated paper?"
    - id: 3384
      author: Edward Z. Yang
      date: "2012-01-28 14:46:30"
      content: "The talk was for this paper: http://eecs.northwestern.edu/~robby/lightweight-metatheory/"
    - id: 3385
      author: Simon Harris
      date: "2012-01-28 21:22:57"
      content: "I'm curious about the \"Move to something really modern, like Scheme!\" comment. What was the context, and was it meant as sarcasm or an indictment on where we are today?"
    - id: 3386
      author: Edward Z. Yang
      date: "2012-01-28 21:26:51"
      content: "It was the ACL2 invited talk. ACL2 is implemented on top of Common Lisp, and I believe someone had asked the question whether or not it was going to ever be updated to a more modern variant of Lisp, like Scheme. Probably a slight jibe, but the speaker was very gracious and commented on the fact that when ACL2 was originally being created, Lisp was probably as durable a language you could get, given the choices during that time."
    - id: 3387
      author: Thomas
      date: "2012-01-29 05:30:19"
      content: "I'm curious. Was it explained why this someone thought that Scheme is more modern than Common Lisp? I guess, the question I was asking myself just now is: What exactly is (more) modern in terms of Lisps or in terms of platforms and languages?"
    - id: 3388
      author: Edward Z. Yang
      date: "2012-01-29 10:27:12"
      content: "*shrug* I guess you'll have to track down the person who asked that question. :-)"
---

Last night, I returned from my very first POPL, very exhausted, and very satisfied. It was great putting faces to names, chatting with potential PhD supervisors (both from the US and in the UK), and reveling in the atmosphere.

Highlights from my files:

- Tony Hoare, on being awarded the ACM SIGPLAN Programming Languages Achievement Award, even though he has received so many other awards, “...it makes me feel a little guilty. I didn’t ask for it!”
- Hoare: “I *like* mistakes; if I find it’s my fault, I can rectify it. If it’s someone else’s fault, there’s not much you can do.”
- Hoare: “Simplicity is *not* an engineering goal.”
- Tony had just described how he retracted an accepted paper because the reasoning on it was intricate, and he didn’t think it presented program proving in a good light. The interviewer complimented him for his principles, saying that if he had a paper accepted which he didn’t think was up to snuff, he probably wouldn’t have the courage to retract it. It was “so brave.” To which Tony replies, “Well, I wish you could!” \[laughter\] “Unfortunately, the pressure to publish has increased. We feel obliged to publish every year, and the quality of the average paper is not improved.”
- One recurring theme: Tony Hoare mentioned that proofs and tests were not rivals, really they were just the same thing... just different. In the “Run your Research” talk, these theme came up again, where this time the emphasis was executable papers (the “run” in “Run your Research”).
- “Don’t just *support* local reasoning, *demand* it!” (Brute force proofs not allowed!)
- On JavaScript: “null is sort of object-y, while undefined is sort of primitive-y.”
- The next set come from the invited speaker from the computer networks community. “During 9/11, on average, the Internet was more stable. The reason for this was the NetOps *went home*.” (on the causes of network outages in practice.)
- “Cisco routers have twenty million lines of code: there’s actually an Ada interpreter in there, as well as a Lisp interpreter.”
- “It used to be acceptable to go down for 100ms... now we have video games.”
- Speaker: “I am the walrus.” (Goo goo g' joob.)
- “I believe we do not reuse theorems. We reuse proof methods, but not the actual theorems. When we write papers, we create very shallow models, and we don’t build on previous work. It’s OK. It’s the design. It doesn’t matter too much. The SML standard was distributed with a bug report, with 100+ mistakes in the original definition. Doesn’t detract from its impact.”
- “Put on the algebraic goggles.”
- “The Navy couldn’t install it \[a system for detecting when classified words were being transmitted on insecure channels\], because doing so would be admitting there was a mistake.”
- “Move to something really modern, like Scheme!” (It’s like investing one trillion, and moving from the thirteenth century to the fourteenth.)
- Strother Moore (co-creator of ACL2): “After retirement, I’ll work more on ACL2, and then I’ll die.”
- “What’s the best way to make sure your C program is conforming?” “Write deterministic code.” \[laughter\]
