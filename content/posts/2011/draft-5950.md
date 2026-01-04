---
title: "Verifiable filesystems"
date: 2011-10-12 18:49:14
slug: 
draft: true
categories: [Miscellaneous]
---

Although formal methods acolytes would like you to believe differently, there are actually few truly mission critical systems ordinary people interact with on a daily basis. After all, these are systems for which we must absolutely, positively have as few bugs as possible, and we can tolerate some number of bugs in most software. But problems with these stringent requirements do exist, and perhaps the best example is the **filesystem**: after all, a corrupt filesystem can absolutely ruin your day, and filesystems in the wild take years to stabilize before anyone is willing to use them (much less run *fsck* on them.) Surely, then, this should be a hot topic for researchers looking for practical applications of their formal verification tools.

Right?

I decided to take a look for prior art in this area. A good start was the position paper "A mini challenge: build a verifiable filesystem"

<http://research.microsoft.com/apps/pubs/default.aspx?id=77932> - awesome, model checking

"Verifying a file system implementation" - correctness proof is uninteresting: what about fsck? What about blocks? "Formally Verifying a File System: A Successful Failure" - failed undergrad project "Designing and Analyzing a Flash File System with Alloy" - limited bound checker (don't check all the states) "POSIX file store in Z/Eves: an experiment in the verified software repository" - the example?

<http://academic.research.microsoft.com/Publication/6062171/an-integrated-formal-methods-tool-chain-and-its-application-to-verifying-a-file-system-model> <http://academic.research.microsoft.com/Publication/5873739/model-checking-part-of-a-linux-file-system> <http://academic.research.microsoft.com/Publication/1831174/on-verifying-a-file-system-implementation>
