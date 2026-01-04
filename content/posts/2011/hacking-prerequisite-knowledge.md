---
title: "Hacking prerequisite knowledge"
date: 2011-05-13 21:22:47
slug: hacking-prerequisite-knowledge
draft: true
categories: [Miscellaneous]
---

<div class="container center">

*Humane humanities for engineers.*

</div>

The more you know, the more you can learn—if knowledge is a circle, the opportunities for growth are on the edges. The bigger the circle, the more opportunities for learning.

PICTURE

Material inside the circle is what we already know, and material outside is what we don’t know. Move too far inside the circle, and things are repetitious or boring. Move too far outside, and things can be pretty overwhelming. Our natural filtering mechanism is involves paying less attention (or zoning out) when we see lots of material we know already, and skimming (or giving up) when a text is way too hard for us. You know that a text is close to your sweet spot when you can read it continuously and be engaged and informed.

PICTURE

But there is no such thing as an essay, or a book, or lecture or any other static piece of learning material that is perfectly tailored for your current level of knowledge (and only the very best tutors will know precisely what information you need next.) As a result, most knowledge you passively receive is not appropriate for you, and thus not very much learning happens.

PICTURE

As we learn more, we get better at learning. After all, more knowledge, more opportunities.

PICTURE

As a general philosophical stance, this is pretty unobjectionable. But in practical terms, it’s little unfair, a sort of catch-22. How can we hack this prerequisite knowledge process? In this blog post, I present a five techniques that have personally served me quite well through my college career. My hope it will be useful to you if you ever have an “I’m about to take an operating systems course but I’ve never programmed a lick of assembly before; better go read the manual...” or a “Hey, wouldn’t it be a great idea if I took a history course, the one subject I swore never to do again after my Junior year in High School” moment. We say nothing about how to motivate such a pursuit of knowledge—I offer no advice there, except that pictures are appealing and I might have attempted to use them to make you read these philosophical rantings. :-)

# The Survey

PICTURE

*Basic idea.* Use a survey text in order to get an idea of what’s important and what the basic conceptual frameworks are, to guide you for further research.

This seems like a pretty obvious one, but the devil is in the last bit: *do further research.* SparkNotes is a terrible idea if you don’t read the source text but it’s pretty useful (albeit sometimes uninspired) tool if you do read the source text. Lecture notes are terse and hard to understand if you didn’t attend lecture, but can jog the memory if you did. Popular presentations of technical fields inevitably distort facts (whether they are science, history, or anything), but the stories—however misleading—may inspire individuals to find out more, discovering the deeper, more accurate stories that are harder to encapsulate in a one hour programme.

In short, you should use a survey text as a map for further exploration, not as a final product to be used for cocktail conversation (well, I guess there’s nothing wrong with that, but that’s not particularly deep learning.) Failure to recognize this can be particularly frustrating for engineers sitting in on humanities classes, who look at the survey material and think, “This is a load of bullshit! Anyone could have come up with this! There’s no solid content here, just smoke and mirrors.” Yes, the lecturer may be bad, but what they’re trying to communicate to you is a nebulous ball of knowledge that they have accumulated over many years by engaging with the existing literature in the field, and the best way to replicate this knowledge is to go do the reading. This is the same way a mathematician will attempt to communicate the intuition of a proof in words. In the end, the way to truly reconstruct his mental state is to reconstruct the formal proof yourself.

*The Picture.* The survey gives a thin, bumpy coverage of knowledge (in green), which in and of itself is not too useful, but can help more detailed research stick (in blue).

# Collateral knowledge

PICTURE

*Basic idea.* When solving a specific, targeted problem, take the time to learn a little extra than what is strictly required to do the job.

The other day, I wanted to know if there was an easy way to recursively check a directory for broken symlinks, and remove them. With the marvels of the modern Internet, I could have simply searched the Internet, a panoply of blog posts all suggesting slightly different Unix incants involving `find` or `perl` that may get the job done. Or I could have asked someone, perhaps someone I know via instant message, or someone anonymous via a question-and-answer site like StackOverflow—at which point the answer would be handed to me on a silver platter on a matter of a few minutes.

There’s a learning opportunity here, one that will slip through your fingers if you just finger macro the command. Given my background knowledge, I know that the utility `find` probably has the facilities I want. I don’t know exactly how: `find` is one of those Unix utilities which does so much that their manpage feels utterly unreadable to any beginner, and I doubt anyone uses all of its features on a regular basis. But the key is that via my specific problem, I can now look at the manpage and find the sections that are relevant to me (don’t know what it’s called? Skim through the whole thing—it’s not that long) and read up about how `find` handles symbolic links. My knowledge is now deeper than if I just copied the command into my terminal.

The power of this technique is that little moments like this come up all the time, and it takes very little extra effort to add a little extra learning to your every-day problem solving activities. But the results accumulate. In my case, putting in a little time with manpage had the side-effect of making me considerably more comfortable with `find`, increasing my range and repertoire.

*The Picture.* The black dots represent the specific, targeted pieces of knowledge. The green halos around them represent the extra knowledge you could also gain using this method.

# The Deep End

PICTURE

*Basic idea.* When tackling a problem that is way outside your knowledge zone.

# The Baleen Whale

PICTURE

# Consolidation

PICTURE

I’d be interested to know your prerequisite knowledge hacks!
