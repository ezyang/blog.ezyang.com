---
title: "A new vision for Mendeley"
date: 2011-07-06 09:00:54
slug: a-new-vision-for-mendeley
categories: [Toolbox]
comments:
    - id: 2764
      author: Steve Dennis
      date: "2011-07-06 10:14:11"
      content: "Thanks for the great blog post Edward :)  You've identified many areas and use cases that we're working towards, and it's good to know that our internal vision aligns a great deal, with that of our users.  A very large chunk of the stuff you mentioned is actively being worked on, with others already on our roadmap.  Mendeley's still a very young product (version 1.0 will release this month if all goes smoothly) and I hope users like yourself will find enough usefulness as we grow, to stick with us or at least check back from time to time to see how things are progressing."
    - id: 2765
      author: wren ng thornton
      date: "2011-07-06 12:57:38"
      content: |
        Dude. If Mendeley offered all that (or even a decent portion of it) I'd be all over it. Especially the free-form "read" bit and a decent way to link together different renditions of an author's or conference's name and say "these are the same entity".
        
        For the record though, papers do have hyperlinks. That's what citations *are* afterall. They're just a lot harder to scrape off a pdf or dead tree than they are to scrape off HTML.
    - id: 2766
      author: Edward Z. Yang
      date: "2011-07-06 13:27:01"
      content: |
        Steve: Glad to hear you are thinking about these things. I was a little dismayed when I downloaded a copy of the dev release of 1.0 and found that these aspects of Mendeley had not really changed very much (having originally discovered Mendeley half a year ago). Of course, building software is hard. :-)
        
        wren ng thornton: I politely disagree about papers having hyperlinks. Citations are links, but they're not *hyper*links, the latter defined as a link which can directly/automatically be followed.
    - id: 2767
      author: Anonymous
      date: "2011-07-06 17:03:04"
      content: Great post. I completely agree.
    - id: 2769
      author: Gabriel
      date: "2011-07-07 07:21:01"
      content: "Well, some people include the DOI number as a hyperlink in their bibliography.  But most journals and conferences do not follow this style."
---

[I use Mendeley](http://blog.ezyang.com/2010/11/reflexivity-qed/) because it lets me easily search for papers I care about. Unfortunately, that seems to be all Mendeley has been doing for me... and that’s a damn shame. Maybe it’s because I’m an undergraduate, still dipping my toe into an ocean of academic research. Mendeley was aimed at practicing researchers, but not people like me, who are stilll aiming for breadth not depth. I can count on two hands the number of technical papers I’ve really dug into—I’m trying to figure out what it is exactly that I want to specialize in.

From this perspective, there are many, many things that Mendeley could be doing for me that it simply isn’t doing right now. And this is not purely a selfish perspective: the world of academics is a relatively small one, and I’d like to think a move towards my preferences is a move towards a larger body of potential users and customers. My request/plea/vision can be summed up as follows: Mendeley needs better metadata.

# Basic metadata

Metadata extraction has been a [long standing complaint](http://feedback.mendeley.com/forums/4941-mendeley-feedback/suggestions/80955-improve-quality-of-automatic-metadata-extraction?ref=title) of many, many Mendeley users. One might at least partially chalk it up to the obsessive-compulsive nature of researchers: we want our metadata to be right, and having to manually fix essentially every paper we import our database creates a huge extra cost on using Mendeley.

How correct should this metadata be?

- If I search by author name, the list of papers I get should be equal in quality to that author’s listing of publications on his personal website. (Name de-duplication is pretty terrifying: Mendeley gives no indication of what permutation of a name is likely to be the right one and gives no advice about whether or not initials or full names should be preferred). As a prospective graduate student with some degree of specialization, knowing which authors I have the most papers of gives hints as to what departments I may be interested in.
- If I search by conference name, the list of papers I get should be equal in quality to the table of contents for that conference’s proceedings. Furthermore, there should be continuity over the years, so that I can search for all-time ICFP, not just 2011’s proceedings. Conferences have a notoriously large number of permutations of spellings (is it ICFP or International Conference for Functional Programming or...), and with no canonicalization keeping these straight is hopeless.

# Funding metadata

Here is something that would be really interesting to extract from papers, that is not readily available anywhere on the Internet: *who funded the piece of work*, and under what program! Researchers are obligated to acknowledge their funding source in a footnote or a section at the end of their paper, and access to this metadata would let me know “Who funds most of the research in this area” or “What grants should I be looking at for the next funding cycle.” This information is, of course, currently passed down as folklore from advisor to advisee. Extracting, polishing and publishing this data would be an interesting endeavor in its own right.

# Social metadata

Papers don’t exist in a vacuum. On the trivial level, any paper is frequently accompanied by a slide deck or a recorded video of a talk—Mendely should track that. But on a deeper level, I’m sure many academics would die from loneliness if they were the only ones working in their field. There is a person behind the paper, and there is more information about them than just what papers they have published. What organization are they are part of? (A prospective grad student would love to know if they are faculty at a particular university.) What organization were they a part of when they published the paper? Who tends to collaborate with who—are there cliques of academics? Who was a member of the panel for what conferences?

One critical component of this is the *citations* of papers. Existing paper databases have concluded that this curation problem is just too hard: they simply publish plain text extracts of the reference sections. But here is a perfect opportunity to harness the power of the social Internet. Wikify the damn metadata, and let academics with an interest in this sector of academia create the mythical hyperlinked web of papers. When I can browse papers like I can browse Wikipedia will be a happy day. *No paper exists in a vacuum.*

# Personal metadata

Mendeley does a great job letting you attach metadata to a specific paper, which is searchable but not much else. But there are plenty of small chunks of information that could be profitably converted into generalized schemes. For example, Mendeley currently stores a bit indicating whether or not a paper is “read” or not. This is woefully insufficient: one does not simply *read* an academic paper (nor does one simply walk into Mordor). Maybe you’ve read the abstract, maybe you’ve skimmed the paper, maybe the paper is one that you are actively attempting to understand as part of some of your research. This workflow should be made first-class and the relevant user interface for it exposed. This information is also important if you’re trying to draw inferences about your paper reading habits from Mendeley’s database: papers you’ve imported but never looked at probably shouldn’t be counted.

# Conclusion

In many ways, this is the call for the equivalent of the “Semantic Web” for academic papers. By in large, we are still attempting to realize this vision for the Internet at large. But there are good reasons to believe that the world of academic papers may be different. For one thing, the web of academic papers has not even reached the point of the traditional Internet: it doesn’t have hyperlinks! Additionally, academic papers are much slower moving than traditional web content and far more permanent. I want there to be no reason for my friends in industry to complain that they’d much rather a researcher publish a blog post rather than a paper. It’s ambitious, I know. But that’s my vision for Mendeley.
