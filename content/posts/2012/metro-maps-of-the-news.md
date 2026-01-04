---
title: "Metro Maps of the News"
date: 2012-12-13 04:44:40
slug: metro-maps-of-the-news
categories: [Visualization]
comments:
    - id: 5344
      author: Anonymous
      date: "2012-12-13 09:24:51"
      content: "I like the suggestion of using this for scientific papers. Do you have an example of that in use?"
    - id: 5346
      author: Edward Z. Yang
      date: "2012-12-13 14:18:11"
      content: |
        Dafna wrote a paper about applying it to that domain: http://www.cs.cmu.edu/~dshahaf/kdd2012-shahaf-guestrin-horvitz.pdf
        
        But as far as a researcher is concerned, the paper is a nice proof of concept but doesn't actually give me anything that I can *use*, alas!
    - id: 5938
      author: Anonymous
      date: "2013-01-22 10:34:54"
      content: |
        I see a bunch of links to papers (which is insightful reading nevetheless) but I do not see any code. Is there opensource implementation of this metro-maps algorythm available somewhere?
        And same question goes for visualazer - because without an actual code it's pretty much worthless.
    - id: 5941
      author: Edward Z. Yang
      date: "2013-01-22 16:20:29"
      content: |
        Ooops. The code lives here: https://github.com/ezyang/metromaps
        
        Our layout algorithm is really fast (i.e. realtime) but as a result, it needs some human input to get out of local optima. If you play around with the debug mode http://metro.ezyang.com/#debug some of the quirks should become clear. Some really good future work would be to figure out how to get the layout algorithm user friendly.
    - id: 5992
      author: Anonymous
      date: "2013-04-01 09:39:55"
      content: |
        Hello,
        Can you share the layout algorithm with us. I see in the sample that X and Y coordinates are already present in the JSON file. Is that a requirement or do we have something that can generate the X and Y co-ordinates at the runtime based on the data in JSON?
        
        Thank you.
        VT
    - id: 6059
      author: Edward Z. Yang
      date: "2013-04-29 05:20:57"
      content: "I posted a video about how to use the layout algorithm. It requires some human input. http://www.youtube.com/watch?v=J9FHip7-c5I"
---

Metro maps are a visual metaphor for complex, interdependent story lines developed by [Dafna Shahaf](http://www.cs.cmu.edu/~dshahaf/). Dafna’s thesis involved techniques for automatically taking a corpus of news articles and extracting a coherent narratives that covered the overall space. For our final [CS448b](https://graphics.stanford.edu/wikis/cs448b-12-fall/) project, we took one of the narratives Dafna had generated and created [a system for displaying the maps.](http://metro.ezyang.com) (The demo is best viewed on a large monitor.)

![image](/img/metromap.png)

We only had enough time to get the viewer aspect polished, but we think that it would not be too difficult to extend this framework for the *construction* of metro maps (in case you don’t have access to Dafna’s algorithm).

This is joint work with Russell Chou and Jacob Jensen.
