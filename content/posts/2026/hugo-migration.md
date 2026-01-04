---
title: "Hugo Migration"
date: '2026-01-04T09:12:54-05:00'
slug: hugo-migration
categories: [AI Coding]
---

This blog has lived on WordPress since it was [initially created](https://blog.ezyang.com/2009/12/iron-blogger/) during a social challenge at MIT to write a blog post a week or pay up with beer.  I remember a very important piece of advice I had been given at that time: don't fuck around with your blog authoring software, just do the minimum viable thing (use Wordpress) and focus on writing posts.

It's 2026 now, the world is different, and in particular the existence of coding agents means that this particular advice falls flat now: it has never been easier to vibe code your own blog software and be done in an afternoon of token generation.  Similarly, over the years, I had been increasingly unhappy about my WordPress setup (too hard to add images, ancient version of WordPress, Markdown has taken over the world why am I still writing in ReST, I love scripts.mit.edu but I definitely don't want to use it to host serious things).  So I typed this into ChatGPT and Claude and asked it what I should migrate too.

> I currently have a Wordpress blog whose 633 posts are written in ReST using rest-wordpress with some manual code edits, and a theme based on Ashley that I also customized. I'd like to migrate to another blogging solution. I care a lot about ensuring the URLs are preserved. To a lesser extent, I also care about the comments, although I'm willing to compromise here (e.g., an offline flow where I have to explicitly publish comments might be OK; I know static site is difficult to support comments; I also know that email newsletter is popular and I'd like to support this modality if possible. I don't use a WYSIWYG editor. It's on Wordpress 5.1.19. It would be nice to have a way for people. Some more niche things plugins I've used is WP LaTeX and Share a Draft but I'm willing to do a lossy conversion if necessary (I don't use LaTeX that much now; it's just important to make sure the old posts still format correctly). Many of my posts have images and I'd like an easier flow than my current flow (where I have to manually upload my images to my server and then hyperlink them into the post). What do you recommend?

It suggested Hugo, which I had played around with before in [AI Blindspots](https://ezyang.github.io/ai-blindspots/), and I figured, "Why not, I'll just ask Claude to do the migration.  A few hours later, two pro sessions worth of tokens and some PHP export scripts, the entire blog was moved over, no muss, no fuss.  I [live streamed a portion of this migration process](https://youtu.be/ak2-Zs2wJX4) although there's nothing that special about it.

I actually wasn't going to write a blog post about this, but I saw [Jeff Geerling's blog](https://www.jeffgeerling.com/blog/2026/migrated-to-hugo/) also had made frontpage Hacker News.  I too haven't figured out how I am going to solve the comments problem on the new format; I also think I will figure out how to get an email newsletter going from the blog.  Here's to seeing if this can encourage you to use LLMs to make the jump for your own personal site!
