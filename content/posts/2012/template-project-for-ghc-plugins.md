---
title: "Template project for GHC plugins"
date: 2012-09-28 18:56:20
slug: template-project-for-ghc-plugins
categories: [GHC]
---

There is a bit of scaffolding involved with making Core-to-Core transforming GHC plugins, so I made a little project, based off of [Max Bolingbroke’s examples](https://github.com/thoughtpolice/strict-ghc-plugin), which is a nice, clean template project which you can use to create your own GHC plugins. In particular, it has documentation and pointers to the GHC source as well as a handy-dandy shell script `rename.sh MyProjectName` which will let you easily rename the template project into whatever name you want. You can [find it on GitHub](https://github.com/ezyang/ghc-plugin-template). I’ll probably be adding more to it as I go along; let me know about any bugs too.
