---
title: "How to embed Ott in your papers... beautifully!"
date: 2014-01-29 17:10:48
slug: how-to-embed-ott-in-your-papers-beautifully
draft: true
categories: [Miscellaneous]
---

If you’re writing papers about programming languages, at some point, you may find it useful to use [Ott](http://www.cl.cam.ac.uk/~pes20/ott/) to help you typeset your definitions and rules. However, there is a little bit of a gap between the [default LaTeX generates](https://github.com/ghc/ghc/blob/master/docs/core-spec/core-spec.pdf?raw=true), and the format you might want to put inside an academic paper. In this post, I’d like to document some of the tricks for getting Ott into the format you want.

1.  **Integrate Ott into your build-system.** Provide a Ott-only target as well as paper target. But version the generated output.
2.  **Utilize Ott’s provided options for TeX output.** `-tex_show_meta` `-tex_name_prefix`
3.  **Customize styling by redefining some of Ott’s built-ins.** Anatomy of an Ott definition.

renewcommand{ottusedrule}\[1\]{\[#1\]\\-1ex\]} renewenvironment{ottdefnblock}\[3\]\[\]{ \#3 }{} letoldottprodlineottprodline renewcommand{ottprodline}\[6\]{oldottprodline{#1}{#2}{#3}{}{#5}{#6}} newcommand{gram}\[1\]{ottgrammartabular{#1ottinterrule}} renewcommand{ottpremise}\[1\]{ \#1 hspace{2em}} renewcommand{ottdrule}\[4\]\[\]{{displaystylefrac{begin{array}{l}#2unskipend{array}}{#3}quadottdrulename{#4}}} renewcommand{ottcomplu}\[5\]{overline{#1}} letoldottdrulenameottdrulename

renewcommand{smallstepusedrule}\[1\]{#1 \\1ex\]} renewenvironment{smallstepdefnblock}\[3\]\[\]{ \#3 }{} letoldsmallstepprodlinesmallstepprodline renewcommand{smallstepprodline}\[6\]{oldsmallstepprodline{#1}{#2}{#3}{}{#5}{#6}} newcommand{newgram}\[1\]{smallstepgrammartabular{#1smallstepinterrule}} renewcommand{smallsteppremise}\[1\]{ (#1) hspace{2em}} renewcommand{smallstepdrule}\[4\]\[\]{#3 & \#2} renewcommand{smallstepcomplu}\[5\]{overline{#1}} letoldsmallstepdrulenamesmallstepdrulename

renewcommand{newrulesusedrule}\[1\]{\[#1\]\\-1ex\]} renewenvironment{newrulesdefnblock}\[3\]\[\]{ \#3 }{} letoldnewrulesprodlinenewrulesprodline renewcommand{newrulesprodline}\[6\]{oldnewrulesprodline{#1}{#2}{#3}{}{#5}{#6}} renewcommand{newrulespremise}\[1\]{ \#1 hspace{2em}} renewcommand{newrulesdrule}\[4\]\[\]{{displaystylefrac{begin{array}{l}#2unskipend{array}}{#3}}} renewcommand{newrulescomplu}\[5\]{overline{#1}} letoldnewrulesdrulenamenewrulesdrulename

4.  **Don’t edit the generated TeX.**
