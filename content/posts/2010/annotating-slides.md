---
title: "Annotating slides"
date: 2010-09-03 09:00:53
slug: annotating-slides
categories: [Toolbox]
comments:
    - id: 1123
      author: Thomas
      date: "2010-09-04 09:01:58"
      content: |
        Clever solution, but seems overly complex to me. If you use latex-beamer then you can just include the notes into your LaTeX source file for the presentation which puts everything together in one place with the added benefit of making it much easier to keep slides and notes consistent should you decide to change either one (the old problem of specification vs. implementation from programming all over again in a different guise).
        
        What I was wondering: How did you create your PDF slides? Looks handwritten/-drawn to me. So I assumed you would use some WYSIWYG tool. But that would have also let you add annotations in one go so my assumption was apparently wrong.
    - id: 1125
      author: Edward Z. Yang
      date: "2010-09-04 11:29:31"
      content: "Heya Thomas! I hand drew my slides with Xournal, so there was no easy built-in \"annotation\" mechanism. So adding annotations after the fact was easier for me."
---

A little trick for your toolbox: after you’ve generated your slide deck and printed it out to PDF, you might want to annotate the slides with comments. These is a good idea for several reasons:

- If you’ve constructed your slides to be text light, they might be optimized for presentation but not for reading later on. (“Huh, here is this diagram, I sure wish I knew what the presenter was saying at that point.”)
- Writing out a dialog to go along the slides is a nonvocal way of practicing your presentation!

But how do you interleave the slide pages with your annotations? With the power of `enscript` and `pdftk`, you can do this entirely automatically, without even having to leave your terminal! Here’s the recipe.

1.  Create an “annotations” text file (we’ll refer to it as `annot.txt`). This will contain your text commentary to accompany the slides. Write the text explaining your first slide, and then insert a *form feed* (`^L`, you can do so by pressing `C-l` in vim (insert mode) or `C-q C-l` in emacs.) Write the text for your second slide. Rinse and repeat.

2.  We now want to render this into a PDF file, with the same dimensions as your slide deck. Figure out what the size of your slides are in pixels, and then edit your `~/.enscriptrc` to contain the following line:

        Media: Slide width height llx lly urx ury

    where ll stands for lower left and ur stands for upper right: these four numbers denote the bounding box for the text. One possible combination for these might be:

        Media: Slide 576 432 18 17 558 415

    We can now invoke enscript to generate a nicely formatted PostScript file of our annotations in the right dimensions, with `enscript annot.txt -p annot.ps -M Slide -B -f Palatino-Roman14` (pick a different font, if you like.)

3.  Convert the resulting PostScript file into a PDF, with `ps2pdf annot.ps`.

4.  Now, with pdftk, we will split our annotations PDF and our slides PDF into individual pages, and then merge them back together into one PDF. We can use `burst` to output the pages, suggestively naming the output files so they interleave correctly:

        mkdir stage
        pdftk slides.pdf burst output stage/%02da.pdf
        pdftk annot.pdf burst output stage/%02db.pdf

    and then we join them back together:

        pdftk stage/*.pdf cat output annotated-slides.pdf

Here’s the full script:

    #!/bin/sh
    set -e
    ANNOT="$1"
    SLIDES="$2"
    OUTPUT="$3"
    if [ -z "$3" ]
    then
        echo "usage: $0 annot.txt slides.pdf output.pdf"
        exit 1
    fi
    TMPDIR="$(mktemp -d)"
    enscript "$ANNOT" -p "$ANNOT.ps" -M Slide -B -f Palatino-Roman14
    ps2pdf "$ANNOT.ps" "$ANNOT.pdf"
    pdftk "$SLIDES" burst output "$TMPDIR/%03da.pdf"
    pdftk "$ANNOT.pdf" burst output "$TMPDIR/%03db.pdf"
    pdftk "$TMPDIR"/*.pdf cat output "$OUTPUT"
    rm -Rf "$TMPDIR"

Don’t forget to define `Slide` in your `.enscriptrc`, and happy annotating!
