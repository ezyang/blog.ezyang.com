---
title: "How to use Vim's textwidth like a pro"
date: 2010-03-05 09:00:53
slug: vim-textwidth
categories: [Toolbox]
comments:
    - id: 134
      author: Andrey Popp
      date: "2010-03-05 12:01:54"
      content: "Yeah, the last one trick I was looking for very long, thanks."
    - id: 135
      author: Mr. Icon
      date: "2010-03-05 12:44:42"
      content: |
        I find fo=want to be useful when working on RST documents or emails. Easy mnemonic, too.
        
        :set fo=want
    - id: 136
      author: steve
      date: "2010-03-05 14:24:09"
      content: |
        <i>vap is also equivalent, although it selects a whole paragraph and is more appropriate if you want to, say, delete it.</i>
        
        What is the difference between vip and vap? I couldn't find one when testing, and the help entries for 'ap' and 'ip' are the same.
    - id: 137
      author: Edward Z. Yang
      date: "2010-03-05 15:11:31"
      content: "Good question! The difference is only visible if you have a blank line(s) after your paragraph. vip (inner) will not select the trailing blank newline; vap (all) will select all trailing newlines. This is most obvious if you have a paragraph afterwards; vipd will leave extra whitespace; vapd will not."
    - id: 138
      author: steve
      date: "2010-03-05 16:06:12"
      content: |
        <i> vip (inner) will not select the trailing blank newline; vap (all) will select all trailing newlines.</i>
        
        Hm. I didn't see that because I tested with the following:
        ____
        FIRST\n
        \n
        SECOND
        --------
        
        On the  F in FIRST, 'yipp' and 'yapp' both result in
        
        FIRST\n
        FIRST\n
        \n
        SECOND
        
        rather than 
        
        FIRST\n
        \n
        FIRST\n
        \n
        SECOND
        
        Although, I suspect that's not got anything to do with 'ap' and 'ip' sometimm, but instead is that the rules of 'p' are more complicated than I think they are.
    - id: 139
      author: Vikas
      date: "2010-03-05 23:22:39"
      content: |
        @steve:
        My expected result of "yapp" in the case mentioned by you is:
        FIRST\n
        FIRST\n
        \n
        SECOND
        \n
        
        To get the result you expect, you should do "yapjp" instead
    - id: 147
      author: mikisvaz
      date: "2010-03-09 15:41:54"
      content: I use gqap to format a paragraph
    - id: 181
      author: Marc
      date: "2010-03-14 01:41:35"
      content: "Hi and thanks for this article, the last trick is really good (the highlight OverLength one), but how could I have a function highlight the overlength chars, and when I call this function again stop highlighting them ? because it's annoying to highlight these chars always, as for some config files there a lots of long lines and I don't need them highlighted. Thx anyway, keep posting good &amp; deep tips (esp. deep ones, going thoroughly on one special feature of our favorite editor)."
    - id: 199
      author: Edward Z. Yang
      date: "2010-03-15 21:14:59"
      content: "I don't know, and I'd love to know the answer if there is one. I suspect this has to do with autocommand naming and selective disabling."
    - id: 769
      author: Jithin
      date: "2010-07-28 06:46:21"
      content: |
        Hi,
        For some reason The highlighting part is not happening for me.
        
        I have these in my vimrc
        
        set textwidth=80
        set fo-=t "prevent auto word wrapping
        augroup vimrc_autocmds
            autocmd BufRead * highlight OverLength ctermbg=darkgrey guibg=#592929
            autocmd BufRead * match OverLength /\%80v.*/
        augroup END
        
        Can you help me out in what is going wrong ?
        
        augroup END
    - id: 775
      author: Edward Z. Yang
      date: "2010-07-28 13:31:43"
      content: "It looks like the BufRead command only triggers when you edit an old file. Try BufEnter; I've updated my post accordingly."
    - id: 3395
      author: William Robertson
      date: "2012-01-31 10:50:33"
      content: "Thanks for the tip! I'd come across textwidth before but never really got to the bottom of it. In particular I found that even when I set tw=0 in my _vimrc, reloading _vimrc itself or any .vim file would set it back to 78 - a bit short for me since I no longer use punch cards or a vt100 monitor. It turned out that the copy of vim.vim in ftplugin (there seem to be several) was the culprit. Copied that to my local vimfiles\\ftplugin, edited it, problem solved :)"
    - id: 3657
      author: Todd Eddy
      date: "2012-04-18 15:36:20"
      content: |
        Just wanted to say thanks for pointing me in right direction (formatoptions) so here's a tip that's different than your "one more neat trick".
        
        In vim 7.3 this is built in.  Place this in your vimrc (not sure if this will format as code so just indent everything between the start and end if to make it look pretty)
        <code>
        " Vim v7.3 settings·                                                           
        if v:version &gt;= 703                                                            
          " Mark ideal text width (set by textwidth)                                   
          set colorcolumn=+1                                                           
        endif
        </code>
        
        now you'll see a bar where the ideal width should be.  Problem I was running into is in programming the autowrap doesn't work too well.  So I have that individual filetype set "formatoptions-=t" so it doesn't auto wrap code but it will still autowrap comments which is what I want.
        
        Rest of my vim stuff if people are curious: https://github.com/vrillusions/dotfiles
    - id: 3821
      author: Faheem
      date: "2012-06-11 13:22:50"
      content: "Thanks! Toggling fo was what I needed. However, regarding using 'vip' for paragraph selection, how can I follow it up with a 'comment (#) the selected paragraph?' Thanks"
    - id: 3822
      author: Faheem
      date: "2012-06-11 14:10:32"
      content: |
        Figured that a simple vmap in my vimrc takes care of the 'comment' bit:
        
        " comment/uncomment blocks of code (in vmode)
        vmap ## :s/^/#/gi:nohl
    - id: 6034
      author: Phil
      date: "2013-04-16 01:08:18"
      content: |
        Since vim 7.3, you can also use:
        
        set colorcolumn=+1
        
        To set a vertical line on {textwidth}+1
        
        <b>Edward:</b> This is a really great suggestion, and what I use now!
    - id: 6257
      author: Jason
      date: "2013-10-05 14:34:58"
      content: "gqip is my new favorite compound command! I've been looking for this for a while!"
    - id: 6323
      author: "Customize Vim | Serenity"
      date: "2013-12-17 21:50:21"
      content: "[&#8230;] for automatically wrapping, there was a good article introducing textwidth and formatoption. However, strangely that they seems no effect on my [&#8230;]"
    - id: 6355
      author: ipirlo
      date: "2014-01-19 00:43:58"
      content: "@Faheem, try the pluggin tcomment or nerd commenter,  quite a few useful commenting commands"
    - id: 6377
      author: arturo
      date: "2014-02-05 16:09:39"
      content: "Long time i was searching something like this, thanks very much!!"
    - id: 6509
      author: Chris Corbyn
      date: "2014-03-10 03:50:31"
      content: |
        set fo+=a
        
        This will give you much more expected behaviour when you are editing in the middle of a line (the entire paragraph will be reformatted as you type).
    - id: 13325
      author: Smoneck
      date: "2015-04-03 16:04:53"
      content: "Awesome! \"set fo+=a\" is just what I've searched for!"
    - id: 13362
      author: ymukhin
      date: "2015-04-06 12:55:45"
      content: "How can I see the current tw setting used by VIM?"
    - id: 13363
      author: Edward Z. Yang
      date: "2015-04-06 13:12:44"
      content: "Just \"<code>:set tw</code>\"."
    - id: 15020
      author: Phil Goetz
      date: "2015-07-10 10:50:17"
      content: "One more detail:  You must NOT have 'l' in your formatoptions if you want text wrapping."
    - id: 20020
      author: zz
      date: "2015-12-27 01:31:34"
      content: |
        " my setting
        set fo+=1rnojmBl
        set fo-=t
        set tw=72
        set colorcolumn=80  "Mark ideal textwidth, I perfer fixed colorcolumn not "+1"
        
        'mB' is for multi-bytes language
        'l' is also needed in my case, used to diable auto-formatting when editing, only done with 'gq'
        'ro' is for more intelligent formatting on comments
        '1n' if for more intelligent formatting on lines start with number sequence
        
        check :h formatoptions and :h fo-table for more detailed explanation
    - id: 20895
      author: Baptiste Wicht
      date: "2016-06-22 08:29:52"
      content: "Hi. Instead of doing \"vipgq\", why don't you simply do \"gqip\" ?"
    - id: 21124
      author: Smithe448
      date: "2016-08-20 03:24:34"
      content: "Hey very nice web site!! Man.. Beautiful.. Wonderful.. I'll bookmark your web site and take the feeds alsoKI am satisfied to seek out a lot of helpful information here in the publish, we want work out extra techniques in this regard, thank you for sharing cfddeaeafkagekda"
    - id: 21841
      author: Barry Gold
      date: "2017-02-26 20:40:10"
      content: |
        I have
        set tw=75
        set fo+=tcjro
        
        and I'm still not getting c- or C++- style comment (/*...*/ or //...) wrapped.
        
        How do I make these comments wrap?
    - id: 21842
      author: Barry Gold
      date: "2017-02-26 20:42:11"
      content: |
        Let me clarify: I'm seeing for example
        // boinon 4890jht b0di8n jhbxui hgkjd nj oingb uiounbjkdsfniogh oignoi oign
        oin
        
        The second line (starting with "oin") should have a // in front of it.
    - id: 21857
      author: Edward Z. Yang
      date: "2017-03-06 21:33:53"
      content: "Barry Gold: On an EMPTY vimrc (i.e. using vim -u NONE), I need set fo+=cro. See also  http://vimdoc.sourceforge.net/htmldoc/change.html#fo-table for the meaning of each of these format options. There might be some interaction with other plugins you have installed that is causing you problems."
    - id: 21876
      author: Sergey
      date: "2017-03-20 16:24:10"
      content: "Also one command I found useful is <code>gw</code>, which is the same as <code>gq</code> but you cursor left in the same place where it was."
    - id: 23195
      author: "How to use Vim’s textwidth like a pro : Inside 245-5D"
      date: "2019-11-18 15:32:52"
      content: "[&#8230;] How to use Vim&rsquo;s textwidth like a pro : Inside 245-5D [&#8230;]"
    - id: 23385
      author: Anonymous
      date: "2020-01-14 13:42:16"
      content: "WTF is cqt?"
    - id: 23953
      author: "Vim syntax coloring: How do I highlight long lines only? - Nirwana Room"
      date: "2020-07-13 02:14:13"
      content: "[&#8230;] How to use Vim&#8217;s textwidth like a pro [&#8230;]"
    - id: 24769
      author: Anonymous
      date: "2020-12-02 17:27:03"
      content: |
        I have a problem with this.
        If you set a file like bufenter *.c ... and after you edit a file with :e, the configuration keeps highlighting the rule. there is way to set bufenter default? or disable it
    - id: 26030
      author: Jonathan Hartley
      date: "2021-05-19 12:35:33"
      content: |
        I prefer the overlength line highlights shown here, instead of cursorcolumn, because this method only highlights lines that are actually too long, so is visually silent if nothing is wrong.
        
        Someone above asked about a function to toggle the highlights. I've been wrestling with it for a while this morning, and I don't really know what I'm doing, but this seems to work. I'd love to hear if it can be sensibly abbreviated.
        
        ```
        " Toggle color highlight on 80th character
        highlight OverLength ctermbg=darkgrey ctermfg=white guibg=#292929
        fun! LongLineHighlightInit()
            if !exists("w:llh")
                call LongLineHighlightOn()
            endif
        endfunction
        fun! LongLineHighlightOn()
            let w:llh = matchadd("OverLength", '\%80v.')
        endfunction
        fun! LongLineHighlightOff()
            call matchdelete(w:llh)
            let w:llh = 0
        endfunction
        fun! LongLineHighlightToggle()
            if !exists("w:llh") || w:llh == 0
                call LongLineHighlightOn()
            else
                call LongLineHighlightOff()
            endif
        endfunction
        augroup LongLineHighlight
            autocmd BufWinEnter * call LongLineHighlightInit()
        augroup end
        nnoremap  8 :call LongLineHighlightToggle()
        ```
    - id: 26031
      author: Jonathan Hartley
      date: "2021-05-19 12:40:36"
      content: "ohdear code doesn't survive comment formatting very well. :-) The final line has a &lt;silent&gt; &lt;Leader&gt; , and ends with &lt;CR&gt; ."
    - id: 31142
      author: Kevin C
      date: "2023-12-13 11:41:06"
      content: "Thank you for the article. It takes a while to learn everything there is to know about using vi(m). I know a lot of the common commands and some not so common ones. Reading articles like this tell me things I didn't know. I try them into practice as I learn them instead of trying to learn it all at once. The gq command will be useful. I can now wrap an entire document by using gqG. I have also now learned of vip and vap. Those are also useful additions to my knowledge of the program."
---

There are lots of little blog posts containing advice about various one-line options you can do in Vim. This post falls into that category, but I'm hoping to do a more comprehensive view into one small subsystem of Vim's configuration: automatic line wrapping.

When programming, automatic line wrapping can be a little obnoxious because even *if* a piece of code is hanging past the recommended 72/80 column width line, you probably don't want to immediately break it; but if you're writing a text document or an email message, that is specifically the behavior you want. By default, vim does no automatic line wrapping for you; turning it on is a question of being able to toggle it on and off when you want it.

Here are the configuration options you care about:

- *textwidth* (or *tw*): controls the wrap width you would like to use. Use `:set tw=72` to set the wrap width; by default it's unset and thus disables line-wrapping. If this value is set, you're entirely at the whimsy of the below *formatoptions*, which is often *filetype* sensitive.
- *formatoptions* (or *fo*): controls whether or not automatic text wrapping is enabled, depending on whether or not the `t` flag is set. Toggle the flag on with `:set fo+=t`, and toggle it off with `:set fo-=t`. There are also a number of auxiliary format options, but they're not as important.
- *wrapmargin* (or *wm*): controls when to wrap based on terminal size; I generally find using this to be a bad idea.

Understanding the interaction between these two options is important. Here is a short table of interactions:

- *tw=0 fo=cq wm=0*: No automatic wrapping, rewrapping will wrap to 80
- *tw=72 fo=cq wm=0*: No automatic wrapping, rewrapping will wrap to 72
- *tw=0 fo=cqt wm=0*: No automatic wrapping, rewrapping will wrap to 72
- *tw=0 fo=cqt wm=5*: Automatic wrapping at a 5 col right margin
- *tw=72 fo=cqt wm=0*: Automatic wrapping at col 72

Notice that to get automatic wrapping you need both *fo+=t* as well as *tw* or *wm* to be nonzero. Note also that some *filetype* will automatically give you *fo+=t*, while others won't.

Here are the keystrokes you care about:

- *gq*: performs a "formatting operation", which in our universe means "rewrap the text." This will respect leading indent and symbolic characters, which is usually nice but a little obnoxious if you're reflowing a bullet point (since the text will suddenly acquire asterisks in front of everything).
- The paragraph motions. The big one is *vip* (preceding *v* puts us in visual mode, for selection), which selects an "inner paragraph"; this means that if you're anywhere inside of a paragraph, you can type *vip* and have the entire thing instantly selected for you, possibly for you to run *gq* subsequently. *vap* is also equivalent, although it selects a whole paragraph and is more appropriate if you want to, say, delete it. The curly braces move you between paragraphs.

The value of *format-options* will drastically change the way Vim behaves, so I highly recommend keeping it displayed some where you can reference it quickly. I use:

    set statusline=...[%{&fo}]...

You probably have a statusline of your own; just add that small snippet minus the ellipses in somewhere convenient. For further good measure, I explicitly say `set fo-=t` in my vimrc, to prevent myself from being surprised (since I do primarily coding in vim).

One more neat trick:

    augroup vimrc_autocmds
      autocmd BufEnter * highlight OverLength ctermbg=darkgrey guibg=#592929 
      autocmd BufEnter * match OverLength /\%74v.*/
    augroup END

This will highlight all characters past 74 columns (tweak that number as desired) in dark grey (tweak that color as desired), and is a nice visual cue when auto linewrapping isn't turned on when you should think about breaking things.
