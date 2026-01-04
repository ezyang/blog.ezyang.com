---
title: "rxvt-unicode for gnome-terminal refugees"
date: 2010-01-06 12:00:24
slug: rxvt-unicode-for-gnome-terminal-refugees
categories: [Toolbox]
comments:
    - id: 20
      author: roy_hu
      date: "2010-01-10 23:58:52"
      content: "Looks like smart-resize is a configure-time option, not a run-time option."
    - id: 21
      author: Edward Z. Yang
      date: "2010-01-11 00:13:09"
      content: "So it is! I've modified my post accordingly; though, I swear that the behavior got better when I set smartResize. Curious!"
    - id: 58
      author: verma
      date: "2010-01-26 13:01:47"
      content: "Hey, thanks for doing this.  I feel like home with urxvt now :)"
    - id: 889
      author: ovejorock
      date: "2010-08-11 18:04:07"
      content: "Great, thanks :D"
    - id: 2054
      author: Jesper Wallin
      date: "2011-04-14 03:49:27"
      content: "Whoa! You just saved my day.. Thanks a ton! :D"
    - id: 2504
      author: Justin Painter
      date: "2011-05-25 16:49:00"
      content: This was a huge help...I just switched from gnome to ion and I thought I was going to have to spend all afternoon toying with color codes to get something usable.
    - id: 2901
      author: zer0thruster
      date: "2011-08-13 12:45:19"
      content: "Cheers for the gnome-terminal color scheme! My eyes thank you."
    - id: 3957
      author: qo
      date: "2012-08-16 01:08:58"
      content: |
        Thank you Edward!   Switching desktops started to feel like a slap in the face.  And forget about trying to do a quick visual diff by rapidly switching between desktops.
        
        gnome-terminal, we barely knew ye.
        
        To complete my transition, the following* mimics gnome-terminal's default behavior when scrolling back in the buffer (i.e. new output doesn't cause a jump to the bottom).
        
        URxvt*scrollTtyOutput: false
        URxvt*scrollWithBuffer: true
        URxvt*scrollTtyKeypress: true
        
        
        * Thanks to kazuo, here: https://bbs.archlinux.org/viewtopic.php?id=57823
    - id: 3997
      author: Tom Ryder
      date: "2012-08-19 04:59:47"
      content: "Great stuff, exactly what I needed to move away from xfce4-terminal for good. Much obliged!"
    - id: 5947
      author: Todd
      date: "2013-01-29 16:26:57"
      content: "how do you remove the menubar from the top of urxvt ?"
    - id: 5948
      author: Edward Z. Yang
      date: "2013-01-29 16:28:47"
      content: "Mmm, it shouldn't have one by default."
    - id: 20453
      author: Michael
      date: "2016-01-18 13:57:12"
      content: "Thank you for moving gnome-terminal's color scheme to urxvt! You saved me so much pain."
    - id: 27960
      author: "Thanks! This is beautiful!"
      date: "2022-07-31 10:40:08"
      content: "you saved me!"
---

When I switched from Ubuntu's default Gnome desktop to the tiling window manager [Xmonad](http://xmonad.org/), I kept around Gnome Terminal, although with the menu bar and the scroll bars removed. I changed from the default white to a nice shade of \#2B2B2B (a hue that [Sup](http://sup.rubyforge.org/) originally introduced me to).

Over the months, however, I got increasingly annoyed at the slowness at which Gnome Terminal rendered when I switched windows (a not uncommon task in a tiling window manager, made especially important when you have a relatively small screen size); the basic symptom was the screen would flash white as the old terminal left and the new one was being drawn. After testing xterm and finding that it did *not* flash when I switched screens, I hereby resolved to find a faster terminal emulator; on the advice of David Benjamin I finally settled on rxvt-unicode, also known as urxvt.

rxvt-unicode is part of a proud tradition of X terminal emulators, so its settings are managed by the X window manager (as opposed to gnome-settings, which gnome-terminal used). You can manipulate the settings at runtime using a program named `xrdb`; but I found it mostly easier to place the settings I wanted in `.Xdefaults`, which automatically gets loaded at session start. The syntax is simple: a newline-separated file, with the form `Appname*option: value`. Appname in the case of rxvt-unicode is `URxvt`.

Having used gnome-terminal for a long time, I was somewhat loathe to part with the colors and behaviors I'd come to love. So here is my `.Xdefaults` file, with notes about what the various bits do:

    URxvt*background: #2B2B2B
    URxvt*foreground: #DEDEDE
    URxvt*font: xft:Monospace:pixelsize=12
    URxvt*boldFont: xft:Monospace:bold:pixelsize=12
    URxvt*saveLines: 12000
    URxvt*scrollBar: false
    URxvt*scrollstyle: rxvt

These parts are all fairly self-explanatory; rxvt-unicode supports anti-aliased fonts, which means bold text looks good (one of the primary reasons I couldn't stand xterm, since bold fonts tend to bleed into each other without anti-aliasing).

    URxvt*perl-ext-common: default,matcher
    URxvt*urlLauncher: firefox
    URxvt*matcher.button: 1
    URxvt*colorUL: #86a2be

These lines implement clickable URLs inside your terminal. The launcher doesn't give any visual cue in your cursor when a link is clickable, but I find the underlining and change in color to be enough change.

    ! black
    URxvt*color0  : #2E3436
    URxvt*color8  : #555753
    ! red
    URxvt*color1  : #CC0000
    URxvt*color9  : #EF2929
    ! green
    URxvt*color2  : #4E9A06
    URxvt*color10 : #8AE234
    ! yellow
    URxvt*color3  : #C4A000
    URxvt*color11 : #FCE94F
    ! blue
    URxvt*color4  : #3465A4
    URxvt*color12 : #729FCF
    ! magenta
    URxvt*color5  : #75507B
    URxvt*color13 : #AD7FA8
    ! cyan
    URxvt*color6  : #06989A
    URxvt*color14 : #34E2E2
    ! white
    URxvt*color7  : #D3D7CF
    URxvt*color15 : #EEEEEC

I absolutely adore gnome-terminal's color scheme, which is a bit more subdued than the rxvt default. So in it goes. The first color is "normal"; the second color is "bright."
