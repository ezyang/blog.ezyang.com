---
title: "Transparent xmobar"
date: 2011-11-28 05:09:32
slug: transparent-xmobar
categories: [Toolbox]
comments:
    - id: 3175
      author: refold
      date: "2011-11-28 11:52:09"
      content: "Nice wallpaper!"
    - id: 3179
      author: kfish
      date: "2011-11-29 00:46:02"
      content: "What on earth is a \"personal statement\"? is \"graduate school\" something kind of dating site?"
    - id: 3181
      author: Edward Z. Yang
      date: "2011-11-29 16:48:23"
      content: "kfish: Nah, that’s what I hear undergrad is for."
    - id: 3199
      author: Jake McArthur
      date: "2011-12-04 15:06:01"
      content: |
        This is a timely patch for me, as I just decided I wanted to try a transparent background today. Just a little bug report. When I set XMobar's position to something aside from Top, the background that XMobar uses is still taken starting from the top left instead of from its actual position.
        
        I tried looking at your patch to see if I could fix it myself, but X stuff is gibberish to me.
    - id: 3200
      author: Edward Z. Yang
      date: "2011-12-04 15:07:49"
      content: "Oh, that's a fascinating bug. I'll see if I can fix that."
    - id: 3780
      author: "This is my Slackware desktop... - Page 105"
      date: "2012-05-26 16:46:59"
      content: "[...] Stalonetray (which, unlike Trayer, works properly when you use xrandr to rotate), a transparent xmobar, and Solarized themes for rxvt-unicode, vim, ls, mc and Eclipse.  [...]"
    - id: 6094
      author: Tux Hat
      date: "2013-05-19 08:06:30"
      content: |
        I tried your git source and it failed to compiled just like what joar got on one of his comments on a github forum.. something about "missing xrendercolor" or something.   It's quite old xmobar 0.14 , i was wondering if this is at all possible with xmobar 0.17 ?
        
        I was just wondering if i can just hack the bloody thing , i don't think its that hard to really get this sorted.  Lack of people doing this or taking any or much interest with Xmobar i do not see any other topic besides what you are doing @ Edward , and i think you work is great, i want to get a tint just like your screenshot up above.
        
        it looks nice if i can somehow set it up to 100/255 &lt; alpha doesn&#039;t work on .xmobarrc say something about unexpected &#039;a&#039; and  i don&#039;t know how to get this sorted :(
        
        any help at all or some info would be very nice :) Thank You!
        
        Tux Hat
    - id: 6102
      author: Edward Z. Yang
      date: "2013-05-22 01:56:58"
      content: "I haven't tried to compile it for a while, it'll take me time before a find a free moment to update things."
    - id: 6170
      author: Edward Z. Yang
      date: "2013-07-07 18:48:06"
      content: "I've updated it to work with the latest xmobar."
    - id: 6311
      author: Arash Rouhani
      date: "2013-12-01 13:11:18"
      content: "Hi Edward. Do you happen to have any dotfiles repository online anywhere? I wonder how you got the graphical icons (battery, wireless) in the first place."
    - id: 6314
      author: Edward Z. Yang
      date: "2013-12-01 19:41:59"
      content: "Arash: You need some tray application. I use 'trayer', and just start it up in my Xsession."
    - id: 6316
      author: Arash Rouhani
      date: "2013-12-02 05:15:08"
      content: "Ah, thank you very much. I also found taffybar (replacing xmobar), tough I can't speak for which one is better."
    - id: 11439
      author: Anonymous
      date: "2014-12-23 03:28:41"
      content: "Nothing seems to have come of that pull request.  Three years on and still no alpha.  I'm running V0.22.1 so I don't want to be dropping your parser, xmobar and utils sources in willy nilly.  Anything we can do to encourage an inclusion into the main branch?"
    - id: 11472
      author: Edward Z. Yang
      date: "2014-12-23 20:00:23"
      content: "I just re-pushed my branch rebased for the latest xmobar. IIRC there was some refactoring that the maintainer wanted before it could be merged in. I haven't done it because I'm lazy, but that probably is how we can get it mainlined."
    - id: 11475
      author: Anonymous
      date: "2014-12-23 22:31:20"
      content: "Hope you can find the time to do that.  It'd be a great feature to have as standard on the main branch."
---

Things I should be working on: *graduate school personal statements.*

What I actually spent the last five hours working on: *transparent xmobar.*

![image](/img/transparent-xmobar.png)

It uses the horrible “grab Pixmap from root X window” hack. You can grab the [patch here](https://github.com/ezyang/xmobar/) but I haven’t put in enough effort to actually make this a configurable option; if you just compile that branch, you’ll get an xmobar that is at 100/255 transparency, tinted black. (The algorithm needs a bit of work to generalize over different tints properly; suggestions solicted!) Maybe someone else will cook up a more polished patch. (Someone should also drum up a more complete set of XRender bindings!)

This works rather nicely with trayer, which support near identical tint and transparency behavior. Trayer also is nice on Oneiric, because it sizes the new battery icon sensibly, whereas stalonetray doesn’t. If you’re wondering why the fonts look antialiased, that’s because I [compiled with XFT support](http://projects.haskell.org/xmobar/#optional-features).

(And yes, apparently I have 101% battery capacity. Go me!)

*Update.* Feature has been prettified and made configurable. Adjust `alpha` in your config file: 0 is transparent, 255 is opaque. I’ve submitted a pull request.
