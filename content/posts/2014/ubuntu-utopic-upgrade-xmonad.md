---
title: "Ubuntu Utopic upgrade (Xmonad)"
date: 2014-12-04 18:48:19
slug: ubuntu-utopic-upgrade-xmonad
categories: [Toolbox]
comments:
    - id: 11273
      author: Robin
      date: "2014-12-18 19:53:29"
      content: "Running unity-settings-daemon rather than gnome-settings-daemon lets me have play/pause again, but not volume controls. Any ideas on that, or is it just part of this same problem?"
    - id: 11274
      author: Edward Z. Yang
      date: "2014-12-18 20:49:09"
      content: |
        Robin: Yes, it's the same problem (though I'm surprised play/pause works: what media player are you using?)
        
        My interim solution has been to use xbindkeys, with this RC:
        
        <pre>
        
        #Volume Up
        "pactl set-sink-volume 0 -- +5%"
            m:0x10 + c:123
            Mod2 + XF86AudioRaiseVolume 
        
        #Volume Down
        "pactl set-sink-volume 0 -- -5%"
            m:0x10 + c:122
            Mod2 + XF86AudioLowerVolume 
        
        #Mute/Unmute
        "pactl set-sink-mute 0 toggle"
            m:0x10 + c:121
            Mod2 + XF86AudioMute 
        
        #Lock Screen
        "gnome-screensaver-command -l"
            m:0x10 + c:160
            Mod2 + XF86ScreenSaver 
        
        #Sleep
        "dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend"
            m:0x10 + c:150
            Mod2 + XF86Sleep 
        </pre>
        
        One downside is you don't get any sound splash saying what the new volume is, nor do you get that nice little "pop" sound which gives you a cue of how soft/loud you are.
    - id: 11883
      author: Robin
      date: "2015-01-04 22:29:58"
      content: |
        I use nuvola as my music player, so far as I know it just uses the normal media key interface.
        
        I did the same thing with xbindkeys for my volume.
    - id: 11900
      author: Edward Z. Yang
      date: "2015-01-05 17:35:42"
      content: "Robin: Some media players manually implement global hotkeys (VLC and foobar fall in this category), which makes them work even without using any sort of dbus stuff. I guess nuvola is also in that category."
    - id: 12591
      author: bewest
      date: "2015-02-07 00:56:54"
      content: |
        I also found a problem with the gnomeRegister function.
        https://gist.github.com/bewest/46b849da04684f0ce401
        
        The new syntax for dbus-send changed, which also puts us back where we were a year ago again.
        
        I found it simplest to ditch gnomeConfig altogether, only keeping the layoutHook from desktopCOnfig, and then use the following bash script using startHook with execScriptHook "startup" &gt;&gt; setWMName "LG3D".
        
        dbus-send --session --print-reply --dest=org.gnome.SessionManager /org/gnome/SessionManager org.gnome.SessionManager.RegisterClient string:xmonad.desktop string:$DESKTOP_AUTO_START_ID
---

I finally got around to upgrading to Utopic. [A year ago](http://blog.ezyang.com/2013/10/xmonad-and-media-keys-on-saucy/) I reported that gnome-settings-daemon no longer provided keygrabbing support. This was eventually reverted for Trusty, which kept everyone's media keys.

I'm sorry to report that in Ubuntu Utopic, the legacy keygrabber is no more:

    ------------------------------------------------------------
    revno: 4015 [merge]
    author: William Hua <william.hua@canonical.com>
    committer: Tarmac
    branch nick: trunk
    timestamp: Tue 2014-02-18 18:22:53 +0000
    message:
      Revert the legacy key grabber. Fixes: https://bugs.launchpad.net/bugs/1226962.

It appears that the Unity team has forked gnome-settings-daemon into unity-settings-daemon (actually this fork happened in Trusty), and as of Utopic gnome-settings-daemon and gnome-control-center [have been gutted](https://bugs.launchpad.net/ubuntu/+source/gnome-settings-daemon/+bug/1318539) in favor of unity-settings-daemon and unity-control-center. Which puts us back in the same situation as a year ago.

I don't currently have a solution for this (pretty big) problem. However, I have solutions for some minor issues which did pop up on the upgrade:

- If your mouse cursor is invisible, try running `gsettings set org.gnome.settings-daemon.plugins.cursor active false`
- If you don't like that the GTK file dialog doesn't sort folders first anymore, try running `gsettings set org.gtk.Settings.FileChooser sort-directories-first true`. ([Hat tip](http://gexperts.com/wp/gnome-3-12-filesnautilus-sort-folders-before-files-issues/))
- And to reiterate, replace calls to gnome-settings-daemon with unity-settings-daemon, and use unity-control-panel to do general configuration.
