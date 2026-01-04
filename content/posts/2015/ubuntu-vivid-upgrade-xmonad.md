---
title: "Ubuntu Vivid upgrade (Xmonad)"
date: 2015-05-29 14:09:30
slug: ubuntu-vivid-upgrade-xmonad
categories: [Toolbox]
---

Another half year, another Ubuntu upgrade. This upgrade went essentially smoothly: the only things that stopped working were my xbindkeys bindings for volume and suspend, which were easy to fix.

# Volume up and down

If you previously had:

    #Volume Up
    "pactl set-sink-volume 0 -- +5%"
        m:0x10 + c:123
        Mod2 + XF86AudioRaiseVolume 

this syntax no longer works: you must place the double dash earlier in the command, as so:

    #Volume Up
    "pactl -- set-sink-volume 0 +5%"
        m:0x10 + c:123
        Mod2 + XF86AudioRaiseVolume 

Do the same for volume down.

# Suspend

If you previously had:

    #Sleep
    "dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend"
         m:0x10 + c:150
         Mod2 + XF86Sleep 

UPower no longer handles suspend; you have to send the command to login:

    #Sleep
    "dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 org.freedesktop.login1.Manager.Suspend boolean:true"
        m:0x10 + c:150
        Mod2 + XF86Sleep 
