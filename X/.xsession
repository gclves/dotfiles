#!/bin/sh

xwallpaper --center pic/puffy2.jpg &
xsetroot -cursor_name left_ptr &
slstatus &

xset b off

# Desktop locking
xidle &

# Trackpad scrolling
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation" 1 
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Button" 2 
xinput set-prop "/dev/wsmouse" "WS Pointer Wheel Emulation Axes" 6 7 4 5

exec dwm

