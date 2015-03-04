#!/bin/sh

# Set up keyboard layout.
setxkbmap -rules evdev -model pc104 -layout "us,ru" -variant ",winkeys" -option "caps:ctrl_modifier,grp:lctrl_toggle,altwin:ctrl_win,grp_led:caps,compose:menu,numpad:microsoft"

# Hide cursor after 1 sec of inactivity.
unclutter -idle 1 &
