#!/usr/bin/env bash

# {{{ Environment settings
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Setup locale
export LANG=en_US.utf8
export LC_MESSAGES=en_US.utf8

# {{{ Resource and keymap settings
userresources="${HOME}/.Xresources"
sysresources="/etc/X11/xinit/.Xresources"

# Merge system and user resources and keymaps
[[ -r "${sysresources}" ]]  && xrdb -merge "${sysresources}"
[[ -r "${userresources}" ]] && xrdb -merge "${userresources}"
# }}}

# {{{ Input settings
#
# Keyboard control, repeat delay and repeat rate
xset r rate 200 30

# http://blog.ssokolow.com/archives/2011/12/24/getting-your-way-with-setxkbmap/
setxkbmap -option \
  -option compose:rctrl \
  -option lv3:ralt_switch \
  -option terminate:ctrl_alt_bkspc \
  -option eurosign:e \
  -option nbsp:level3n


# Pointer control, acceleration and threshold
xset m 30/10 4

# Pointer appearance
#   - xcb does not support Xcursor yet
xsetroot -cursor_name left_ptr

# Disable the touchpad while typing
syndaemon -k -i 2 -d &
# }}}

xrandr --output DP-3 --mode 1920x1080 --pos 0x255  --auto --output DP-2 --mode 1920x1080 --pos 1920x0 --auto --rotate left
