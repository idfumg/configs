#!/bin/sh

# load in defaults and keymaps

if [ -f ~/.Xresources ]; then
	xrdb -merge ~/.Xresources
fi

if [ -f ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  		[ -x "$f" ] && . "$f"
 	done
 	unset f
fi

setxkbmap -layout us,ru -variant -option grp:alt_shift_toggle,terminate:ctrl_alt_bksp &

# start awesome windows manager

exec awesome
