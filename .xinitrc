#!/bin/sh

xmodmap -e "clear Lock"
setxkbmap -layout us -variant dvp
xset r rate 130 30
setxkbmap -option caps:backspace

exec myxmonad
