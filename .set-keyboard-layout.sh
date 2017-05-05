

setxkbmap -layout us -variant dvp
xset r rate 130 30
setxkbmap -option caps:backspace
xmodmap -e "clear Lock"
xmodmap -e "keycode 10 = percent ampersand"



