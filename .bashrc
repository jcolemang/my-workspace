#
# ~/.bashrc
#
# If not running interactively, don't do anything
[[ $- != *i* ]] && return


force_color_prompt=yes

GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
PURPLE="$(tput setaf 5)"
TEAL="$(tput setaf 6)"
ITALIC=$(tput sitm)
BOLD=$(tput bold)
RESET=$(tput sgr0)

alias ls='ls --color=auto'
PS1='\n$(date +"%a, %r")${RESET} | \w \n λ '
# export PS1="\e[0;31m[\u@\h \W]\$ \e[m "


# my stuff
bind -x '"\C-l": clear && echo && ls'

# aliases
alias em="emacsclient -nc"
alias killemacs="emacsclient -e \"(kill-emacs)\""
alias umlet="java -jar /home/coleman/Classes/CSSE374SoftwareDesign/UMLet/umlet.jar"
alias update="/home/coleman/Code/Workstation/copy-workstation.sh"
alias homemonitor="sudo xrandr --output VGA1 --auto --right-of eDP1"
alias runmagma="/home/coleman/SourceRepos/magma/magma"
alias lua="lua5.3"

alias asdf="bash /home/coleman/.set-keyboard-layout.sh"
alias asht="bash /home/coleman/.set-keyboard-layout.sh"
alias aoeu="setxkbmap -layout us -variant workman"
alias sissy="setxkbmap -layout us"

alias c="clear && echo && ls"

alias notebook="~/anaconda3/bin/jupyter-notebook ~/Classes/Current/deep-learning/"

# my common typos
alias please='sudo'
alias sl="ls"
alias l="ls"

# classes
alias classes="cd ~/Classes/Current/"
alias plc="cd ~/Classes/Current/plc/"
alias stats="cd ~/Classes/Current/stats/"
alias design="cd ~/Classes/Current/functional-design/"
alias senior="cd ~/Classes/Current/senior-project/"
alias learning="cd ~/Classes/Current/deep-learning/"

# configuring editor
export EDITOR=vim
# set -o vi
