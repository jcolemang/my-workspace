#
# ~/.bashrc
#
# If not running interactively, don't do anything
[[ $- != *i* ]] && return


force_color_prompt=yes

alias ls='ls --color=auto'
PS1='\n$(date +"%a, %r") | \w \n λ '
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
alias asdf="bash /home/coleman/.set-keyboard-layout.sh"

alias c="clear && echo && ls"

# my common typos
alias please='sudo'
alias sl="ls"
alias l="ls"

# classes
alias classes="cd ~/Classes/Current/"
alias plc="cd ~/Classes/Current/plc/"
alias codes="cd ~/Classes/Current/algebraic-codes/"
alias theory="cd ~/Classes/Current/theory-of-comp/"
alias databases="cd ~/Classes/Current/advanced-databases/"

# configuring editor
export EDITOR=vim
set -o vi
