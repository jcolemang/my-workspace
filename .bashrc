#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return


force_color_prompt=yes

alias ls='ls --color=auto'
PS1='\e[1;35m\n\w \n λ \e[m'
# export PS1="\e[0;31m[\u@\h \W]\$ \e[m "


# my stuff

# aliases
alias em="emacsclient -nc"
alias killemacs="emacsclient -e \"(kill-emacs)\""
alias balsamiq="sudo wine /opt/Balsamiq_Mockups_3/balsamiq.exe"
alias camunda="/home/coleman/Classes/CSSE371/camunda-modeler/camunda-modeler"
alias umlet="java -jar /home/coleman/Classes/CSSE374SoftwareDesign/UMLet/umlet.jar"
alias telegram="/home/coleman/SourceRepos/Telegram/Telegram"
alias update="/home/coleman/Code/Workstation/copy-workstation.sh"
alias homemonitor="sudo xrandr --output VGA1 --auto --right-of eDP1"

# my common typos
alias please='sudo'
alias sl="ls"
alias l="ls"

# classes
alias classes="cd ~/Classes"
alias plc="cd ~/Classes/Current/plc/"
alias codes="cd ~/Classes/Current/algebraic-codes/"
alias theory="cd ~/Classes/Current/theory-of-comp/"
alias data="cd ~/Classes/Current/advanced-databases/"

# configuring editor
export EDITOR=vim
set -o vi
