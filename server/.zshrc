#!/usr/local/bin/zsh

# set PATH so it includes user's private bin if it exists                                                                               
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

alias ls="ls --color"
alias ll="ls --color -olhA"
alias e='emacsclient -q -a emacs'

EDITOR=emacsclient

# Key Bindings

#   On the Mac, open up the Terminal preferences, under your =Basic=
#   settings, select the =Keyboard= tab and select the checkbox for
#   =Use option as meta key= to take avantage of these.

bindkey -e

autoload -U colors && colors

# Set up the prompt
autoload -Uz promptinit
promptinit

PROMPT='%{%F{blue}%}$ %{%f%}%'
RPROMPT='%{%F{yellow}%}%~%{%f%}%'

setopt histignorealldups sharehistory

# Completion

autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''

# Show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Some auto completion setups will have a /title/ for a menu. This
#   may be helpful or obnoxious. So, this may disappear sometime:

zstyle ":completion:*:descriptions" format "%B%d%b"

# My Functions

#   Load up my [[file:sh-functions.org][shared functions]]. 
#   These can be shared with Bash.

if [[ -f $HOME/.sh-funcs.sh ]]
then
    source $HOME/.sh-funcs.sh
fi

# Each machine I'm on (work or home), may have some special functions
#   or aliases, so let's load anything that begins with =.zshrc-=:

if [[ -f $HOME/.zshrc-* ]]
then
    for SH_FILE in $HOME/.zshrc-*
    do
        source $SH_FILE
    done
fi


# If we have a tmux session running, attach to it, otherwise, start                                                                     
# one up.                                                                                                                               
if ps -u $USER -o fname | grep tmux >/dev/null
then
    if [ -z "$TMUX" ]
    then
        tmux attach
    fi
else
    exec tmux
fi
