:

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/bin

# IP=$(ifconfig | grep addr: | grep '10.' | cut -d: -f2 | cut -d' ' -f1)
IP=$(hostname -i)
# export PS1="bork:\w $ "
PS1="\[\e[0;32m\]$IP\[\e[m\]:\[\e[1;34m\]\w\[\e[m\] $ "
export PS1

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d $HOME/py-env ]
then
    . $HOME/py-env/bin/activate
fi

# If we have a tmux session running, attach to it, otherwise, start
# one up....oh, and let's start emacs as a daemon too.

if [ "$TERM" = "screen" ] && [ "$HOME/.bash_aliases" ]
then
    source "$HOME/.bash_aliases"
    grep '^# ' "$HOME/.bash_aliases" | sed 's/^# *//'
    echo
fi

if [ -e $HOME/openrc ]
then
    source $HOME/openrc
fi
