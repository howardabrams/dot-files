#!/bin/sh

# The following aliases have been installed on this machine.
#
# - `go` to make a new SSH connection to a remote host.
function go {
    if [ "$1" = "-n" ]
    then
        NAME=$2
        shift 2
    else
        SCRIPT='if (/\.([0-9]+)$/) {
   print "host-$1";
} elsif (/ ([^ @]+)$/) {
   print $1;
} else {
   print $_;
}'
        NAME=$(echo "$*" | perl -ne "$SCRIPT")
    fi
    tmux neww -n "$NAME" "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no $*"
}

# - `nw` to create a new Tmux window. Give ‘er a command.
function nw() {
    tmux neww $*
}

# - `irb` for a TMUX window into an interactive Ruby session (v. 1.9)
alias irb='tmux neww -n ruby "/opt/chef/embedded/bin/irb $*"'

# - `root` for a TMUX window as the root user.
alias root='tmux neww -n root "sudo su -"'

# - 'goall' creates a window for each and every node.
function goall() {
    if [ -n "$1" ]
    then
        HOSTS=$(onova list | sed 's/  */:/g' | cut -d':' -f2 | grep $1)
    else
        HOSTS=$(onova list | sed 's/  */:/g' | cut -d':' -f2)
    fi

    for HOST in $HOSTS
    do
        SSH_OPTS="-o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
        NAME=$(echo $HOST | cut -d'-' -f2)
        if [ "$NAME" == "os" ]; then
            NAME="controller"
        fi
        if [ "$NAME" == "chef" ]; then
            SSH_OPTS="$SSH_OPTS -i wpc3x.pem"
        fi
        IP=$(nova list | grep $HOST | cut -d= -f2 | sed 's/ .*//')
        echo tmux neww -n $NAME "ssh $SSH_OPTS $IP"
        tmux neww -n $NAME "ssh $SSH_OPTS $IP"
        sleep 5
    done
}
