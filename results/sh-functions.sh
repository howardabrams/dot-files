#!/bin/sh

# Technical Gunk

#   If not running interactively, we don't need to do anything:

[ -z "$PS1" ] && return

# Listing Files

#   Using the GNU versions of =ls=, which can be installed by Homebrew
#   or Ports, via: =port install coreutils=

alias ls="gls --color"
alias ll="gls --color -lhA"

# f

#    Wrapper around 'find' that excludes some useless directories, like
#    =classes= and =.git= that ignores case, and does an /or/ around
#    every file name.

#    The first parameter can be a source directory to look for the
#    file(s), or you can do something like 'src/../some-file.txt' to
#    look in the 'src' directory for the files.

function f {
    START_PATH='.'
    FILES="$*"

    # If we have a phrase src/../nav.handlebars, then we want to look
    # in the 'src' directory for the filename given:
    if echo "$1" | grep '\.\.' >/dev/null
    then
        set $(echo "$1" | sed 's/\/*\.\.\/*/  /');
    fi

    # If the first option is a directory, then look in that path, otherwise,
    # start searching from the current directory.
    if [ -d "$1" -a -n "$2" ]
    then
        START_PATH=$1
        shift
        FILES="$*"
    fi
    
    # Ignore the classes and .git directories, as well as look for every
    # file name given.
    find $START_PATH \
        -not \( -path '*classes*' -or -path '*node_modules*' -or -path '.git*' \) \
        -and -iname $(perl -e 'print join " -o -iname ", @ARGV' $FILES)
}

# sf

#    Wrapper around 'find' that returns only a single file. Helpful for calls
#    to an editor when you are pretty sure of the name of the file.

function sf {
    f "*$1*" | head -1
}

# ef

#   Combines my 'f' and 'e' functions to easily edit a file in the local
#   directory tree solely by the file name.

function ef {
    e $(f $*)
}

# Window Title

#   Can we title the terminal window? Note, don't call this function
#   =title= or bad things will happen.

function xtitle {
    # Old Style? echo -n -e "\033]0;$*\007";;
    case "$1" in
        -t)     shift; echo "\e]1;$*\a";;
        -w)     shift; echo "\e]2;$*\a";;
        -b)     shift; echo "\e]0;$*\a";;
         *)     echo "\e]0;$*\a";;
    esac
}

# Tab

#   Opens a tab in the `Terminal` program and start something running in it.

function xtab() {
  TITLE="$1"
  if [[ $1 = "-t" ]]
  then
      TITLE="$2"
      shift
      shift
  fi
  
  HISTFILE=$HOME/.zsh_histories/$(echo $TITLE | sed 's/ /_/g')

  osascript <<EOF
    tell application "System Events"
      tell process "Terminal" to keystroke "t" using command down
    end
    tell application "Terminal"
      activate
      set custom title of first window to "$TITLE"
      -- do script "echo '\e]1;$TITLE\a'" in window 1
      do script with command "xtitle $TITLE; HISTFILE=$HISTFILE; clear; $*" in window 1
    end tell
EOF
}

# Note Aliases

#   My Note scripts allow me to create and search my text files.

alias gilt="$HOME/bin/note -e aquamacs -g gilt -t gilt"
alias work="$HOME/bin/note -e aquamacs -g gilt -t gilt"
alias diary="$HOME/bin/note -e aquamacs -g personal -t personal"
export LATEST="$HOME/Dropbox/Notes/gilt/latest"

alias notes-find="notes -a find"
alias notes-view="notes -a view"
alias notes-list="notes -a list"
alias notes-show="notes -a find -f"
alias notes-export="notes -a export"

if [ -f "$HOME/.notes/notes-tag-helper" ]
then
    source "$HOME/.notes/notes-tag-helper"
fi

# Beep

#   I can put this at the end of a long running command and have it
#   tell me when it is complete.

function beep {
    if [ $? -eq 0 ]
    then
        echo $1
        MSG="The background process has completed."
        SAY="I am done."
        if [ -n "$1" ]; then
            MSG="$1 has completed."
            SAY="$1 is done."
        fi
        terminal-notifier -message "$MSG" -title "Process Complete"
        say "$SAY"
    else
        MSG="The background process has failed."
        SAY="I have failed."
        if [ -n "$1" ]; then
            MSG="$1 has failed."
            SAY="$1 has failed."
        fi
        terminal-notifier -message "$MSG" -title "Process Failed"
        say "$SAY"
        /bin/ls /no-file 2>/dev/null   # Make next process know previous failed
    fi
}

# Clip

#   If you want to gather data from the output, but starting with a
#   particular line, and ending with another, use =clip=. For instance:

# #+BEGIN_EXAMPLE
#   nmap -A 192.168.0.1 | clip 'PORT ' 'Service detection performed'
# #+END_EXAMPLE

#   Will show just the "good" stuff from the =nmap= command.

#   Function takes three arguments:

#   1. The text (regular expression, actually) to use to begin printing
#   2. The text to use to end printing (isn't actually
#      printed... should it?)
#   3. Optional text inserted at the beginning of each line.

function clip {
  FIRST=$1
  ENDING=$2
  PADDING=${3:-""}

  perl -ne "\$s=1 if (/$FIRST/); \$s=0 if (/$ENDING/); print \"$PADDING\$_\" if (\$s==1);"
}

# Whitespace Removers

#    These alias remove trailing whitespace and lines containing
#    nothing by spaces/tabs.

alias pre-commit='git status --porcelain | egrep '\''^[MA]'\'' | cut -d '\'' '\'' -f 3 | xargs perl -pi -e '\''s/\t/    /g; s/[\t ]+$//'\'''
alias pre-add='git status --porcelain | grep "^ M" | cut -d" " -f3 | xargs git add'
alias white='xargs perl -pi -e '\''s/\t/    /g; s/[\t ]+$//'\'''

# Pull

#    Allows me to pull new information from the remote branch, but not
#    loose anything.

function pull {
    git stash
    git pull
    git stash pop
}

# Directory Bookmarks

#   [[https://github.com/huyng/bashmarks][This script]] allows us to leave bookmarks to "popular" directories,
#   to jump directly there with a single name.

#   - s bookmarkname - saves the curr dir as bookmarkname
#   - g bookmarkname - jumps to the that bookmark
#   - g b[TAB] - tab completion is available
#   - p bookmarkname - prints the bookmark
#   - p b[TAB] - tab completion is available
#   - d bookmarkname - deletes the bookmark
#   - d [TAB] - tab completion is available
#   - l - list all bookmarks

# The following may already be aliases...
for A in l g d
do
  if [ alias = $(whence -w $A | cut -d: -f2) ]
  then
    unalias $A
  fi
done

if [ -e ~/.bash.d/bashmarks.sh ]
then
    source ~/.bash.d/bashmarks.sh
fi

# Scala Helpers

#    SBT requires more memory than it deserves.

alias bigsbt='java -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -jar /opt/local/share/sbt/sbt-launch.jar'
