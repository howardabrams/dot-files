#!/usr/bin/env zsh
# -------------------------------------------------------------------------
#  Loops through every file in the current directory to allow you to
#  refile (move) it to some other directory. Each file displays a little
#  menu of folders as well as options to view, edit, delete it.
# -------------------------------------------------------------------------

PARENT=".." # Looks for subdirectories in the parent directory

while getopts "p:" o
do
    case "$o" in
          p)  PARENT="$OPTARG";;
        [?])  print >&2 "Usage: refile [-p parent-directory] dir1 dir2 ... dirn"
            exit 1;;
    esac
done
shift `expr $OPTIND - 1`

DIRS=${*:-technical personal rpg other}   # My default directories

for f in *
do          
    answer="V"

    while [ "$answer" = "V" -o "$answer" = "E" ]
    do
        echo    ">>> $f"
        echo    "   [V]iew  [E]dit  [I]gnore  [D]elete"
        echo -n "   "

        for i in `echo $DIRS`
        do
            first=`echo $i | cut -c1`    
            rest=`echo $i | cut -c2-`
            echo -n "[$first]$rest  "
        done

        read answer
        if [ "$answer" = "V" ]
        then
            echo
            echo "-=-=-=- $f -=-=-=-"
            less -EFKMX "$f"
            echo
        fi
        if [ "$answer" = "E" ]
        then
            ${EDITOR:-/Applications/Emacs.app/Contents/MacOS/bin/emacsclient} ${f}
        fi
    done

    case $answer in
        "D")    rm "$f";;
        "Q")    exit;;
          *)    for i in `echo $DIRS`; do
                    first=`echo $i | cut -c1`    
                    if [ "$first" = "$answer" ]; then
                        mkdir -p "$PARENT/$i"
                        mv "$f" "$PARENT/$i"
                    fi
                done;;
    esac
done
