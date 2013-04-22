#!/usr/bin/env zsh

for f in *
do          
    answer="v"

    while [ "$answer" = "v" -o "$answer" = "e" ]
    do
        echo ">>> $f"
        echo -n "  [v]iew  [t]echnical  [p]ersonal  [o]ther  [e]dit  [d]elete ? "
        read answer
        if [ "$answer" = "v" ]
        then
            less ${f}
        fi
        if [ "$answer" = "e" ]
        then
            /Applications/Emacs.app/Contents/MacOS/bin/emacsclient ${f}
        fi
    done

    case $answer in
        "t")    mv "$f" ../Tech-Notes;;
        "p")    mv "$f" ../tmp-pers;;
        "o")    mv "$f" ../tmp-other;;
        "d")    rm "$f";;
    esac
done
