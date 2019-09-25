;;; BUILD --- Set up all the dot-files for updating system
;;
;; Author: Howard Abrams <howard@howardabrams.com>
;; Copyright © 2019, Howard Abrams, all rights reserved.
;; Created: 22 September 2019
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Simple Emacs script used to build all my supported
;; environmental dot-files. Everything, that is, except
;; for emacs. That has been replaced.
;;
;; We should be able to execute this as a shell script-like thing if
;; emacs is located in /usr/local/bin ... and we don't mind starting a
;; new instance. If we want to find the emacsclient (and it isn't
;; obvious), perhaps we could do something like this:
;;
;;     EMACS=${EDITOR:-/usr/local/bin/emacs}
;;     EMACS=$(echo $EMACS | sed 's/client//')
;;     if [ ! -e $EMACS ]; then
;;        EMACS=$(which emacs)
;;     else
;;         EMACS="$EMACS -a ${1:-/usr/local/bin/emacs}"
;;     fi
;;     $EMACS --load "./build.el"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Need to get the directory to my 'dot-files' source code. While
;; developing, we just evaluate this buffer, so 'buffer-file-name' is
;; our friend. If we load this file instead, we need to use
;; 'load-file-name':
(defconst dot-files-src  (if load-file-name
                             (file-name-directory load-file-name)
                           (file-name-directory (buffer-file-name))))

;; My special functions for doing script are not in a loadable location.
(require 'scripty) ; "~/.spacemacs.d/elisp/scripty.el"


(scripty-mksymlink  "${dot-files-src}/bash_profile" "${HOME}/.bash_profile")
(scripty-mksymlink  "${dot-files-src}/config/i3" "${HOME}/.config/i3")

;; Yeah, this makes me snicker every time I see it.
(scripty-mksymlink  "${dot-files-src}/vimrc" "${HOME}/.vimrc")

;; Install stuff for Linux ...
(if (scripty-ubuntu?)
    (scripty-sudo "snap install ripgrep --classic"
                  "snap install telegram-desktop"
                  "snap install spotify"
             "apt install -y i3-wm fonts-font-awesome"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; build.el ends here
