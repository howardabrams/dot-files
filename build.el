#!/usr/local/bin/emacs
;;; build -- Set up all the dot-files for updating system

;;; Commentary:

;; Simple Emacs script used to build/tangle all my support
;; environmental dot-files.
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

;;; Code:

(require 'org)         ;; The org-mode goodness
(require 'ob)          ;; org-mode export system
(require 'ob-tangle)   ;; org-mode tangling process
(require 'sh-funcs)    ;; My special functions for doing scripts

;; Need to get the directory to my 'dot-files' source code. While
;; developing, we just evaluate this buffer, so 'buffer-file-name' is
;; our friend. If we load this file instead, we need to use
;; 'load-file-name':
(defconst dot-files-src  (if load-file-name
                             (file-name-directory load-file-name)
                             (file-name-directory (buffer-file-name))))

;; Where all of the .el files will live and play:
(defconst dest-elisp-dir (ha/get-file "${user-emacs-directory}/elisp"))

;; The Script Part ... here we do all the building and compilation work.

(defun ha-build-dot-files ()
  "Takes all my 'dot files' in this directory and deploys a new
encironment (or updates an existing system)."
  (interactive)

  ;; Initially create some of the destination directories
  (ha/mkdir "$HOME/.oh-my-zsh/themes")
  (ha/mkdir "${user-emacs-directory}/elisp")

  (ha/tangle-files "${dot-files-src}/*.org")

  ;; Some Elisp files are just symlinked instead of tangled...
  (ha/mksymlinks "${dot-files-src}/emacs.d/*.el"
                 "${user-emacs-directory}/elisp")

  ;; Just link the entire directory instead of copying the snippets:
  (ha/mksymlink  "${dot-files-src}/snippets"
                 "${user-emacs-directory}/snippets")

  ;; Yeah, this makes me snicker every time I see it.
  (ha/mksymlink  "${dot-files-src}/vimrc" "${HOME}/.vimrc")

  ;; All of the .el files I've eithe tangled or linked should be comp'd:
  (mapc 'byte-compile-file
        (ha/get-files "${user-emacs-directory}/elisp/*.el"))

  (message "Finished building dot-files- Restart Emacs."))


(defun ha/tangle-file (file)
  "Given an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)   ;;  (expand-file-name file \"$DIR\")
  (org-babel-tangle)
  (kill-buffer))


(defun ha/tangle-files (path)
  "Given a directory, PATH, of 'org-mode' files, tangle the source
code out of all literate programming files."
  (interactive "D")
  (mapc 'ha-tangle-file (ha/get-files path)))


(defun ha/get-dot-files ()
  "Pulls and builds the latest from the Github repository.  We
then load the resulting Lisp code."
  (interactive)
  (magit-fetch dot-files-src)
  (magit-checkout "master")
  (ha-build-dot-files)
  (require 'init-main))


(ha-build-dot-files)  ;; Do it

(provide 'dot-files)
;;; build.el ends here
