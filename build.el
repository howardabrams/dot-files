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

;; My special functions for doing script are not in a loadable location.q
(defvar script-funcs-src (concat (file-name-directory (buffer-file-name))
                                 "elisp/shell-script-funcs.el"))
(require 'shell-script-funcs script-funcs-src)

;; Need to get the directory to my 'dot-files' source code. While
;; developing, we just evaluate this buffer, so 'buffer-file-name' is
;; our friend. If we load this file instead, we need to use
;; 'load-file-name':
(defconst dot-files-src  (if load-file-name
                             (file-name-directory load-file-name)
                           (file-name-directory (buffer-file-name))))

(defconst ha/emacs-directory (concat (getenv "HOME") "/.emacs.d/"))

;; Where all of the .el files will live and play:
(defconst dest-elisp-dir (ha/get-path "${ha/emacs-directory}/elisp"))

;; The Script Part ... here we do all the building and compilation work.

(defun ha/build-dot-files ()
  "Compile and deploy 'init files' in this directory."
  (interactive)

  ;; Initially create some of the destination directories
  (ha/mkdir "$HOME/.oh-my-zsh/themes")
  (ha/mkdir "${ha/emacs-directory}/elisp")

  (ha/tangle-files "${dot-files-src}/*.org")

  ;; Some Elisp files are just symlinked instead of tangled...
  (ha/mksymlinks "${dot-files-src}/elisp/*.el"
                 "${ha/emacs-directory}/elisp")

  ;; Just link the entire directory instead of copying the snippets:
  (ha/mksymlink  "${dot-files-src}/snippets"
                 "${ha/emacs-directory}/snippets")

  ;; Just link the entire directory instead of copying the snippets:
  (ha/mksymlink  "${dot-files-src}/templates"
                 "${ha/emacs-directory}/templates")

  ;; Some Elisp files are just symlinked instead of tangled...
  (ha/mksymlinks "${dot-files-src}/bin/[a-z]*"
                 "${HOME}/bin")

  ;; Yeah, this makes me snicker every time I see it.
  (ha/mksymlink  "${dot-files-src}/vimrc" "${HOME}/.vimrc")

  ;; All of the .el files I've eithe tangled or linked should be comp'd:
  ;; (mapc 'byte-compile-file
  ;;       (ha/get-files "${ha/emacs-directory}/elisp/*.el" t))

  (message "Finished building dot-files- Resetting Emacs.")
  (require 'init-main (ha/get-path "${user-emacs-directory}elisp/init-main.el")))


(defun ha/tangle-file (file)
  "Given an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)   ;;  (expand-file-name file \"$DIR\")
  (org-babel-tangle)
  (kill-buffer))


(defun ha/tangle-files (path)
  "Given a directory, PATH, of 'org-mode' files, tangle source code out of all literate programming files."
  (interactive "D")
  (mapc 'ha/tangle-file (ha/get-files path)))


(defun ha/get-dot-files ()
  "Pull and build latest from the Github repository.  Load the resulting Lisp code."
  (interactive)
  (let ((git-results
         (shell-command (concat "cd " dot-files-src "; git pull"))))
    (if (not (= git-results 0))
        (message "Can't pull the goodness. Pull from git by hand.")
      (load-file (concat dot-files-src "/emacs.d/shell-script-funcs.el"))
      (load-file (concat dot-files-src "/build.el"))
      (require 'init-main))))

(ha/build-dot-files)  ;; Do it

(provide 'dot-files)
;;; build.el ends here
