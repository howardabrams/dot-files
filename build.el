#!/usr/local/bin/emacs
;;; build -- Set up all the dot-files for updating system

;;; Commentary:

;;; Simple Emacs script used to build/tangle all my support
;;; environmental dot-files.

;;; Code:

(require 'org)
;; (require 'org-exp-blocks)
(require 'ob)
(require 'ob-tangle)

(defun ha-tangle-file (file)
  "Given an 'org-mode' FILE, tangle the source code."
  (interactive "f")
  (find-file file)   ;;  (expand-file-name file \"$DIR\")
  (org-babel-tangle)
  (kill-buffer))

(defun ha-tangle-files (dir)
  "Given a directory, DIR, of 'org-mode' files, tangle the source code out of all literate programming files."
  (interactive "D")
  (mapc 'ha-tangle-file
        (directory-files dir nil ".*\.org")))

(defun ha-mksymlink (orig link)
  "Create a symbolic to ORIG.  If LINK is an existing link, it is deleted first.  LINK could also refer to a directory."
  (interactive "fSource file:\ndDestination link:")
  (if (and (file-symlink-p link) (not (file-directory-p link)))
      (delete-file link))
  (make-symbolic-link orig link t))

(defun ha-link-dot-files (src dest)
  (mapc (lambda (file) (ha-mksymlink src dest))
        (directory-files src nil ".*\.el")))


;; The Script Part ... here we do all the building and
;; compilation work.

(defun ha-build-dot-files ()
  "Takes all my 'dot files' in this directory and deploys a new encironment (or updates an existing system)."
  (interactive)
  (let ((home  (concat (getenv "HOME") "/"))
        (cwd   (file-name-directory (buffer-file-name)))
        (elisp (concat user-emacs-directory "elisp")))

    ;; Initially create some of the destination directories
    (make-directory (concat home ".oh-my-zsh/themes") t)
    (ha-tangle-files cwd)

    ;; For files that are not in a literate state of mind,
    ;; we just need to symlink them into the right location
    ;; in my home directory ...
    (ha-link-dot-files (concat cwd "emacs.d") elisp)
    (ha-mksymlink (concat cwd "snippets")
                  (concat user-emacs-directory "snippets"))
    (ha-mksymlink (concat cwd "vimrc")
                  (concat home ".vimrc"))

    ;; Finally byte compile all my Elisp files...
    (mapc 'byte-compile-file (directory-files elisp t ".*\.el"))
    (message "Finished building dot-files- Restart Emacs.")))


(ha-build-dot-files)  ;; Do it

(provide 'dot-files)

;;; build.el ends here
