;; Utility functions for trying to do shell scripts in Elisp

(require 'em-glob)

(defun ha/substring-replace (old-str new-str beg end)
  "Returns a new string where a subsection of OLD-STR has been
replaced with NEW-STR beginning at position BEG and ending at
END."
   (concat (substring old-str 0 beg) new-str (substring old-str end)))


(defun ha/getvar (var-name)
  "Returns the value of a variable or environment variable."
  (or (getenv var-name) (eval (read var-name))))

;; (ha/getvar "user-emacs-directory")
;; (ha/getvar "HOME")


(defun ha/substr-variables (str)
  "Replace shell-like '$VAR' and '${variables}' in STR with the
equivalent environment variables or Elisp variables.  For
instance: $HOME/.emacs.d could return /home/howard/.emacs.d --
Keep in mind that this is just a string, it does not do any
validation to see if any files exist."

  ;; This function recursively calls this function with more and more
  ;; embedded variables substituted out, until no more variables are
  ;; found, and then it returns the results.
  ;;
  ;; Begin by checking to see if the string starts with ~ ...
  (if (string-prefix-p "~/" str)
      (ha/substr-variables
       (concat (getenv "HOME") (string-remove-prefix "~" str)))

    ;; Variables can either be simple $BLAH or ${some-larger}...
    (let ((s (or (string-match "${\\([^ ]*\\)}" str)
                 (string-match "$\\([A-z_]*\\)" str)))
          (e (match-end 0)))
      (if (not s)             ; No $ matches?
          str                 ; Then just return the string.
        (ha/substr-variables  ; Recursively call with first var sub'd
         (ha/substring-replace str (ha/getvar (match-string 1 str)) s e))))))

;; (ha/substr-variables "$HOME/.emacs.d/elisp/*.el")
;; (ha/substr-variables "${user-emacs-directory}elisp/*.el")


(defun ha/get-files (path &optional full)
  "Returns a list of files that match the glob pattern, PATH.  This allows shell-like variable substitution from the environment, like $HOME, or from variables defined by `setq'. If FULL is specified, it returns the absolute pathnames for each file."
  (let ((subbed-path (ha/substr-variables path)))
    (condition-case nil
        (directory-files (file-name-directory subbed-path)
                         full
                         (eshell-glob-regexp
                          (file-name-nondirectory subbed-path)))
      (error '()))))

;; (ha/get-files "$HOME/.emacs.d/elisp/*.el")
;; => ("init-blog.el" "init-client.el" "init-clojure.el" ...)

;; (ha/get-files "$HOME/.emacs.d/elisp/*.el" t)
;; => ("/home/howard/.emacs.d/elisp/init-blog.el" "/home/howard/.emacs.d/elisp/init-client.el" ...)

;; (ha/get-files "${user-emacs-directory}/elisp/*.el")
;; => ("init-blog.el" "init-client.el" "init-clojure.el" ...)

;; (ha/get-files "/foo/bar/*")  => nil


(defun ha/get-path (path)
  "Return a file specification based on PATH. We should expand this function so that glob patterns work when specifying the parent, but shouldn't worry about matching any particular file."
  (ha/substr-variables path))

;; (ha/get-file "$HOME/.emacs.d/elisp/*.el" t)
;; (ha/get-file "/foo/bar" t)


;; ----------------------------------------------------------------------

;; The following functions are basic "shell" like functions that take
;; a path that refers to files. This allows us to not have to call
;; (ha/get-files) directly.

(defun ha/mkdir (path)
  "Create a directory specified by PATH, which can contain embedded environment variables and Emacs variables, e.g. '$HOME/Work/foobar'."
  (make-directory (ha/get-path path) t))


(defun ha/mksymlink (orig link)
  "Create a symbolic to ORIG.  If LINK is an existing link, it is
deleted first.  LINK could also refer to a directory.  Note: Both
parameters are strings that can accept embedded environment and
Lisp variables, e.g. '$HOME/Work/foo.el' and
'${user-emacs-directory}/elisp/bar.el"
  (let ((orig-file (ha/get-path orig))
        (link-file (ha/get-path link)))

    (if (file-symlink-p link-file)
        (delete-file link-file))
    ;; (message "%s -> %s" orig-file link-file)
    (make-symbolic-link orig-file link-file t)))


(defun ha/mksymlinks (files dest)
  "Create an absolute symbolic link for each file specified in
FILES into the directory, DEST (where each link will have the
same name. Both parameters can be specified with glob patterns
and embedded environment and Emacs variables,
e.g. '$HOME/.emacs.d/*.el'"
  (mapc (lambda (file) (ha/mksymlink file dest)) (ha/get-files files t)))


(defun ha/mkdirs (path)
  "Create one or more directories specified by PATH, which can
contain embedded environment variables and Emacs variables,
e.g. '$HOME/Work/foobar'."
  (mapc (lambda (dir) (make-directory x t)) (ha/get-files path)))


(provide 'sh-funcs)
