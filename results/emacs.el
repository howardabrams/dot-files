
;;; ------------------------------------------
;;; Do not edit the generated file, as it has
;;; been generated, as a tangled file, by the
;;; fandifluous org-mode.
;;;
;;; Source: ~/Work/dot-files/emacs.org
;;; ------------------------------------------

;; Directory Structure

;;    In case this is the first time running this on a computer, we need
;;    to make sure the following directories have been created.

(let* ((subdirs '("elisp" "backups" "snippets" "ac-dict"))
       (fulldirs (mapcar (lambda (d) (concat user-emacs-directory d)) subdirs)))
  (dolist (dir fulldirs)
    (when (not (file-exists-p dir))
      (message "Make directory: %s" dir)
      (make-directory dir))))

;; Extra Packages

;;    Extra packages not available via the package manager go in my
;;    personal stash at: =$HOME/.emacs.d/elisp=

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

;; Make sure that =PATH= variable for finding binary files can
;;    is the same as what Emacs will look for binary files...but only if
;;    we are running from the =Applications= directory.

(when window-system
  (let ((path-from-shell (shell-command-to-string "bash -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Package Manager

;;    Emacs has become like every other operating system, and now has a
;;    [[http://tromey.com/elpa/][package manager]] with its own collection repository, but since it is
;;    so conservative, we need to add more repositories to get all the
;;    sweet goodness, I demand.

(require 'package)

(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ("tromey"    . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; While we can now do a =package-list-packages=, you can install and
;;    everything is good, however, we can't =require= any of these
;;    packages (in order to customize them in this file) until we do
;;    this:

(package-initialize)

;; Installing Extra Packages

;;    Not sure why the package management system doesn't come with a
;;    programmatic way to specify what packages should be installed. Oh
;;    yeah, this is pretty new. Looks like everyone just rolls there own,
;;    so this is mine.

(defun filter (condp lst)
  "Emacs Lisp doesn’t come with a ‘filter’ function to keep elements that satisfy
a conditional and excise the elements that do not satisfy it. One can use ‘mapcar’
to iterate over a list with a conditional, and then use ‘delq’ to remove the ‘nil’
values."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun inverse-filter (condp lst)
  "A filter function, but returns a list of the entries that don't match the predicate."
  (delq nil
        (mapcar (lambda (x) (and (not (funcall condp x)) x)) lst)))

(defun packages-install (packages)
  "Given a list of packages, this will install them from the standard locations."
  (let ((to-install (inverse-filter 'package-installed-p packages)))
    (when to-install
      (package-refresh-contents)
      (dolist (it to-install)
          (package-install it)
      (delete-other-windows)))))

;; This means that at any point in my configuration file, I can
;;    specify a list of packages to make sure they are installed.

(packages-install
               '(auto-complete
                 circe
                 color-theme
                 color-theme-sanityinc-tomorrow
                 color-identifiers-mode  ;; Color variables differently
                 highlight-tail          ;; Used only sporadically
                 dired-details
                 epl
                 expand-region
                 flycheck
                 flycheck-color-mode-line
                 flyspell
                 git-commit-mode
                 git-gutter-fringe
                 gitconfig-mode
                 gitignore-mode
                 graphviz-dot-mode
                 iy-go-to-char
                 key-chord
                 mac-key-mode
                 magit
                 markdown-mode
                 multiple-cursors
                 nlinum
                 smart-mode-line
                 ;; redo+             ;; If not installed, edit mac-key-mode
                 smex
                 undo-tree
                 visual-regexp
                 yasnippet))

;; Package Verification

;;    The =(require)= is a problem if the library isn't available, and if
;;    it isn't available, then this file dies and doesn't complete.
;;    Seems like it would be nice to wrap the configuration of a package
;;    in a block that is ignored if the package isn't available.

;;    The following code was found [[http://stackoverflow.com/questions/7790382/how-to-determine-whether-a-package-is-installed-in-elisp][in this discussion]], but I believe
;;    I'll be phasing this out now that I can simply install a long list
;;    of packages when I start.

(defun autofeaturep (feature)
  "For a feature symbol 'foo, return a result equivalent to:
(or (featurep 'foo-autoloads) (featurep 'foo))
Does not support subfeatures."
  (catch 'result
    (let ((feature-name (symbol-name feature)))
      (unless (string-match "-autoloads$" feature-name)
        (let ((feature-autoloads (intern-soft (concat feature-name "-autoloads"))))
          (when (and feature-autoloads (featurep feature-autoloads))
            (throw 'result t))))
      (featurep feature))))

;; Variables

;;    General settings about me that other packages can use. The biggest
;;    problem is guessing my email address based on what computer I am using:

(if (equal "howard.abrams" user-login-name)
    (setq user-mail-address "howard.abrams@workday.com")
  (setq user-mail-address "howard.abrams@gmail.com"))

;; Tabs vs Spaces

;;    I have learned to distrust tabs in my source code, so let's make
;;    sure that we only have spaces.

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Window Size

;;    Hrm ... I'm always increasing the window size, so I might as well
;;    just have it default to the size I want:

(add-to-list 'default-frame-alist '(height . 46))
(add-to-list 'default-frame-alist '(width . 80))

;; The bell is pretty obnoxious when it dings during scrolling.

(setq ring-bell-function 'ignore)

;; Display Settings

;;    I've been using Emacs for many years, and appreciate a certain
;;    minimalist approach to its display. While you can turn these off
;;    with the menu items now, it is just as easy to set them here.

(setq initial-scratch-message "") ;; Uh, I know what Scratch is for
(setq visible-bell t)             ;; Get rid of the beeps
(scroll-bar-mode 0)               ;; Scrollbars are wasted screen estate
(tool-bar-mode 0)                 ;; Toolbars were only cool with XEmacs

(unless (window-system)
  (menu-bar-mode 0))              ;; No menus... but only in text mode

;; Font Settings

;;    I love syntax highlighting.

(global-font-lock-mode 1)

;; Specify the default font as =Source Code Pro=, which should already
;;    be [[http://blogs.adobe.com/typblography/2012/09/source-code-pro.html][downloaded]] and installed.

(when (window-system)
  (set-frame-font "Source Code Pro")
  (set-face-attribute 'default nil :font "Source Code Pro" :height 140)
  (set-face-font 'default "Source Code Pro"))

;; Line Numbers

;;    Do you want line numbers on the left side:

(line-number-mode 1)

;; Color Theme

;;    Use the color theme project by following [[http://www.nongnu.org/color-theme/][these instructions]].
;;    We now can do =M-x color-theme-<TAB> RET=

(require 'color-theme)

;; The color themes work quite well, except they don't know about the
;;    org-mode source code blocks, so we need to set up a couple
;;    functions that we can use to set them.

(defun org-src-color-blocks-light ()
  "Colors the block headers and footers to make them stand out more for lighter themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
    ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
   '(org-block-background
     ((t (:background "#FFFFEA"))))
   '(org-block-end-line
     ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))))

   ;; Looks like the minibuffer issues are only for v23
   ; (set-face-foreground 'minibuffer "black")
   ; (set-face-foreground 'minibuffer-prompt "red")
)

(defun org-src-color-blocks-dark ()
  "Colors the block headers and footers to make them stand out more for dark themes"
  (interactive)
  (custom-set-faces
   '(org-block-begin-line
     ((t (:foreground "#008ED1" :background "#002E41"))))
   '(org-block-background
     ((t (:background "#111111"))))
   '(org-block-end-line
     ((t (:foreground "#008ED1" :background "#002E41")))))

   ;; Looks like the minibuffer issues are only for v23
   ; (set-face-foreground 'minibuffer "white")
   ; (set-face-foreground 'minibuffer-prompt "white")
)

;; My main reason for wanting to use the color theme project is to
;;    switch between /black on white/ during the day, and /white on
;;    black/ at night.

(defun color-theme-my-default ()
  "Tries to set up a normal color scheme"
  (interactive)
  (color-theme-sanityinc-tomorrow-day)
  (org-src-color-blocks-light))

;; During the day, we use the "standard" theme:
(global-set-key (kbd "<f9> d") 'color-theme-my-default)

;; A good late-night scheme that isn't too harsh
(global-set-key (kbd "<f9> l") (lambda () (interactive)
                                 (color-theme-sanityinc-tomorrow-eighties)
                                 (org-src-color-blocks-dark)))

;; Programming late into the night
(global-set-key (kbd "<f9> m") (lambda () (interactive)
                                 (color-theme-sanityinc-tomorrow-bright)
                                 (org-src-color-blocks-dark)))

;; Too harsh? Use the late night theme
(global-set-key (kbd "<f9> n") (lambda () (interactive)
                                 (color-theme-sanityinc-tomorrow-night)
                                 (org-src-color-blocks-dark)))

;; Macintosh

;;    I like the ability to use the Command key to turn a standard Emacs
;;    into a more Macintosh-specific application. (See [[http://stackoverflow.com/questions/162896/emacs-on-mac-os-x-leopard-key-bindings][these online notes]])

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)

  ;; Aquamacs-specific code:
  (when (boundp 'aquamacs-version)
    (global-set-key [(alt k)] 'nlinum-mode))

  ;; Emacs on Mac specific code:
  (unless (boundp 'aquamacs-version)
    ;; Since I already have Command-V for pasting, I
    ;; don't need Ctrl-V to do that, so disable CUA:
    (cua-mode -1)

    (require 'mac-key-mode)
    (mac-key-mode 1)

    (define-key mac-key-mode-map [(alt o)] 'ido-find-file)

    ;; I'd rather selectively bind Meta-I to my italics function,
    ;; instead of showing the file in the Finder.
    (define-key mac-key-mode-map (kbd "A-i") nil)
    (define-key mac-key-mode-map [(alt 2)] 'mac-key-show-in-finder)

    (define-key mac-key-mode-map [(alt +)] 'text-scale-increase)
    (define-key mac-key-mode-map [(alt _)] 'text-scale-decrease)
    (define-key mac-key-mode-map [(alt l)] 'goto-line)
    (define-key mac-key-mode-map [(alt w)] 'delete-single-window)
    (define-key mac-key-mode-map [(alt m)] 'toggle-meta-key)
    (define-key mac-key-mode-map [(alt k)] 'nlinum-mode))

;; Since the default ls for the Mac isn't so good, I always have the
;;    GNU ls version available in =/usr/local/bin/gls=.

(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/usr/local/bin/gls"))

;; I hate the default implementation of Command-M. Now,
;;    pressing Command-M will toggle whether the Option key is a
;;    standard Option key or a Meta key:

(defun toggle-meta-key ()
  "Toggles whether the Mac option key is an option key or a meta key."
  (interactive)
  (if (eq mac-option-modifier 'meta)
      (setq mac-option-modifier nil)
    (setq mac-option-modifier 'meta)))

;; I would like Command-W to close a frame, but only if it only has a
;;    single window in it. I found this code on [[http://www.emacswiki.org/emacs/frame-cmds.el][this site]].

(defun delete-single-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (one-window-p t)
        (delete-frame)
        (delete-window (selected-window)))))

;; Skype

;;     I normally mute Skype with some Alfred keystroke running some
;;     AppleScript. However, Emacs will grab all keystrokes before
;;     passing them on, so I need this function:

(defun mute-skype ()
   "Mutes or unmutes Skype via an AppleScript call."
   (interactive)
   (let ((mute-script "tell application \"Skype\"
  if \(send command \"GET MUTE\" script name \"MuteToggler\"\) is equal to \"MUTE ON\" then
    send command \"SET MUTE OFF\" script name \"MuteToggler\"
  else
    send command \"SET MUTE ON\" script name \"MuteToggler\"
  end if
end tell"))
     (progn
       (call-process "osascript" nil nil nil "-ss" "-e" mute-script)
       (message "Skype (un)mute message has been sent."))))

(global-set-key (kbd "C-M-A-m") 'mute-skype)

;; Undo and Redo

;;     According to [[http://ergoemacs.org/emacs/emacs_best_redo_mode.html][this article]], I get better functionality than the
;;     =redo+= plugin (which I can't seem to get working well).

(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)

(when (eq system-type 'darwin)
  (unless (boundp 'aquamacs-version)
    (define-key mac-key-mode-map [(alt z)] 'undo-tree-undo)
    (define-key mac-key-mode-map [(alt S-z)] 'undo-tree-redo)))

(global-set-key (kbd "C-z") 'undo) ; Zap to character isn't helpful
(global-set-key (kbd "C-S-z") 'redo)

;; More Key Definitions

;;    Clearly, the most important keybindings are the function keys,
;;    right? Here is my list of needs:

;;    - *F1* - Help? Isn't Control-H good enough?
;;    - *F2* - Standard alternate meta key with lots of bindings
;;    - *F3* - Define a keyboard macro
;;    - *F4* - Replay a keyboard macro
;;    - *F5* - Slime-JS REPL
;;    - *F6* - Setup window or move forward with demonstration.
;;    - *F7* - Switch to another window
;;    - *F8* - Switch to buffer
;;    - *F9* - My other meta key for changing colors and other odd
;;      bindings that I actually don't use that often

(global-set-key (kbd "<f6>") 'setup-windows)
(global-set-key (kbd "<f7>") 'other-window)
(global-set-key (kbd "<f8>") 'ido-switch-buffer)

;; Change window configuration and then return to the old
;;    configuration with [[http://www.emacswiki.org/emacs/WinnerMode][winner-mode]].  Use =Control-C Arrow= keys to
;;    cycle through window/frame configurations.

(winner-mode 1)

;; Better Newline

;;    Since =paredit= and other modes automatically insert final
;;    characters like semi-colons and parenthesis, what I really want is
;;    to hit return from the /end of the line/. Pretty simple function.

(defun newline-for-code ()
  "Inserts a newline character, but from the end of the current line."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

;; And we can bind that to the free, /Meta-Return/:

(global-set-key (kbd "M-RET") 'newline-for-code)

;; Key Chords

;;    Key Chords allows you to use any two keys pressed at the same time
;;    to trigger a function call. Interesting possibilities, but of
;;    course, you don't want it to make any mistakes.

;;    Like 'xo' seemed good for 'other-window' until I needed to type
;;    the word, 'ox'.

;;    I like vi's =.= command, where it quickly repeats the last command
;;    you did. Emacs has similar functionality, but I never remember
;;    =C-x z=, so let's map it to something else.

(if (autofeaturep 'key-chord)
    (progn
      (require 'key-chord)
      (key-chord-mode +1)

      (key-chord-define-global ",." 'repeat)
      (key-chord-define-global "qw" 'query-replace)
      (key-chord-define-global "xb" 'ido-switch-buffer)
      (key-chord-define-global "xf" 'ido-find-file)

      (if (autofeaturep 'iy-go-to-char)
          (progn
            (require 'iy-go-to-char)

            (key-chord-define-global "fg" 'iy-go-to-char)
            (key-chord-define-global "df" 'iy-go-to-char-backward)))))

;; Recent File List

;;    According to [[http://www.emacswiki.org/emacs-es/RecentFiles][this article]], Emacs already has the recent file
;;    listing available, just not turned on.

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Multiple Cursors

;;    While I'm not sure how often I will use [[https://github.com/emacsmirror/multiple-cursors][multiple-cursors]] project,
;;    I'm going to try to remember it is there. It doesn't have any
;;    default keybindings, so I set up the suggested:

(if (autofeaturep 'multiple-cursors)
      (progn
        (require 'multiple-cursors)
        (global-set-key (kbd "C->") 'mc/mark-next-like-this)
        (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
        (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

;; Auto Complete

;;    This feature scans the code and suggests completions for what you
;;    are typing. Useful at times ... annoying at others.

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))

(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-dictionary
               ac-source-yasnippet
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))

(ac-config-default)
(global-auto-complete-mode t)

;; Abbreviation Mode

;;    Using the built-in [[http://www.emacswiki.org/emacs/AbbrevMode][Abbreviation Mode]], but setting it up only for
;;    the text modes:

(add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

;; While you can make abbreviations in situ, I figured I should
;;    /pre-load/ a bunch that I use:

(define-abbrev-table 'global-abbrev-table
  '(("HA" "Howard Abrams")
    ("CCB" "CI x CD x BP")
    ("WD" "Workday")
    ("btw" "by the way")
    ("func" "function")
    ("note" "*Note:*")
    ("OS" "OpenStack")
    ("NG" "AngularJS")
    ("JS" "JavaScript")
    ("CS" "CoffeeScript")))

;; Yas Snippet

;;    The [[https://github.com/capitaomorte/yasnippet][yasnippet project]] allows me to create snippets of code that
;;    can be brought into a file, based on the language.

(require 'yasnippet)
(yas-global-mode 1)

;; Inside the =snippets= directory should be directories for each
;;    mode, e.g.  =clojure-mode= and =org-mode=. This connects the mode
;;    with the snippets.

(add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))

;; [[https://code.google.com/p/js2-mode/][js2-mode]] is good, but its name means that Yas' won't automatically
;;    link it to its =js-mode=. This little bit of magic does the linking:

(add-hook 'js2-mode-hook '(lambda ()
                            (make-local-variable 'yas-extra-modes)
                            (add-to-list 'yas-extra-modes 'js-mode)
                            (yas-minor-mode 1)))

;; Dash

;;    The [[http://kapeli.com/][Dash product]] is nice, and [[https://github.com/Kapeli/dash-at-point][this project]] allows Emacs to open
;;    Dash for documentation of anything with a =C-c d= keystroke:

(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key (kbd "C-c d") 'dash-at-point)

;; Note Grep

;;    First, we need to have the =find-grep= ignore =.git= directories
;;    and search for wholewords:

(setq grep-find-command
      "find . -type f '!' -wholename '*/.git/*' -print0 | xargs -0 -e grep -nHPi -e ")
(setq rep-highlight-matches t)

;; I use the standard [[http://emacswiki.org/emacs/GrepMode#toc1][grep package]] in Emacs and wrap it so that I
;;    can easily search through my notes. However, I'm using a later
;;    version of Gnu Grep. On Mac OS X, run these two commands:

;; #+BEGIN_EXAMPLE
;; brew tap homebrew/dupes
;; brew install homebrew/dupes/grep
;; #+END_EXAMPLE

;;    Now we can call the =ggrep= command:

(defun ngrep (reg-exp)
  "Searches the Notes and ORG directory tree for an expression.
The pipe symbol separates an org-mode tag from the phrase to search,
e.g. jquery|appendTo searches only the files with a 'jquery' tag."
  (interactive "sSearch note directories: ")
  (let ((search-dir "~/Notes ~/Technical")
        (options "--line-number --with-filename --word-regexp --ignore-case")
        (the-args (split-string reg-exp "\|")))
    (if (> (length the-args) 1)
        (let* ((the-tag (car the-args))
               (reg-exp (combine-and-quote-strings (cdr the-args)))
               (files   (split-string (shell-command-to-string
                             (concat
                              "grep -r --files-with-matches '#+TAGS: .*"
                              the-tag "' " search-dir)) "\n")))
          (progn
            (message "Searching for %s with tag of %s" reg-exp the-tag)
            (grep-find (concat "grep " options " " reg-exp " "
                               (combine-and-quote-strings files)))))
      (let* ((file-exts '( "*.org" "*.md" "*.txt" "*.markdown"))
             (file-types (mapconcat (function (lambda (x) (concat "--include '" x "'")))  file-exts " ")))
        (progn
          (message "Searching in %s" search-dir)
          (grep-compute-defaults)
          (grep-find (concat "grep -r -e " reg-exp " " options " "
                             file-types " " search-dir)))))))

(global-set-key (kbd "C-x C-n") 'ngrep)
;; (global-set-key (kbd "C-x C-r") 'rgrep)

;; Don't forget that after doing a =C-x C-f= to find a file, you can
;;    hit another =M-f= to do a find the given directory (and subs).

;;    Also, you can do a full locate with =C-x C-l=:

(global-set-key (kbd "C-x C-l") 'locate)
(setq locate-command "mdfind")  ;; Use Mac OS X's Spotlight

;; Then, we can use it like:

(setq ispell-personal-dictionary
    (concat (getenv "HOME") "/.dictionary.txt"))

(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; IDO (Interactively DO Things)

;;     According to [[http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/][Mickey]], IDO is the greatest thing.

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; According to [[https://gist.github.com/rkneufeld/5126926][Ryan Kneufeld]], we could make IDO work
;;     vertically, which is much easier to read.

(setq ido-decorations                                                      ; Make ido-mode display vertically
      (quote
       ("\n-> "           ; Opening bracket around prospect list
        ""                ; Closing bracket around prospect list
        "\n   "           ; separator between prospects
        "\n   ..."        ; appears at end of truncated list of prospects
        "["               ; opening bracket around common match string
        "]"               ; closing bracket around common match string
        " [No match]"     ; displayed when there is no match
        " [Matched]"      ; displayed if there is a single match
        " [Not readable]" ; current diretory is not readable
        " [Too big]"      ; directory too big
        " [Confirm]")))   ; confirm creation of new file or buffer

(add-hook 'ido-setup-hook                                                  ; Navigate ido-mode vertically
          (lambda ()
            (define-key ido-completion-map [down] 'ido-next-match)
            (define-key ido-completion-map [up] 'ido-prev-match)
            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

;; Backup Settings

;;     This setting moves all backup files to a central location.
;;     Got it from [[http://whattheemacsd.com/init.el-02.html][this page]].

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control

(setq vc-make-backup-files t)

;; Line Numbers

;;     Turn =nlinum-mode= on/off with =Command-K= (see the [[*Macintosh][Macintosh]]
;;     section above).  However, turn this on automatically for
;;     programming modes?

(add-hook 'prog-mode-hook 'nlinum-mode)

;; Smart Scan

;;     Use the =M-n= to search the buffer for the word the cursor is
;;     currently pointing. =M-p= to go backwards.

(load-library "smart-scan")

;; Dired Options

;;     The associated group name isn't too useful when viewing the dired output.

(setq dired-listing-switches "-alGh")

;; This enhancement to dired hides the ugly details until you hit
;;     '(' and shows the details with ')'. I also change the [...] to a
;;     simple asterisk.

(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "* ")

;; Save Place

;;     The [[http://www.emacswiki.org/emacs/SavePlace][Save Place]] mode will... well, save your place in between
;;     Emacs sessions.

(require 'saveplace)
(setq-default save-place t)

;; Strip Whitespace on Save

;;     When I save, I want to always, and I do mean always strip all
;;     trailing whitespace from the file.

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Uniquify

;;     Get rid of silly <1> and <2> to buffers with the same file name,
;;     using [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html][uniquify]].

(require 'uniquify)

;; Expand Region

;;     Wherever you are in a file, and whatever the type of file, you
;;     can slowly increase a region selection by logical segments.

(if (autofeaturep 'expand-region)
    (progn
      (require 'expand-region)
      (global-set-key (kbd "C-=") 'er/expand-region)))

;; Smart Mode Line

;;     I like the cleanliness of the [[https://github.com/Bruce-Connor/smart-mode-line][Smart Mode Line]]:

(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

(custom-set-variables '(sml/active-background-color "dark blue"))

;; Especially since you can limit the pathname of the displayed
;;     filename.

(add-to-list 'sml/replacer-regexp-list '("^~/Google Drive/" ":Goo:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Other/dot-files" ":.:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Work/wpc-api/server/" ":API:"))
(add-to-list 'sml/replacer-regexp-list '("^~/Work/wpc-fai/ci/" ":CI:"))

;; Hiding some Minor modes in the mode line is real swell. This
;;     leaves the mode-line with only important stuff.

(add-to-list 'sml/hidden-modes " GitGutter")
(add-to-list 'sml/hidden-modes " Undo-Tree")

;; Better Searching and Visual Regular Expressions

;;     Only after you've started an =isearch-forward= do you wish you had
;;     regular expressions available, so why not just switch those defaults?

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; The [[https://github.com/benma/visual-regexp.el][Visual Regular Expressions]] project highlights the matches
;;     while you try to remember the differences between Perl's regular
;;     expressions and Emacs'...

;;     Begin with =C-c r= then type the regexp. To see the highlighted
;;     matches, type =C-c a= before you hit 'Return' to accept it.

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)

;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

;; Flycheck

;;     [[https://github.com/flycheck/flycheck][Flycheck]] seems to be quite superior to good ol' Flymake.

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; Org-Mode

;;   See [[file:emacs-org.org][emacs-org-mode.el]] for details on my [[http://www.orgmode][Org-Mode]] settings.

(require 'init-org-mode)

;; Or if I should load it after the Clojure mode kicks in?

(eval-after-load 'clojure-mode '(require 'init-clojure))

;; Python

;;    See [[file:emacs-python.org][emacs-python.el]] for details on working with Python.
;;    Not sure if I should just load it directly, like:

(load-library "init-python")

;; Emacs Lisp

;;    The most important change to Emacs Lisp is colorizing the
;;    variables:

(add-hook 'emacs-lisp-mode-hook 'color-identifiers-mode)

;; Might as well pretty up the lambdas, because, we can!

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(lambda\\)\\>" (0 (prog1 ()
                                                   (compose-region (match-beginning 1)
                                                                   (match-end 1)
                                                                   ?λ))))))

;; Git

;;    Git is [[http://emacswiki.org/emacs/Git][already part of Emacs]]. However, [[http://philjackson.github.com/magit/magit.html][Magit]] is sweet.

(autoload 'magint "magit"
  "Hooking Git up to supported files." t nil)

(global-set-key (kbd "M-C-g") 'magit-status)

;; I install and use the [[https://github.com/syohex/emacs-git-gutter-fringe][Git Gutter Fringe]] as it works better with
;;    windowing versions of Emacs.

(if (autofeaturep 'git-gutter-fringe)
    (progn
      (when (window-system)
        (require 'git-gutter-fringe)
        (global-git-gutter-mode +1)
        (setq-default indicate-buffer-boundaries 'left)
        (setq-default indicate-empty-lines +1))))

;; Markdown

;;    Don't use Markdown nearly as much as I used to, but I'm surprised
;;    that the following extension-associations aren't the default:

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))

;; Using the =org-text-wrapper= function, I create some wrapper
;;    functions to make it easier to bold text in Markdown files:

(defun markdown-bold () "Wraps the region with double asterisks."
  (interactive)
  (org-text-wrapper "**"))
(defun markdown-italics () "Wraps the region with asterisks."
  (interactive)
  (org-text-wrapper "*"))
(defun markdown-code () "Wraps the region with equal signs."
  (interactive)
  (org-text-wrapper "`"))

;; Now I can associate some keystrokes to =markdown-mode=:

(add-hook 'markdown-mode-hook
      (lambda ()
        (local-set-key (kbd "A-b") 'markdown-bold)
        (local-set-key (kbd "A-i") 'markdown-italics)
        (local-set-key (kbd "A-=") 'markdown-code)))

;; Wiki

;;    Now that Atlassian changed this Wiki system so that [[https://code.google.com/p/confluence-el/][confluence.el]]
;;    doesn't work anymore (yeah, not an improvement, Atlassian), I can
;;    still use the =confluence-edit-mode= for anything with a =.wiki=
;;    extension.

(autoload 'confluence-edit-mode "confluence-edit-mode.el"
   "Major mode for editing Wiki documents" t)
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . confluence-edit-mode))

;; I would also like to create and use some formatting wrappers.

(defun wiki-bold () "Wraps the region with single asterisks."
  (interactive)
  (org-text-wrapper "*"))
(defun wiki-italics () "Wraps the region with underbars."
  (interactive)
  (org-text-wrapper "_"))
(defun wiki-code () "Wraps the region with curly brackets."
  (interactive)
  (org-text-wrapper "{{" "}}"))

;; Now I can associate some keystrokes to =markdown-mode=:

(add-hook 'confluence-edit-mode-hook
      (lambda ()
        (local-set-key (kbd "A-b") 'wiki-bold)
        (local-set-key (kbd "A-i") 'wiki-italics)
        (local-set-key (kbd "A-=") 'wiki-code)))

;; To get [[http://plantuml.sourceforge.net/download.html][PlantUML]] working in Emacs, first, download the Jar and place
;;    in the =~/bin= directory. We then set the "mode" working for
;;    editing the files:

(setq plantuml-jar-path (concat (getenv "HOME") "/bin/plantuml.jar"))

;; Second, to get [[http://zhangweize.wordpress.com/2010/08/25/creating-uml-images-by-using-plantuml-and-org-babel-in-emacs/][PlantUML]] working in org-mode, set a different variable:

(setq org-plantuml-jar-path (concat (getenv "HOME") "/bin/plantuml.jar"))

;; Eshell

;;    Great shell with some good tweaks taken from [[https://github.com/eschulte/emacs24-starter-kit/blob/master/starter-kit-eshell.org][the Starter Kit]]
;;    project. Ignoring the =.git= directories seem like a good idea.

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

;; Eshell would get somewhat confused if I ran the following commands
;;    directly through the normal Elisp library, as these need the better
;;    handling of ansiterm:

(add-hook 'eshell-mode-hook
   '(lambda nil
      (add-to-list 'eshell-visual-commands "ssh")
      (add-to-list 'eshell-visual-commands "tail")))

;; Need the correct PATH even if we start Emacs from the GUI:

(setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:"
         (getenv "PATH")))

;; If any program wants to pause the output through the =$PAGER=
;;    variable, well, we don't really need that:

(setenv "PAGER" "cat")

;; Gotta have some [[http://www.emacswiki.org/emacs/EshellAlias][shell aliases]], right?

(defalias 'e 'find-file)
(defalias 'emacs 'find-file)

;; Replacing the window with the new buffer may not be what I want.

(defalias 'ee 'find-file-other-window)

;; Some of my favorite bash aliases, can be even more helpful in
;;    Eshell.

;;    However, my =gst= command should be an alias to =magit-status=, but
;;    using the =alias= doesn't pull in the current working directory, so
;;    I make it a function, instead:

(defun eshell/gst (&rest args)
    (magit-status (pop args) nil))

(defun eshell/l (&rest args)
    (dired (pop args) nil))

;; Twitter

;;    I know, I know, reading my [[http://www.emacswiki.org/emacs-en/TwitteringMode][twitter feed in Emacs]] is pretty geeking
;;    awesome. And I can filter out tweets that match a pattern that annoys me:

(setq twittering-tweet-filters '("kickstart" "#burritowatch"))

(defun twittering-filter-tweets ()
  (setq non-matching-statuses '())
  (dolist (status twittering-new-tweets-statuses)
    (setq matched-tweets 0)
    (dolist (pat twittering-tweet-filters)
      (if (string-match pat (cdr (assoc 'text status)))
          (setq matched-tweets (+ 1 matched-tweets))))
    (if (= 0 matched-tweets)
        (setq non-matching-statuses (append non-matching-statuses `(,status)))))
  (setq new-statuses non-matching-statuses))

(add-hook 'twittering-new-tweets-hook 'twittering-filter-tweets)

;; Need to enable spell-checking for the Twitter mode.

(add-hook 'twittering-edit-mode-hook (lambda () (ispell-minor-mode) (flyspell-mode)))

;; Circe

;;    I find reading Twitter and IRC in Emacs a good idea. Really. Small
;;    bits of the Emacs window are accessible and whatnot. Currently,
;;    however, [[https://github.com/jorgenschaefer/circe/wiki][Circe]] isn't available in the standard locations, so I have
;;    it downloaded and installed, and need the following configuration:

(require 'circe)

(setq circe-network-options
      `(("Ciphermonkeys"
         :host "irc.ciphermonkeys.org"
         :nick "ha"
         :channels ("#1101"))))

;; Perhaps we want to join other channels ... you know, just for
;;    fun to see if there is something else to waste time.

(defun irc ()
  "Connect to all my IRC servers... well, just this one."
  (interactive)
  (circe "Ciphermonkeys"))

;; Let's hide all the JOIN, PART and other messages that I don't care
;;    about:

(circe-set-display-handler "JOIN" (lambda (&rest ignored) nil))
(circe-set-display-handler "PART" (lambda (&rest ignored) nil))
(circe-set-display-handler "QUIT" (lambda (&rest ignored) nil))

;; Circe (actually, lui) has the ability to intercept long pastes if
;;    it is done in a single input. Lui will then ask if the user would
;;    prefer to use a paste service.

(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

(setq lui-flyspell-p t
      lui-flyspell-alist '((".*" "american")))
