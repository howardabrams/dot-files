
;;; ------------------------------------------
;;; Do not edit the generated file, as it has
;;; been generated, as a tangled file, by the
;;; fandifluous org-mode.
;;;
;;; Source: ~/Dropbox/dot-files/emacs.org
;;; ------------------------------------------

;; Extra Packages

;;    Extra packages not available via the package manager go in my
;;    personal stash at: =$HOME/.emacs.d=

(add-to-list 'load-path "~/.emacs.d/")

;; Make sure that PATH can references our "special" directories:
(setenv "PATH" (concat (getenv "HOME") "/bin:/usr/bin"
                       ":/usr/local/bin:/opt/local/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin"
                       (getenv "PATH")))

;; Package Manager

;;    Emacs has become like every other operating system, and now has a
;;    [[http://tromey.com/elpa/][package manager]] with its own collection of repositories. Of
;;    course, now, instead of trying to figure out and maintain
;;    packages, we have to keep track of what packages live on what
;;    repos. This is [[http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/][an improvement]].

;;    Add more package archives, like the [[http://marmalade-repo.org/][Marmalade repository]].

; (load "~/.emacs.d/elpa/package.el") Needed for version 23 only!
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

;; Package Loading

;;    The =(require)= is a problem if the library isn't available, and if
;;    it isn't available, then this file dies and doesn't complete.
;;    Seems like it would be nice to wrap the configuration of a package
;;    in a block that is ignored if the package isn't available.

;;    This is what I found [[http://stackoverflow.com/questions/7790382/how-to-determine-whether-a-package-is-installed-in-elisp][in this discussion]].

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

;; Font Settings

;;    I love syntax highlighting.

(global-font-lock-mode 1)

;; Specify the default font as =Source Code Pro=, which should already
;;    be [[http://blogs.adobe.com/typblography/2012/09/source-code-pro.html][downloaded]] and installed.

(set-frame-font "Source Code Pro")
(set-face-attribute 'default nil :font "Source Code Pro" :height 140)
(set-face-font 'default "Source Code Pro")

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
  ;; GNU ls version available in /usr/local/bin/gls
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

;;    Change window configuration and then return to the old
;;    configuration with [[http://www.emacswiki.org/emacs/WinnerMode][winner-mode]].  Use =Control-C Arrow= keys to
;;    cycle through window/frame configurations.

(if (autofeaturep 'winner-mode)
    (progn
      (winner-mode 1)))

;; I like the ability to move the current line up or down by just
;;    doing =S-M-up= and =S-M-down= (just like in Eclipse):

(load-library "line-move")

;; Key Chords

;;    Key Chords allows you to use any two keys pressed at the same time
;;    to trigger a function call. Interesting possibilities, but of
;;    course, you don't want it to make any mistakes.

;;    I like vi's =.= command, where it quickly repeats the last command
;;    you did. Emacs has similar functionality, but I never remember
;;    =C-x z=, so let's map it to something else.

(if (autofeaturep 'key-chord)
    (progn
      (require 'key-chord)
      (key-chord-mode +1)

      (key-chord-define-global ",." 'repeat)
      (key-chord-define-global "qw" 'query-replace)
      (key-chord-define-global "xo" 'other-window)
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

(autoload 'scala-mode "scala-mode"
  "Programming mode for Scala." t nil)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

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

(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

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
;;     vertically. Not sure if I like this, but we'll try.

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

(if (autofeaturep 'smart-mode-line)
    (progn
      (require 'smart-mode-line)
      (custom-set-variables '(sml/active-background-color "dark blue"))

      (add-to-list 'sml/replacer-regexp-list '("^~/Google Drive/" ":Goo:"))
      (add-to-list 'sml/replacer-regexp-list '("^~/Work/wpc-api/server/" ":API:"))
      (add-to-list 'sml/hidden-modes " Undo-Tree")
      (sml/setup)))

;; Initial Settings

;;    Initialization of Org Mode by hooking it into YASnippets, which
;;    should allow me to easily add templates to my files.

(add-hook 'org-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; Journaling

;;    Didn't realize that [[http://www.emacswiki.org/emacs/OrgJournal][org-journal]] essentially does what I have been
;;    doing by hand. With a little customization, I don't have to change
;;    anything else:

(if (autofeaturep 'org-journal)
    (progn
      (require 'org-journal)
      (setq org-journal-dir "~/journal/")))

;; All my journal entries will be formatted using org-mode:

(add-to-list 'auto-mode-alist '("[0-9]*$" . org-mode))

;; The date format is essentially, the top of the file.

(setq org-journal-date-format "#+TITLE: Journal Entry- %Y-%m-%d (%A)")

;; The time format is the heading for each section. I will set it to a
;;    blank since I really don't care about the time I add a section.

(setq org-journal-time-format "")

;; A function to easily load today (and yesterday's) journal entry.

(defun journal-file-today ()
  "Creates and load a file based on today's date."
  (interactive)
  (let ((daily-name (format-time-string "%Y%m%d")))
    (find-file (expand-file-name
                (concat "~/journal/" daily-name)))))

;; Since I sometimes (not often) forget to create

(defun journal-file-yesterday ()
  "Creates and load a file based on yesterday's date."
  (interactive)
  (let ((daily-name (format-time-string "%Y%m%d"
     (time-subtract (current-time) (days-to-time 1)))))
    (find-file (expand-file-name
                (concat "~/journal/" daily-name)))))

;; I really would really like to read what I did last year "at this
;; time", and by that, I mean, 365 days ago, plus or minus a few to get
;; to the same day of the week.

(defun journal-last-year-file ()
  "Returns the string corresponding to the journal entry that
happened 'last year' at this same time (meaning on the same day
of the week)."
(let* ((last-year-seconds (- (float-time) (* 365 24 60 60)))
       (last-year (seconds-to-time last-year-seconds))
       (last-year-dow (nth 6 (decode-time last-year)))
       (this-year-dow (nth 6 (decode-time)))
       (difference (if (> this-year-dow last-year-dow)
                       (- this-year-dow last-year-dow)
                     (- last-year-dow this-year-dow)))
       (target-date-seconds (+ last-year-seconds (* difference 24 60 60)))
       (target-date (seconds-to-time target-date-seconds)))
  (format-time-string "%Y%m%d" target-date)))

(defun journal-last-year ()
  "Loads last year's journal entry, which is not necessary the
same day of the month, but will be the same day of the week."
  (interactive)
  (let ((journal-file (concat org-journal-dir (journal-last-year-file))))
    (find-file journal-file)))

;; Org-Mode Sprint Note Files

;;     At the beginning of each sprint, we need to set this to the new
;;     sprint file.

(setq current-sprint "2013-24")

(defun current-sprint-file ()
  (expand-file-name (concat "~/Notes/Sprint-" current-sprint ".org")))

(defun get-current-sprint-file ()
  "Loads up the org-mode note associated with my current sprint."
  (interactive)
  (find-file (current-sprint-file)))

;; Recent and Heavily Used Files

;;     Daily note-taking goes into my sprint file notes, so this makes a
;;     global short-cut key.

(global-set-key (kbd "C-x C-u") 'get-current-sprint-file)

;; Org-Mode Colors

;;   Before we load =org-mode= proper, we need to set the following
;;   syntax high-lighting parameters. These are used to help bring out
;;   the source code during literate programming mode.

;;   This information came from [[http://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html][these instructions]], however, they tend
;;   to conflict with the /color-theme/, so we'll turn them off for now.

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

;; Library Loading

;;    The standard package manager (and most recent versions of Emacs)
;;    include =org-mode=, however, I want the latest version that has
;;    specific features for literate programming.

;;    Org-mode is installed in the global directory.

(require 'org)
(require 'ob-tangle)

;; Global Key Bindings

;;    The =org-mode= has some useful keybindings that are helpful no
;;    matter what mode you are using currently.

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(global-set-key (kbd "C-M-|") 'indent-rigidly)

;; Local Key Bindings

;;    A couple of short-cut keys to make it easier to edit text.

(defun org-text-wrapper (txt &optional endtxt)
  "Wraps the region with the text passed in as an argument."
  (if (use-region-p)
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (goto-char (point-min))
        (insert txt)
        (goto-char (point-max))
        (if endtxt
            (insert endtxt)
          (insert txt)))
    (if (looking-at "[A-z]")
        (save-excursion
          (if (not (looking-back "[     ]"))
              (backward-word))
          (progn
            (mark-word)
            (org-text-wrapper txt endtxt)))
      (progn
        (insert txt)
        (let ((spot (point)))
          (insert txt)
          (goto-char spot))))))

(defun org-text-bold () "Wraps the region with asterisks."
  (interactive)
  (org-text-wrapper "*"))
(defun org-text-italics () "Wraps the region with slashes."
  (interactive)
  (org-text-wrapper "/"))
(defun org-text-code () "Wraps the region with equal signs."
  (interactive)
  (org-text-wrapper "="))

;; Now we can associate some keystrokes to the org-mode:

(add-hook 'org-mode-hook
      (lambda ()
        (local-set-key (kbd "A-b") 'org-text-bold)
        (local-set-key (kbd "A-i") 'org-text-italics)
        (local-set-key (kbd "A-=") 'org-text-code)))

;; When pasting certain kinds of links, the "text" may be obvious.

(defun org-generate-link-description (url description)
  (cond
   ((string-match "jira.workday" url)
    (replace-regexp-in-string "https://jira.+/browse/" "" url))
   ((string-match "crucible.workday" url)
    (replace-regexp-in-string "https://crucible.+/cru/" "" url))
   (t description)))

(setq org-make-link-description-function 'org-generate-link-description)

;; I'm often typing Jira entries that match a particular link pattern.

(defun jira-link (b e)
  "Wraps the region with an org-mode link."
  (interactive "r")
  (save-restriction
    (narrow-to-region b e)
    (let ((jiraid (buffer-substring (point-min) (point-max))))
      (goto-char (point-min))
      (insert "[[https://jira.workday.com/browse/" jiraid "][")
      (goto-char (point-max))
      (insert "]]"))))

;; Speed Keys

;;    If point is at the beginning of a headline or code block in
;;    org-mode, single keys do fun things. See =org-speed-command-help=
;;    for details (or hit the ? key at a headline).

(setq org-use-speed-commands t)

;; Specify the Org Directories

;;    I keep all my =org-mode= files in a few directories, and I would
;;    like them automatically searched when I generate agendas.

(setq org-agenda-files '("~/Dropbox/org/personal" 
                         "~/Google Drive/technical" 
                         "~/Dropbox/org/project"))

;; Auto Note Capturing

;;    Let's say you were in the middle of something, but would like to
;;    /take a quick note/, but without affecting the file you are
;;    working on. This is called a "capture", and is bound to the
;;    following key:

(global-set-key (kbd "C-c c") 'org-capture)

;; This will bring up a list of /note capturing templates/:

(setq org-capture-templates
      '(("n" "Thought or Note" entry (file "~/Technical/general-notes.org")
         "* %i%?\n    %a" :empty-lines 1)

        ("w" "General Sprint Note" entry (file+headline (current-sprint-file) "Work Issues")
         "*** %i%?" :empty-lines 1)
        ("r" "Retrospective Status" entry (file+headline (current-sprint-file) "Status/Accomplishments")
         "*** %i%?\n  Linked: %a" :empty-lines 1)
        ("g" "Retrospective Goodness" entry (file+headline (current-sprint-file) "Keep Doing (Good)")
         "*** %i%?" :empty-lines 1)
        ("b" "Retrospective Badness" entry (file+headline (current-sprint-file) "Stop Doing (Bad)")
         "*** %i%?" :empty-lines 1)
        ("i" "Retrospective Improvement" entry (file+headline (current-sprint-file) "Start Doing (Improvements)")
         "*** %i%?" :empty-lines 1)
        ("x" "Note for Next Sprint" entry (file+headline (current-sprint-file) "Notes for Next Sprint")
         "*** %i%?" :empty-lines 1)

        ("p" "Personal Journal" entry (file+datetree "~/Technical/personal.org")
         "* Projects\n\n  %i%?\n\n  %a" :empty-lines 1)))

;; General notes go into this file:
(setq org-default-notes-file "~/Technical/personal.org")

;; Checking Things Off

;;    When I check off an item as done, sometimes I want to add some
;;    details about the completion (this is really only helpful when I'm
;;    consulting). 

;;    With this setting, each time you turn an entry from a TODO state
;;    into the DONE state, a line 'CLOSED: [timestamp]' will be inserted
;;    just after the headline. If you turn the entry back into a TODO
;;    item through further state cycling, that line will be removed
;;    again.

; (setq org-log-done 'time)
(setq org-log-done 'note)

;; Org Publishing

;;    The brilliance of =org-mode= is the ability to publish your notes
;;    as HTML files into a web server. See [[http://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html][these instructions]].

(require 'org-publish)

(setq org-publish-project-alist  '(
  ("org-notes"
   :base-directory        "~/Dropbox/org/"
   :base-extension        "org"
   :publishing-directory  "~/Sites/"
   :recursive             t
   :publishing-function   org-publish-org-to-html
   :headline-levels       4             ; Just the default for this project.
   :auto-preamble         t
   :auto-sitemap          t             ; Generate sitemap.org automagically...
   :makeindex             t
   :section-numbers       nil
   :table-of-contents     nil
   :style "<link rel=\"stylesheet\" href=\"../css/styles.css\" type=\"text/css\"/><link rel=\"stylesheet\" href=\"css/styles.css\" type=\"text/css\"/> <script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js\" type=\"text/javascript\"></script> <link href=\"http://ajax.googleapis.com/ajax/libs/jqueryui/1.7.2/themes/smoothness/jquery-ui.css\" type=\"text/css\" rel=\"Stylesheet\" />    <script src=\"https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.min.js\" type=\"text/javascript\"></script> <script =\"text/jacascript\" src=\"js/script.js\"></script>"
   )

  ("dot-files"
   :base-directory       "~/Dropbox/dot-files/"
   :base-extension       "org"
   :publishing-directory "~/Dropbox/dot-files/docs"
   :recursive            f
   :publishing-function   org-publish-org-to-html
   :auto-preamble         t
   :auto-sitemap          t             ; Generate sitemap.org automagically...
   :makeindex             f
   :section-numbers       nil
   :table-of-contents     nil
   )

  ("org-static"
   :base-directory       "~/Dropbox/org/"
   :base-extension       "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
   :publishing-directory "~/Sites/"
   :recursive            t
   :publishing-function  org-publish-attachment
   )

  ("all" :components ("org-notes" "org-static" "dot-files"))))

;; I really, really would like to affect the output of the
;;    exported/published HTML files to make them /prettier/.

(setq org-export-html-style "<link rel='stylesheet' href='http://www.howardism.org/styles/org-export-html-style.css' type='text/css'/>
<script src='http://use.edgefonts.net/source-sans-pro.js'></script>
<script src='http://use.edgefonts.net/source-code-pro.js'></script>")

;; Default Export Settings

;;     To make the =org-mode= export defaults closer to my liking
;;     (without having to put specific #+PROPERTY commands), start by
;;     =describe-variable= the =org-export-plist-vars= variable.

;;     This returns the list of variables that can be customized:

(setq org-export-with-section-numbers nil)
(setq org-export-with-toc nil)
(setq org-export-skip-text-before-1st-heading nil)

(setq org-export-html-postamble nil) ;; don't need any gunk at end

; (setq org-export-creator-info nil)
; (setq org-export-email-info nil)
; (setq org-export-author-info nil)
; (setq org-export-time-stamp-file nil)
; (setq org-export-html-with-timestamp nil)

;; Publishing as Presentation

;;     Out of all [[http://orgmode.org/worg/org-tutorials/non-beamer-presentations.html][the ideas]] online, I prefer using [[http://meyerweb.com/eric/tools/s5/][S5]], and by loading
;;     [[https://github.com/eschulte/org-S5/blob/master/org-export-as-s5.el][this code]], we can issue =org-export-as-s5=:

(autoload 'org-export-as-s5 "org-export-as-s5"
          "Module extension for Presentations for Org-Mode." t nil)

;; And let's tie this to a keystroke to make it easier to use:

(global-set-key (kbd "<f9> p") 'org-export-as-s5)

;; Presentations

;;    Currently generating presentations from my org-mode files using
;;    [[https://github.com/hakimel/reveal.js/][reveal.js]] and [[https://github.com/yjwen/org-reveal][org-reveal]].

(require 'ox-reveal)

(setq org-reveal-root (concat "file://" (getenv "HOME") "/Other/reveal.js"))

(setq org-reveal-postamble "Howard Abrams")

;; MobileOrg

;;    I use [[http://mobileorg.ncogni.to/doc/getting-started/using-dropbox/][Dropbox with MobileOrg]] in order to read my notes on my iPad.

;;    The "global" location of my Org files on my local system:

(setq org-directory "~/Dropbox/org/personal")

;; Set the name of the file where new notes will be stored

(setq org-mobile-inbox-for-pull "~/Dropbox/org/flagged.org")

;; Set to <your Dropbox root directory>/MobileOrg.

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; The Tower of Babel

;;    The trick to literate programming is in the [[http://orgmode.org/worg/org-contrib/babel/intro.html][Babel project]], which
;;    allows org-mode to not only interpret source code blocks, but
;;    evaluate them and tangle them out to a file.

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh         . t)
   (js         . t)
   (coffee     . t)
   (emacs-lisp . t)
   (perl       . t)
   (scala      . t)
   (clojure    . t)
   (python     . t)
   (dot        . t)
   (css        . t)
   (plantuml   . t)))

;; Just Evaluate It

;;     I'm normally fine with having my code automatically evaluated.

(setq org-confirm-babel-evaluate nil)

;; Font Coloring in Code Blocks
    
;;     Normally, fontifying the individual code blocks makes it
;;     impossible to work with, so instead of turning it on at the global
;;     level for all blocks, I created a couple of keystrokes to
;;     selectively colorize one block at a time.

; (setq org-src-fontify-natively t)

(global-set-key (kbd "<f9> g") 'org-src-fontify-buffer)
(global-set-key (kbd "<f9> f") 'org-src-fontify-block)

;; Clojure

;;    Me like Clojure, and since it is a LISP, then Emacs likes it too.

(if (autofeaturep 'clojure-mode)
    (progn
      (require 'clojure-mode)
      (add-hook 'clojure-mode-hook
                '(lambda ()
                   (yas/minor-mode-on)))

      ;; This makes Compojure macro calls look nicer.
      ;; https://github.com/weavejester/compojure/wiki
      (define-clojure-indent
        (defroutes 'defun)
        (GET 2)
        (POST 2)
        (PUT 2)
        (DELETE 2)
        (HEAD 2)
        (ANY 2)
        (context 2))))

;; Most LISP-based programming is better with rainbow ponies:

(if (autofeaturep 'rainbow-delimiters)
    (progn
      (add-hook 'prog-mode-hook  'rainbow-delimiters-mode)
      (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)))

;; Paredit

;;     One of the cooler features of Emacs is the [[http://emacswiki.org/emacs/ParEdit][ParEdit mode]] which
;;     keeps all parenthesis balanced in Lisp-oriented languages.
;;     See this [[http://www.emacswiki.org/emacs/PareditCheatsheet][cheatsheet]].

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

;; To associate specific language modes with ParEdit, first create a
;;     helper function:

(defun turn-on-paredit () (paredit-mode 1))

;; Then associate the following Lisp-based modes with ParEdit:

(add-hook 'emacs-lisp-mode-hook       'turn-on-paredit)
(add-hook 'lisp-mode-hook             'turn-on-paredit)
(add-hook 'lisp-interaction-mode-hook 'turn-on-paredit)
(add-hook 'scheme-mode-hook           'turn-on-paredit)
(add-hook 'clojure-mode-hook          'turn-on-paredit)
(add-hook 'cider-repl-mode-hook       'turn-on-paredit)
(add-hook 'sibiliant-mode-hook        'turn-on-paredit)

;; ElDoc

;;     Need to get [[http://emacswiki.org/emacs/ElDoc][ElDoc]] working with Clojure (oh, and with Emacs Lisp).
;;     Do I need [[https://gist.github.com/tomykaira/1386472][this EL file]]?

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Cider

;;   The [[https://github.com/clojure-emacs/cider][Cider project]] is da bomb. Usage:

;;    - =cider-jack-in= - For starting an nREPL server and setting
;;      everything up. Keyboard: =C-c M-j=
;;    - =cider= to connect to an existing nREPL server.

;;   Don't care much for the extra buffers that show up when you start:

(setq nrepl-hide-special-buffers t)

;; Stop the error buffer from popping up while working in buffers other than the REPL:

(setq cider-popup-stacktraces nil)

;; Scala

;;    We need to load the [[https://github.com/haxney/scala-mode][scala mode]].
;;    We follow [[http://www.scala-lang.org/node/354][these instructions]] to hook it up with [[http://code.google.com/p/yasnippet/][Yasnippet]].

(autoload 'scala-mode "scala-mode"
  "Programming mode for Scala." t nil)

;; Shouldn't this be done by default?
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)
             (scala-mode-feature-electric-mode)))

;; We follow [[http://jawher.net/2011/01/17/scala-development-environment-emacs-sbt-ensime/][these instructions]] to set it up with [[https://github.com/aemoncannon/ensime][Ensime]], since
;;    it current is not available as a package.

(if (file-exists-p "~/.emacs.d/ensime")
    (progn
      (add-to-list 'load-path "~/.emacs.d/ensime/elisp")
      (autoload 'ensime-mode "ensime-mode"
        "Programming support mode for Scala." t nil)
      (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)))

;; JavaScript

;;    JavaScript should have three parts:
;;    - Syntax highlight (already included)
;;    - Syntax verification (with flymake-jshint)
;;    - Interactive REPL

;;    Why yes, it seems that the JavaScript mode has a special
;;    indentation setting. Go below?

(setq js-basic-indent 2)
(setq-default js2-basic-indent 2)

(setq-default js2-basic-offset 2)
(setq-default js2-auto-indent-p t)
(setq-default js2-cleanup-whitespace t)
(setq-default js2-enter-indents-newline t)
(setq-default js2-global-externs "jQuery $")
(setq-default js2-indent-on-enter-key t)
(setq-default js2-mode-indent-ignore-first-tab t)

(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

;; We'll let fly do the error parsing...
(setq-default js2-show-parse-errors nil)

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Change the word "function" to just an "f":

(font-lock-add-keywords
 'js2-mode `(("\\(function *\\)("
             (0 (progn (compose-region (match-beginning 1) (match-end 1) "ƒ")
                       nil)))))

;; Place warning font around TODO and others:

(font-lock-add-keywords 'js2-mode
                        '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                           1 font-lock-warning-face t)))

;; FlyMake and JSHint

;;    While editing JavaScript is baked into Emacs, it is kinda cool to
;;    have it give you red sections based on [[http://www.jshint.com/][jshint]].
;;    This is done with [[http://www.emacswiki.org/emacs/FlymakeJavaScript][FlyMake]].

(autoload 'flymake-jshint "flymake-jshint"
  "Error and linting support mode for JavaScript." t nil)

(add-hook 'js-mode-hook
          (lambda () (flymake-mode 1)))

;; Refactoring JavaScript

;;     The [[https://github.com/magnars/js2-refactor.el][js2-refactor]] mode should start with `C-c C-m` and then a two-letter mnemonic shortcut.

;;        * =ef= is =extract-function=: Extracts the marked expressions out into a new named function.
;;        * =em= is =extract-method=: Extracts the marked expressions out into a new named method in an object literal.
;;        * =ip= is =introduce-parameter=: Changes the marked expression to a parameter in a local function.
;;        * =lp= is =localize-parameter=: Changes a parameter to a local var in a local function.
;;        * =eo= is =expand-object=: Converts a one line object literal to multiline.
;;        * =co= is =contract-object=: Converts a multiline object literal to one line.
;;        * =eu= is =expand-function=: Converts a one line function to multiline (expecting semicolons as statement delimiters).
;;        * =cu= is =contract-function=: Converts a multiline function to one line (expecting semicolons as statement delimiters).
;;        * =ea= is =expand-array=: Converts a one line array to multiline.
;;        * =ca= is =contract-array=: Converts a multiline array to one line.
;;        * =wi= is =wrap-buffer-in-iife=: Wraps the entire buffer in an immediately invoked function expression
;;        * =ig= is =inject-global-in-iife=: Creates a shortcut for a marked global by injecting it in the wrapping immediately invoked function expression
;;        * =ag= is =add-to-globals-annotation=: Creates a =/*global */= annotation if it is missing, and adds the var at point to it.
;;        * =ev= is =extract-var=: Takes a marked expression and replaces it with a var.
;;        * =iv= is =inline-var=: Replaces all instances of a variable with its initial value.
;;        * =rv= is =rename-var=: Renames the variable on point and all occurrences in its lexical scope.
;;        * =vt= is =var-to-this=: Changes local =var a= to be =this.a= instead.
;;        * =ao= is =arguments-to-object=: Replaces arguments to a function call with an object literal of named arguments. Requires yasnippets.
;;        * =3i= is =ternary-to-if=: Converts ternary operator to if-statement.
;;        * =sv= is =split-var-declaration=: Splits a =var= with multiple vars declared, into several =var= statements.
;;        * =uw= is =unwrap=: Replaces the parent statement with the selected region.

(if (autofeaturep 'js2-refactor)
    (progn
      (require 'js2-refactor)
      (js2r-add-keybindings-with-prefix "C-c C-m")))

;; Server JS with Node.js

;;      Use [[http://js-comint-el.sourceforge.net][js-comint]], but hook it up with node.js:

(autoload 'js-comint "js-comint"
  "Hooking JavaScript interpreter up to the JS Files." t nil)

(setenv "NODE_NO_READLINE" "1")   ;; Turn off fancy node prompt
;; Use node as our repl
(setq inferior-js-program-command "node")

;; According to [[http://nodejs.org/api/all.html#all_repl][these instructions]], we set the =NODE_NO_READLINE=
;;     variable.

;;     Need some prompt configuration for the REPL:

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)
           (replace-regexp-in-string ".*1G.*3G" "&GT;" output)
           (replace-regexp-in-string "&GT;" "> " output)))))

;; Start the JavaScript node REPL with: =run-js=
;;     Set up some helpful keyboard instructions:

(add-hook 'js2-mode-hook
        (lambda () 
          (local-set-key (kbd "C-c C-c") #'js-send-buffer)
          (local-set-key (kbd "C-c C-r") #'js-send-region)
          (local-set-key (kbd "C-c C-s") #'js-send-last-sexp)
          (local-set-key (kbd "C-c C-z") #'run-js)))

;; Slime-JS

;;      Slime seems a lot better for REPL work than js-comint.

(add-hook 'after-init-hook
  #'(lambda ()
    (when (locate-library "slime-js")
      (require 'setup-slime-js))))

;; Coffee
 
;;     Gotta load up CoffeeScript files, but I use a special shell
;;     script that loads up my special 'coughy' environment

(setq coffee-command (concat (getenv "HOME") "/bin/coughy"))

;; Python

;;    Stole Python package ideas from [[https://github.com/gabrielelanaro/emacs-for-python][Gabriel Elanaro's git project]].  The
;;    question is whether I use Rope or Jedi for auto-completion.  Seems
;;    like Rope is better, so I will use it instead of Jedi... for now.

;;    Make sure that PATH can reference the Python executables, and
;;    since I am installing a updated Python...

(setenv "PATH" (concat "/Library/Frameworks/Python.framework/Versions/2.7/bin:" (getenv "PATH")))

;; WSGI files are just Python files in disguise, so tell them to use
;;    the Python environment:

(add-to-list 'auto-mode-alist '("\\.wsgi$" . python-mode))

;; Debugging

;;     Use the [[https://pypi.python.org/pypi/virtualenv][virtualenv]] world of goodness, but only if it is installed.
;;     This allows me to =M-x virtualenv-workon= and specify the virtual
;;     environment to run all the Python gunk from within Emacs.

(if (autofeaturep 'virtualenv)
    (progn
        (require 'virtualenv)))

;; Flymake for Python

;;     Lint-style syntax checking for Python builds on the regular
;;     Flymake package.

(if (autofeaturep 'flymake-python-pyflakes)
    (progn
      (require 'flymake-python-pyflakes)
      (add-hook 'python-mode-hook 'flymake-python-pyflakes-load)))

;; Nose

;;     Unit test and code coverage tool for Python now comes to Emacs
;;     with [[http://ivory.idyll.org/articles/nose-intro.html][Python Nose]].

(if (autofeaturep 'nose)
      (progn
       (require 'nose)

       ;;   Include this line only for people with non-eco non-global test
       ;;   runners... like the Python Koans:
       (add-to-list 'nose-project-names
                    "~/Google\ Drive/python_koans/python2")))

;; IPython

;;    Got iPython and EIN? Great! Remember, pre-install the following packages:
;;    - websocket
;;    - request
;;    - ein

(if (autofeaturep 'ein)
      (progn 
        (require 'ein)
        (setq ein:use-auto-complete t)))

;; Rope

;;     After installing the following Python libraries using =pip= (in a
;;     global environment):

;;     - [[http://rope.sourceforge.net/index.html][Rope]]
;;     - [[http://rope.sourceforge.net/ropemacs.html][Ropemacs]]
;;     - [[https://pypi.python.org/pypi/ropemode][Ropemode]]

;;     And have installed [[http://pymacs.progiciels-bpi.ca/pymacs.html][pymacs]], with both =package-install= as well as
;;     by cloning [[https://github.com/pinard/Pymacs.git][this Git repo]] and issuing a =make install=.
;;     According to [[http://stackoverflow.com/questions/2855378/ropemacs-usage-tutorial][this discusssion]], we /just/ need to:

;; (require 'pymacs)
  
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

(add-hook 'python-mode-hook
          (lambda ()
            (pymacs-load "ropemacs" "rope-")
            (setq ropemacs-enable-autoimport t)))

(defun rope-before-save-actions () 
  ;; Does nothing but save us from an error.
  )
(defun rope-after-save-actions () 
  ;; Does nothing but save us from an error.
  )
(defun rope-exiting-actions () 
  ;; Does nothing but save us from an error.
  )

;; HTML, CSS and Web Work

;;    The basic web features of Emacs are often good enough, but
;;    [[https://github.com/smihica/emmet-mode][Emmet-Mode]] looks pretty sweet.

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.

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

;; Eshell

;;    E-shell doesn't read the [[http://www.emacswiki.org/emacs/EshellAlias][standard shell resource]] files or allow
;;    shell functions and aliases, so I need to create emacs-specific
;;    ones for =e= and =emacs= to simply call =find-file=:

(defun eshell/e (file)
  (find-file file))
(defun eshell/emacs (file)
  (find-file file))

;; Replacing the window with the new buffer may not be what I want.

(defun eshell/ee (file)
  (find-file-other-window file))

;; PlantUML

;;    To get [[http://plantuml.sourceforge.net/download.html][PlantUML]] working in Emacs, first, get the "mode" working for
;;    editing the files:

(setq plantuml-jar-path (concat (getenv "HOME") "/bin/plantuml.jar"))

;; Second, to get [[http://zhangweize.wordpress.com/2010/08/25/creating-uml-images-by-using-plantuml-and-org-babel-in-emacs/][PlantUML]] working in org-mode, set a different variable:

(setq org-plantuml-jar-path (concat (getenv "HOME") "/bin/plantuml.jar"))

;; exec-path

;;    The exec-path should be based on the value built up of the standard
;;    =PATH= environment variable, but it doesn't seem to, so we'll do that.

(setq exec-path (split-string (getenv "PATH") ":"))

;; Then the following code will work:

(if (file-exists-p "~/.emacs.d/clojuredocs.el")
    (load-library "clojuredocs"))
