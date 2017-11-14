;;; powerline.el --- fancy statusline

;; Name: Emacs Powerline
;; Author: Unknown
;; Version: 1.0
;; Keywords: statusline

;;; Commentary:

;; This package simply provides a minor mode for fancifying the status line.

;; Modified by: Jonathan Chu
;; Modified by: Howard Abrams

;;; Code:

(require 'cl)

(defvar powerline-color1)
(defvar powerline-color2)

(setq powerline-color1 "grey22")
(setq powerline-color2 "grey40")

(set-face-attribute 'mode-line nil
                    :background "OliveDrab3"
                    :foreground "black"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(defun get-arrow-dots (leftp width height)
  (let* ((halfheight (/ height 2)))
    (mapconcat
     (apply-partially 'format "\"%s\"")
     (mapcar
      (lambda (n)
	(let* ((nx (if (< n halfheight) n (- height n)))
	       (dots (make-string nx ?.))
	       (spaces (make-string (- width nx) ? )))
	  (if leftp
	      (concat dots spaces)
	    (concat spaces dots))))
      (arrow-number-sequence height))
     ",\n")))

(defun arrow-number-sequence (height)
  (if (oddp height)
      (number-sequence 1 height)
    (let* ((halfheight (/ height 2)))
      (append
       (number-sequence 1 halfheight)
       (cons halfheight (number-sequence (1+ halfheight) (1- height)))))))

(defun get-arrow-xpm (direction width height &optional color1 color2)
  "Create an XPM left arrow."
  (let* ((leftp (eq 'left direction))
         (fg (if leftp color1 color2))
         (bg (if leftp color2 color1)))
    (create-image
     (format "/* XPM */
static char * arrow_left[] = {
\"%d %d 2 1\",
\". c %s\",
\"  c %s\",
%s};"
             width height
             (if fg fg "None")
             (if bg bg "None")
             (get-arrow-dots leftp width height))
     'xpm t :ascent 'center)))

(defun mode-line-height ()
  "The mode line height with its current font face."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

(defun proportional-arrow-xpm
    (direction color1 color2)
  (let* ((r 1.5)
         (m-height (mode-line-height))
         (height (if (evenp m-height) m-height (+ 1 m-height)))
         (width (floor (/ height r))))
    (get-arrow-xpm direction width height color1 color2)))

(defun arrow-left-xpm
  (color1 color2)
  "Return an XPM left arrow string representing."
  (proportional-arrow-xpm 'left color1 color2))

(defun arrow-right-xpm
    (color1 color2)
  "Return an XPM right arrow string representing."
  (proportional-arrow-xpm 'right color1 color2))

(defun curve-right-xpm
    (color1 color2)
  "Return an XPM right curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_right[] = {
\"12 26 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"         ...\",
\"         ...\",
\"       .....\",
\"       .....\",
\"       .....\",
\"      ......\",
\"      ......\",
\"      ......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"     .......\",
\"      ......\",
\"      ......\",
\"      ......\",
\"       .....\",
\"       .....\",
\"       .....\",
\"         ...\",
\"         ...\",
\"           .\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-left-xpm (color1 color2)
  "Return an XPM left curve string representing."
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"12 26 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"...         \",
\"...         \",
\".....       \",
\".....       \",
\".....       \",
\"......      \",
\"......      \",
\"......      \",
\".......     \",
\".......     \",
\".......     \",
\".......     \",
\".......     \",
\".......     \",
\".......     \",
\".......     \",
\"......      \",
\"......      \",
\"......      \",
\".....       \",
\".....       \",
\".....       \",
\"...         \",
\"...         \",
\".           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun make-xpm
  (name color1 color2 data)
  "Return an XPM image for lol data"
  (create-image
   (concat
    (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
            (downcase (replace-regexp-in-string " " "_" name))
            (length (car data))
            (length data)
            (if color1 color1 "None")
            (if color2 color2 "None"))
    (let ((len  (length data))
          (idx  0))
      (apply 'concat
             (mapcar #'(lambda (dl)
                        (setq idx (+ idx 1))
                        (concat
                         "\""
                         (concat
                          (mapcar #'(lambda (d)
                                     (if (eq d 0)
                                         (string-to-char " ")
                                       (string-to-char ".")))
                                  dl))
                         (if (eq idx len)
                             "\"};"
                           "\",\n")))
                     data))))
   'xpm t :ascent 'center))

(defun half-xpm
  (color1 color2)
  (make-xpm "half" color1 color2
            (make-list 18
                       (append (make-list 6 0)
                               (make-list 6 1)))))

(defun percent-xpm
    (pmax pmin we ws width color1 color2)
  (let* ((fs   (if (eq pmin ws)
                   0
                 (round (* 17 (/ (float ws) (float pmax))))))
         (fe   (if (eq pmax we)
                   17
                 (round (* 17 (/ (float we) (float pmax))))))
         (o    nil)
         (i    0))
    (while (< i 18)
      (setq o (cons
               (if (and (<= fs i)
                        (<= i fe))
                   (append (list 0) (make-list width 1) (list 0))
                 (append (list 0) (make-list width 0) (list 0)))
               o))
      (setq i (+ i 1)))
    (make-xpm "percent" color1 color2 (reverse o))))


;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of the given function."
  (let ((table-sym (gensym))
    (val-sym (gensym))
    (args-sym (gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
     (if ,val-sym
         ,val-sym
       (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))

(memoize 'arrow-left-xpm)
(memoize 'arrow-right-xpm)
(memoize 'curve-left-xpm)
(memoize 'curve-right-xpm)
(memoize 'half-xpm)
(memoize 'percent-xpm)

(defvar powerline-minor-modes nil)
(defvar powerline-arrow-shape 'arrow)
(defun powerline-make-face
    (bg &optional fg)
  (if bg
      (let ((cface (intern (concat "powerline-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)2
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                        ; :weight 'normal
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                        ; :weight 'normal
                                  :box nil))
          (set-face-attribute cface nil
                              :foreground "white"
                              :background bg
                                        ; :weight 'normal
                              :box nil))
        cface)
    nil))

(defun powerline-make-left
    (string color1 &optional color2 localmap)
  (let ((plface (powerline-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display
                     (cond ((eq powerline-arrow-shape 'arrow)
                            (arrow-left-xpm color1 color2))
                           ((eq powerline-arrow-shape 'curve)
                            (curve-left-xpm color1 color2))
                           ((eq powerline-arrow-shape 'half)
                            (half-xpm color2 color1))
                           (t
                            (arrow-left-xpm color1 color2)))
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq powerline-arrow-shape
                                                  (cond ((eq powerline-arrow-shape 'arrow) 'curve)
                                                        ((eq powerline-arrow-shape 'curve) 'half)
                                                        ((eq powerline-arrow-shape 'half)  'arrow)
                                                        (t                                 'arrow)))
                                            (force-mode-line-update))))
       ""))))

(defun powerline-make-right
    (string color2 &optional color1 localmap)
  (let ((plface (powerline-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
         (propertize " " 'display
                     (cond ((eq powerline-arrow-shape 'arrow)
                            (arrow-right-xpm color1 color2))
                           ((eq powerline-arrow-shape 'curve)
                            (curve-right-xpm color1 color2))
                           ((eq powerline-arrow-shape 'half)
                            (half-xpm color2 color1))
                           (t
                            (arrow-right-xpm color1 color2)))
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq powerline-arrow-shape
                                                  (cond ((eq powerline-arrow-shape 'arrow) 'curve)
                                                        ((eq powerline-arrow-shape 'curve) 'half)
                                                        ((eq powerline-arrow-shape 'half)  'arrow)
                                                        (t                                 'arrow)))
                                            (force-mode-line-update))))
       "")
     (if arrow
         (propertize "" 'face plface)
       "")
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))

;; get-scroll-bar-mode is not available in emacs 23.2
(if (not (functionp 'get-scroll-bar-mode))
    (defun get-scroll-bar-mode () scroll-bar-mode))

(defun powerline-make-fill
  (color right-indent)
  ;; justify right by filling with spaces to right fringe, 20 should be calculated
  (let ((plface (powerline-make-face color)))
    (if (eq 'right (get-scroll-bar-mode))
        (propertize " " 'display `((space :align-to (- right-fringe ,(+ 1 right-indent))))
                    'face plface)
      (propertize " " 'display `((space :align-to (- right-fringe ,(+ 4 right-indent))))
                  'face plface))))

(defun strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun powerline-items-length
  (powerline-items)
  (length
   (strip-text-properties
    (format-mode-line
     (list "%e"
           powerline-items )))))

(defun powerline-pull-right
  (powerline-items)
  (append
   (list (powerline-make-fill powerline-color2  (powerline-items-length powerline-items)) )
   powerline-items
   ))

(defun powerline-make-text
  (string color &optional fg localmap)
  (let ((plface (powerline-make-face color)))
    (if string
        (if localmap
            (propertize string 'face plface 'mouse-face plface 'local-map localmap)
          (propertize string 'face plface))
      "")))

(defun powerline-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (powerline-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (powerline-make-left   string color1 color2 localmap))
        ((eq side 'left)               (powerline-make-left   string color1 color1 localmap))
        ((eq side 'right)              (powerline-make-right  string color1 color1 localmap))
        (t                             (powerline-make-text   string color1 localmap))))

(defmacro defpowerline (name string)
  `(defun ,(intern (concat "powerline-" (symbol-name name)))
       (side color1 &optional color2)
     (powerline-make side
                     ,string
                     color1 color2)))

(defun powerline-mouse (click-group click-type string)
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                   nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

(defpowerline arrow       "")
(defpowerline buffer-id   (propertize "%b"
                                      'face `(:weight 'ultra-bold
                                                      :height 2000
                                                      :foreground "green")
                                      'help-echo "What the hello."))

(defvar powerline-buffer-size-suffix t)
(defpowerline buffer-size (propertize
                           (if powerline-buffer-size-suffix
                               "%I"
                             "%i")
                           'local-map (make-mode-line-mouse-map
                                       'mouse-1 (lambda () (interactive)
                                                  (setq powerline-buffer-size-suffix
                                                        (not powerline-buffer-size-suffix))
                                                  (force-mode-line-update)))))
(defpowerline lcl         current-input-method-title)
(defpowerline rmw         "%*")

(defun powerline-get-icon (name alt-sym help-message)
  "Returns a propertized icon if available, otherwise, returns ALT-SYM."
  (if (not (fboundp 'all-the-icons-faicon))
      alt-sym
    (propertize (format " %s" (all-the-icons-faicon name))
                'face `(:family ,(all-the-icons-faicon-family))
                'display '(raise -0.1)
                'help-echo help-message)))

(defpowerline modified
  (condition-case ex
      (let ((state (vc-git-state (buffer-file-name))))
        (cond ((buffer-modified-p)  (powerline-get-icon "pencil" "✦" "Modified buffer"))
              ((eq state 'edited)   (powerline-get-icon "pencil" "✦" "Modified buffer, unregistered changes"))
              ((eq state 'unregistered) (powerline-get-icon "question" "❓" "Unregistered file in VCS"))
              ((eq state 'missing)  (powerline-get-icon "exclamation" "⁈" "File exists only in VCS, not on the hard disk"))
              ((eq state 'ignored)  (powerline-get-icon "ban" "♟" "Ignored file in VCS"))
              ((eq state 'added)    (powerline-get-icon "plus" "➕" "File will be registered in VCS in the next commit"))
              (t " ")))
    (error (powerline-get-icon "exclamation" "⁈" (car ex)))))

(defpowerline major-mode  (propertize (format-mode-line mode-name)
                                      'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
                                      'local-map (let ((map (make-sparse-keymap)))
                                                   (define-key map [mode-line down-mouse-1]
                                                     `(menu-item ,(purecopy "Menu Bar") ignore
                                                                 :filter (lambda (_) (mouse-menu-major-mode-map))))
                                                   (define-key map [mode-line mouse-2] 'describe-mode)
                                                   (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                                                   map)))

;; The =use-package= project diminishes most minor modes, but I still
;; have one left:

(defun trim-minor-modes (s)
  (replace-regexp-in-string "ARev" "↺" s))
;; (replace-regexp-in-string "FlyC" "✓" s)

(defpowerline minor-modes
  (let ((mms (split-string (trim-minor-modes (format-mode-line minor-mode-alist)))))
    (apply 'concat
           (mapcar #'(lambda (mm)
                       (propertize (if (string= (car mms)
                                                mm)
                                       mm
                                     (concat " " mm))
                                   'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                                   'local-map (let ((map (make-sparse-keymap)))
                                                (define-key map [mode-line down-mouse-1]   (powerline-mouse 'minor 'menu mm))
                                                (define-key map [mode-line mouse-2]        (powerline-mouse 'minor 'help mm))
                                                (define-key map [mode-line down-mouse-3]   (powerline-mouse 'minor 'menu mm))
                                                (define-key map [header-line down-mouse-3] (powerline-mouse 'minor 'menu mm))
                                                map)))
                   mms))))
(defpowerline row         "%4l")
(defpowerline column      "%3c")
(defpowerline percent     "%6p")
(defpowerline narrow
  (let (real-point-min real-point-max)
    (save-excursion
      (save-restriction
        (widen)
        (setq real-point-min (point-min) real-point-max (point-max))))
    (when (or (/= real-point-min (point-min))
              (/= real-point-max (point-max)))
      (propertize "Narrow"
                  'help-echo "mouse-1: Remove narrowing from the current buffer"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 'mode-line-widen)))))

(defpowerline status      "%s")
(defpowerline emacsclient mode-line-client)
(defpowerline vc vc-mode)

(defpowerline percent-xpm
  (propertize " "
              'display
              (let (pmax
                    pmin
                    (ws (window-start))
                    (we (window-end)))
                (save-restriction
                  (widen)
                  (setq pmax (point-max))
                  (setq pmin (point-min)))
                (percent-xpm pmax pmin we ws 15 color1 color2))))

(defpowerline project-vc
  (ignore-errors
    (let* ((git (when (buffer-file-name)
                  (vc-git-mode-line-string (buffer-file-name))))
           (help (when git
                   t))
           (icon (propertize ""
                             'face `(:height 0.4)
                             'display '(raise -0.1))))
      (concat
       (projectile-project-name)
       (when git
         (replace-regexp-in-string "Git\\([:-]?\\)\\(@?\\)"
                                   (concat "\\1 " icon  " ")
                                   git))))))



;; In order to check if the current mode matches a particular mode, we
;; need to do something like:

(defun is-mode-p (mode)
  "Predicate to return `true' if the current buffer's major mode matches the requested MODE."
  (buffer-local-value 'major-mode (current-buffer))
  (eq major-mode mode))

;; Let's display the current Ruby version and Gemset, but only if the
;; buffer's mode is set to Ruby:

(defun current-ruby-mode-line ()
  "Display the Ruby version and Gemset (using RVM) if `ruby-mode' is enabled. Nil otherwise."
  (ignore-errors
    (when (is-mode-p 'ruby-mode)
      (concat (replace-regexp-in-string "ruby-" "" rvm--current-ruby)
              (propertize " \xe92b " ; "\xe92a"
                          'face `(:family "all-the-icons" :height 1.2)
                          'display '(raise -0.1))
              (when rvm--current-gemset
                rvm--current-gemset)))))

;; Display the current Python virtual environment, using =pyenv=:

(defun current-python-mode-line ()
  "Display the Python virtual environment and version if `python-mode' is enabled. Nil otherwise."
  (ignore-errors
    (when (and (is-mode-p 'python-mode) (pyenv-mode-version))
      (concat
       (propertize "\xe928 "
                   'face `(:family "all-the-icons" :height 1.2)
                   'display '(raise -0.1))
       (pyenv-mode-version)))))

(defpowerline lang-version
  (or (current-python-mode-line)
      (current-ruby-mode-line)
      (when (fboundp #'eyebrowse-mode-line-indicator)
        (eyebrowse-mode-line-indicator))
      " "))

;; Display the function name if we are in a programming mode:
(defpowerline which-function
  (if (and (derived-mode-p 'prog-mode) which-function-mode)
      (which-function)
    "%4l:%3c"))

(defpowerline display-time display-time-string)

;; Evil Mode Details
;;
;; Change the bottom right corner of the mode line to not only display
;; the textual details of the evil state, but also to colorize it to
;; make it more obvious.

(defface powerline-evil-insert-face
  '((((class color))
     (:background "green" :foreground "black" :weight bold))
    (t (:weight bold)))
  "face to fontify mode-line for Evil's `insert' state"
  :group 'powerline)

(defface powerline-evil-normal-face
  '((((class color))
     (:background "red" :foreground "black" :weight bold))
    (t (:weight bold)))
  "face to fontify mode-line for Evil's `normal' state"
  :group 'powerline)

(defface powerline-evil-visual-face
  '((((class color))
     (:background "yellow" :foreground "black" :weight bold))
    (t (:weight bold)))
  "face to fontify mode-line for Evil's `visual' state"
  :group 'powerline)

(defface powerline-evil-motion-face
  '((((class color))
     (:background "blue" :foreground "black" :weight bold))
    (t (:weight bold)))
  "face to fontify mode-line for Evil's `motion' state"
  :group 'powerline)

;; (defface powerline-evil-emacs-face
;;   '((((class color))
;;      (:background "blue violet" :foreground "black" :weight bold))
;;     (t (:weight bold)))
;;   "face to fontify mode-line for Evil's `emacs' state"
;;   :group 'powerline)

(defun powerline-evil-face (active)
  (let ((face (intern (concat "powerline-evil-" (symbol-name evil-state) "-face"))))
    (cond ((equal (symbol-name evil-state) "emacs") 'mode-line)
          ((and active (facep face))
           face)
          (active 'mode-line)
          (t 'powerline-inactive1))))

(defun powerline-evil-state ()
  "Displays *my* version of displaying the evil state."
  (case evil-state
    ('normal " Ⓝ")
    ('insert " Ⓘ")
    ('visual " Ⓥ")
    ('motion " Ⓜ")
    (t       " Ⓔ")))

(defpowerline evil
  (powerline-evil-state))

;; My customizations:
(setq powerline-arrow-shape 'curve)
;; (setq display-time-format "%I:%M")
;; (display-time-mode)

(setq mode-line-format
      (list "%e"
            '(:eval (list
                     (powerline-lcl           'left       powerline-color1)
                     (powerline-modified      'left       powerline-color1)
                     (powerline-arrow         'right  nil powerline-color1)
                     (powerline-buffer-id     'left   nil powerline-color2)
                     (powerline-project-vc    'left       powerline-color2)
                     (powerline-arrow         'left       powerline-color2 powerline-color1)
                     (powerline-major-mode    'left       powerline-color1)
                     (powerline-minor-modes   'left       powerline-color1)
                     (powerline-narrow        'left       powerline-color1 powerline-color2)
                     (powerline-lang-version  'center     powerline-color2)
                     (powerline-pull-right (list
                                            (powerline-evil 'right nil (powerline-evil-face t))
                                            (powerline-which-function nil nil (powerline-evil-face t)))))

                    )))

(provide 'my-powerline)

;;; powerline.el ends here
