;;; color-theme-leuven.el --- Light, but colorful color theme

;; Copyright (C) 2003-2012 Fabrice Niessen
;; Time-stamp: <2012-06-22 Fri 09:49 Fabrice on MEDIACENTER>

;; Author: Fabrice Niessen <fni@missioncriticalit.com>
;; Keywords: emacs, color theme, config

;; $Revision: 7518 $
;; $Date: 2012-05-03 15:35:07 +0200 (Thu, 03 May 2012) $

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this file; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;* TODO:

;; - Have a look at http://websitetips.com/colortools/sitepro/
;; - See [[http://www.codestyle.org/css/font-family/sampler-CombinedResults.shtml][The most common fonts]]

;;* Code:

(eval-when-compile (require 'color-theme))

(defun color-theme-leuven ()
  "Color theme by Fabrice Niessen."
  (interactive)
  (let*
      (;; increase `max-lisp-eval-depth' to 600, if needed
       (max-lisp-eval-depth (max 600 max-lisp-eval-depth))

       ;; "inherited" properties, independently of definition order
       (cancel '(:slant italic :strike-through t :foreground "gray55"))
       (clock-line '(:box (:line-width 1 :color "#335EA8") :foreground "black" :background "#EEC900"))
       (code-block '(:foreground "#000088" :background "#FBF9EA"))
       (code-inline '(:box (:line-width 1 :color "#DDDDDD") :foreground "#000088" :background "#FFFFE0"))
       (column '(:height 1.0 :weight normal :slant normal :underline nil :strike-through nil :foreground "black" :background "#FFC058"))
       (diff-added '(:foreground "#008000" :background "#DDFFDD"))
       (diff-hunk-header '(:box (:line-width 1 :color "#FFE0FF") :foreground "#990099" :background "#FFEEFF"))
       (diff-none '(:foreground "gray33"))
       (diff-removed '(:foreground "#A60000" :background "#FFDDDD"))
       (directory '(:weight bold :foreground "blue" :background "#FFFFD2"))
       ;; (highlight-line '(:underline "#CACACA" :foreground "#575757" :background "#FFFECD")) ;; was background #C6C3C6
       (highlight-line '(:inverse-video t))
       (link '(:underline t :foreground "#006DAF"))
       (marked-line '(:weight bold :foreground "white" :background "red"))
       (match '(:background "#FFFF99"))
       (ol1 '(:family "DejaVu Sans" :height 1.44 :weight bold :overline "#A7A7A7" :foreground "#3C3C3C" :background "#F0F0F0")) ; 1.8
       (ol2 '(:height 1.0 :weight bold :overline "#123555" :foreground "#123555" :background "#E5F4FB")) ; 1.8
       (ol3 '(:height 1.0 :weight bold :overline "#005522" :foreground "#005522" :background "#EFFFEF")) ; 1.6
       (ol4 '(:height 1.0 :weight bold :foreground "#EA6300")) ; 1.3
       (ol5 '(:height 1.0 :weight bold :slant normal :foreground "#E3258D")) ; 1.2
       (ol6 '(:height 1.0 :weight bold :slant italic :foreground "#0077CC")) ; 1.1
       (ol7 '(:height 1.0 :weight bold :slant italic :foreground "#2EAE2C")) ; 1.1
       (ol8 '(:height 1.0 :weight bold :slant italic :foreground "#FD8008")) ; 1.1
       (shadow '(:foreground "#7F7F7F"))
       (string '(:foreground "#008000"))
       (symlink '(:foreground "deep sky blue"))
       (vc-branch '(:box (:line-width 1 :color "#00CC33") :foreground "black" :background "#AAFFAA"))
       (mail-header-name '(:weight bold :foreground "black"))
       (subject '(:weight bold :foreground "#CF5D60"))
       (region '(:background "#D2D9E0"))
       )

    (color-theme-install
     `(color-theme-leuven
       ;; frame parameters
       ((background-color . "white")
        (background-mode . light)
        (border-color . "#333333")
        (cursor-color . "#15FF00")
        (foreground-color . "#333333")
        (mouse-color . "black"))

       ;; faces
       (default ((t (nil)))) ;; required for original htmlize (otherwise, "Invalid face" error when exporting Org code blocks)
       (bold ((t (:weight bold :foreground "black"))))
       (bold-italic ((t (:weight bold :slant italic :foreground "black"))))
       (italic ((t (:slant italic :foreground "#1A1A1A"))))
       (underline ((t (:underline t))))

       ;; bbdb
       (bbdb-company ((t (:slant italic :foreground "steel blue"))))
       (bbdb-field-name ((t (:weight bold :foreground "steel blue"))))
       (bbdb-field-value ((t (:foreground "steel blue"))))
       (bbdb-name ((t (:underline t :foreground "#FF6633"))))

       ;; bookmarks
       (bm-fringe-persistent-face ((t (:background "#366CA4"))))
       (bm-persistent-face ((t (:background "#4183C6"))))

       ;; browse-kill-ring
       (browse-kill-ring-separator-face ((t (:weight bold :foreground "slate gray"))))

       ;; calendar
       (calendar-today ((t (:weight bold :background "#CCCCFF")))) ; "yellow"
       (diary-face ((t (:foreground "#87C9FC"))))  ;"dark cyan"
       (holiday-face ((t (:background "#B6B2AE"))))  ; "red"

       (cfw:face-annotation ((t (:foreground "RosyBrown" :inherit cfw:face-day-title))))
       (cfw:face-day-title ((t (:background "#F8F9FF"))))
       (cfw:face-default-content ((t (:foreground "#2952A3"))))
       (cfw:face-default-day ((t (:weight bold :inherit cfw:face-day-title))))
       (cfw:face-disable ((t (:foreground "DarkGray" :inherit cfw:face-day-title))))
       (cfw:face-grid ((t (:foreground "SlateBlue"))))
       (cfw:face-header ((t (:foreground "blue" :background "#D4E5FF" :weight bold))))
       (cfw:face-holiday ((t (:background "#FFD5E5"))))
       (cfw:face-periods ((t (:background "#668CD9" :foreground "white" :slant italic))))
       (cfw:face-saturday ((t (:foreground "SlateGray4" :background "gray90" :weight bold))))
       (cfw:face-select ((t (:background "#C3C9F8"))))
       (cfw:face-sunday ((t (:foreground "red2" :background "#FFD5E5" :weight bold))))
       (cfw:face-title ((t (:foreground "DarkGrey" :weight bold :height 2.0 :inherit variable-pitch))))
       (cfw:face-today ((t (:background "#FFF7D7"))))
       (cfw:face-today-title ((t (:background "#FAD163"))))
       (cfw:face-toolbar ((t (:foreground "gray90" :background "gray90"))))
       (cfw:face-toolbar-button-off ((t (:foreground "LightSkyBlue4" :background "white"))))
       (cfw:face-toolbar-button-on ((t (:foreground "LightPink3" :background "gray94"))))

       (change-log-date-face ((t (:foreground "purple"))))
       ;; (change-log-email
       (change-log-file ((t (:weight bold :foreground "#4183C4"))))
       ;; (change-log-list
       ;; (change-log-name

       ;; IRC
       (circe-highlight-all-nicks-face ((t (:foreground "blue" :background "#F0F0F0")))) ; other nick names
       (circe-highlight-nick-face ((t (:foreground "#009300" :background "#F0F0F0")))) ; messages with my nick cited
       (circe-my-message-face ((t (:foreground "#8B8B8B" :background "#F0F0F0"))))
       (circe-originator-face ((t (:foreground "blue"))))
       (circe-prompt-face ((t (:foreground "red"))))
       (circe-server-face ((t (:foreground "#99CAE5"))))
       (lui-button-face ((t ,link)))
       (lui-highlight-face ((t (:box '(:line-width 1 :color "#CC0000") :foreground "#CC0000" :background "#FFFF88")))) ; my nickname
       (lui-time-stamp-face ((t (:foreground "purple"))))

       ;; column-marker
       (column-marker-1-face ((t (:background "#EBFFEB"))))
       (column-marker-2-face ((t (:background "#FFFFEB"))))
       (column-marker-3-face ((t (:background "#FFEBEB"))))

       (comint-highlight-input ((t ,code-block)))
       (comint-highlight-prompt ((t (:foreground "#008ED1" :background "#EAEAFF"))))

       ;; used in modeline by grep and compile
       (compilation-error ((t (:weight bold :foreground "red"))))
       (compilation-info ((t (:weight bold :foreground "#2A489E")))) ;; used for grep
       (compilation-line-number ((t (:bold t :foreground "#A535AE"))))
       (compilation-warning ((t (:weight bold :foreground "orange"))))

       (css-property ((t (:foreground "#00AA00"))))
       (css-selector ((t (:weight bold :foreground "blue"))))

       ;; custom
       (custom-button ((t (:background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
       (custom-button-mouse ((t (:background "grey90" :foreground "black" :box (:line-width 2 :style released-button)))))
       (custom-button-pressed ((t (:foreground "black" :background "light grey" :box (:line-width 2 :style pressed-button)))))
       (custom-button-pressed-unraised ((t (:underline t :foreground "magenta4"))))
       (custom-button-unraised ((t (:underline t))))
       (custom-changed ((t (:foreground "white" :background "blue"))))
       (custom-comment ((t (:background "gray85"))))
       (custom-comment-tag ((t (:foreground "blue4"))))
       (custom-documentation ((t (nil))))
       (custom-face-tag ((t (:family "Sans Serif" :weight bold :height 1.2))))
       (custom-group-tag ((t (:bold t :foreground "blue1" :weight bold :height 1.2))))
       (custom-group-tag-1 ((t (:bold t :family "Sans Serif" :foreground "red1" :weight bold :height 1.2))))
       (custom-invalid ((t (:foreground "yellow" :background "red"))))
       (custom-link ((t (:underline t :foreground "blue1"))))
       (custom-modified ((t (:foreground "white" :background "blue"))))
       (custom-rogue ((t (:foreground "pink" :background "black"))))
       (custom-saved ((t (:underline t))))
       (custom-set ((t (:foreground "blue" :background "white"))))
       (custom-state ((t (:foreground "green4"))))
       (custom-themed ((t (:background "blue1" :foreground "white"))))
       (custom-variable-button ((t (:weight bold :underline t))))
       (custom-variable-tag ((t (:bold t :family "Sans Serif" :foreground "blue1" :weight bold :height 1.2))))

       ;; cvs
       (cvs-filename-face ((t (:foreground "blue4"))))
       (cvs-handled-face ((t (:foreground "pink"))))
       (cvs-header-face ((t (:weight bold :foreground "blue4"))))
       (cvs-marked-face ((t (:weight bold :foreground "green3"))))
       (cvs-msg-face ((t (:slant italic :foreground "gray55"))))
       (cvs-need-action-face ((t (:foreground "orange"))))
       (cvs-unknown-face ((t (:foreground "red"))))

       ;; diff
       (diff-added ((t ,diff-added)))
       (diff-changed ((t (:foreground "blue" :background "#DDDDFF"))))
       (diff-context ((t ,diff-none)))
       (diff-file-header ((t (:foreground "#0000CC" :background "#EAF2F5"))))
       (diff-file1-hunk-header  ((t (:foreground "dark magenta" :background "#EAF2F5"))))
       (diff-file2-hunk-header  ((t (:foreground "#2B7E2A" :background "#EAF2F5"))))
       (diff-header ((t (:foreground "#999999" :background "#EAF2F5"))))
       (diff-hunk-header ((t ,diff-hunk-header)))
       (diff-index ((t (:family "Sans Serif" :height 1.1 :weight bold :foreground "#4183C4" :background "#EAF2F5"))))
       (diff-indicator-added ((t (:background "#AAFFAA"))))
       (diff-indicator-changed ((t (:background "#AAAAFF"))))
       (diff-indicator-removed ((t (:background "#FFAAAA"))))
       (diff-removed ((t ,diff-removed)))

       (log-view-file ((t (:foreground "#0000CC" :background "#EAF2F5"))))
       ;; (log-view-message

       ;; dircolors
       (dircolors-face-asm ((t (:foreground "black"))))
       (dircolors-face-backup ((t (:foreground "black"))))
       (dircolors-face-compress ((t (:foreground "red"))))
       (dircolors-face-dir ((t ,directory)))
       (dircolors-face-doc ((t (:foreground "black"))))
       (dircolors-face-dos ((t (:foreground "green3"))))
       (dircolors-face-emacs ((t (:foreground "black"))))
       (dircolors-face-exec ((t (:foreground "green3"))))
       (dircolors-face-html ((t (:foreground "black"))))
       (dircolors-face-img ((t (:foreground "black"))))
       (dircolors-face-lang ((t (:foreground "black"))))
       (dircolors-face-lang-interface ((t (:foreground "black"))))
       (dircolors-face-make ((t (:foreground "black"))))
       (dircolors-face-objet ((t (:foreground "black"))))
       (dircolors-face-package ((t (:foreground "red"))))
       (dircolors-face-paddb ((t (:foreground "black"))))
       (dircolors-face-ps ((t (:foreground "black"))))
       (dircolors-face-sound ((t (:foreground "black"))))
       (dircolors-face-tar ((t (:foreground "red"))))
       (dircolors-face-text ((t (:foreground "black"))))
       (dircolors-face-yacc ((t (:foreground "black"))))

       ;; dired
       (dired-directory ((t ,directory)))
       (dired-header ((t ,directory)))
       (dired-ignored ((t (:strike-through t :foreground "red"))))
       (dired-mark ((t ,marked-line)))
       (dired-marked ((t ,marked-line)))
       (dired-symlink ((t ,symlink)))

       ;; dired+
       (diredp-compressed-file-suffix ((t (:foreground "red"))))
       (diredp-date-time ((t (:foreground "purple"))))
       (diredp-dir-heading ((t ,directory)))
       (diredp-dir-priv ((t ,directory)))
       (diredp-exec-priv ((t (:background "#03C03C"))))
       (diredp-executable-tag ((t (:foreground "green3" :background "white"))))
       (diredp-file-name ((t (:foreground "black"))))
       (diredp-file-suffix ((t (:foreground "#008000"))))
       (diredp-flag-mark-line ((t ,marked-line)))
       (diredp-ignored-file-name ((t (:strike-through t :foreground "red"))))
       (diredp-read-priv ((t (:background "#0A99FF"))))
       (diredp-write-priv ((t (:foreground "white" :background "#FF4040"))))

       ;; ediff
       (ediff-current-diff-A ((t (:foreground "gray33" :background "#FFDDDD"))))
       (ediff-current-diff-B ((t (:foreground "gray33" :background "#DDFFDD"))))
       (ediff-current-diff-C ((t (:foreground "black" :background "cyan"))))
       (ediff-even-diff-A ((t (:foreground "black" :background "light grey"))))
       (ediff-even-diff-B ((t (:foreground "black" :background "light grey"))))
       (ediff-fine-diff-A ((t (:foreground "#A60000" :background "#FFAAAA"))))
       (ediff-fine-diff-B ((t (:foreground "#008000" :background "#55FF55"))))
       (ediff-odd-diff-A ((t (:foreground "black" :background "light grey"))))
       (ediff-odd-diff-B ((t (:foreground "black" :background "light grey"))))

       ;; egg
       (egg-branch ((t ,vc-branch)))
       (egg-diff-add ((t ,diff-added)))
       (egg-diff-del ((t ,diff-removed)))
       (egg-diff-file-header ((t (:family "Sans Serif" :height 1.1 :weight bold :foreground "#4183C4"))))
       (egg-diff-hunk-header ((t ,diff-hunk-header)))
       (egg-diff-none ((t ,diff-none)))
       (egg-header ((t (:weight bold :height 1.1))))
       (egg-section-title ((t (:family "Sans Serif" :height 1.8 :weight bold :foreground "cornflower blue"))))

       (escape-glyph ((t (:foreground "#008ED1"))))

       (file-name-shadow ((t ,shadow)))

       ;; flypell
       (flyspell-duplicate-face ((t (:underline "#008000"))))
       (flyspell-incorrect-face ((t (:underline "red"))))

       ;; LaTeX
       (font-latex-bold-face ((t (:weight bold :foreground "medium sea green"))))
       (font-latex-math-face ((t (:foreground "blue"))))
       (font-latex-sectioning-1-face ((t (:family "Sans Serif" :height 2.7 :weight bold :foreground "cornflower blue"))))
       (font-latex-sectioning-2-face ((t ,ol1)))
       (font-latex-sectioning-3-face ((t ,ol2)))
       (font-latex-sectioning-4-face ((t ,ol3)))
       (font-latex-sectioning-5-face ((t ,ol4)))
       (font-latex-sedate-face ((t (:foreground "#FF5803"))))
       (font-latex-string-face ((t (:bold t :foreground "#0066FF"))))
       (font-latex-verbatim-face ((t (:foreground "tan1"))))

       ;; font-lock
       (font-lock-builtin-face ((t (:foreground "#FF5803")))) ;; was orchid
       (font-lock-comment-delimiter-face ((t (:foreground "#EE0000"))))
       (font-lock-comment-face ((t (:slant italic :foreground "#EE0000"))))
       (font-lock-constant-face ((t (:foreground "#009944")))) ;; was dark cyan
       (font-lock-doc-face ((t (:foreground "#BA2121"))))
       (font-lock-doc-string-face ((t (:foreground "#63639C"))))
       (font-lock-function-name-face ((t (:foreground "#1A50B8")))) ;; was blue
       (font-lock-keyword-face ((t (:bold t :foreground "#A535AE")))) ;; was purple1
       (font-lock-preprocessor-face ((t (:bold t :foreground "#A3A3A3")))) ;; was red, see `printf' in AWK
       (font-lock-reference-face ((t (:foreground "dark cyan"))))
       (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
       (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
       (font-lock-string-face ((t ,string)))
       (font-lock-type-face ((t (:foreground "#1B781F")))) ;; was #3AA221
       (font-lock-variable-name-face ((t (:foreground "#2E91AF")))) ;; was magenta
       (font-lock-warning-face ((t (:weight bold :foreground "red"))))

       (fringe ((t (:foreground "#808080" :background "#DDEEFF"))))

       ;; (git-header-face)
       ;; (git-ignored-face)
       ;; (git-mark-face)
       ;; (git-permission-face)
       ;; (git-separator-face)
       ;; (git-status-face)
       ;; (git-unknown-face)
       ;; (git-unmerged-face)
       ;; (git-uptodate-face)

       ;; Gnus
       (gnus-cite-attribution-face ((t (:foreground "#5050B0"))))
       (gnus-cite-face-1 ((t (:foreground "#5050B0"))))
       (gnus-cite-face-2 ((t (:foreground "#660066"))))
       (gnus-cite-face-3 ((t (:foreground "#007777"))))
       (gnus-cite-face-4 ((t (:foreground "#990000"))))
       (gnus-cite-face-5 ((t (:foreground "#000099"))))
       (gnus-cite-face-6 ((t (:foreground "#BB6600"))))
       (gnus-cite-face-7 ((t (:foreground "#5050B0"))))
       (gnus-cite-face-8 ((t (:foreground "#660066"))))
       (gnus-cite-face-9 ((t (:foreground "#007777"))))
       (gnus-cite-face-10 ((t (:foreground "#990000"))))
       (gnus-emphasis-bold ((t (:weight bold))))
       (gnus-emphasis-highlight-words ((t (:foreground "yellow" :background "black"))))
       (gnus-group-mail-1-empty-face ((t (:foreground "#5050B0"))))
       (gnus-group-mail-1-face ((t (:weight bold :foreground "#FF50B0"))))
       (gnus-group-mail-2-empty-face ((t (:foreground "#660066"))))
       (gnus-group-mail-2-face ((t (:weight bold :foreground "#FF0066"))))
       (gnus-group-mail-3-empty-face ((t (:foreground "#808080"))))
       (gnus-group-mail-3-face ((t (:weight bold :foreground "black"))))
       (gnus-group-mail-low-empty-face ((t ,cancel)))
       (gnus-group-mail-low-face ((t ,cancel)))
       (gnus-group-news-1-empty-face ((t (:foreground "#5050B0"))))
       (gnus-group-news-1-face ((t (:weight bold :foreground "#FF50B0"))))
       (gnus-group-news-2-empty-face ((t (:foreground "#660066"))))
       (gnus-group-news-2-face ((t (:weight bold :foreground "#FF0066"))))
       (gnus-group-news-3-empty-face ((t (:foreground "#808080"))))
       (gnus-group-news-3-face ((t (:weight bold :foreground "black"))))
       (gnus-group-news-4-empty-face ((t (:foreground "#990000"))))
       (gnus-group-news-4-face ((t (:weight bold :foreground "#FF0000"))))
       (gnus-group-news-5-empty-face ((t (:foreground "#000099"))))
       (gnus-group-news-5-face ((t (:weight bold :foreground "#FF0099"))))
       (gnus-group-news-6-empty-face ((t (:foreground "#808080"))))
       (gnus-group-news-6-face ((t (:weight bold :foreground "gray50"))))
       (gnus-header-content-face ((t (:family "Sans Serif" :foreground "#786FB4"))))
       (gnus-header-from-face ((t (:family "Sans Serif" :foreground "blue"))))
       (gnus-header-name-face ((t ,mail-header-name)))
       (gnus-header-newsgroups-face ((t (:family "Sans Serif" :foreground "#3399CC"))))
       (gnus-header-subject-face ((t ,subject)))
       (gnus-picon-face ((t (:foreground "yellow" :background "white"))))
       (gnus-picon-xbm-face ((t (:foreground "yellow" :background "white"))))
       (gnus-signature-face ((t (:foreground "#7F7F7F"))))
       (gnus-splash-face ((t (:foreground "#FF8C00"))))
       (gnus-summary-cancelled-face ((t ,cancel)))

       (gnus-summary-high-ancient-face ((t (:weight normal :foreground "#808080" :background "#FFFFE6"))))
       (gnus-summary-high-read-face ((t (:weight normal :foreground "#808080" :background "#FFFFE6"))))
       (gnus-summary-high-ticked-face ((t (:weight normal :foreground "black" :background "#E7AEB0"))))
       (gnus-summary-high-unread-face ((t (:weight normal :foreground "black" :background "#FFFFCC"))))

       (gnus-summary-low-ancient-face ((t (:slant italic :foreground "gray55"))))
       (gnus-summary-low-read-face ((t (:slant italic :foreground "gray55" :background "#E0E0E0"))))
       (gnus-summary-low-ticked-face ((t (:slant italic :foreground "black" :background "#E7AEB0"))))
       (gnus-summary-low-unread-face ((t (:slant italic :foreground "black"))))

       (gnus-summary-normal-ancient-face ((t (:foreground "#808080"))))
       (gnus-summary-normal-read-face ((t (:foreground "#808080"))))
       (gnus-summary-normal-ticked-face ((t (:foreground "black" :background "#E7AEB0"))))
       (gnus-summary-normal-unread-face ((t (:foreground "black"))))

       (gnus-summary-selected-face ((t (:foreground "black" :background "#FFD0D0" :underline t))))
       (gnus-x-face ((t (:foreground "black" :background "white"))))

       (header-line ((t (:weight bold :underline "black" :overline "black" :foreground "black" :background "#FFFF88"))))
       (highlight ((t ,highlight-line)))
       (highlight-symbol-face ((t (:background "#FFFFA0"))))
       (hl-line ((t ,highlight-line)))

       ;; helm
       (helm-bookmarks-su-face ((t (:foreground "red"))))
       (helm-candidate-number ((t (:foreground "black" :background "#FFFF66"))))
       (helm-dir-heading ((t (:foreground "blue" :background "pink"))))
       (helm-dir-priv ((t (:foreground "dark red" :background "light grey"))))  ;; why not `directory' face?
       (helm-file-name ((t (:foreground "blue"))))
       (helm-gentoo-match-face ((t (:foreground "red"))))
       (helm-grep-running ((t (:weight bold :foreground "white"))))
       (helm-isearch-match ((t (:background "#CCFFCC"))))
       (helm-match ((t ,match)))
       (helm-overlay-line-face ((t (:underline t :foreground "white" :background "IndianRed4"))))
       (helm-selection ((t ,highlight-line)))
       (helm-source-header ((t (:family "Sans Serif" :height 1.3 :weight bold :foreground "white" :background "#666699"))))
       (helm-visible-mark ((t ,marked-line)))
       (helm-w3m-bookmarks-face ((t (:underline t :foreground "cyan1"))))

       (html-helper-bold-face ((t (:weight bold :foreground "black"))))
       (html-helper-italic-face ((t (:slant italic :foreground "black"))))
       (html-helper-underline-face ((t (:underline t :foreground "black"))))
       (html-tag-face ((t (:foreground "blue"))))

       ;; Info / info+
       (info-file ((t (:family "Sans Serif" :height 1.8 :weight bold :box (:line-width 1 :color "#0000CC") :foreground "cornflower blue" :background "LightSteelBlue1")))) 
       (info-header-node ((t (:underline t :foreground "orange"))))  ; nodes in header
       (info-header-xref ((t (:underline t :foreground "dodger blue"))))  ; cross references in header
       (info-menu-header ((t (:family "Sans Serif" :height 1.6 :weight bold :underline t :foreground "#00CC00"))))  ; menu titles (headers) -- major topics
       (info-menu-star ((t (:foreground "black"))))  ; every 3rd menu item
       (info-node ((t (:underline t :foreground "blue"))))  ; node names
       (info-quoted-name ((t ,code-inline)))
       (info-string ((t ,string)))
       (info-title-1 ((t ,ol1)))
       (Info-title-1-face ((t ,ol1)))
       (Info-title-2-face ((t ,ol2)))
       (Info-title-3-face ((t ,ol3)))
       (Info-title-4-face ((t ,ol4)))
       (info-xref ((t (:weight bold :underline t :foreground "blue"))))  ; unvisited cross-references
       (info-xref-visited ((t (:weight bold :foreground "magenta4"))))  ; previously visited cross-references

       ;; highlighting matches
       (isearch ((t (:weight bold :foreground "#00AA00" :background "#99FF99"))))
       (isearch-fail ((t (:weight bold :foreground "black" :background "#FF9999"))))
       (isearch-lazy-highlight-face ((t (:weight bold :foreground "#990099" :background "#FF66FF")))) ;; for GNU Emacs
       (isearch-secondary ((t (:weight bold :foreground "#990099" :background "#FF66FF")))) ;; for XEmacs

       (light-symbol-face ((t (:background "#FFFFA0"))))

       (linum ((t (:foreground "#AFB7BA" :background "#DDEEFF"))))

       (magit-branch ((t ,vc-branch)))
       (magit-diff-add ((t ,diff-added)))
       (magit-diff-del ((t ,diff-removed)))
       (magit-diff-file-header ((t (:family "Sans Serif" :height 1.1 :weight bold :foreground "#4183C4"))))
       (magit-diff-hunk-header ((t ,diff-hunk-header)))
       (magit-diff-none ((t ,diff-none)))
       (magit-header ((t (:foreground "white" :background "#FF4040"))))
       (magit-item-highlight ((t (:background "#EAF2F5"))))
       (magit-item-mark ((t ,marked-line)))
       (magit-log-head-label ((t (:box (:line-width 1 :color "blue" :style nil)))))
       (magit-log-tag-label ((t (:box (:line-width 1 :color "#00CC00" :style nil)))))
       (magit-section-title ((t (:family "Sans Serif" :height 1.8 :weight bold :foreground "cornflower blue"))))

       ;; make
       (makefile-space-face ((t (:background "hot pink"))))
       (makefile-targets ((t (:weight bold :foreground "blue"))))

       (match ((t ,match)))

       ;; Message
       (message-cited-text-face ((t (:foreground "#5050B0"))))
       (message-header-cc-face ((t (:family "Sans Serif" :foreground "blue"))))
       (message-header-name-face ((t ,mail-header-name)))
       (message-header-newsgroups-face ((t (:family "Sans Serif" :foreground "#3399CC"))))
       (message-header-other-face ((t (:family "Sans Serif" :foreground "#3399CC"))))
       (message-header-subject-face ((t ,subject)))
       (message-header-to-face ((t (:family "Sans Serif" :foreground "blue"))))
       (message-header-xheader-face ((t (:foreground "red"))))
       (message-mml-face ((t (:foreground "forest green"))))
       (message-separator-face ((t (:family "Sans Serif" :weight bold :foreground "red"))))

       (mm-uu-extract ((t ,code-block)))

       (minibuffer-noticeable-prompt ((t (:weight bold :foreground "black" :background "gold"))))
       (minibuffer-prompt ((t (:weight bold :foreground "black" :background "gold"))))

       (moccur-current-line-face ((t (:background "#FFFFCC" :foreground "black"))))
       (moccur-face ((t (:background "#FFFF99" :foreground "black"))))

       ;; GNU Emacs mode-line
       (mode-line ((t (:box (:line-width 1 :color "#1A2F54") :foreground "#85CEEB" :background "#335EA8"))))
       (mode-line-buffer-id ((t (:weight bold :foreground "white"))))
       (mode-line-emphasis ((t (:weight bold :foreground "white"))))
       (mode-line-highlight ((t (:foreground "yellow"))))
       ;; (mode-line-inactive ((t (:box (:line-width 1 :color "#636363") :foreground "#818181" :background "#C6C3C6")))) ;; very good as hi-line as well
       (mode-line-inactive ((t (:box (:line-width 1 :color "#4E4E4C") :foreground "#F0F0EF" :background "#9B9C97"))))

       ;; XEmacs modeline
       (modeline ((t (:box (:line-width 1 :color "#1A2F54") :foreground "#85CEEB" :background "#335EA8"))))
       (modeline-buffer-id ((t (:weight bold :foreground "white" :background "#335EA8"))))
       (modeline-mousable ((t (:foreground "#85CEEB" :background "#335EA8")))) ; major-mode
       (modeline-mousable-minor-mode ((t (:foreground "#85CEEB" :background "#335EA8"))))

       (mumamo-background-chunk-major ((t (:background "white"))))

       ;; non-breaking space
       (nobreak-space ((t (:background "#C6C3C6"))))

       (nxml-attribute-local-name-face ((t (:foreground "magenta"))))
       (nxml-attribute-value-delimiter-face ((t (:foreground "green4"))))
       (nxml-attribute-value-face ((t (:foreground "green4"))))
       (nxml-comment-content-face  ((t (:slant italic :foreground "red"))))
       (nxml-comment-delimiter-face ((t (:foreground "red"))))
       (nxml-element-local-name-face ((t (:foreground "blue"))))
       (nxml-element-local-name ((t (:box (:line-width 1 :color "#999999") :background "#DEDEDE" :foreground "#000088"))))
       (nxml-processing-instruction-target-face ((t (:foreground "purple1"))))
       (nxml-tag-delimiter-face ((t (:foreground "blue"))))
       (nxml-tag-slash-face ((t (:foreground "blue"))))

       ;; Org
       (org-agenda-clocking ((t ,clock-line)))
       (org-agenda-column-dateline ((t ,column)))
       ;; (org-agenda-column-dateline ((t (:background "deep sky blue" :height 79 :family "Consolas"))))
       (org-agenda-current-time ((t (:underline t :foreground "#1662AF"))))
       (org-agenda-calendar-event ((t (:foreground "white" :background "#1662AF"))))
       (org-agenda-calendar-sexp ((t (:foreground "black" :background "#80CBFF"))))
       (org-agenda-date ((t (:height 1.6 :weight normal :foreground "#0063F5")))) ; "#87C9FC"
       (org-agenda-date-today ((t (:height 1.6 :weight bold :foreground "#1662AF"))))  ; "#CCCCFF"
       (org-agenda-date-weekend ((t (:height 1.6 :weight normal :foreground "dim gray"))))  ; "#B6B2AE"
       (org-agenda-diary ((t (:weight bold :foreground "green4" :background "light blue"))))
       (org-agenda-dimmed-todo-face ((t (:foreground "gold2"))))  ; org-blocked-todo
       (org-agenda-done ((t (:foreground "#555555" :background "#EEEEEE"))))
       (org-agenda-filter-category ((t (:weight bold :foreground "orange"))))
       (org-agenda-filter-tags ((t (:weight bold :foreground "orange"))))
       (org-agenda-restriction-lock ((t (:weight bold :foreground "white" :background "orange"))))
       (org-agenda-structure ((t (:height 1.6 :weight bold :box (:line-width 1 :color "#999999") :foreground "#666666" :background "#CCCCCC"))))
       (org-archived ((t (:foreground "gray70"))))
       (org-beamer-tag ((t (:box (:line-width 1 :color "#FABC18") :foreground "#2C2C2C" :background "#FFF8D0"))))
       (org-block ((t ,code-block)))
       (org-block-background ((t (:background "#FFFFE0"))))
       (org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#555555" :background "#E2E1D5"))))
       (org-block-end-line ((t (:overline "#A7A6AA" :foreground "#555555" :background "#E2E1D5"))))
       (org-checkbox ((t (:weight bold :foreground "white" :background "#777777" :box (:line-width 1 :style pressed-button)))))
       (org-clock-overlay ((t (:foreground "white" :background "SkyBlue4"))))
       (org-code ((t ,code-inline)))
       (org-column ((t ,column)))
       ;; (org-column ((t (:background "gold" :height 79 :family "Consolas"))))
       (org-column-title ((t ,column)))
       ;; (org-column-title ((t (:background "gold" :height 79 :family "Consolas"))))
       (org-date ((t (:underline t :foreground "purple"))))
       (org-default ((t (:foreground "#333333"))))
       (org-dim ((t (:foreground "#AAAAAA"))))
       (org-document-info ((t (:foreground "#484848"))))
       (org-document-info-keyword ((t (:foreground "#008ED1" :background "#EAEAFF"))))
       (org-document-title ((t (:family "Sans Serif" :height 1.8 :weight bold :foreground "black"))))
       (org-done ((t (:weight bold :box (:line-width 1 :color "#BBBBBB") :foreground "#BBBBBB" :background "#F0F0F0"))))
       (org-drawer ((t (:foreground "light sky blue"))))
       (org-ellipsis ((t (:underline "#B0EEB0" :foreground "#00BB00"))))
       (org-example ((t (:foreground "blue" :background "#EAFFEA"))))
       (org-footnote ((t (:underline t :foreground "#008ED1"))))
       (org-formula ((t (:foreground "chocolate1"))))
       (org-headline-done ((t (:height 1.0 :weight bold :strike-through "#BEBEBE" :foreground "#C5C5C5")))) ; 1.4
       ;; (org-hide ((t (:foreground "#666666" :background "#FFFFCC"))))
       (org-hide ((t (:foreground "#E2E2E2"))))
       (org-inlinetask ((t (:box (:line-width 1 :color "#EBEBEB") :foreground "#777777" :background "#FFFFD6"))))
       (org-latex-and-export-specials ((t (:foreground "blue"))))
       (org-level-1 ((t ,ol1)))
       (org-level-2 ((t ,ol2)))
       (org-level-3 ((t ,ol3)))
       (org-level-4 ((t ,ol4)))
       (org-level-5 ((t ,ol5)))
       (org-level-6 ((t ,ol6)))
       (org-level-7 ((t ,ol7)))
       (org-level-8 ((t ,ol8)))
       (org-link ((t ,link)))
       (org-list-dt ((t (:weight bold :foreground "#335EA8"))))
       (org-meta-line ((t (:foreground "#008ED1" :background "#EAEAFF"))))
       (org-mode-line-clock ((t ,clock-line)))
       (org-mode-line-clock-overrun ((t (:weight bold :box (:line-width 1 :color "#335EA8") :foreground "white" :background "#FF4040"))))
       (org-property-value ((t (:foreground "#00A000"))))
       (org-quote ((t (:slant italic :foreground "dim gray" :background "#FFFFE0"))))
       (org-scheduled ((t (:slant italic :foreground "#0063DC"))))
       (org-scheduled-previously ((t (:weight bold :foreground "#373737"))))
       (org-scheduled-today ((t (:foreground "black" :background "#FFFFCB"))))
       (org-sexp-date ((t (:foreground "purple"))))
       (org-special-keyword ((t (:foreground "#00BB00" :background "#EAFFEA"))))
       (org-table ((t (:foreground "dark green" :background "#EAFFEA"))))
       (org-tag ((t (:height 1.0 :weight normal :slant italic :box (:line-width 1 :color "#B7CCF1") :foreground "#3C424F" :background "#E5ECFA"))))
       (org-target ((t (:underline t))))
       (org-time-grid ((t (:foreground "#6D6D6D"))))
       (org-todo ((t (:weight bold :box (:line-width 1 :color "#D8ABA7") :foreground "#D8ABA7" :background "#FFE6E4"))))
       (org-upcoming-deadline ((t (:foreground "#FF5555"))))
       ;; (org-upcoming-deadline ((t (:foreground "white" :background "#E9A36A" :weight bold))))
       (org-verbatim ((t (:box (:line-width 1 :color "#DDDDDD") :foreground "#000088" :background "#E0FFE0"))))
       (org-verse ((t (:slant italic :foreground "dim gray" :background "#EEEEEE"))))
       (org-warning ((t (:weight bold :foreground "black" :background "#CCE7FF"))))

       (outline-1 ((t ,ol1)))
       (outline-2 ((t ,ol2)))
       (outline-3 ((t ,ol3)))
       (outline-4 ((t ,ol4)))
       (outline-5 ((t ,ol5)))
       (outline-6 ((t ,ol6)))
       (outline-7 ((t ,ol7)))
       (outline-8 ((t ,ol8)))

       ;; pabbrev
       (pabbrev-debug-display-label-face ((t (:background "chartreuse"))))
       (pabbrev-suggestions-label-face ((t (:weight bold :foreground "white" :background "purple"))))
       (pabbrev-suggestions-face ((t (:weight bold :foreground "white" :background "red"))))

       ;; parentheses
       (paren-face-match ((t (:foreground "white" :background "#FF3F3F"))))
       (paren-face-mismatch ((t (:weight bold :foreground "white" :background "purple"))))
       (paren-face-no-match ((t (:weight bold :foreground "white" :background "purple"))))

       (pesche-space ((t (:background "lemon chiffon"))))
       (pesche-tab ((t (:background "gold"))))

       ;; pretty print ^L
       (pp^L-highlight ((t (:strike-through t))))

       (recover-this-file ((t (:background "white" :background "#FF3F3F"))))

       ;; selected region
       (region ((t ,region)))  ; for GNU Emacs
       (zmacs-region ((t ,region)))  ; for XEmacs

       ;; used by Org-mode for highlighting matched entries and keywords
       (secondary-selection ((t ,match)))

       (shadow ((t ,shadow)))

       ;; for `cat <<EOF' in shell scripts
       (sh-heredoc ((t (:foreground "blue" :background "#FBF9EA"))))

       ;; shell
       (shell-option-face ((t (:foreground "forest green"))))
       (shell-output-2-face ((t (:foreground "blue"))))
       (shell-output-3-face ((t (:foreground "purple"))))
       (shell-output-face ((t (:foreground "black"))))
       (shell-prompt-face ((t (:weight bold :foreground "yellow"))))

       ;; parentheses
       (show-paren-match ((t (:foreground "white" :background "#FF3F3F"))))
       (show-paren-mismatch ((t (:weight bold :foreground "white" :background "purple"))))

       ;; speedbar
       (speedbar-button-face ((t (:foreground "green4"))))
       (speedbar-directory-face ((t (:foreground "blue4"))))
       (speedbar-file-face ((t (:foreground "cyan4"))))
       (speedbar-highlight-face ((t (:background "green"))))
       (speedbar-selected-face ((t (:underline t :foreground "red"))))
       (speedbar-tag-face ((t (:foreground "brown"))))

       ;; subversion
       (svn-status-directory-face ((t ,directory)))
       (svn-status-filename-face ((t (:weight bold :foreground "#4183C4"))))
       (svn-status-locked-face ((t (:weight bold :foreground "red"))))
       (svn-status-marked-face ((t ,marked-line)))
       (svn-status-marked-popup-face ((t (:weight bold :foreground "green3"))))
       (svn-status-switched-face ((t (:slant italic :foreground "gray55"))))
       (svn-status-symlink-face  ((t ,symlink)))
       (svn-status-update-available-face ((t (:foreground "orange"))))

       ;; TeX
       (tex-verbatim ((t (:foreground "blue"))))

       ;; tool-bar
       (tool-bar ((t (:box (:line-width 1 :style released-button) :foreground "black" :background "gray75"))))

       ;; tooltip
       (tooltip ((t (:foreground "black" :background "light yellow"))))

       ;; show trailing whitespace
       (trailing-whitespace ((t (:background "#F6EBFE"))))

       (traverse-match-face ((t (:weight bold :foreground "blue violet"))))

       (vc-annotate-face-FF3F3F ((t (:foreground "#FF3F3F" :background "black"))))
       (vc-annotate-face-FF6C3F ((t (:foreground "#FF3F3F" :background "black"))))
       (vc-annotate-face-FF993F ((t (:foreground "#FF993F" :background "black"))))
       (vc-annotate-face-FFC63F ((t (:foreground "#FF993F" :background "black"))))
       (vc-annotate-face-FFF33F ((t (:foreground "#FFF33F" :background "black"))))
       (vc-annotate-face-DDFF3F ((t (:foreground "#FFF33F" :background "black"))))
       (vc-annotate-face-B0FF3F ((t (:foreground "#B0FF3F" :background "black"))))
       (vc-annotate-face-83FF3F ((t (:foreground "#B0FF3F" :background "black"))))
       (vc-annotate-face-56FF3F ((t (:foreground "#4BFF4B" :background "black"))))
       (vc-annotate-face-3FFF56 ((t (:foreground "#4BFF4B" :background "black"))))
       (vc-annotate-face-3FFF83 ((t (:foreground "#3FFFB0" :background "black"))))
       (vc-annotate-face-3FFFB0 ((t (:foreground "#3FFFB0" :background "black"))))
       (vc-annotate-face-3FFFDD ((t (:foreground "#3FF3FF" :background "black"))))
       (vc-annotate-face-3FF3FF ((t (:foreground "#3FF3FF" :background "black"))))
       (vc-annotate-face-3FC6FF ((t (:foreground "#3F99FF" :background "black"))))
       (vc-annotate-face-3F99FF ((t (:foreground "#3F99FF" :background "black"))))
       (vc-annotate-face-3F6CFF ((t (:foreground "#3F3FFF" :background "black"))))
       (vc-annotate-face-3F3FFF ((t (:foreground "#3F3FFF" :background "black"))))

       ;; w3m
       (w3m-anchor ((t ,link)))
       (w3m-arrived-anchor ((t (:foreground "purple1"))))
       (w3m-bitmap-image-face ((t (:foreground "gray4" :background "green"))))
       (w3m-bold ((t (:weight bold :foreground "medium sea green"))))
       (w3m-current-anchor ((t (:weight bold :underline t :foreground "blue"))))
       (w3m-form ((t (:underline t :foreground "tan1"))))
       (w3m-form-button-face ((t (:weight bold :underline t :foreground "gray4" :background "light grey"))))
       (w3m-form-button-mouse-face ((t (:underline t :foreground "light grey" :background "#2B7E2A"))))
       ;; (w3m-form-button-mouse-face ((t (:background "orange"))))
       (w3m-form-button-pressed-face ((t (:weight bold :underline t :foreground "gray4" :background "light grey"))))
       ;; (w3m-form-button-pressed-face ((t (:background "yellow"))))
       (w3m-header-line-location-content-face ((t (:foreground "#7F7F7F":background "#F7F7F7"))))
       (w3m-header-line-location-title-face ((t (:foreground "#2C55B1" :background "#F7F7F7"))))
       (w3m-history-current-url-face ((t (:foreground "lemon chiffon"))))
       ;; (w3m-history-current-url-face ((t (:foreground "LightSkyBlue" :background "SkyBlue4"))))
       (w3m-image-face ((t (:weight bold :foreground "DarkSeaGreen2"))))
       (w3m-link-numbering ((t (:foreground "#B4C7EB"))))     ;; mouseless browsing
       (w3m-strike-through-face ((t (:strike-through t))))
       (w3m-underline-face ((t (:underline t))))
       ;; (w3m-tab-background-face ((t (:foreground "white" :background "#21364B"))))
       ;; (w3m-tab-selected-face ((t (:foreground "black" :background "Gray85" :box (:line-width 1 :style nil)))))
       ;; (w3m-tab-selected-retrieving-face ((t (:background "gray85" :foreground "white" :box (:line-width -1 :style nil)))))
       ;; (w3m-tab-unselected-face ((t (:foreground "gray20" :background "gray70" :box (:line-width 1 :style nil)))))
       ;; (w3m-tab-unselected-retrieving-face ((t (:foreground "white" :background "gray50" :box (:line-width -1 :style nil)))))

       (which-func ((t (:weight bold :foreground "white"))))

       (widget-button-face ((t ,link)))
       (widget-button-pressed-face ((t (:foreground "red"))))
       (widget-documentation-face ((t (:foreground "green4"))))
       (widget-field-face ((t (:background "gray85"))))
       (widget-inactive-face ((t (:foreground "dim gray"))))
       (widget-single-line-field-face ((t (:background "gray85"))))

       (yas/field-debug-face ((t (:background "ivory2"))))
       (yas/field-highlight-face ((t (:background "DarkSeaGreen1"))))
       ))))

(add-to-list 'color-themes
             '(color-theme-leuven "Leuven" "Fabrice Niessen"))

(provide 'color-theme-leuven)


;; This is for the sake of Emacs.
;; Local Variables:
;; ispell-local-dictionary: "en_US"
;; mode: outline-minor
;; eval: (when (locate-library "rainbow-mode") (require 'rainbow-mode) (rainbow-mode))
;; End:

;;; color-theme-leuven.el ends here
