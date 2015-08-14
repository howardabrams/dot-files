;;; ox-confluence --- Confluence Wiki Back-End for Org Export Engine

;; Copyright (C) 2012, 2014 Sébastien Delafond

;; Author: Sébastien Delafond <sdelafond at gmx dot net>
;; Keywords: outlines, confluence, wiki

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ox-confluence.el lets you convert Org files to confluence files
;; using the ox.el export engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-confluence)
;;
;; Export Org files to confluence:
;; M-x org-confluence-export-as-confluence RET
;;
;;; Code:

(require 'ox)
(require 'ox-ascii)

;; Define the backend itself
(org-export-define-derived-backend 'confluence 'ascii
  :translate-alist '((bold                . org-confluence-bold)
                     (example-block       . org-confluence-example-block)
                     (fixed-width         . org-confluence-fixed-width)
                     (verbatim            . org-confluence-verbatim)
                     (footnote-definition . org-confluence-empty)
                     (footnote-reference  . org-confluence-empty)
                     (headline            . org-confluence-headline)
                     (italic              . org-confluence-italic)
                     (item                . org-confluence-item)
                     (link                . org-confluence-link)
                     (paragraph           . org-confluence-paragraph)
                     (plain-list          . org-confluence-plain-list)
                     (property-drawer     . org-confluence-property-drawer)
                     (quote-block         . org-confluence-quote-block)
                     (quote-section       . org-confluence-quote-section)
                     (section             . org-confluence-section)
                     (src-block           . org-confluence-src-block)
                     (strike-through      . org-confluence-strike-through)
                     (table               . org-confluence-table)
                     (table-cell          . org-confluence-table-cell)
                     (table-row           . org-confluence-table-row)
                     (template            . org-confluence-template)
                     (underline           . org-confluence-underline)))

;; Helper functions

(defun org-confluence-unfill-string (s)
  "Remove initial and trailing whitespace and newlines throughout string, S."
  (org-trim (replace-regexp-in-string "[\n\r\t ]+" " " s)))

(defun org-confluence-paragraph-string (s)
  "Remove initial and trailing whitespace and newlines throughout string, S, except for paragraph separators."
  (let ((paragraphs (split-string s "\n[ \t]*\n")))
    (string-join
     (mapcar 'org-confluence-unfill-string paragraphs)
     "\n\n")))


;; All the functions we use
(defun org-confluence-bold (bold contents info)
  (format "*%s*" contents))

(defun org-confluence-empty (empty contents info)
  "")

(defun org-confluence-example-block (example-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let ((content (org-export-format-code-default example-block info)))
    (org-confluence--block "none" "Confluence" content)))

(defun org-confluence-italic (italic contents info)
  (format "_%s_" contents))

(defun org-confluence-item2 (item contents info)
  (concat (make-string (1+ (org-confluence--li-depth item)) ?\-)
          " "
          (org-trim contents)))

(defun org-confluence-checkbox (checkbox info)
  "Format CHECKBOX into Confluence (even though Confluence won't take it.  We take the CHECKBOX to be either 'on' or 'off' and we ignore INFO."
  (case checkbox (on "(/)")
        (off "(x)")
        (trans "(-)")
        (t "")))

(defun org-confluence-plain-list (plain-list contents info)
  "Wrap a list with the html-ish stuff, but only if descriptive."
  (let* (arg1 ;; (assoc :counter (org-element-map plain-list 'item
         (type (org-element-property :type plain-list)))
    (case type
      (descriptive (format "{html}<dl>\n%s\n</dl>{html}" contents))
      (otherwise   contents))))

(defun org-confluence-format-list-item (contents type checkbox info
                                                 &optional term-counter-id
                                                 headline)
  "Format a list item into Confluence."
  (let ((checkbox (concat (org-confluence-checkbox checkbox info)
                          (and checkbox " ")))
        (br "\n\n"))
    (message "Got %s with %s" type contents)
    (concat
     (case type
       (ordered
        (let* ((counter term-counter-id)
               (extra (if counter (format " value=\"%s\"" counter) "")))
          (concat
           (format "# %s" extra)
           (when headline (concat headline br)))))
       (unordered
        (let* ((id term-counter-id)
               (extra (if id (format " id=\"%s\"" id) "")))
          (concat
           (format "* %s" extra)
           (when headline (concat headline br)))))
       (descriptive
        (let* ((term term-counter-id))
          (setq term (or term "   "))
          ;; Check-boxes in descriptive lists are associated to tag.
          (concat (format "<dt>%s</dt>"
                          (concat checkbox term))
                  "<dd>"))))
     (unless (eq type 'descriptive) checkbox)
     (and contents (org-confluence-unfill-string contents)))))

(defun org-confluence-item (item contents info)
  "Transcode an ITEM element from Org to Confluence.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
         (type       (org-element-property :type plain-list))
         (counter    (org-element-property :counter item))
         (checkbox   (org-element-property :checkbox item))
         (tag  (let ((tag (org-element-property :tag item)))
                 (and tag (org-export-data tag info)))))
    (org-confluence-format-list-item
     contents type checkbox info (or tag counter))))

(defun org-confluence-fixed-width (fixed-width contents info)
  (format "\{\{%s\}\}" contents))

(defun org-confluence-verbatim (verbatim contents info)
  (format "\{\{%s\}\}"
          (org-element-property :value verbatim)))

(defun org-confluence-headline (headline contents info)
  (let ((low-level-rank (org-export-low-level-p headline info))
        (text (org-export-data (org-element-property :title headline)
                               info))
        (level (org-export-get-relative-level headline info)))
    ;; Else: Standard headline.
    (format "h%s. %s\n\n%s" level text
            (if (org-string-nw-p contents) contents
              ""))))

(defun org-confluence-link (link desc info)
  (let ((raw-link (org-element-property :raw-link link)))
    (concat "["
            (when (org-string-nw-p desc) (format "%s|" desc))
            (cond
             ((string-match "^confluence:" raw-link)
              (replace-regexp-in-string "^confluence:" "" raw-link))
             (t
              raw-link))
            "]")))

(defun org-confluence-paragraph (paragraph contents info)
  (org-confluence-unfill-string contents))

(defun org-confluence-property-drawer (property-drawer contents info)
  (and (org-string-nw-p contents)
       (format "\{\{%s\}\}" contents)))

(defun org-confluence-quote-block (block contents info)
  (format "\{quote\}%s\{quote\}"
          (org-confluence-paragraph-string contents)))

(defun org-confluence-quote-section (section contents info)
  contents)

(defun org-confluence-section (section contents info)
  contents)

(defun org-confluence-src-block (src-block contents info)
  ;; FIXME: provide a user-controlled variable for theme
  (let* ((lang (org-element-property :language src-block))
         (language (if (string= lang "sh") "bash" ;; FIXME: provide a mapping of some sort
                     lang))
         (content (org-export-format-code-default src-block info)))
    (org-confluence--block language "Emacs" content)))

(defun org-confluence-strike-through (strike-through contents info)
  (format "-%s-" contents))

(defun org-confluence-table (table contents info)
  contents)

(defun org-confluence-table-row  (table-row contents info)
  (concat
   (if (org-string-nw-p contents) (format "|%s" contents)
     "")
   (when (org-export-table-row-ends-header-p table-row info)
     "|")))

(defun org-confluence-table-cell  (table-cell contents info)
  (let ((table-row (org-export-get-parent table-cell)))
    (concat
     (when (org-export-table-row-starts-header-p table-row info)
       "|")
     contents "|")))

(defun org-confluence-template (contents info)
  (let ((depth (plist-get info :with-toc)))
    (concat (when depth "\{toc\}\n\n") contents)))

(defun org-confluence-underline (underline contents info)
  (format "+%s+" contents))

(defun org-confluence--block (language theme contents)
  (concat "\{code:theme=" theme
          (when language (format "|language=%s" language))
          "}\n"
          contents
          "\{code\}\n"))

(defun org-confluence--li-depth (item)
  "Return depth of a list item; -1 means not a list item"
  ;; FIXME check whether it's worth it to cache depth
  ;;       (it gets recalculated quite a few times while
  ;;       traversing a list)
  (let ((depth -1)
        (tag))
    (while (and item
                (setq tag (car item))
                (or (eq tag 'item) ; list items interleave with plain-list
                    (eq tag 'plain-list)))
      (when (eq tag 'item)
        (incf depth))
      (setq item (org-export-get-parent item)))
    depth))

;; main interactive entrypoint
(defun org-confluence-export-as-confluence
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CONFLUENCE Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'confluence "*org CONFLUENCE Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(defun ox-export-to-confluence
    (&optional async subtreep visible-only body-only ext-plist)
  "Export `org-mode' to buffer of text suitable for Atlassians' Confluence 5.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CONFLUENCE Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-confluence-export-as-confluence async subtreep
                                       visible-only body-only
                                       ext-plist))

(provide 'ox-confluence)

;;; ox-confluence ends here
