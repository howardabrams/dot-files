;;; BETTER-BREADCRUMBS --- A ringless, ordered breadcrumb trail
;;
;; Author: Howard Abrams <howard.abrams@workday.com>
;; Copyright © 2017, Howard Abrams, all rights reserved.
;; Created: 25 April 2017
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;;  Based on ideas developed during a PDX Emacs Hack Night in Portland
;;  Oregon- http://howardism.org/Technical/Emacs/hack-night-summary-1.html
;;
;;  The idea is to be able to create a transient trail of breadcrumbs
;;  through buffers, and return to them in order, beginning to end (or
;;  end to beginning). If, in the middle of the trail, you drop
;;  another breadcrumb, that position is placed in the middle of the
;;  trail next to the previous visited breadcrumb.
;;
;;  *Note:* The same keybinding/function is used to both drop and
;;  remove breadcrumb. To make it easier, a line in a buffer can only
;;  contain one breadcrumb, and dropping a crumb on a line that
;;  already has one, deletes it instead.
;;
;;  Typically one turns on \\[better-breadcrumbs-mode] to add keybindings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Create a fringe icon that looks sort of like a bookmark
(define-fringe-bitmap 'bbc-marker [254 254 254 254 254 238 198 130] 8 8 'center)

(defface bbc-fringe-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:foreground "DarkOrange1"))
    (((class color)
      (background dark))  (:foreground "DarkOrange4")))
  "Face used to highlight lines with a breadcrumb in the fringe.")

(setq lexical-binding 1)

;; TODO: Change this to a (lexical-let) binding:

(defvar bbc--breadcrumbs (list)
  "List of breadcrumbs for this session")
(defvar bbc--current-crumb 0 "An index into the breadcrumbs")


(cl-defstruct bbc--breadcrumb marker line note overlay)

(defun bbc--new-breadcrumb (&optional note)
  "Return a breadcrumb for the current position.

Each stored breadcrumb contains the buffer and character position
(marker), but also any annotation note. We also cache the line
(used for quicker comparisons). The overlay stores the fringe icon,
and we need to keep it with the crumb so that we can remove it
later, see \\[bbc-drop-crumb]."
  (make-bbc--breadcrumb :marker (point-marker)
                        :line (line-number-at-pos)
                        :note note
                        :overlay (bbc--show-crumb)))

(defun bbc--find-crumb-on-line ()
  "Return the breadcrumb on the current line, `nil' otherwise."
  (let ((line (line-number-at-pos)))
    (cl-reduce (lambda (results crumb)
              (if (eq (bbc--breadcrumb-line crumb) line)
                  crumb
                results))
            bbc--breadcrumbs :initial-value nil)))

(defun bbc--list-insert (list index element)
  "Insert ELEMENT into the LIST, at INDEX."
  ;; Calculated position is based on the behavior of the `last' and
  ;; `butlast' functions.
  (let ((pos (1- (- (length list) index))))

    (append (butlast list pos)           ; First section
            (list element)              ; Element as a list
            (last list pos))))           ; Second section

(defun bbc-annotate-crumb (note)
  "Annotates the current breadcrumb with NOTE.

Each time you return to the breadcrumb mark, the annotation is
displayed in the mini-buffer.

To annotate and create a breadcrumb mark at the same time, call
\\[bbc-drop-crumb] with a prefix (C-u), as it calls this function
interactively to prompt for a small annotation for this mark."
  (interactive "sNote for Crumb:")
  (let ((crumb (bbc--find-crumb-on-line)))
    (when crumb
      (setf (bbc--breadcrumb-note crumb) note))))

(defun bbc--show-crumb ()
  "Displays an icon in the fringe where a bookmark has been placed.

Returns the buffer overlay sequence created, which must be passed
to \\[bbc--hide-crumb] in order to remove the fringe icon."
  (let ((overlay (make-overlay (point) (1+ (point))))
        (marker-string "*ignored*"))
    (put-text-property 0 (length marker-string) 'display
                       (list 'left-fringe 'bbc-marker 'bbc-fringe-face)
                       marker-string)
    (overlay-put overlay 'before-string marker-string)
    overlay))

(defun bbc--hide-crumb (crumb)
  "Removes the fringe icon associated with a CRUMB."
  (let ((overlay (bbc--breadcrumb-overlay crumb)))
    (overlay-put overlay 'before-string nil)))

(defun bbc-drop-crumb (prefix)
  "Places a 'breadcrumb' at the current position of the point in
the buffer file.  The trail of breadcrumbs is a linear sequence,
and it remembers where you are in the trail, and will insert the
crumb in between crumbs on the trail.

Calling this function with a prefix (C-u), it prompts for a small
annotation for this mark, and each time you return to the mark,
that annotation is displayed in the mini-buffer.

If a breadcrumb mark has already been set on the current line,
then calling this function will delete that mark.

Return to the mark by calling either \\[bbc-next-crumb] or
\\[bbc-previous-crumb]."
  (interactive "P")
  (let ((this-crumb (bbc--find-crumb-on-line)))
    ;; If we already have a crumb on the current line, delete it
    (if this-crumb
        (bbc--delete-crumb this-crumb)

      (setq bbc--breadcrumbs
            (bbc--list-insert bbc--breadcrumbs bbc--current-crumb
                              (bbc--new-breadcrumb)))
      (setq bbc--current-crumb (1+ bbc--current-crumb))
      ;; Since the breadcrumb has been created, we can simply call to
      ;; annotate it:
      (when prefix
        (call-interactively #'bbc-annotate-crumb)))))

(defun bbc-clear-crumbs ()
  "Remove all crumbs from the current session."
  (interactive)
  (setq bbc--breadcrumbs (list))
  (setq bbc--current-crumb 0))

(defun bbc--delete-crumb (crumb)
  "Removes a breadcrumb marker from the trail (list)."
  (bbc--hide-crumb crumb)
  (delete crumb bbc--breadcrumbs))

(defun bbc-follow-crumb ()
  "Position the point on the breadcrumb specified by \\[bbc--current-crumb]."
  (if bbc--breadcrumbs
      (let* ((crumb (nth bbc--current-crumb bbc--breadcrumbs))
             (mark  (bbc--breadcrumb-marker crumb))
             (note  (bbc--breadcrumb-note crumb))
             (buf   (marker-buffer mark))
             (poit  (marker-position mark)))
        (pop-to-buffer buf)
        (goto-char poit)
        (when (and note (> (length note) 0))
          (message note)))))

(defun bbc-previous-crumb ()
  "Return to an earlier breadcrumb marker (based on current index
into the trail specified by \\[bbc-current-crumb]. Does not
change the index when calling this function when at the first
breadcrumb marker in the trail."
  (interactive)
  (when (> bbc--current-crumb 0)
    (setq bbc--current-crumb (1- bbc--current-crumb)))
  (bbc-follow-crumb))

(defun bbc-next-crumb ()
  "Return to an later breadcrumb marker (based on current index
into the trail specified by \\[bbc-current-crumb].

Does not change the index when calling this function when at the
last breadcrumb marker in the trail. See \\[bbc-previous-crumb]
for moving to a previously dropped breadcrumb marker."
  (interactive)
  (when (< bbc--current-crumb (1- (length bbc--breadcrumbs)))
    (setq bbc--current-crumb (1+ bbc--current-crumb)))
  (bbc-follow-crumb))

(defun bbc-first-crumb ()
  "Sets the point to the first breadcrumb on the stack."
  (interactive)
  (setq bbc--current-crumb 0)
  (bbc-follow-crumb))

(defun bbc-latest-crumb ()
  "Sets the point to the latest created breadcrumb on the trail."
  (interactive)
  (setq bbc--current-crumb (1- (length bbc--breadcrumbs)))
  (bbc-follow-crumb))


(defun bbc-drop-crumb-defun (prefix)
  "Drops a breadcrumb mark at the beginning of the current defun.

See \\[bbc-drop-crumb] for details of a breadcrumb marker behavior."
  (interactive "P")
  (save-excursion
    (beginning-of-defun)
    (bbc-drop-crumb prefix)))


(define-minor-mode better-breadcrumbs-mode
  "Toggle Better Breadcrumbs mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When enabled, Better Breadcrumbs provides the following keys:

  * C-c >    \\[bbc-drop-crumb]
  * C-c .    \\[bbc-next-crumb]
  * C-c ,    \\[bbc-previous-crumb]
  * C-c C->  Drops crumb at the beginning of the current function

Since `C-c .' conflicts with an org-mode binding, `C-c C-,'
aliases to `bbc-next-crumb'.

Note: Calling `bbc-drop-crumb' a second time deletes the crumb,
call with a prefix (C-u) prompts for a note to annotate it."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " ⁛"
  :global t
  ;; The minor mode bindings.
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c .")   'bbc-next-crumb)
            (define-key map (kbd "C-c C-,") 'bbc-next-crumb)
            (define-key map (kbd "C-c ,")   'bbc-previous-crumb)
            (define-key map (kbd "C-c >")   'bbc-drop-crumb)
            (define-key map (kbd "C-c C->") 'bbc-drop-crumb-defun)
            map)
  :group 'bbc)

(provide 'better-breadcrumbs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; better-breadcrumbs.el ends here
