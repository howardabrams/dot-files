;;; CIDER-ALWAYS-EVAL --- Evaluates expressions whenever function changes
;;
;; Author: Howard Abrams <howard.abrams@gmail.com>
;; Copyright © 2015, Howard Abrams, all rights reserved.
;; Created: 17 December 2015
;; Keywords: languages, clojure, cider
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extension to Cider to advice the `cider-eval-defun-at-point' so
;;  that after evaluating the function, it also evaluates a collection
;;  of expressions in another buffer, placing the results of each at
;;  the end of the line.
;;
;;  To use, simply run:
;;
;;     (require 'cider-always-eval)
;;
;;  Then start the buffer with:
;;
;;     M-x cider-create-expression-collection-buffer
;;
;;  In the buffer that shows up, write any expressions you want
;;  evaluated. Typically, these would be calling the function
;;  currently under development. Then, each time you evaluate a
;;  function using the `cider-eval-defun-at-point', the results will
;;  be displayed.
;;
;;  Trigger more by advising other functions:
;;
;;      (advice-add 'cider-eval-region :after #'cider-eval-expression-collection)
;;
;;  Note: Do NOT advice `cider-eval-last-sexp' as that is being used
;;  for evaluation. Need to change that.
;;
;;
;;  The Backstory: Frank and I were at the PDX Emacs Hackers meetup,
;;  and he mentioned that he liked this feature in Lighttable, where
;;  you could set up a buffer of expressions, and then when you
;;  modified the function, it would re-evaluate them automatically.
;;
;;  So, without actually seeing Light Table do this, I figured I would
;;  build something similar. This is really a preliminary stab at the
;;  process, as the code calls `cider-eval-last-sexp', but it really
;;  should be calling `cider-interactive-eval'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;    - v 0.1 - Preliminary code that technically works.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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

(defcustom cider-expression-collection-buffer-name "*cider-expressions*"
  "Buffer name for Clojure expressions to be evaluated when functions are redefined."
  :group 'cider)

(defun cider-create-expression-collection-buffer ()
  "Create or open the `expression-collection' buffer."
  (interactive)
  (pop-to-buffer cider-expression-collection-buffer-name)
  (when (= (point-min) (point-max))
    (insert ";; Cider Expressions Evaluator:
;;   Evaluates every top-level expression in this buffer.
;;   Deletes comments following expressions, so place each on their own line.

")
    (clojure-mode 1)))

(defun cider-eval-expression-insert-results ()
  "Evaluate expression at point, and insert results as comment."
  (forward-sexp)
  (when (not (= (point) (line-end-position)))
    (kill-line))
  (cider-eval-last-sexp 4)    ; Evaluate and insert into buffer
  (insert " ; => ")           ; Turn the evaluation into a comment
                                        ; TODO: Call `cider-interactive-eval'.
  (sit-for .1)                ; Wait for results... hrm... Don't likey
                                        ; TODO: Wait until completed
  (end-of-line))

(defun cider-eval-expression-collection ()
  "Pop to the `expression-collection' buffer (if exists), and evaluate the forms."
  (interactive)
  (let ((b (get-buffer cider-expression-collection-buffer-name)) )
    (when b
      (pop-to-buffer b)
      (goto-char (point-min))
      (while (search-forward "(" nil t)
        (backward-char)
        (cider-eval-expression-insert-results)))))

(advice-add 'cider-eval-defun-at-point :after #'cider-eval-expression-collection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'cider-always-eval)

;;; cider-always-eval.el ends here
