;;; zlisp-text.el --- Text functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 14:42:32
;; URL:        not distributed yet
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY  WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;

;;; Code:

(require 'cl-lib)

(defvar zlisp-text-ellipsis "…"
  "Character(s) to use for ellipses.")

(defun zlisp/elide-text (text width)
  (let* ((text-length  (length text))
         (elide-length (length zlisp-text-ellipsis)))
    (if (> text-length width)
        (concat (substring text 0 (min (1- width) text-length))
                zlisp-text-ellipsis)
      text)))

(defun zlisp/has-space-at-boundary-p (string)
  "Check whether STRING has any whitespace on the boundary.

Return LEFT, RIGHT, BOTH, or NIL."
  (let ((result nil))
    (when (string-match-p "^[[:space:]]+" string)
      (setq result 'left))
    (when (string-match-p "[[:space:]]+$" string)
      (if (eq result 'left)
          (setq result 'both)
        (setq result 'right)))
    result))

(defun zlisp/is-there-space-around-point-p ()
  "Check whether there is whitespace around point.
Returns LEFT, RIGHT, BOTH or NIL."
  (let ((result nil))
    (when (< (save-excursion
               (skip-chars-backward "[:space:]"))
             0)
      (setq result 'left))
    (when (> (save-excursion
               (skip-chars-forward "[:space:]"))
             0)
      (if (eq result 'left)
          (setq result 'both)
        (setq result 'right)))
    result))

(defun zlisp/set-point-before-yanking (string)
  "Put point in the appropriate place before yanking STRING."
  (let ((space-in-yanked-string (zlisp/has-space-at-boundary-p string))
        (space-at-point (zlisp/is-there-space-around-point-p)))
    (cond ((and (eq space-in-yanked-string 'left)
                (eq space-at-point 'left))
           (skip-chars-backward "[:space:]"))
          ((and (eq space-in-yanked-string 'right)
                (eq space-at-point 'right))
           (skip-chars-forward "[:space:]")))))

(defun zlisp/set-point-before-yanking-if-in-text-mode (string)
  "If in a mode derived from text mode, set point at STRING before yank."
  (when (derived-mode-p 'text-mode)
    (zlisp/set-point-before-yanking string)))

(advice-add 'insert-for-yank :before
            #'zlisp/set-point-before-yanking-if-in-text-mode)

(defun zlisp/yaml-wrap ()
  "Wrap region in --- for a yaml block."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "---" "\n")
    (goto-char start)
    (insert "---" "\n")))

(defun zlisp/forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point.

If ARG is provided, it will be used as an argument to `forward-sexp` et al."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(provide 'zlisp-text)

;;; zlisp-text.el ends here.
