;;; zlisp-text.el --- Text functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
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

;;; BUG: needs org
(defun zlisp-fill-paragraph ()
  "If in an org buffer use `org-fill-paragraph'; else use `fill-paragraph'."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (call-interactively #'org-fill-paragraph)
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'zlisp-fill-paragraph)

(defun zlisp-unfill-paragraph (&optional region)
  "Takes a multi line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(global-set-key (kbd "M-Q") #'zlisp-unfill-paragraph)

;;; TODO: Teach this about other programming modes.
(defun zlisp--insert-comment (title char)
  (interactive)
  (let* ((line (make-string 67 (string-to-char char)))
         (start (if (member major-mode '(emacs-lisp-mode
                                         lisp-mode
                                         common-lisp-mode))
                    ";;;"
                  "// "))
         (seperator (concat start line)))
    (when (> (current-column) 0)
      (end-of-line)
      (newline))
    (insert (format "%s\n%s{{{ %s:\n\n%s}}}\n%s"
                    seperator
                    start
                    title
                    start
                    seperator))
    (previous-line)))

(defsubst zlisp-insert-group-comment ()
  (interactive)
  (zlisp--insert-comment "Group" "*"))

(defsubst zlisp-insert-major-comment ()
  (interactive)
  (zlisp--insert-comment "Major" "="))

(defsubst zlisp-insert-minor-comment ()
  (interactive)
  (zlisp--insert-comment "Minor" "-"))

(defun has-space-at-boundary-p (string)
  "Check whether STRING has any whitespace on the boundary.

Return 'left, 'right, 'both, or NIL."
  (let ((result nil))
    (when (string-match-p "^[[:space:]]+" string)
      (setq result 'left))
    (when (string-match-p "[[:space:]]+$" string)
      (if (eq result 'left)
          (setq result 'both)
        (setq result 'right)))
    result))

(defun is-there-space-around-point-p ()
  "Check whether there is whitespace around point.
Returns 'left, 'right, 'both or NIL."
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

(defun set-point-before-yanking (string)
  "Put point in the appropriate place before yanking STRING."
  (let ((space-in-yanked-string (has-space-at-boundary-p string))
        (space-at-point (is-there-space-around-point-p)))
    (cond ((and (eq space-in-yanked-string 'left)
                (eq space-at-point 'left))
           (skip-chars-backward "[:space:]"))
          ((and (eq space-in-yanked-string 'right)
                (eq space-at-point 'right))
           (skip-chars-forward "[:space:]")))))

(defun set-point-before-yanking-if-in-text-mode (string)
  "Invoke `set-point-before-yanking' in text modes."
  (when (derived-mode-p 'text-mode)
    (set-point-before-yanking string)))

(advice-add 'insert-for-yank :before
            #'set-point-before-yanking-if-in-text-mode)

(defun zlisp-yaml-wrap ()
  "Wrap region in --- for a yaml block."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "---" "\n")
    (goto-char start)
    (insert "---" "\n")))

(defun zlisp-forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(provide 'zlisp-text)

;;; zlisp-text.el ends here.
