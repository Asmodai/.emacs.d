;;; zlisp-ui.el --- UI functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 14:49:53
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

;;; BUG: markdown!
;;; BUG: Org!
(defun zlisp-toggle-display-markup ()
  "Toggle the display of markup in markdown and org modes"
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-toggle-link-display)
    (if markdown-hide-markup
        (markdown-toggle-markup-hiding 0)
      (markdown-toggle-markup-hiding))))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it; then it
takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun zlisp--quit-p (&optional prompt)
  "Return t if this session should be killed. Prompts the user for
      confirmation."
  (or (yes-or-no-p (format "%s" (or prompt
                                    "Quit ZMACS?")))
      (ignore (message "Aborted"))))

(setq confirm-kill-emacs nil)

(add-hook 'kill-emacs-query-functions #'zlisp--quit-p)

(defvar *zlisp-quit-messages*
  '(;; from Doom
    "Let's beat it -- This is turning into a bloodbath!"
    "I wouldn't leave if I were you. DOS is much worse."
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; Custom
    "Emacs! Emacs!! Emacs!!!"
    "The King is dead, long live the King!"
    "Like you have somewhere better to be..."
    "Don't worry, I won't tell everyone you're a failure"
    "(setq nothing t everything 'permitted)"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    "You are *not* prepared!")
  "A list of quit messages, picked randomly by `zlisp-quit'. Taken from
      http://doom.wikia.com/wiki/Quit_messages and elsewhere.")

(defun zlisp--quit (&rest _)
  (zlisp--quit-p
   (format "%s  Quit?"
           (nth (random (length *zlisp-quit-messages*))
                *zlisp-quit-messages*))))

(remove-hook 'kill-emacs-query-functions #'zlisp--quit-p)
(add-hook 'kill-emacs-query-functions #'zlisp--quit)

(provide 'zlisp-ui)

;;; zlisp-ui.el ends here.
