;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; electric-shift-lock-mode.el --- Electric shift locking, ala ZWEI.
;;;
;;; Time-stamp: <Friday Mar 23, 2012 19:17:14 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2012 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2008 John Paul Wallington
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    23 Mar 2012 19:03:02
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; Licenseas published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program isdistributed in the hope that it will be
;;; useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

;;; shift-lock.el --- Electric Shift Lock Mode

(defgroup shift-lock nil
  "Electric Shift Lock Mode.
A minor mode that uppercases everything except comments and strings."
  :group 'convenience
  :group 'editing
  :group 'lisp
  :group 'programming
  :link '(emacs-commentary-link "shift-lock")
  :prefix 'shift-lock)

(defcustom shift-lock-upcase-existing-text nil
  "Non-nil means `shift-lock-mode' should uppercase existing buffer text.
Comments and strings aren't uppercased.  If the buffer text isn't
syntactically valid then it will give up gracefully."
  :type 'boolean
  :group 'shift-lock)

(defcustom shift-lock-case-table 'ascii-case-table
  "Case table that `shift-lock-mode' uses to convert to uppercase."
  :type 'variable
  :group 'shift-lock)

(defsubst shift-lock-looking-at-comment-p (&optional pt)
  "Return non-nil if text after point PT is within a comment."
  (nth 4 (syntax-ppss pt)))

(defsubst shift-lock-looking-at-string-p (&optional pt)
  "Return non-nil if text after point PT is within a string."
  (nth 3 (syntax-ppss pt)))

(defun shift-lock-unbalanced-p (begin end)
  "Return non nil if region is syntactically unbalanced."
  (let ((unbalanced nil))
    (condition-case data
         (scan-sexps begin end)
       (scan-error (setq unbalanced t))
       (error (cond ((eq 'scan-error (car data))
                     (setq unbalanced t))
                    (t (signal (car data) (cdr data))))))
    unbalanced))

(defun shift-lock-upcase-region (begin end)
  "Convert the region to upper case except for strings and comments."
  (interactive "r")
  (with-case-table (symbol-value shift-lock-case-table)
    (save-excursion
      (if (shift-lock-unbalanced-p begin end)
          (message "Unbalanced expression")
          (let ((pt begin)
                (upcase-p nil))
            (goto-char pt)
            (while (< (point) end)
              (setq pt (point))
              (setq upcase-p (not (or (shift-lock-looking-at-comment-p)
                                      (shift-lock-looking-at-string-p))))
              (skip-syntax-forward (string (char-syntax (char-after))))
              (if upcase-p          
                  (upcase-region pt (point)))
              (setq upcase-p nil)))))))

(defun shift-lock-upcase-buffer ()
  "Convert the current buffer to upper case except for strings and comments."
  (interactive)
  (shift-lock-upcase-region (point-min) (point-max)))

(defun shift-lock-self-insert ()
  "Insert the character you type, upcased if within a string or comment."
  (interactive)
  (with-case-table (symbol-value shift-lock-case-table)
    (insert (if (or (shift-lock-looking-at-comment-p)
                    (shift-lock-looking-at-string-p))
                last-command-char
                (upcase last-command-char)))))

(defvar shift-lock-mode-map
  (let ((map (make-keymap)))
    (if (fboundp 'command-remapping)
        (define-key map [remap self-insert-command] 'shift-lock-self-insert)
        (substitute-key-definition 'self-insert-command
                                   'shift-lock-self-insert
                                   map))
    map)
  "Minor mode keymap for `shift-lock-mode'.")

(add-hook 'hack-local-variables-hook 'run-local-vars-shift-lock-mode-hook)

(defun run-local-vars-shift-lock-mode-hook ()
  (let ((lc? (assoc 'Lowercase file-local-variables-alist)))
    (when lc?
      (if (or (eq (cdr lc?) 'Yes)
              (eq (cdr lc?) t))
          (electric-shift-lock-mode 0)
          (electric-shift-lock-mode 1)))))

(define-minor-mode shift-lock-mode
    "Toggle Electric Shift Lock mode.

With arg, turn Electric Shift Lock mode off iff arg is a
non-positive number; if arg is nil, toggle Electric Shift Lock
mode; anything else turns Electric Shift Lock mode on.

Electric Shift Lock mode is a minor mode that uppercases the
characters that you type unless they are in comments or strings."
  nil " Shift" 'shift-lock-mode-map
  (if shift-lock-upcase-existing-text
      (shift-lock-upcase-buffer)))

(defalias 'electric-shift-lock-mode 'shift-lock-mode)

(provide 'shift-lock)

;;; electric-shift-lock-mode.el ends here
