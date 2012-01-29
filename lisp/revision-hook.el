;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; revision-hook.el --- File revision hook.
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 00:40:32 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2011-12 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    16 Feb 2011 12:59:54
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

(eval-when-compile
  (require 'cl))

(defvar file-header-update-alist ()
  "Used by `update-file-header' to know what to do in a file.  It is a
list of sets of cons cells where the car is a regexp and the cdr is
the function to call if the string is found near the start of the
file.")

(defsubst delete-and-forget-line ()
  "Delete the current line.  Do not add it to the kill ring."
  (let* ((start (point))
         (stop (progn
                 (end-of-line)
                 (point)))
         (str (buffer-substring start stop)))
    (delete-region start stop)
    str))

(defun update-write-count ()
  "Increments the update count for the `Revision:' header."
  (let* ((str (delete-and-forget-line))
         (rem (read-from-string str))
         (num (car rem)))
    (if (numberp num)
        (insert (format "%s" (1+ num))
                (substring str (cdr rem)))
        (error "Invalid number for update count `%'." str))))

(defun register-file-header-action (regexp fun)
  "Register an action for a file header update."
  (let ((ml (assoc regexp file-header-update-alist)))
    (if ml
        (setcdr ml fun)
        (setq file-header-update-alist
              (cons (cons regexp fun)
                    file-header-update-alist)))))

;;; Register some actions
(register-file-header-action "Revision:   " 'update-write-count)

;;;###autoload
(defun update-file-header ()
  "Update the file header."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region 1 (min 50000 (1- (buffer-size))))
      (let ((patterns file-header-update-alist))
        (setq last-command nil)
        (while patterns
          (goto-char (point-min))
          (when (re-search-forward (car (car patterns)) nil t)
            (goto-char (match-end 0))
            (funcall (cdr (car patterns))))
          (setq patterns (cdr patterns)))))))

;;;###autoload
(defun auto-update-file-header ()
  "Update the file header if the file is modified.

If the file is modified, sized greater than 100 and the buffer is not
read-only, then call `update-file-header'."
  (and (> (buffer-size) 100)
       (buffer-modified-p)
       (not buffer-read-only)
       (update-file-header)
       nil))

(provide 'revision-hook)

;;; revision-hook.el ends here

