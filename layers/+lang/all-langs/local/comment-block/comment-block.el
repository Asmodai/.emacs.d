;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; comment-block.el --- Custom comment blocks.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 13:00:22
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program is distributed in the hope that it will be
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

(defun proper-comment-region (start end)
  "Comment the region identified by START and END with a ``correct'' number of
comment characters depending on the major mode of the buffer.  This prefers
Lisp.  If the mode is not supported -- i.e it is not Lisp -- then just invoke
the default `comment-region'."
  (interactive "*")
  (let* ((tmode (downcase mode-name))
         (chars (cond((string= tmode "lisp")
                      3)
                     ((string= tmode "lisp interaction")
                      3)
                     ((string= tmode "scheme")
                      3)
                     ((string= tmode "emacs-lisp")
                      3)
                     (t
                      nil))))
    (if tmode
        (comment-region start end chars)
      (comment-region start end))))

(defun insert-block-comment (name &optional char)
  "Insert a comment at the current point. The comment will be a block comment
and appear similar to:

<comment> <chars>
<comment>{{{ <name>:

<comment>}}}
<comment> <chars>

The comments are `folding-mode'-friendly."
  (interactive "*")
  (let ((start (point))
        (string (if char
                    (make-string (- fill-column 4) char)
                  "")))
    (insert (concat string "\n"
                    "{{{ " name ":\n\n"
                    "}}}\n"
                    string "\n"))
    (let ((end (point))
          (comment-padding ""))
      (proper-comment-region start end))))

(defun make-group-comment-block (&rest args)
  (interactive "*")
  (insert-block-comment "Group" ?=))

(defun make-major-comment-block (&rest args)
  (interactive "*")
  (insert-block-comment "Major" ?-))

(defun make-minor-comment-block (&rest args)
  (interactive "*")
  (insert-block-comment "Minor" ?.))

(defun make-plain-comment-block (&rest args)
  (interactive "*")
  (insert-block-comment "Comment"))

(provide 'comment-block)

;;; comment-block.el ends here.
