;;; zlisp-versioning.el --- Versioning stuff  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    01 Nov 2024 09:16:51
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

(eval-when-compile
  (require 'cl-lib))

;; Stolen from spacemacs.
(defun zlisp/git-get-current-branch-rev ()
  "Return the hash of the head commit on the current branch.
Return NIL if an error occurred."
  (let ((proc-buffer "git-get-current-branch-head-hash")
        (default-directory (file-truename user-emacs-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "rev-parse" "--short" "HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-string)
              (goto-char (point-min))
              (replace-regexp-in-string
               "\n$" ""
               (buffer-substring (line-beginning-position)
                                 (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun zlisp/git-get-current-tag ()
  "Return the current tag (if any)."
  (let* ((default-directory (file-truename user-emacs-directory))
         (tag (string-trim (shell-command-to-string "git describe --tags --exact-match 2>/dev/null"))))
    (if (string-empty-p tag)
        nil
      tag)))

(provide 'zlisp-versioning)

;;; zlisp-versioning.el ends here.
