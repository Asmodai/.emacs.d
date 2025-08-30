;;; zlisp-list.el --- List hacks.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    29 Aug 2025 00:32:11
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

;;;; List conversion:

(defun zlisp/list->alist (name strings)
  "Convert STRINGS into an alist of (:name-N . string).
NAME is a symbol that prefixes each keyword.
E.g. (zlisp/list->alist 'tint '(\"#aaa\" \"#bbb\"))
 => ((:tint-0 . \"#aaa\") (:tint-1 . \"#bbb\"))"
  (let ((count 0))
    (mapcar (lambda (s)
              (prog1
                  (cons (intern (format ":%s-%d" name count)) s)
                (setq count (1+ count))))
            strings)))

;;;; Provide package:

(provide 'zlisp-list)

;;; zlisp-list.el ends here.
