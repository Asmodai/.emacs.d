;;; zlisp-memory.el --- Memory utilities  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    31 Oct 2024 07:14:45
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
(require 'zlisp-math)

(defun zmacs/room ()
  "Show approximate memory usages for Emacs Lisp."
  (prog1 nil
    (let ((info (garbage-collect)))
      (cl-loop for elt in info
               collect (cl-destructuring-bind (name size used &optional free)
                           elt
                         (let* ((all-fmt "%-15s %-4d %-12s")
                                (free-fmt "%-12s")
                                (fmt      (if free
                                              (concat all-fmt " " free-fmt)
                                            (concat all-fmt " -"))))
                           (format fmt name size
                                   (zlisp/iec-binary-units (* used size) t)
                                   (when free
                                     (zlisp/iec-binary-units (* free size) t)))))
               into lines

               finally
               (return
                (princ
                 (concat "Emacs Lisp approximate memory information:\n\n"
                         "Kind            Size Used         Unused\n"
                         "--------------- ---- ------------ ------------\n"
                         (mapconcat #'identity lines "\n")
                         "\n")))))))

(provide 'zlisp-memory)

;;; zlisp-memory.el ends here.
