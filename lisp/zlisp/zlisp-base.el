;;; zlisp-base.el --- Base utilities  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    24 Oct 2024 04:59:50
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

(define-inline zlisp-xcons (x y)
  `(cons ,x ,y))

;;; Zetalisp `si:neq'
(define-inline zlisp-neq (x y)
  `(not (eq ,x ,y)))

(defun zlisp-mem (pred item list)
  (dolist (elt list)
    (and (funcall pred list (car elt))
         (return elt))))

;;; Zetalisp `si:defprop'
(define-inline zlisp-defprop (sym value indicator)
  (when (not (symbolp sym))
    (error "%S is not a symbol." sym)
    `(setf (get ',sym ',indicator) ',value)))

(provide 'zlisp-base)

;;; zlisp-base.el ends here.
