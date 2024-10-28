;;; zmacs-prog-debug.el --- Debug packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:31:25
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

;;;; Functions:

(defvar *zmacs--debug-additional-debuggers* '())

(defun zmacs--debug-generate-symbol (debugger)
  "Create RealGUD interactive function name from DEBUGGER."
  (intern (concat "realgud:" debugger)))

(defun zmacs--add-realgud-debugger (mode debugger)
  "Add a RealGUD DEBUGGER to MODE."
  (let ((dbg-name (zmacs--debug-generate-symbol debugger)))
    (autoload dbg-name "realgud" nil t)))

;;;; RealGUD:

(use-package realgud
  :defer t
  :init
  (dolist (debugger (mapcar #'zmacs--debug-generate-symbol
                            *zmacs--debug-additional-debuggers*))
    (autoload debugger "realgud" nil t)))

;;;; Provide package:

(provide 'zmacs-prog-debug)

;;; zmacs-prog-debug.el ends here.
