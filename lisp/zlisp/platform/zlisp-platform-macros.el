;;; zlisp-platform-macros.el --- Platform macros  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    23 Oct 2024 17:28:09
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
  (require 'cl-lib)
  (require 'zlisp-platform-predicates))

(eval-when-compile
  (defmacro zlisp/when-macos (&rest body)
    "Evaluate BODY only when the host system is macOS."
    (declare (indent 0))
    (if (zlisp/macos-p)
        `(progn ,@body)
      nil))

  (defmacro zlisp/when-windows (&rest body)
    "Evaluate BODY only when the host system is Windows."
    (declare (indent 0))
    (if (zlisp/windows-p)
        `(progn ,@body)
      nil))

  (defmacro zlisp/when-unix (&rest body)
    "Evaluate BODY only when the host system is a Unix."
    (declare (indent 0))
    (if (zlisp/windows-p)
        `(progn ,@body))))

(provide 'zlisp-platform-macros)

;;; zlisp-platform-macros.el ends here.
