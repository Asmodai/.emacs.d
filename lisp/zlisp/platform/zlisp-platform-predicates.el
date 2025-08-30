;;; zlisp-platform-predicates.el --- Useful predicates  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    23 Oct 2024 16:29:28
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

(eval-when-compile
  (defvar zlisp-system-type
    (cond ((memq system-type '(ms-windows win386)) :windows)
          ((eq system-type 'windows-nt)            :windows-nt)
          ((eq system-type 'gnu/linux)             :gnu/linux)
          ((eq system-type 'gnu)                   :gnu-hurd)
          ((eq system-type 'gnu/kfreebsd)          :gnu/kfreebsd)
          ((eq system-type 'darwin)                :darwin)
          ((eq system-type 'berkeley-unix)         :bsd)
          (t                                       :unix))
    "The host system."))

(defsubst zlisp/windows-32-p ()
  "T if we are running on a non-NT version of Windows."
  (eq zlisp-system-type :windows))

(defsubst zlisp/windows-nt-p ()
  "T if we are running on Windows NT."
  (eq zlisp-system-type :windows-nt))

(defsubst zlisp/windows-p ()
  "T if we are running on some sort of Windows."
  (or (zlisp/windows-nt-p)
      (zlisp/windows-32-p)))

(defsubst zlisp/gnu/linux-p ()
  "T if we are running on a GNU/Linux system."
  (eq zlisp-system-type :gnu/linux))

(defsubst zlisp/gnu-hurd-p ()
  "T if we are running on a GNU Hurd system."
  (eq zlisp-system-type :gnu-hurd))

(defsubst zlisp/gnu/kfreebsd-p ()
  "T if we are running on GNU/kFreeBSD."
  (eq zlisp-system-type :gnu/kfreebsd))

(defsubst zlisp/darwin-p ()
  "T if we are running on macOS or Darwin."
  (eq zlisp-system-type :darwin))

(defsubst zlisp/macos-p ()
  "T if we are running on macOS."
  (and (zlisp/darwin-p)
       (memq window-system '(mac ns))))

(defsubst zlisp/bsd-p ()
  "T if we are running on a BSD of some kind."
  (or (eq zlisp-system-type :bsd)
      (zlisp/gnu/kfreebsd-p)))

(defsubst zlisp/unix-p ()
  "T if we are running on some kind of Unix system."
  (or (zlisp/gnu/linux-p)
      (zlisp/gnu-hurd-p)
      (zlisp/bsd-p)
      (zlisp/macos-p)
      (eq zlisp-system-type :unix)))

(defsubst zlisp/display-backend ()
  "Return the display backend type for the current frame.
  
Possible symbols:  'x, 'pgtk, 'w32, 'ns, 'pc."
  (framep (selected-frame)))
      
(defsubst zlisp/wayland-p ()
  "T if the display system is Wayland."
  (eq (zlisp/display-backend) 'pgtk))

(defsubst zlisp/x11-p ()
  "T if the display system is X11."
  (eq (zlisp/display-backend) 'x))

(defsubst zlisp/cocoa-p ()
  "T if the display system is Cocoa."
  (eq (zlisp/display-backend) 'ns))

(provide 'zlisp-platform-predicates)

;;; zlisp-platform-predicates.el ends here.
