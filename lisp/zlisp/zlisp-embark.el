;;; zlisp-embark.el --- Embark functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    31 Oct 2024 17:23:36
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
(require 'which-key)

(defun zlisp/dired-here (file)
  "Open dired in the directory containing FILE."
  (dired (file-name-directory file)))

(defun zlisp/consult-rg-here (file)
  "Consult `ripgrep' in the directory containing FILE."
  (let ((default-directory (file-name-directory file)))
    (consult-ripgrep)))

(defun zlisp/embark-which-key-indicator ()
  "An embark indicator that displays keymaps using `which-key'.

The `which-key' help message will show the type and value of the current target
followed by an ellipsis if there are further targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets)
                     "…"
                   "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymap) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil
       nil
       t
       (lambda (binding)
         (not (string-suffix-p "-argument" (cdr binding))))))))

(defun zlisp/embark-hide-which-key-indicator (fn &rest args)
  "Hide the `which-key' indicator when using the CR prompter for FN and ARGS."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators (remq #'zmacs/embark-which-key-indicator
                                 embark-indicators)))
    (apply fn args)))

(provide 'zlisp-embark)

;;; zlisp-embark.el ends here.
