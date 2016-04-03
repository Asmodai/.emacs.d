;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; bootstrap-emacs-backports.el --- Older Emacs functionality.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 06:17:07
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

(unless (featurep 'subr-x)
  ;; `subr-x' function for Emacs 24.3 and below.

  (defsubst string-join (strings &optional separator)
    "Join all STRINGS using SEPARATOR."
    (mapconcat 'identity strings separator))

  (defsubst string-trim-left (string)
    "Remove leading whitespace from STRING."
    (if (string-match "\\`[ \t\n\r]+" string)
        (replace-match "" t t string)
      string))

  (defsubst string-trim-right (string)
    "Remove trailing whitespace from STRING."
    (if (string-match "[ \t\n\r]+\\'" string)
        (replace-match "" t t string)
      string))

  (defsubst string-trim (string)
    "Remove leading and trailing whitespace from STRING."
    (string-trim-left (string-trim-right string)))

  (defsubst string-empty-p (string)
    "Check whether STRING is empty."
    (string= string ""))

  )                                     ; subr-x

(unless (fboundp 'with-eval-after-load)
  ;; `with-eval-after-load' function for Emacs 24.3 and below

  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1)
             (debug t))
    `(eval-after-load ,file
       (lambda ()
         ,@body)))

  )                                     ; with-eval-after-load

(provide 'bootstrap-emacs-backports)

;;; bootstrap-emacs-backports.el ends here
