;;; zlisp-time.el --- Time functions.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 13:39:54
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

(defun zlisp//format-date (fmt &optional time zone)
  "Use FMT to format the time value TIME, using ZONE if provided."
  (let ((system-time-locale "en_GB.UTF-8"))
    (format-time-string fmt time zone)))

(defun zlisp/insert-timestamp ()
  "Produce a timestamp."
  (interactive)
  (insert (zlisp//format-date "%Y-%02m-%02d-%02H:%02M:%02S")))

(defun zlisp/insert-rfc3339-timestamp ()
  "Produce an RFC3339 timestamp."
  (interactive)
  (insert (zlisp//format-date "%Y-%02m-%02dT%02H:%02M:%02SZ"
                             (current-time)
                             0)))

(defun zlisp/insert-date ()
  "Insert the current date."
  (interactive)
  (insert (zlisp//format-date "%A, %B %d %Y")))

(defun zlisp/insert-time ()
  "Insert the current time."
  (interactive)
  (insert (zlisp//format-date "%H:%M:%S")))

(defun zlisp/insert-date-and-time ()
  "Insert the current date and time."
  (interactive)
  (insert (zlisp//format-date "%m-%d-%Y %H:%M:%S")))

(provide 'zlisp-time)

;;; zlisp-time.el ends here.
