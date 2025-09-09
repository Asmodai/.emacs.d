;;; zlisp-timing.el --- Timing utilities  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    23 Oct 2024 16:21:42
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

;; See https://stackoverflow.com/q/23622296
(defmacro zlisp/measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time))
         (inhibit-message t))
     ,@body
     (message "
;; ======================================================
;; %s *Elapsed time: %.06f*
;; ======================================================
"
              (if load-file-name
                  (file-name-nondirectory (format "%s |" load-file-name))
                "")
              (float-time (time-since time)))))

(defmacro zlisp/simple-measure-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

(provide 'zlisp-timing)

;;; zlisp-timing.el ends here.
