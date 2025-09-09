;;; zlisp-dired.el --- Dired stuff  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    31 Oct 2024 17:43:48
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
(require 'dired)
(require 'dired-ranger)
(require 'peep-dired)

;;;; Dired:

(defun zlisp/dired-updirectory ()
  "Navigate to the parent directory in Dired."
  (interactive)
  (find-alternate-file ".."))

;;;; Dired Ranger:

;; Allow for cycling from bottom to top of dired buffer and vice versa.
(defun zlisp/dired-wrap ()
  "Cycle from bottom to top of buffer"
  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook
            (defun zlisp//dired-wrap-1 ()
              ""
              (if (= 1 (save-excursion
                         (forward-line)))
                  (goto-line 3))
              (if (= -1 (save-excursion
                          (forward-line -1)))
                  (goto-line (count-lines
                              (point-min)
                              (point-max)))))))

;;;; Peep:

(defun zlisp/peep-dired-open ()
  "Open files from `peep-dired' and clean up."
  (interactive)
  (peep-dired-kill-buffers-without-window)
  (dired-find-file)
  (delete-other-windows))

;;;; Provide package:

(provide 'zlisp-dired)

;;; zlisp-dired.el ends here.
