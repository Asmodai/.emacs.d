;;; zmacs-scratch.el --- Persistent scratch  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    26 Oct 2024 10:26:11
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

;;;; Variables:

(defvar *zmacs-scratch-file-path*
  (expand-file-name (concat *zmacs-cache-directory* "/scratch"))
  "Location of the ZMACS persisent scratch file.")

(defvar *zmacs-scratch-message*
  ";; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with `C-x C-f' and enter text in its buffer.

"
  "Initial scratch message.")

(defvar *zmacs-persistent-scratch-message*
  (format ";; This is a persistent, unkillable scratch buffer.
;; The contents of this buffer will be saved to the file
;; \"%s\".
;;

"
          *zmacs-scratch-file-path*)
  "Initial scratch message for persistent scratch buffer.")

;;;; Functions:

(defun zmacs--bury-scratch ()
  "Don't kill the scratch buffer, bury it."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn
        (bury-buffer)
        nil)
    t))

(add-hook 'kill-buffer-query-functions #'zmacs--bury-scratch)

(defun zmacs-immortal-scratch ()
  "Switch to the immortal scratch buffer."
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
    t))

(defun zmacs-save-persistent-scratch ()
  "Save the contents of the scratch buffer."
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max)
                  *zmacs-scratch-file-path*)))

(defun zmacs-load-persistent-scratch ()
  "Reload the scratch buffer."
  (if (file-exists-p *zmacs-scratch-file-path*)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert-file-contents *zmacs-scratch-file-path*))))

(and (bound-and-true-p zmacs-use-persistent-scratch)
     (progn
       (add-hook 'kill-buffer-query-functions #'zmacs-immortal-scratch)
       (add-hook 'after-init-hook #'zmacs-load-persistent-scratch)
       (add-hook 'kill-emacs-hook #'zmacs-save-persistent-scratch)
       (run-with-idle-timer 300 t #'zmacs-save-persistent-scratch)))

(setq initial-scratch-message
      (if (bound-and-true-p zmacs-use-persistent-scratch)
          *zmacs-persistent-scratch-message*
        *zmacs-scratch-message*))

;;;; Provide package:

(provide 'zmacs-scratch)

;;; zmacs-scratch.el ends here.
