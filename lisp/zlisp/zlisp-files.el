;;; zlisp-files.el --- File functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 14:24:08
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
(require 'consult)

(defun zlisp/setup-kill-and-archive-region ()
  "Delete and suspend a region to the end of zmacs-archive.el."
  (interactive)
  (append-to-file (region-beginning)
                  (region-end)
                  (concat user-emacs-directory "zmacs-archive.el"))
  (delete-region (region-beginning)
                 (region-end)))

(defvar zmacs--files-sources-data
  `(("Init Files"  ?i ,*zmacs-lisp-directory*))
  "Define titles, quick keys, and directories for ZMACS source files.")

(defun zlisp//files-make-source (name char dir)
  "Return a source list from NAME, CHAR, and DIR suitable for `consult--multi'."
  (let ((idir (propertize (file-name-as-directory dir) 'invisible t)))
    `(:name     ,name
      :narrow   ,char
      :category file
      :face     consult-file
      :items    ,(lambda ()
                   (mapcar (lambda (f)
                             (concat idir f))
                           (directory-files dir nil "[^.].*[.].+")))
      :action   ,(lambda (f)
                   (find-file f)))))

(defun zlisp/search-zmacs-files ()
  "Search all ZMACS files with `consult-ripgrep'."
  (interactive)
  (require 'consult)
  (let ((consult-ripgrep-args
         (mapconcat 'identity (list "rg"
                                    "--null"
                                    "--line-buffered"
                                    "--max-columns=1000"
                                    "--path-separator /"
                                    "--smart-case"
                                    "--no-heading"
                                    "--line-number"
                                    "--hidden"
                                    "--glob=lisp/**"
                                    "--glob=!straight"
                                    "--glob=!var"
                                    "--glob=!.git/ .")
                    " ")))
    (if (executable-find "rg")
        (consult-ripgrep user-emacs-directory)
      (message "Please install `rg' first."))))

(defun zlisp/load-init-file ()
  "Load the base init file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun zlisp/load-custom-file ()
  "Load custom.el."
  (interactive)
  (load-file (concat user-emacs-directory "custom.el")))

(defun zlisp/search-in-input-dir ()
  "Search for a string in the input directory."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'consult-ripgrep)))

(defun zlisp/get-string-from-file (filePath)
  "Read a file from FILEPATH and return the contents as a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun zlisp/duplicate-file ()
  "Duplicate a file."
  (interactive)
  (dired-do-copy-regexp "\\(.*\\)\\.\\(.*\\)" "\\1 (copy).\\2"))

(defun zlisp/move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))

(defun zmacs/make-parent-directory ()
  "Make sure the directory of function `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'zmacs/make-parent-directory)

(provide 'zlisp-files)

;;; zlisp-files.el ends here.
