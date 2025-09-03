;;; zmacs-notes.el --- Note packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:29:27
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

(defvar *zmacs-notes-directory*
  (expand-file-name (concat zmacs-storage-directory "notes/"))
  "Location of note files.")

;;;; Packages:
;;;;; Denote:

(use-package denote
  :commands (denote
             denote-create-note
             denote-link-ol-store)
  :custom
  (denote-directory *zmacs-notes-directory*)
  (denote-date-prompt-use-org-read-date t)
  (denote-file-type                     'org)
  (denote-allow-multi-word-keywords     nil)
  (denote-prompts                       '(title
                                          keywords
                                          subdirectory))
  (denote-known-keywords                '("workbook"
                                          "project"
                                          "idea"
                                          "research"))
  (denote-link-backlinks-display-buffer-action
   (quote ((display-buffer-reuse-window
            display-buffer-in-side-window)
           (inhibit-same-window . t)
           (side . bottom)
           (slot . 99)
           (window-height . 10)))))

(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t))
  (add-to-list 'org-capture-templates
               `("t" "Quick Todo" entry
                 (file ,(expand-file-name  "todo.org" zmacs-org-directory))
                 "* TODO %?\n:PROPERTIES:\n:created: %U\n:END:\n%a")))

(defun zmacs-denote-capture-note ()
  "Create a new note with Denote."
  (interactive)
  (org-capture nil "n"))

(defun zmacs-denote-capture-todo ()
  "Create a new todo task with Denote."
  (interactive)
  (let* ((title (read-string "Title: "))
         (file  (denote title '("task") nil "org")))
    (find-file file)
    (insert (format "* TODO %s\n:PROPERTIES:\n:created: %s\n:END:\n\n"
                    title (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (save-buffer)
    (zmacs-agenda-scan-denote)))

(defun zmacs-insert-header-and-time-property ()
  "Insert an Org heading with a timestamp property."
  (interactive)
  (insert "* Heading")
  (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %T]"))
  (org-set-property "TOC" ":include all"))

(defun zmacs-denote-workbook-create-entry ()
  "Create an entry tagged 'workbook' with the date as its title."
  (interactive)
  (denote
   (format-time-string "%Y %A %e %B")
   '("workbook")
   nil
   (concat denote-directory "workbook")
   nil
   nil)
  (zmacs-insert-header-and-time-property))

;;;; Citar-Denote:

(use-package citar-denote
  :commands (citar-create-note
             citar-open-notes
             citar-denote-add-citekey)
  :config
  (citar-denote-mode))

;;;; Consult Notes:

(use-package consult-notes
  :custom
  (consult-notes-file-dir-sources
   `(("ZMACS Notes" ?- ,*zmacs-notes-directory*)))
  :commands (consult-notes
             consult-notes-search-in-all-notes))

(defun zmacs-notebook ()
  "Open `consult-notes' in a buffer window."
  (interactive)
  (require 'consult)
  (require 'denote)
  (let ((vertico-buffer-display-action
         '(display-buffer-reuse-window)))
    (consult-notes)))

;;;; Provide package:

(provide 'zmacs-notes)

;;; zmacs-notes.el ends here.
