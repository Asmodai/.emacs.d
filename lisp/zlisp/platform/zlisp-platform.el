;;; zlisp-platform.el --- Main platform module  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    23 Oct 2024 17:21:22
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
(require 'zlisp-platform-predicates)
(require 'zlisp-platform-macros)
(require 'zlisp-features)

(eval-when-compile
  (cl-pushnew *zlisp-system-type* *zlisp-features*))

(defvar *zlisp-emacs-binary* nil
  "The Emacs binary file used on this system.")

(zlisp-when-unix
  (setq *zlisp-emacs-binary* (or (executable-find "/usr/bin/emacs")
                                 (executable-find "/usr/local/bin/emacs"))))

(zlisp-when-macos
  (require 'zlisp-platform-macos)
  (setq *zlisp-emacs-binary* (zlisp-macos-emacs-path)))

(zlisp-when-windows
  (require 'zlisp-platform-windows)
  (setq *zlisp-emacs-binary "emacs.exe"))

(defun emacs-build-description-string ()
  "Run `emacs-build-description' and return the result as a string."
  (with-temp-buffer
    (emacs-build-description)
    (buffer-string)))

;;;###autoload
(defun zlisp-get-home-directory ()
  "Return the path representing a users' home directory."
  (cond ((zlisp-unix-p)
         (getenv "HOME"))
        ((zlisp-windows-nt-p)
         (let ((dir (getenv "HOMEDIR")))
           (when (null dir)
             (setq dir (getenv "USERPROFILE")))
           dir))
        ((zlisp-windows-32-p)
         (let ((dir (getenv "HOME")))
           (when (null dir)
             (setq dir "C:\\"))
           dir))))

;;;###autoload
(defvar user-home-directory (zlisp-get-home-directory)
  "Directory containing the users' file on the current operating system.")

(defun make-home-directory-path (pathspec)
  "Create a new path spec from PATHSPEC rooted to the user's home directory."
  (expand-file-name (concat user-home-directory "/" pathspec "/")))

(provide 'zlisp-platform)

;;; zlisp-platform.el ends here.
