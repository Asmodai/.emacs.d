;;; zlisp-platform-macos.el --- macOS support  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    23 Oct 2024 10:14:29
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

;;;; macOS-specific settings:

(setq ns-alternate-modifier    'super
      ns-command-modifier      'meta
      ns-right-option-modifier 'nil
      ns-pop-up-frames         nil
      ns-use-srgb-colorspace   t)

;;;; pixel scroll package:

(use-package pixel-scroll
  :ensure nil
  :custom
  (pixel-scroll-precision-use-momentum t)
  :hook
  (after-init . pixel-scroll-precision-mode))

;;;; Emacs bianry:

(defun zlisp/macos-emacs-path ()
  (let ((app-wrapper "Emacs.app/Contents/MacOS/Emacs"))
    (or (executable-find (concat "/Applications/" app-wrapper))
        (executable-find (expand-file-name
                          (concat "~/Applications/" app-wrapper))))))

;;;; macos path:

;; Support for Homebrew.
(defun zlisp/macos-exec-path ()
  "Check for any macOS-specific additions to `exec-path'."
  (when (file-exists-p "/opt/homebrew/bin")
    (message "ZMACS/macOS: Adding homebrew to `exec-path'.")
    (setenv "PATH" (concat "/opt/homebrew/bin/:"
                           (getenv "PATH")))
    (cl-pushnew "/opt/homebrew/bin/" exec-path)))

;;;; Provide package:

(provide 'zlisp-platform-macos)

;;; zlisp-platform-macos.el ends here.
