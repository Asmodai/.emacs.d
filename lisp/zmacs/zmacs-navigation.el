;;; zmacs-navigation.el --- Navigation packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:10:49
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
;;;; Requirements:

(eval-when-compile
  (require 'cl-lib))

;;;; Imenu list outline:

(use-package imenu-list
  :commands (imenu-list-smart-toggle
             imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-position 'left))

;;;; Save place:

(use-package saveplace
  :ensure nil
  :hook (emacs-startup . save-place-mode)
  :config
  (setq save-place-file (concat *zmacs-cache-directory* "saved-places")
        save-place-forget-unreadable-files nil))

;;;; Go To Change:

(use-package goto-last-change
  :bind (("C-\"" . #'goto-last-change)))

;;;; Recent files:

(use-package recentf
  :ensure nil
  :defer 2
  :custom
  (recentf-save-file (concat *zmacs-cache-directory* "recentf"))
  (recentf-max-saved-items 500)
  (recentf-max-menu-items 10)
  :config
  (recentf-mode 1))

;;;; Goto address:

(use-package goto-addr
  :ensure nil
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode        . goto-address-prog-mode)
         (eshell-mode      . goto-address-mode)
         (text-mode        . goto-address-mode)
         (shell-mode       . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>"   . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(defun zmacs-goto-journal ()
  (interactive)
  (find-file (concat zmacs-storage-directory "journal.org")))

(defun zmacs-goto-early-init.el ()
  "Open early-init.el file"
  (interactive)
  (find-file "~/.emacs.d/early-init.el"))

(defun zmacs-goto-init.el ()
  "Open init.el file"
  (interactive)
  (find-file user-init-file))

(defun zmacs-goto-custom.el ()
  "Open custom.el file"
  (interactive)
  (find-file custom-file))

(defun zmacs-goto-emacs-dir ()
  "Open Emacs directory"
  (interactive)
  (find-file user-emacs-directory))

(defun zmacs-goto-org-files ()
  "Open directory with org files"
  (interactive)
  (find-file zmacs-storage-directory))

(defun zmacs-goto-projects ()
  "Open projects dir"
  (interactive)
  (find-file zmacs-projects-directory))

;;;; Jump in buffer:

(defun zmacs-jump-in-buffer ()
  "Jump between headlines in a buffer using consult."
  (interactive)
  (cond ((eq major-mode 'org-mode)
         (call-interactively 'consult-org-heading))
        (t
         (call-interactively 'consult-outline))))

;;;; Provide package:

(provide 'zmacs-navigation)

;;; zmacs-navigation.el ends here.
