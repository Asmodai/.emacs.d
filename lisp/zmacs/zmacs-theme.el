;;; zmacs-theme.el --- Theme  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 15:42:59
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

;;; Don't confirm themes.
(setq custom-safe-themes t)

(defcustom zmacs-custom-themes-dir
  (concat *zmacs-lisp-directory* "themes/")
  "Set a custom themes directory path."
  :group 'zmacs-emacs
  :type  'string)

(setq-default custom-theme-directory zmacs-custom-themes-dir)

;; find all themes recursively in custom-theme-folder
(let ((basedir custom-theme-directory))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".")
                      (equal f "..")))
             (file-directory-p (concat basedir f)))
        (progn
          (add-to-list 'load-path              (concat basedir f))
          (add-to-list 'custom-theme-load-path (concat basedir f))))))

(defun zmacs-disable-all-themes ()
  "Disable all active themes & reset mode-line."
  (interactive)
  (progn
    (dolist (i custom-enabled-themes)
      (disable-theme i))
    ;; disable window-divider mode
    (window-divider-mode -1)
    ;; revert to mode line
    (setq-default header-line-format nil)
    (setq-default mode-line-format
                  '((:eval
                     (list
                      "%b "
                      "%m "
                      (cond ((and buffer-file-name (buffer-modified-p))
                             (propertize "(**)" 'face `(:foreground "#f08290")))
                            (buffer-read-only "(RO)" ))
                      " %l:%c %0"
                      " "
                      ))))
    (force-mode-line-update)))

(defun zmacs-load-theme ()
  (interactive)
  (progn
    (zmacs-disable-all-themes)
    (call-interactively 'load-theme)))

(defvar zmacs-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'zmacs-after-load-theme-hook))

(use-package zmacs-themes
  :ensure nil
  :config
  (load-theme 'zmacs-dark))

(defun zmacs--system-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ;; ('light (progn
    ;;           (load-theme 'lambda-light)
    ;;           (customize-save-variable zmacs-active-theme 'light-theme)))
    ('dark (progn
             (load-theme 'zmacs-dark)
             (customize-save-variable zmacs-active-theme 'dark-theme)))))

;;;;; Define User Theme
(defcustom zmacs-ui-theme nil
  "Default user theme."
  :group 'lambda-emacs
  :type 'symbol)

;; If set, load user theme, otherwise load lambda-themes
(cond ((bound-and-true-p zmacs-ui-theme)
       (load-theme zmacs-ui-theme t))
      ;; ((eq zmacs-active-theme 'light-theme)
      ;;  (load-theme 'zmacs-light t))
      ((eq zmacs-active-theme 'dark-theme)
       (zmacs--system-apply-theme 'zmacs-dark))
      (t
       (zmacs--system-apply-theme 'zmacs-dark)))

;; kind-icon needs to have its cache flushed after theme change
(with-eval-after-load 'kind-icon
  (add-hook 'zmacs-themes-after-load-theme-hook #'kind-icon-reset-cache))


(provide 'zmacs-theme)

;;; zmacs-theme.el ends here.
