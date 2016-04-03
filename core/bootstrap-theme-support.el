;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; bootstrap-theme-support.el --- Theme support.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 06:17:07
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

(defconst emacs-built-in-themes (custom-available-themes)
  "List of built-in Emacs themes.")

(defface org-kbd
  '((t
     (:background "LemonChiffon1"
      :foreground "black"
      :box (:line-width 2 :color nil :style released-button))))
  "Face for displaying key bindings in Bootstrap documents."
  :group 'org-faces)

(defvar *bootstrap-themes* '(spacemacs-dark))

(defvar *bootstrap-current-theme* nil
  "The currently-used theme.")

(defconst *bootstrap-theme-name-to-package*
  '((spacemacs-dark  . spacemacs-theme)
    (spacemacs-light . spacemacs-theme)))

(defun bootstrap::get-theme-package (theme)
  (cond ((memq theme emacs-built-in-themes)
         nil)
        ((assq theme *bootstrap-theme-name-to-package*)
         (cdr (assq theme *bootstrap-theme-name-to-package*)))
        (t
         (intern (format "%S-theme" theme)))))

(defun bootstrap:load-theme (theme)
  (unless (memq theme (custom-available-themes))
    (cond ((or (eq theme 'spacemacs-light)
	       (eq theme 'spacemacs-dark))
           (bootstrap:load-or-install-package 'spacemacs-theme)
           (add-to-list 'load-path (bootstrap::get-package-directory
                                    'spacemacs-theme))
           (require 'spacemacs-common)
           (deftheme spacemacs-dark "Spacemacs theme, the dark version")
           (deftheme spacemacs-light "Spacemacs theme, the light version"))
          ((assq theme *bootstrap-theme-name-to-package*)
           (let* ((pkg (bootstrap::get-theme-package theme))
                  (pkg-dir (bootstrap:load-or-install-package pkg)))
             (add-to-list 'custom-theme-load-path pkg-dir)))
          (t
           (let ((pkg (bootstrap::get-theme-package theme)))
             (bootstrap:load-or-install-package pkg)))))
  (load-theme theme t))

(defadvice load-theme (after bootstrap:load-theme-adv activate)
  (let ((theme (ad-get-arg 0)))
    (setq *bootstrap-current-theme* theme)
    (bootstrap:post-theme-init theme)))

(defun bootstrap:post-theme-init (theme)
  (interactive)
  (when (fboundp 'bootstrap:set-flycheck-mode-line-faces)
    (bootstrap:set-flycheck-mode-line-faces))
  (when (fboundp 'bootstrap:customize-powerline-faces)
    (bootstrap:ustomize-powerline-faces))
  (when (fboundp 'powerline-reset)
    (powerline-reset)))

(provide 'bootstrap-theme-support)

;;; bootstrap-theme-support.el ends here.
