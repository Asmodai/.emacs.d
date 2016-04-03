;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Emacs Lisp layer packages.
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

(setq emacs-lisp-packages
      '(company
        eldoc
        elisp-slime-nav
        (emacs-lisp :location built-in)
        flycheck
        ielm
        macrostep
        semantic
        smartparens
        srefactor))

(use-package ielm
  :config
  (progn
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>")
                            (goto-char current-point))
          (lisp-indent-line))))))
    
(defun emacs-lisp:post-init-company ()
  (bootstrap:add-company-hook ielm-mode)
  (push '(company-files company-capf) *company-backends-ielm-mode*))

(defun emacs-lisp:init-elisp-slime-nav ()
  (use-package elisp-slime-anv
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))))

(defun emacs-lisp:init-macrostep ()
  (use-package macrostep
    :defer t
    :mode ("\\*.el\\'" . emacs-lisp-mode)))

(defun emacs-lisp:post-init-flycheck ()
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun emacs-lisp:post-init-semantic ()
  (bootstrap:enable-semantic-mode 'emacs-lisp-mode)
  (eval-after-load 'semantic
    '(semantic-default-elisp-setup)))

(defun emacs-lisp:post-init-srefactor ()
  (add-hook 'emacs-lisp-mode-hook 'bootstrap:lazy-load-srefactor)
  (use-package srefactor-lisp
    :commands (srefactor-lisp-format-buffer
               srefactor-lisp-format-defun
               srefactor-lisp-format-sexp
               srefactor-lisp-one-line)))

(defun emacs-lisp:post-init-smartparens ()
  (defun bootstrap:eval-current-form-sp (&optional arg)
    (interactive "P")
    (require 'smartparens)
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp)))

  (defun bootstrap:eval-current-symbol-so ()
    (interactive)
    (require 'smartparens)
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp))))

(defun bootstrap:emacs-lisp-hook ()
  (font-lock-mode 1)
  (fci-mode 1)
  (smartparens-mode 1)
  (eldoc-mode 1)
  (auto-fill-mode 1))

(defun bootstrap:ielm-hook ()
  (font-lock-mode 1)
  (fci-mode -1)
  (smartparens-mode -1)
  (eldoc-mode 1)
  (auto-fill-mode 1))

(defun emacs-lisp:init-emacs-lisp ()
  (push 'company-capf *company-backends-emacs-lisp-mode*)
  (bootstrap:add-company-hook emacs-lisp-mode)
  (add-hook 'emacs-lisp-mode-hook 'bootstrap:emacs-lisp-hook)
  (add-hook 'lisp-interaction-mode-hook 'bootstrap:emacs-lisp-hook)
  (add-hook 'ielm-mode-hook 'bootstrap:ielm-hook))

;;; packages.el ends here.
