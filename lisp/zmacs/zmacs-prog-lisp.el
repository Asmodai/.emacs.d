;;; zmacs-prog-lisp.el --- Emacs Lisp packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:32:39
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
(require 'zmacs-programming)

;;;; Interior Emacs Lisp Mode:

(use-package ielm
  :ensure nil
  :demand t
  :custom
  (ielm-dynamic-return           t)
  (ielm-dynamic-multiline-inputs t)
  (ielm-header                   "*** Welcome to IELM ***"))

;;;; Flycheck:

(use-package flycheck-package
  :defer t
  :hook (emacs-lisp-mode . flycheck-package-setup)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-elsa
  :defer t
  :hook (emacs-lisp-mode . flycheck-elsa-setup))

;;;; Inspector:

(use-package inspector
  :defer t)

;;;; Tags:

(use-package ggtags
  :defer t)

;;;; Macrostep:

(use-package macrostep
  :defer t
  :mode (("\\*.el\\'" . emacs-lisp-mode)
         ("Cask\\'"   . emacs-lisp-mode)))

;;;; Nameless:

(use-package nameless
  :defer t
  :init
  (progn
    (setq nameless-separator nil
          nameless-prefix    ">"))
  (put 'nameless-current-name 'safe-local-variable #'stringp))

;;;; Overseer:

(use-package overseer
  :defer t)

;;;; EDebug:

(use-package edebug
  :defer t)

;;;; Bug Hunter:

(use-package bug-hunter
  :defer 2)

;;;; Elisp-Def:

(use-package elisp-def
  :defer t)

;;;; EMR:

(use-package emr
  :defer t)

;;;; ESup:

(use-package esup
  :ensure nil
  :init
  (unless (package-installed-p 'esup)
    (package-vc-install "https://github.com/kiennq/esup.git"))
  :commands esup)

;;;; Dash:

(use-package dash
  :ensure t
  :defer t
  :commands (global-dash-fontify-mode
             dash-register-info-lookup)
  :config
  (global-dash-fontify-mode)
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))

;;;; Hooks:

(add-hook 'emacs-lisp-mode 'electric-pair-mode)
(add-hook 'lisp-mode       'electric-pair-mode)

(add-hook 'lisp-interaction-mode #'zmacs/deactivate-smartparens)
(add-hook 'lisp-interaction-hook #'zmacs/deactivate-paredit)

;;;; Define package:

(provide 'zmacs-prog-lisp)

;;; zmacs-prog-lisp.el ends here.
