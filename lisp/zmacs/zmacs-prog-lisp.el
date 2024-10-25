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

(eval-when-compile
  (require 'cl-lib))

(use-package flycheck-package
  :defer t
  :hook
  (emacs-lisp-mode . flycheck-package-setup)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-elsa
  :defer t
  :hook
  (emacs-lisp-mode . flycheck-elsa-setup))

(use-package inspector
  :defer t)

(use-package ggtags
  :defer t)

(use-package macrostep
  :defer t
  :mode (("\\*.el\\'" . emacs-lisp-mode)
         ("Cask\\'"   . emacs-lisp-mode)))

(use-package nameless
  :defer t
  :init
  (progn
    (setq nameless-separator nil
          nameless-prefix    ">"))
  (put 'nameless-current-name 'safe-local-variable #'stringp))

(use-package overseer
  :defer t)

(use-package rainbow-identifiers
  :defer t)

;;; Smart parentheses.
(use-package smartparens
  :defer t
  :init
  (progn
    (setq sp-show-pair-delay
          ;; Use this form to allow users to override this setting from
          ;; dotspacemacs/user-init
          (or (bound-and-true-p sp-show-pair-delay) 0.2)
          ;; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil))
  :config
  (progn
    (require 'smartparens-config)
    (show-smartparens-global-mode +1)
    (zmacs:diminish smartparens-mode " 🄪" " SP")))

(defun zmacs::deactivate-smartparens (&optional global)
  "Deactivate `smartparens-mode' and `smartparens-strict-mode'.

If GLOBAL is non-NIL then we work on the global modes."
  (if global
      (progn
        (when smartparens-global-strict-mode
          (smartparens-global-strict-mode -1))
        (smartparens-global-mode -1))
    (when (and (boundp 'smartparens-strict-mode)
               smartparens-strict-mode)
      (smartparens-strict-mode -1))
    (smartparens-mode -1)))

(use-package edebug
  :defer t)

(use-package bug-hunter
  :defer 2)

(use-package elisp-def
  :defer t)

(use-package emr
  :defer t)

(use-package esup
  :ensure nil
  :init
  (unless (package-installed-p 'esup)
    (package-vc-install "https://github.com/kiennq/esup.git"))
  :commands esup)

(use-package dash
  :ensure t
  :defer t
  :commands (global-dash-fontify-mode
             dash-register-info-lookup)
  :config
  (global-dash-fontify-mode)
  (with-eval-after-load 'info-look
    (dash-register-info-lookup)))

(provide 'zmacs-prog-lisp)

;;; zmacs-prog-lisp.el ends here.
