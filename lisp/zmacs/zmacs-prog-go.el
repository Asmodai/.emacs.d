;;; zmacs-prog-go.el --- Go packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    28 Oct 2024 04:23:16
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

(require 'cl-lib)
(require 'zlisp-platform)

;;;; Eglot setup:

(require 'jsonrpc)

(setq-default eglot-workspace-configuration
              '((:gopls . ((staticcheck . t)
                           (matcher     . "CaseSensitive")))))

(let ((gopls (cond ((zlisp/unix-p)
                    (expand-file-name
                     (concat zmacs-projects-directory "Go/bin/gopls")))
                   (t
                    (error "Go has not been configured!")))))
  (when (file-exists-p gopls)
    (message (format "ZMACS/GO: Configured '%s' as LSP backend." gopls))
    (add-to-list 'eglot-server-programs `(go-mode ,gopls))
    (add-to-list 'eglot-server-programs `(go-ts-mode ,gopls))))

;;;; Customize:

(defgroup zmacs-golang ()
  "ZMACS Golang."
  :group 'zmacs-emacs)

(defcustom zmacs-go-tab-width 8
  "The tab width to use in Go mode."
  :type 'integer
  :group 'zmacs-golang
  :tag "Go tab width")

(defcustom zmacs-go-format-before-save t
  "Should gofmt be run on a buffer before saving?"
  :type 'boolean
  :group 'zmacs-golang
  :tag "Format before save?")

;;;; Package:
;;;;; Main package:

(use-package go-mode
  :defer t
  :hook ((go-mode . eglot-ensure)
         (go-mode . outline-minor-mode)
         (go-mode . outli-mode)
         (go-mode . flycheck-mode))
  :init
  (require 'outline)
  (require 'outline-minor-faces)
  :custom
  (go-test-verbose t))

;;;;; Tree-Sitter:

(use-package go-ts-mode)
;;(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))
;;(add-to-list 'major-mode-remap-alist '(go-mod-mode . go-mod-ts-mode))
;;(add-hook 'go-ts-mode-hook #'eglot-ensure)
;;(add-hook 'go-mod-ts-mode-hook #'eglot-ensure)
(add-hook 'go-mode-hook #'eglot-ensure)

(when (require 'treesit-fold nil t)
  (add-hook 'go-ts-mode-hook #'treesit-fold-mode))

(when (require 'treesit-expand-region nil t)
  (define-key go-ts-mode-map (kbd "C-=") #'treesit-expand-region)
  (define-key go-ts-mode-map (kbd "C--") #'treesit-contract-region))

;;;;; Eldoc:

(use-package go-eldoc
  :after go-mode
  :demand t
  :init (go-eldoc-setup))

;;;;; flycheck-golangci-lint:

(use-package flycheck-golangci-lint
  :ensure t
  :after go-mode
  :demand t
  :hook ((go-mode    . flycheck-golangci-lint-setup)
         (go-ts-mode . flycheck-golangci-lint-setup)))

;;;;; zlisp-go:

(use-package zlisp-go
  :ensure nil
  :after (go-ts-mode
          flycheck-golangci-lint)
  :demand t
  :commands (zlisp/setup-go-project
             zlisp/go-enable-flycheck-golangci-lint
             zlisp/go-setup-tab-width
             zlisp/go-setup-format)
  :custom
  (go-packages-function 'zlisp/go-packages-gopkgs)
  :config
  (progn
    (require 'zlisp-go)

    (add-hook 'go-mode-local-vars-hook 'zlisp/go-setup-tab-width)
    (add-hook 'go-mode-local-vars-hook 'zlisp/go-setup-format)
    (add-hook 'go-mode-local-vars-hook 'zlisp/go-enable-flycheck-golangci-lint)

    (add-hook 'go-ts-mode-local-vars-hook 'zlisp/go-setup-tab-width)
    (add-hook 'go-ts-mode-local-vars-hook 'zlisp/go-setup-format)
    (add-hook 'go-ts-mode-local-vars-hook 'zlisp/go-enable-flycheck-golangci-lint)

    (setq go-packages-function 'zlisp/go-package-gopkgs)

    (zlisp/go-setup-format)
    (zlisp/go-setup-project)
    (zlisp/go-enable-flycheck-golangci-lint)))

;;;; Provide package:

(provide 'zmacs-prog-go)

;;; zmacs-prog-go.el ends here.
