;;; zmacs-prog-go.el --- Go packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
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

(require 'cl-lib)
(require 'zlisp-platform)

;;;; Egot setup:

(require 'jsonrpc)

(setq-default eglot-workspace-configuration
              '((:gopls . ((staticcheck . t)
                           (matcher     . "CaseSensitive")))))

(let ((gopls (cond ((zlisp-unix-p)
                    (expand-file-name
                     (concat zmacs-projects-directory "Go/bin/gopls")))
                   (t
                    (error "Go has not been configured!")))))
  (when (file-exists-p gopls)
    (message (format "Configured '%s' as LSP backend." gopls))
    (add-to-list 'eglot-server-programs `(go-mode ,gopls))))

;;;; Customize:

(defgroup zmacs-golang ()
  "ZMACS Golang"
  :group 'zmacs-emacs)

(defcustom zmacs-go-tab-width 8
  "The tab width to use in Go mode."
  :type 'integer
  :group 'zmacs-golang)

(defcustom zmacs-go-format-before-save t
  "Should gofmt be run on a buffer before saving?"
  :type 'boolean
  :group 'zmacs-golang)

;;;; Helper functions:

(defun zmacs--go-set-tab-width ()
  "Set Go mode tab width."
  (when zmacs-go-tab-width
    (setq-local tab-width zmacs-go-tab-width)))

(defun zmacs--go-setup-format ()
  "Conditionally set up formatting on save."
  (if zmacs-go-format-before-save
      (add-hook 'before-save-hook 'gofmt-before-save)
    (remove-hook 'before-save-hook 'gofmt-before-save)))

(defun zmacs--go-packages-gopkgs ()
  "Return a list of all Go packages using `gopkgs'."
  (sort (process-lines "gopkgs") #'string<))

;;;; Set up Project:

(defun project-find-go-module (dir)
  "Locate a go module."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

;;;; Package:
;;;;; Main package:

(use-package go-mode
  :hook ((go-mode-local-vars . zmacs--go-set-tab-width)
         (go-mode-local-vars . zmacs-go-setup-format)
         (go-mode            . eglot-ensure)
         (go-mode            . outline-minor-mode)
         (go-mode            . outli-mode))
  :init
  (require 'outline)
  (require 'outline-minor-faces)
  :custom
  (go-packages-function 'zmacs--go-packages-gopkgs)
  (go-test-verbose      t))

;;;;; Eldoc:

(use-package go-eldoc
  :defer t)

;;;;; Fill struct:

(use-package go-fill-struct
  :defer t)

;;;;; Gen Test:

(use-package go-gen-test
  :defer t)

;;;;; Go Guru:

(use-package go-guru
  :defer t)

;;;;; Go Impl:

(use-package go-impl
  :defer t)

;;;;; Go Rename:

(use-package go-rename
  :defer t)

;;;;; Go Tag:

(use-package go-tag
  :defer t)

;;;;; Godoctor:

(use-package godoctor
  :defer t)

;;;; Provide package:

(provide 'zmacs-prog-go)

;;; zmacs-prog-go.el ends here.
