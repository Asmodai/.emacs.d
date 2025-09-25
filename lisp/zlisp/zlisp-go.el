;;; zlisp-go.el --- Go functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    29 Oct 2024 07:08:10
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
(require 'project)
(require 'flycheck-golangci-lint)

(defun zlisp/go-find-cmd (command)
  "See if COMMAND has been installed into the Go directory structure."
  (let ((path (cond ((zlisp/unix-p)
                     (expand-file-name
                      (concat zmacs-projects-directory "Go/bin/" command)))
                    (t
                     (error "Go has not been configured!")))))
    (file-exists-p path)))

(defun zlisp/go-setup-tab-width ()
  "Set tab width for Go mode."
  (message "ZLISP/GO: Setting up tab width.")
  (when zmacs-go-tab-width
    (setq-local tab-width zmacs-go-tab-width)))

(defun zlisp/go-setup-format ()
  "Conditionally set up formatting on save."
  (message "ZLISP/GO: Setting up for `gofmt' on save.")
  (if zmacs-go-format-before-save
      (add-hook 'before-save-hook 'gofmt-before-save)
    (remove-hook 'before-save-hook 'gofmt-before-save)))

(defun zlisp/go-package-gopkgs ()
  "Return a list of all Go packages using `gopkgs'."
  (sort (process-lines "gopkgs") #'string<))

(defun project-find-go-module (dir)
  "Locate a Go module for DIR."
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(defun zlisp/go-setup-project ()
  "Set up Go project functions."
  (message "ZLISP/GO: Setting up `go module' for project.")
  (add-hook 'project-find-functions #'project-find-go-module))

(defun zlisp/go-enable-flycheck-golangci-lint ()
  "Enable `flycheck-golangci-linter' and disable overlapping linters."
  (message "ZLISP/GO: Setting up flycheck for golangci-lint.")
  (setq flycheck-disabled-checkers '(go-gofmt
                                     go-golint
                                     go-vet
                                     go-errcheck
                                     ;;go-staticcheck
                                     go-unconvert))
  (flycheck-golangci-lint-setup)
  (flycheck-add-next-checker 'go-build '(warning . golangci-lint) t)
  (flycheck-add-next-checker 'go-test  '(warning . golangci-lint) t)

  (cond ((flycheck-may-use-checker 'go-test)
         (flycheck-select-checker 'go-test))
        ((flycheck-may-use-checker 'go-build)
         (flycheck-select-checker 'go-build))))

(provide 'zlisp-go)

;;; zlisp-go.el ends here.
