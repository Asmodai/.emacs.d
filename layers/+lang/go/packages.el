;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Go layer packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 02:16:14
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

(setq go-packages '(company
                    company-go
                    flycheck
                    indent-guide
                    go-mode
                    go-eldoc
                    gorepl-mode
                    go-playground
                    (go-oracle :location built-in)
                    (go-rename :location local)))

(defun go:post-init-flycheck ()
  (bootstrap:add-flycheck-hook 'go-mode-hook)
  (bootstrap:add-flycheck-hook 'flycheck-golangci-lint-setup))

(defun go:init-go-mode ()
  (when (unix-p)
    (dolist (var '("GOPATH"))
      (unless (getenv var)
        (exec-path-from-shell-copy-env var))))

  (use-package go-mode
    :defer t
    :config
    (progn
      (add-hook 'before-save-hook 'gofmt-before-save)

      (defun bootstrap:go-run-tests (args)
        (interactive)
        (save-selected-window
          (async-shell-command (concat "go test " args))))

      (defun bootstrap:go-run-package-tests ()
        (interactive)
        (bootstrap:go-run-tests ""))

      (defun bootstrap:go-run-package-tests-nested ()
        (interactive)
        (bootstrap:go-run-tests "./..."))

      (defun bootstrap:go-run-test-current-function ()
        (interactive)
        (if (string-match "_test\\.go" buffer-file-name)
            (let ((test-method (if *go-use-gocheck-for-testing*
                                   "-check.f"
                                 "-run")))
              (save-excursion
                (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]]+\\)(.*)")
                (bootstrap:go-run-tests
                 (concat test-method
                         "='"
                         (match-string-no-properties 2)
                         "'"))))
          (message "Must be in a _test.go file to run go-run-test-current-function")))

      (defun bootstrap:go-run-test-current-suite ()
        (interactive)
        (if (string-match "_test\.go" buffer-file-name)
            (if *go-use-gocheck-for-testing*
                (save-excursion
                  (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]]+(.*)")
                  (bootstrap:go-run-tests
                   (concat "-check.f='" (match-string-no-properties 2) "'")))
              (message "Gocheck is needed to test the current suite"))
          (message "Must be in a _test.go file to run go-test-current-suite")))

      (defun bootstrap:go-run-main ()
        (interactive)
        (shell-command
         (format "go run %s"
                 (shell-quote-argument (buffer-file-name))))))))

(defun go:init-go-eldoc ()
  (add-hook 'go-hook-mode 'go-eldoc-setup))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun go:post-init-company ()
    (bootstrap:add-company-hook go-mode))

  (defun go:init-company-go ()
    (use-package company-go
      :if (bootstrap-layer:package-used-p 'company)
      :defer t
      :init (push 'company-go *company-backends-go-mode*))))

(defun load-gopath-file (gopath name)
  (let* ((sep (if (windows-p)
                  ";"
                ":"))
         (paths (split-string gopath sep))
         found)
    (loop for p in paths
          for file = (concat p name)
          when (file-exists-p file)
          do
          (load-file file)
          (setq found t)
          finally
          return found)))

(defun go:init-go-oracle ()
  (let ((go-path (getenv "GOPATH")))
    (if (not go-path)
        (bootstrap-buffer:warning
         "GOPATH variable not found, not loading go-oracle.")
      (load-gopath-file
       go-path
       "/src/golang.org/x/tools/cmd/oracle/oracle.el"))))

(defun go:init-go-rename ()
  (use-package go-rename
    :defer t))

(defun go:init-gorepl-mode ()
  (use-package gorepl-mode
    :defer t
    :init
    (bootstrap:add-to-hook 'go-mode-hook 'gorepl-mode)))

(defun go:init-go-playground ()
  (use-package go-playground
    :defer t))

(defun go:post-init-indent-guide ()
  (bootstrap:add-to-hooks 'indent-guide-mode '(go-mode-hook)))

;;; packages.el ends here.
