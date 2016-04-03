;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- ccmode packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Dec 2015 00:02:54
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

(setq cc-packages
      '(cc-mode
        (ppindent :location local)
        (c-comment-edit :location local)
        disaster
        clang-format
        cmake-mode
        company
        company-c-headers
        company-ycmd
        flycheck
        gdb-mi
        helm-cscope
        helm-gtags
        semantic
        stickyfunc-enhance
        ycmd
        redspace
        indent-guide
        xcscope
        (code-style :location local)))

(unless (version< emacs-version "24.4")
  (add-to-list 'cc-packages 'srefactor))

(defun cc:init-cc-mode ()
  (use-package cc-mode
    :defer t
    :init (add-to-list 'auto-mode-alist
                       `("\\.h$" . ,*cc-mode-default-mode-for-headers*))
    :config
    (progn
      (require 'compile)
      (require 'align)
      (c-toggle-auto-newline))))

(defun cc:init-c-comment-edit ()
  (use-package c-comment-edit
    :defer t
    :init (setq c-comment-leader "  ")))

(defun cc:init-ppindent ()
  (use-package ppindent
    :defer t))

(defun cc:init-disaster ()
  (use-package disaster
    :defer t
    :commands (disaster)))

(defun cc:init-clang-format ()
  (use-package clang-format
    :if *cc-enable-clang-support*))

(defun cc:init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\'" . cmake-mode)
           ("\\.cmake\\'"        . cmake-mode))
    :init
    (progn
      (push 'company-cmake *company-backends-cmake-mode*))))

(defun cc:post-init-company ()
  (bootstrap:add-company-hook cmake-mode)
  (bootstrap:add-company-hook c-mode-common)

  (when *cc-enable-clang-support*
    (push 'company-clang *company-backends-c-mode-common*)

    (defun company-mode:find-clang-complete-file ()
      (when buffer-file-name
        (let ((dir (locate-dominating-file buffer-file-name
                                           ".clang_complete")))
          (when dir
            (concat (file-name-as-directory dir) ".clang_complete")))))

    (defun company-mode:load-clang-complete-file (cc-file)
      (let ((invocation-dir (expand-file-name (file-name-as-directory cc-file)))
            (case-fold-search nil)
            compile-flags)
        (with-temp-buffer
          (insert-file-contents cc-file)
          (while (re-search-forward "\\(-I\\|-isystem\n\\)\\(\\S-\\)" nil t)
            (replace-match (format "%s%s" (match-string 1)
                                   (expand-file-name (match-string 2)
                                                     invocation-dir))))
          (setq compile-flags
                (mapcar #'(lambda (line)
                            (if (string-match "[ \t]+$" line)
                                (replace-match "" t t line)
                              line))
                        (split-string (buffer-string) "\n" t))))
        compiler-flags))

    (defun company-mode:more-than-prefix-guesser ()
      (unless company-clang-arguments
        (let* ((cc-file (company-mode:find-clang-complete-file))
               (flags (if cc-file
                          (company-mode:load-clang-complete-file cc-file)
                        '())))
          (setq-local company-clang-arguments flags)
          (setq flycheck-clang-args flags)))
      (company-clang-guess-prefix))

    (setq company-clang-prefix-guesser 'company-mode:more-than-prefix-guesser)))

(when (bootstrap-layer:package-used-p 'auto-completion)
  (defun cc:init-company-c-headers ()
    (use-package company-c-headers
      :if (bootstrap-layer:package-used-p 'company)
      :defer t
      :init (push 'company-c-headers *company-backends-c-mode-common*))))

(defun cc:post-init-flycheck ()
  (bootstrap:add-to-hooks 'flycheck-mode '(c-mode-hook c++-mode-hook)))

(defun cc:init-gdb-mi ()
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default when `M-x gdb'
     gdb-many-windows t
     ;; Non-nil means display source file containing the main routine at startup
     gdb-show-main t)))

(defun cc:post-init-semantic ()
  (bootstrap:enable-semantic-mode 'c-mode)
  (bootstrap:enable-semantic-mode 'c++-mode))

(defun cc:post-init-srefactor ()
  (bootstrap:add-to-hooks 'bootstrap:lazy-load-srefactor
                          '(c-mode-hook c++-mode-hook)))

(defun cc:post-init-stickyfunc-enhance ()
  (bootstrap:add-to-hooks 'bootstrap:lazy-load-stickyfunc-enhance
                          '(c-mode-hook c++-mode-hook)))

(defun cc:post-init-ycmd ()
  (add-hook 'c++-mode-hook 'ycmd-mode))

(defun cc:post-init-company-ycmd ()
  (push 'company-ycmd *company-backends-c-mode-common*))

(defun cc:init-code-style ()
  (use-package code-style
    :defer t
    :init
    (progn
      (require 'code-style)
      (bootstrap:add-to-hooks 'google-set-c-style
                              '(c-mode-hook c++-mode-hook objc-mode-hook))
      (bootstrap:add-to-hooks 'google-make-newline-indent
                              '(c-mode-hook c++-mode-hook objc-mode-hook)))))

(defun cc:post-init-indent-guide ()
  (bootstrap:add-to-hooks 'indent-guide-mode
                          '(c-mode-hook c++-mode-hook objc-mode-hook
                                        java-mode-hook)))

(defun cc:post-init-redspace-mode ()
  (bootstrap:add-to-hooks 'redspace-mode 
                          '(c-mode-hook c++-mode-hook objc-mode-hook
                                        java-mode-hook)))

;;; packages.el ends here.
