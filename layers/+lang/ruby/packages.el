;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Ruby packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 00:30:58
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

(setq ruby-packages
      '(bundler
        chruby
        company
        flycheck
        redspace-mode
        indent-guide
        rbenv
        robe
        rspec-mode
        rubocop
        ruby-test-mode
        ruby-rools
        rvm
        smartparens))

(if *ruby-enable-enh-ruby-mode*
    (add-to-list 'ruby-packages 'enh-ruby-mode)
    (add-to-list 'ruby-packages 'ruby-mode))

(defun ruby:init-bundler ()
  (use-package bundler
      :defer t))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun ruby:post-init-company ()
    (bootstrap:add-company-hook ruby-mode)
    (bootstrap:add-company-hook enh-ruby-mode)
    (with-eval-after-load 'company-dabbrev-code
      (dolist (mode '(ruby-mode enh-ruby-mode))
        (push mode company-dabbrev-code-modes)))))

(defun ruby:init-chruby ()
  (use-package chruby
      :if (equal *ruby-version-manager* 'chruby)
      :defer t
      :init
      (progn
        (defun bootstrap::enable-chruby ()
          "Enable `chruby', use `.ruby-version' if it exists."
          (let ((version-file-path (chruby--locate-file ".ruby-version")))
            (chruby)
            (if version-file-path
                (progn
                  (chruby-use (chruby--read-version-from-file
                               version-file-path))
                  (message (concat "[chruby] Using Ruby version "
                                   "from .ruby-version file.")))
                (message "[chruby] Using the currently active Ruby."))))
        (bootstrap:add-to-hooks 'bootstrap::enable-chruby
                                '(ruby-mode-hook enh-ruby-mode-hook)))))

(defun ruby:init-enh-ruby-mode ()
  (use-package enh-ruby-mode
    :mode (("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
           ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
    :interpreter "ruby"
    :config
    (progn
      (setq enh-ruby-deep-indent-paren nil
            enh-ruby-hanging-paren-deep-indent-level 2))))

(defun ruby:post-init-flycheck ()
  (bootstrap:add-flycheck-hook 'ruby-mode-hook)
  (bootstrap:add-flycheck-hook 'enh-ruby-mode-hook))

(defun ruby:init-rbenv ()
  (use-package rbenv
    :if (equal *ruby-version-manager* 'rbenv)
    :defer t
    :init
    (progn
      (defun bootstrap::enable-rbenv ()
        "Enable `rbenv' using `.ruby-version' if available."
        (require 'rbenv)
        (let ((version-file-path (rbenv--locate-file ".ruby-version")))
          (global-rbenv-mode)
          (if version-file-path
              (progn
                (rbenv-use (rbenv--read-version-from-file
                            version-file-path))
                (message (concat "[rbenv] Using Ruby version "
                                 "from .ruby-version file")))
              (message "[rbenv] Using the currently active Ruby."))))
      (bootstrap:add-to-hooks 'bootstrap::enable-rbenv
                              '(ruby-mode-hook enh-ruby-mode-hook)))))

(defun ruby:init-robe ()
  (use-package robe
    :defer t
    :init
    (progn
      (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
        (add-hook hook 'robe-mode))
      (when (bootstrap-layer:layer-used-p 'auto-completion)
        (push 'company-robe company-backends-enh-ruby-mode)
        (push 'company-robe company-backends-ruby-mode)))
    :config
    (progn
      (bootstrap:hide-lighter robe-mode))))

(defun ruby:init-rspec-mode ()
  (use-package rspec-mode
    :defer t
    :config
    (progn
      (bootstrap:hide-lighter rspec-mode))))

(defun ruby:init-rubocop ()
  (use-package rubocop
    :defer t
    :init (bootstrap:add-to-hooks 'rubocop-mode
                                  '(ruby-mode-hook enh-ruby-mode-hook))))

(defun ruby:init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :init (bootstrap:add-to-hooks 'redspace-mode
                                  '(ruby-mode-hook enh-ruby-mode-hook))))

(defun ruby:init-ruby-tools ()
  (use-package ruby-tools
    :defer t
    :init (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
            (add-hook hook 'ruby-tools-mode))
    :config (bootstrap:hide-lighter ruby-tools-mode)))

(defun ruby:init-ruby-test-mode ()
  (use-package ruby-test-mode
    :defer t
    :init
    (progn
      (defun bootstrap::enable-ruby-test-mode ()
        "Conditionally enable `ruby-test-mode'."
        (when (eq *ruby-test-runner* 'ruby-test)
          (ruby-test-mode)))
      (bootstrap:add-to-hooks 'bootstrap::enable-ruby-test-mode
                              '(ruby-mode-hook enh-ruby-mode-hook)))
    :config (bootstrap:hide-lighter ruby-test-mode)))

(defun ruby:init-rvm ()
  (use-package rvm
    :if (equal *ruby-version-manager* 'rvm)
    :defer t
    :init
    (progn
      (setq rspec-use-rvm t)
      (bootstrap:add-to-hooks 'rvm-activate-corresponding-ruby
                              '(ruby-mode-hook enh-ruby-mode-hook)))))

(defun ruby:post-init-smartparens ()
  (bootstrap:use-package-add-hook smartparens
    :post-config
    (sp-with-modes (if *ruby-enable-enh-ruby-mode*
                       'enh-ruby-mode
                       'ruby-mode)
      (sp-local-pair
       "{" "}"
       :pre-handlers '(sb-ruby-pre-handler)
       :post-handlers '(sb-ruby-post-handler
                        (bootstrap:smartparens-pair-newline-and-indent "RET"))
       :suffix ""))))

(defun ruby:post-init-indent-guide ()
  (bootstrap:add-to-hooks 'indent-guide-mode '(ruby-mode-hook)))

(defun ruby:post-init-redspace-mode ()
  (bootstrap:add-to-hooks 'redspace-mode '(ruby-mode-hook)))

;;; packages.el ends here.
