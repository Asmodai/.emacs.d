;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Elixir packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 18:18:59
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; Licenseas published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program isdistributed in the hope that it will be
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

(setq elixir-packages
      '(alchemist
        company
        elixir-mode
        popwin
        ruby-end))

(defun elixir:init-alchemist ()
  (use-package alchemist
    :defer t
    :init
    (progn
      (add-hook 'elixir-mode-hook 'alchemist-mode)
      (setq alchemist-project-compile-when-needed t)
      (push 'alchemist-company *company-backends-elixir-mode*)
      (push 'alchemist-company *company-backends-alchemist-iex-mode*))))

(defun elixir:post-init-company ()
  (bootstrap:add-company-hook elixir-mode)
  (bootstrap:add-company-hook alchemist-iex-mode))

(defun elixir:init-elixir-mode ()
  (use-package elixir-mode
    :defer t))

(defun elixir:pre-init-popwin ()
  (bootstrap:use-package-add-hook popwin
    :post-config
    (push '("*mix*" :tail t :noselect t) popwin:special-display-config)))

(defun elixir:init-ruby-end ()
  (use-package ruby-end
    :defer t
    :init
    (progn
      (defun bootstrap::ruby-end ()
        (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
             "\\(?:^\\|\\s-+\\)\\(?:do\\)")
        (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers)
             nil)
        (ruby-end-mode +1))
      (add-hook 'elixir-mode-hook 'bootstrap::ruby-end)
      (remove-hook 'ruby-mode-hook 'ruby-end-mode)
      (remove-hook 'enh-ruby-mode-hook 'ruby-end-mode))
    :config
    (progn
      (bootstrap:hide-lighter ruby-end-mode)
      (remove-hook 'ruby-mode-hook 'ruby-end-mode)
      (remove-hook 'ehn-ruby-mode-hook 'ruby-end-mode))))

;;; packages.el ends here
