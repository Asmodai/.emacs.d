;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Erlang packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 18:34:30
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

(setq erlang-packages
      '(erlang
        flycheck
        company
        imenu))

(defun erlang:post-init-company ()
  (add-hook 'erlang-mode-hook 'company-mode)
  (bootstrap:add-company-hook erlang-mode))

(defun erlang:init-erlang ()
  (use-package erlang
    :defer t
    :config
    (progn
      (require 'erlang-start)

      (add-hook 'erlang-mode-hook
                (lambda ()
                  (run-hooks 'prog-mode-hook)))

      (setq erlang-compile-extra-opts '(debug_info))
      (setq inferior-erlang-machine-options '("-sname" "emacs")))
      (setq inferior-erlang-prompt-timeout t)

      (setq erl-nodename-cache
            (make-symbol
               (concat
                 "emacs@"
                 (car (split-string (shell-command-to-string "hostname"))))))))

(defun erlang:post-init-flycheck ()
  (bootstrap:add-flycheck-hook 'erlang-mode-hook))

(defun erlang:post-init-imenu ()
  (add-hook 'erlang-mode-hook 'imenu-add-menubar-index))

;;; packages.el ends here.
