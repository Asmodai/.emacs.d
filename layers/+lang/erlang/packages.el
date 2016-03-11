;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Erlang packages.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2016 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    11 Mar 2016 18:34:30
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

(setq erlang-packages
      '(company
        erlang
        flycheck))

(defun erlang:post-init-company ()
  (add-hook 'erlang-mode-hook 'company-mode))

(defun erlang:init-erlang ()
  (use-package erlang
    :defer t
    :init
    (progn
      (add-hook 'erlang-mode-hook
                (lambda ()
                  (run-hooks 'prog-mode-hook)))
      (setq erlang-compile-extra-opts '(debug_info)))
    :config
    (require 'erlang-start)))

(defun erlang:post-init-flycheck ()
  (bootstrap:add-flycheck-hook 'erlang-mode-hook))

;;; packages.el ends here
