;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- C# packages.
;;;
;;; Copyright (c) 2016 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    27 Sep 2016 05:53:50
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

(setq csharp-packages
      '(company
        csharp-mode))

(defun csharp:init-csharp-mode ()
  (use-package csharp-mode
    :defer t
    :init
    (progn
      (push 'company-omnisharp *company-backends-csharp-mode*))
    :config
    (progn
      (when (fboundp 'google-set-c-style)
        (add-hook 'csharp-mode-hook 'google-set-c-style)
        (add-hook 'csharp-mode-hook 'google-make-newline-indent)))))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun csharp:post-init-company ()
    (bootstrap:add-company-hook csharp-mode)))

;;; packages.el ends here.
