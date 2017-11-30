;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Lua packages.
;;;
;;; Copyright (c) 2017 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    30 Nov 2017 22:11:19
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

(setq lua-packages
      '((lua-mode :location local)))

(defun lua:init-lua-mode ()
  (use-package lua-mode
    :init
    (require 'lua-mode)
    :config
    (progn
      (autoload 'lua-mode "lua-mode" "Lua editing tools." t)
      (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
      (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))))

;;; packages.el ends here.
