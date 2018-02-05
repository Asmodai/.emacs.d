;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- LSL scripting packages.
;;;
;;; Copyright (c) 2018 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Feb 2018 00:07:11
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

(setq lsl-packages
      '((lsl-mode :location local)))

(defun lsl:init-lsl-mode ()
  (use-package lsl-mode
    :init
    (progn
      (autoload 'lsl-mode "lsl-mode" "Load lsl-mode" t)
      (add-to-list 'auto-mode-alist '("\\.lsl\\'" . lsl-mode)))))

;;; packages.el ends here.
