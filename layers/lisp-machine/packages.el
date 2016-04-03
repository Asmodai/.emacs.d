;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Lisp Machine layer packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 06:17:07
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

(setq lisp-machine-packages
      '(help+
        help-fns+
        (symbolics :location local :step post)))

(defun lisp-machine:init-help+ ()
  (use-package help+
    :defer t))

(defun lisp-machine:init-help-fns+ ()
  (use-package help-fns+
    :defer t))

(defun lisp-machine:init-symbolics ()
  (use-package symbolics
    :defer t
    :init (require 'symbolics)
    :config (symbolics:install-keymap)))

;;; packages.el ends here.
