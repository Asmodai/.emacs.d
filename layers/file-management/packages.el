;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- File management layer packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 18:16:33
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

(setq file-management-packages '(sunrise-commander
                                 sunrise-x-modeline
                                 sunrise-x-tabs
                                 sunrise-x-tree))

(defun file-management:init-sunrise-commander ()
  (use-package sunrise-commander
    :defer t))

(defun file-management:init-sunrise-x-modeline ()
  (use-package sunrise-x-modeline
    :defer t))

(defun file-management:init-sunrise-x-tabs ()
  (use-package sunrise-x-tabs
    :defer t))

(defun file-management:init-sunrise-x-tree ()
  (use-package sunrise-x-tree
    :defer t))

;;; packages.el ends here.
