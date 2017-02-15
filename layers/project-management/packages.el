;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Package management addons.
;;;
;;; Copyright (c) 2016 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    15 Oct 2016 21:02:24
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

(setq project-management
      '(eproject :location local))

(defun projet-management:init-eproject ()
  (use-package eproject
    :defer nil
    :pin manual
    :init
    (progn
      (require 'eproject)
      (require 'eproject-extras))))

;;; packages.el ends here.
