;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Language parser / compiler generators.
;;;
;;; Copyright (c) 2017 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    19 Feb 2017 00:11:31
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

(setq parser-packages
      '(bison-mode
        (flex-mode :location local)))

(defun parser:init-bison-mode ()
  (use-package bison-mode
    :defer t))

(defun parser:init-flex-mode ()
  (use-package flex-mode
    :defer t
    :init (progn
            (require 'flex-mode))))

;;; packages.el ends here.
