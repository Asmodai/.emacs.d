;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Lisp packages.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2016 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    10 Mar 2016 17:37:03
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

(setq lisp-packages
      '(slime
        (cl-indent-patches :location local)))

(defun lisp:init-slime ()
  (use-package slime
    :commands slime-mode
    :init
    (progn
      (setq slime-contribs '(slime-fancy
                             slime-asdf
                             slime-repl
                             slime-autodoc
                             slime-scratch)
            inferior-lisp-program "sbcl")

      (setq slime-complete-symbol*-fancy t)
      (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

      (bootstrap:add-to-hooks 'slime-mode '(lisp-mode-hook
                                            slime-repl-mode-hook
                                            scheme-mode-hook)))))

(defun lisp:init-cl-indent-patches ()
  (use-package cl-indent-patches
    :init (progn
            (require 'cl-indent-patches))))

;;; packages.el ends here
