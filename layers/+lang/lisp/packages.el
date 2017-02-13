;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Lisp packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    10 Mar 2016 17:37:03
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

(setq lisp-packages
      '(slime
        (cl-indent-patches :location local)
        ;; TODO: Fix this shit, man.
        ;;(shift-lock        :location local)
        ))

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

      (defvar *interactive-lisp-mode* 'slime)

      (defsubst slime-p ()
        (eq *interactive-lisp-mode* 'slime))

      (when (slime-p)
        (setq inferior-lisp-program
              (cond ((unix-p)       "ccl")
                    ((windows-nt-p) "c:/ccl/xw86cl.exe")
                    ((t             "lisp")))))

      (bootstrap:add-to-hooks 'slime-mode '(lisp-mode-hook
                                            slime-repl-mode-hook
                                            scheme-mode-hook)))))

(defun lisp:init-cl-indent-patches ()
  (use-package cl-indent-patches
    :init (progn
            (require 'cl-indent-patches))))

(defun lisp:init-shift-lock ()
  (use-package shift-lock
    :defer t
    :init
    (progn
      (require 'shift-lock)
      (bootstrap:add-to-hook 'lisp-mode-hook 'electric-shift-lock-mode))))

;;; packages.el ends here.
