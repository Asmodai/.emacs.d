;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-ibuffer.el --- ibuffer hacks.
;;;
;;; Time-stamp: <Wednesday Jul 17, 2013 15:23:44 asmodai>
;;; Revision:   1
;;;
;;; Copyright (c) 2013 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    17 Jul 2013 14:03:38
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
;;; Please make sure that Ibuffer is loaded.
;;;
;;;}}}

(if (and (or (emacs>=20-p)
             (xemacs>=21-p))
         (not (featurep 'ibuffer)))
    (require 'ibuffer))

(when (featurep 'ibuffer)
    
  (setq ibuffer-expert t
        ibuffer-default-srting-mode 'major-mode
        ibuffer-fontification-level t
        ibuffer-saved-filter-groups
          (quote
           (("my-ibuffer-groups"
             ("Dired" (mode . dired-mode))
             ("Documentation"
              (or (mode . help-mode)))
             ("Fundamental"
              (mode . fundamental-mode))
             ("Lisp"
              (or (mode . emacs-lisp-mode)
                  (mode . lisp-mode)
                  (mode . common-lisp-mode)
                  (mode . scheme-mode)))
             ("Programming"
              (or (mode . c-mode)
                  (mode . c++-mode)
                  (mode . cperl-mode)
                  (mode . ruby-mode)
                  (mode . java-mode)
                  (mode . python-mode)
                  (mode . shell-script-mode)
                  (mode . objc-mode)
                  (mode . asm-mode)
                  (mode . makefile-mode)
                  (mode . cmake-mode)))
             ("Markup"
              (or (mode . html-mode)
                  (mode . sgml-mode)
                  (mode . xml-mode)
                  (mode . tex-mode)
                  (mode . latex-mode)
                  (mode . texinfo-mode)))
             ("Emacs"
              (or (name . "^\\*scratch\\*$")
                  (name . "^\\*Messages\\*$")))))))
  
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
              (ibuffer-switch-to-saved-filter-groups
               "my-ibuffer-groups")))
  
  ;;
  ;; ^x-^b - Show ibuffer.
  (global-set-key "\C-x\C-b" 'ibuffer))

;;; site-ibuffer.el ends here
