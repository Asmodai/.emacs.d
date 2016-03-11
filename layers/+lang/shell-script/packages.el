;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Shell scripts of various kinds.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2016 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    11 Mar 2016 18:08:32
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

(setq shell-scripts-packages
      '(fish-mode
        (sh-script :location built-in)))

(defun shell-scripts:init-fish-mode ()
  (use-package fish-mode
    :defer t))

(defun shell-scripts:init-sh-script ()
  (use-package sh-script
    :defer t
    :init
    (progn
      (dolist (pattern '("\\.zsh\\'"
                         "zlogin\\'"
                         "zlogout\\'"
                         "zpreztorc\\'"
                         "zprofile\\'"
                         "zshenv\\'"
                         "zshrc\\'"))
        (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))

      (defun bootstrap::setup-shell ()
        (when (and buffer-file-name
                   (string-match-p "\\.zsh\\'" buffer-file-name))
          (sh-set-shell "zsh")))

      (add-hook 'sh-mode-hook 'bootstrap::setup-shell))))

;;; packages.el ends here
