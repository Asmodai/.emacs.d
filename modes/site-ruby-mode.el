;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-ruby-mode.el --- Ruby Mode
;;;
;;; Time-stamp: <Tuesday Jan 24, 2012 15:35:26 asmodai>
;;; Revision:   10
;;;
;;; Copyright (c) 2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    23 Jan 2012 18:39:07
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
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
;;; }}}
;;; {{{ Commentary:
;;;
;;; }}}

(when emacs>=19-p
  (setq ruby-program-name "c:/ruby193/bin/irb.bat --inf-ruby-mode")

  (autoload 'run-ruby "inf-ruby")
  (autoload 'inf-ruby-keys "inf-ruby")

  (defun my-ruby-mode-hooks ()
    (when (or emacs=20-p
              emacs=21-p)
      (turn-on-font-lock)
      (font-lock-mode 1))
    (auto-fill-mode t)
    (inf-ruby-keys))

  (setq
   ;;
   ;; auto-mode
   auto-mode-alist
   (append '(("\\.rb$" . ruby-mode)) auto-mode-alist)
   ;;
   ;; interpreter-mode
   interpreter-mode-alist
   (append '(("ruby" . ruby-mode)) interpreter-mode-alist))

  ;;
  ;; Hooks
  (add-hook 'c-mode-hook 'ruby-style-c-mode)
  (add-hook 'c++-mode-hook 'ruby-style-c-mode)
  (add-hook 'ruby-mode 'my-ruby-mode-hooks)
  
  ;;
  ;; Company mode
  (when (featurep 'company)
    (add-hook 'ruby-mode 'company-mode)))

;;; ruby-mode.el ends here
