;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; keybindings.el --- Mac OS X keybindings.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 18:23:10
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

(when (mac-os-x-p)

  (when (display-graphic-p)
    (setq mac-command-key-is-meta nil)
    (setq mac-command-modifier 'super))
  
  (when *osx-use-option-as-meta*
    (setq mac-option-key-is-meta t)
    (setq mac-option-modifier 'meta))
  
  (global-unset-key (kbd "M-3"))       ; Unbind M-3 first.
  (global-set-key (kbd "M-3")          ; Bind so the UK keyboard can
                  (lambda ()            ; generate the hash symbol.
                    (interactive)
                    (insert "#"))))

;;; keybindings.el ends here
