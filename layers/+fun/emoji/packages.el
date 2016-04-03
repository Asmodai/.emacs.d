;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Emoji!
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 17:04:17
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

(setq emoji-packages
      '(emoji-cheat-sheet-plus
        company-emoji))

(defun emoji:init-emoji-cheat-sheet-plus ()
  (use-package emoji-cheat-sheet-plus
    :if (not (terminal-p))
    :commands (emoji-cheat-sheet-plus-insert
               emoji-cheat-sheet-plus-buffer
               emoji-cheat-sheet-plus-display-mode)
    :init
    (progn
      (defun bootstrap:delay-emoji-cheat-sheet-hook ()
        "Workaround for org buffers."
        (run-at-time 0.1 nil 'emoji-cheat-sheet-plus-display-mode)))))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun emoji:init-company-emoji ()
    (use-package company-emoji
      :if (bootstrap-layer:layer-used-p 'company)
      :defer t
      :init (setq company-emoji-insert-unicode nil))))

;;; packages.el ends here.
