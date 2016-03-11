;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- deft packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 23:09:22
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

(setq deft-packges '(deft))

(defun deft:init-deft ()
  (use-package deft
    :defer t
    :init
    (progn
      (setq deft-extensions '("org" "md" "txt")
            deft-text-mode 'org-mode
            deft-use-filename-as-title t)

      (defun bootstrap:deft ()
        (interactive)
        (deft)
        (when (fboundp 'hungry-delete-mode)
          (hungry-delete-mode -1))))))

;;; packages.el ends here
