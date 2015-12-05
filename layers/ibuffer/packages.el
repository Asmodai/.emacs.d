;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- ibuffer packages.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2015 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    04 Dec 2015 22:56:16
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

(defvar '(ibuffer
          ibuffer-projectile))

(defun ibuffer:init-ibuffer ()
  (use-package ibuffer
    :defer t
    :init
    (progn
      (global-unset-key (kbd "C-x C-b"))
      (global-set-key (kbd "C-x C-b") 'ibuffer)

      (defun bootstrap::ibuffer-group-by-modes ()
        (when (eq ibuffer-group-buffers-by 'modes)
          (bootstrap::ibuffer-create-buffs-grup)))
      (add-hook 'ibuffer-hook 'bootstrap::ibuffer-group-by-modes))))

(defun ibuffer:init-ibuffer-projectile ()
  (use-package ibuffer-projectile
    :defer t
    :init
    (progn
      (defun bootstrap::ibuffer-group-by-projects ()
        (when (eq ibuffer-group-buffers-by 'projects)
          (ibuffer-projectile-set-filter-groups)
          (unless (eq ibuffer-sorting-mode 'alphabetic)
            (ibuffer-do-sort-by-alphabetic))))
      (add-hook 'ibuffer-hook 'bootstrap::ibuffer-group-by-projects))))

;;; packages.el ends here
