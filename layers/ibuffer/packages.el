;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- ibuffer packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 22:56:16
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; Original author: Aleksandr Guljajev <aleksandr.guljajev@gmail.com>
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

(defvar ibuffer-packages
  '((ibuffer :location built-in)
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
          (bootstrap::ibuffer-create-buffs-group)))
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

;;; packages.el ends here.
