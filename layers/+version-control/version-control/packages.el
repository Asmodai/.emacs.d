;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Version control packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Dec 2015 16:03:37
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

(setq version-control-packages '(diff-mode
                                 diff-hl))

(defun version-control:init-diff-mode ()
  (use-package diff-mode
    :defer t))

(defun version-control:init-diff-hl ()
  (use-package diff-hl-change
    :init
    (progn
      (setq diff-hl-side 'right)
      (global-diff-hl-mode)

      (unless (display-graphic-p)
        (setq diff-hl-side 'left)
        (diff-hl-margin-mode)))))

;;; packages.el ends here
