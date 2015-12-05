;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Packages used by all language modes.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2015 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    04 Dec 2015 13:33:49
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

(setq all-langs-packages
      '((comment-block :location local)))

(defun all-langs:init-comment-block ()
  (use-package comment-block
    :defer t
    :init (require 'comment-block)))

;;; packages.el ends here
