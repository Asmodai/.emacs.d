;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Packages used by all language modes.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 13:33:49
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

(setq all-langs-packages
      '(ido
        (comment-block :location local)
        (doc-mode      :location local)))

(defun all-langs:post-init-ido ()
  (bootstrap:add-to-hooks 'ido-mode '(program-mode-hook)))

(defun all-langs:init-comment-block ()
  (use-package comment-block
    :defer t
    :init (require 'comment-block)))

(defun all-langs:init-doc-mode ()
  (use-package doc-mode
    :defer t
    :init
    (progn
      (require 'doc-mode)

      (bootstrap:add-to-hooks 'doc-mode '(c-mode-hook
                                          c++-mode-hook
                                          php-mode-hook))
      (bootstrap:hide-lighter doc-mode))))

;;; packages.el ends here.
