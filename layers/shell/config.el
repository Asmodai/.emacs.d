;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; config.el --- Shell configuration.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 21:42:43
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

(bootstrap:defvar-company-backends eshell-mode)

(defvar shell-default-shell (if (eq window-system 'w32)
                                  'eshell
                                'ansi-term))

(defvar shell-default-position 'bottom)

(defvar shell-default-height 30)

(defvar shell-default-term-shell "/bin/bash")

(defvar *shell-enable-smart-eshell* t)

(defvar *shell-protect-eshell-prompt* t)

;;; config.el ends here.
