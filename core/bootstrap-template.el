;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; bootstrap-template.el --- Load template library.
;;;
;;; Copyright (c) 2017 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    13 Feb 2017 15:46:27
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

(require 'template)
(template-initialize)

(setq template-default-directories
      (append (list (concat user-home-directory
                            ".emacs.d/templates/"))
              template-default-directories))

;; custom.el isn't overriding this.
(setf template-subdirectories
      (list (expand-file-name (concat +bootstrap-directory+ "templates/"))
            (expand-file-name (concat user-home-directory "templates/"))))

(provide 'bootstrap-template)

;;; bootstrap-template.el ends here.
