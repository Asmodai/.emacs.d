;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Dash packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Dec 2015 15:24:40
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

(setq dash-packages '(helm-dash))

(cond ((mac-os-x-p)
       (push 'dash-at-point dash-packages))
      ((and (linux-p)
            (not (terminal-p)))
       (push 'zeal-at-point dash-packages)))

(defun dash:init-helm-dash ()
  (use-package helm-dash
    :defer t
    :config
    (defun dash::activate-package-docsets (path)
      (setq helm-dash-docsets-path path
            helm-dash-common-docsets (helm-dash-installed-docsets))
      (message (format "Activated %d docsets from: %s"
                       (length helm-dash-common-docsets)
                       path)))

    (dash::activate-package-docsets *dash-helm-dash-docset-path*)))

(defun dash:init-dash-at-point ()
  (use-package dash-at-point
    :defer t))

(defun dash:init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :config
    (push '(web-mode . "html,css,javascript") zeal-at-point-mode-alist)))

;;; packages.el ends here
