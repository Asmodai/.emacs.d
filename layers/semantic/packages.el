;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Semantic packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 21:51:19
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

(defvar semantic-packages
  '(semantic
    stickyfunc-enhance))


(unless (version< emacs-version "24.4")
  (add-to-list 'semantic-packages 'srefactor))

(defvar semantic-excluded-packages '())

(defun bootstrap:enable-semantic-mode (mode)
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook (lambda ()
                     (require 'semantic)
                     (add-to-list 'semantic-default-submodes
                                  'global-semantic-stickyfunc-mode)
                     (add-to-list 'semantic-default-submodes
                                  'global-semantic-idle-summary-mode)
                     (semantic-mode 1)))))

(defun semantic:init-semantic ()
  (use-package semantic
    :defer t
    :init
    (progn
      (setq srecore-map-save-file (concat +bootstrap-cache-directory+
                                          "srecode-map.el"))
      (setq semanticdb-default-save-directory
            (concat +bootstrap-cache-directory+ "semanticdb/"))

      (unless (file-exists-p semanticdb-default-save-directory)
        (make-directory semanticdb-default-save-directory)))))

(defun semantic:init-srefactor ()
  (use-package srefactor
    :defer t
    :init
    (progn
      (defun bootstrap:lazy-load-srefactor ()
        (require 'srefactor)))))

(defun semantic:init-stickyfunc-enhance ()
  (use-package stickyfunc-enhance
    :defer t
    :init
    (defun bootstrap:lazy-load-stickyfunc-enhance ()
      (require 'stickyfunc-enhance))))

;;; packages.el ends here.
