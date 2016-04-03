;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- SQL packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 17:25:02
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

(defvar sql-packages '(sql
                       sql-indent))

(defun sql:init-sql ()
  (use-package sql
    :defer t
    :config
    (progn
      (setq *bootstrap-sql-highlight-table* sql-product-alist
            *bootstrap-sql-startable*
              (remove-if-not
               (lambda (product)
                 (sql-get-product-feature (car product)
                                          :sqli-program))
               sql-product-alist)
            sql-pop-to-buffer-after-send-region nil)

      (defun bootstrap::sql-source (products)
        "Return a source for helm selection"
        `((name . "SQL Products")
          (candidates . ,(mapcar
                          (lambda (product)
                            (cons (sql-get-product-feature (car product)
                                                           :name)
                                  (car product)))
                          products))
          (action . (lambda (candidate)
                      (helm-marked-candidates)))))

      (defun bootstrap:sql-highlight ()
        "Set SQL dialect-specific highlighting."
        (interactive)
        (let ((product (car
                        (helm :sources
                              (list (bootstrap::sql-source
                                     *bootstrap-sql-highlight-table*))))))
          (sql-set-product product)))

      (defun bootstrap:sql-start ()
        "Set SQL dialect-specific highlighting and start an SQLi process."
        (interactive)
        (let ((product (car (helm
                             :sources
                             (list (bootstrap::sql-source
                                    *bootstrap-sql-startable*))))))
          (sql-set-product product)
          (sql-product-interactive product)))

      (defun bootstrap:sql-send-string-and-focus ()
        "Sent the buffer to SQLi and switch to the buffer."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (call-interactively 'sql-send-string)))

      (defun bootstrap:sql-send-buffer-and-focus ()
        "Sent the buffer to SQLi and switch to the buffer."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-buffer)))

      (defun bootstrap:sql-send-paragraph-and-focus ()
        "Send the paragraph to SQLi and switch to the buffer."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-paragraph)))

      (defun bootstrap:sql-send-region-and-focus ()
        "Send the region to SQLi and switch to the buffer."
        (interactive "r")
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-region start end)))

      (add-hook 'sql-interactive-mode-hook
                (lambda ()
                  (toggle-truncate-lines t))))))

(defun sql:init-sql-indent ()
  (use-package sql-indent
    :defer t))

;;; packages.el ends here.
