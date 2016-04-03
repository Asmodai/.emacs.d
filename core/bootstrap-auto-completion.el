;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; bootstrap-auto-completion.el --- Core auto-completion.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 06:17:07
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

(require 'bootstrap-buffer)

(defmacro bootstrap:defvar-company-backends (mode)
   (bootstrap-buffer:message (format "Creading company backend for %s." mode))
  `(defvar ,(intern (format "*company-backends-%S*" mode))
     '((company-dabbrev-code company-gtags company-etags company-keywords)
       company-files company-dabbrev)
     ,(format "Company backend list for %S." mode)))

(defmacro bootstrap:add-company-hook (mode)
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "bootsrap:init-company-%S" mode)))
        (backend-list (intern (format "*company-backends-%S*" mode))))
    `(when (bootstrap-layer:package-used-p 'company)
       (defun ,func ()
         ,(format "Initialise company for %S." mode)
         (when *auto-completion-enable-snippets-in-popup*
           (setq ,backend-list (mapcar 'boostrap::show-snippets-in-company
                                       ,backend-list)))
         (set (make-variable-buffer-local 'auto-completion-front-end)
                'company)
         (set (make-variable-buffer-local 'company-backends)
                ,backend-list))
       (add-hook ',mode-hook ',func t)
       (add-hook ',mode-hook 'company-mode t))))

(defmacro bootstrap:disable-company (mode)
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "bootstrap::init-company-%S" mode))))
    `(progn
       (remove-hook ',mode-hook ',func)
       (remove-hook ',mode-hook 'company-mode))))

(defun bootstrap::show-snippets-in-company (backend)
  (if (or (not *auto-completion-enable-snippets-in-popup*)
          (and (listp backend)
               (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend)
                backend
              (list backend))
            '(:with company-yasnippet))))

(defmacro bootstrap:enable-auto-complete (mode)
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "bootstrap::init-auto-complete-%S" mode))))
    `(when (bootstrap-layer:package-used-p 'auto-complete)
       (defun ,func ()
         ,(format "Initialise auto-complete for %S." mode)
         (set (make-variable-buffer-local 'auto-completion-front-end)
                'auto-complete)
         (set (make-variable-buffer-local 'company-backends)
                ,(intern (format "*company-backends-%S*" mode))))
       (add-hook ',mode-hook ',func)
       (add-hook ',mode-hook 'auto-complete-mode))))

(provide 'bootstrap-auto-completion)

;;; bootstrap-auto-completion.el ends here
