;;; zmacs-templates.el --- Template package  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:30:22
;; URL:        not distributed yet
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY  WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;

;;; Code:
;;;; Requirements:

(eval-when-compile
  (require 'cl-lib))

;;;; File templates:

;; `template.el' should live somewhere in the path.
(require 'template)
(template-initialize)

(let ((dir (expand-file-name (concat user-emacs-directory "templates/"))))
  (setf template-default-directories (list dir)
        template-subdirectories      (list dir)))

;;;; Tempel

(use-package tempel
  :defer nil
  :ensure t
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert))
  :custom
  (tempel-path (concat user-emacs-directory "tempel"))
  :init
  (defun zmacs--tempel-setup-capf ()
    (setq-local completion-at-point-functions
                  (cons #'tempel-expand
                        completion-at-point-functions)))

  (add-hook 'conf-mode-hook #'zmacs--tempel-setup-capf)
  (add-hook 'prog-mode-hook #'zmacs--tempel-setup-capf)
  (add-hook 'text-mode-hook #'zmacs--tempel-setup-capf))

(use-package tempel-collection)

(provide 'zmacs-templates)

;;; zmacs-templates.el ends here.
