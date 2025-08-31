;;; zmacs-prog-html.el --- HTML packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    28 Oct 2024 10:23:27
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

;;;; Web mode:

(use-package web-mode
  :ensure t
  :defer t)

;;;; SGML mode:

(use-package sgml-mode
  :ensure t
  :defer t)

(defun zmacs-html-close-tag-if-necessary ()
  "Call `sgml-close-tag' if the prior tag is an opening tag."
  (when (looking-back "<\\s-*\\([^</> \t\r\n]+\\)[^</>]*>")
    (let ((tag (match-string 1)))
      (unless (and (not (sgml-unclosed-tag-p tag))
                   (looking-at (concat "\\s-*<\\s-*/\\s-*" tag "\\s-*>")))
        (sgml-close-tag)))))

(defun zmacs-html-insert-gt ()
  "Insert a `>' character."
  (interactive)
  (insert ">")
  (save-excursion
    (zmacs-html-close-tag-if-necessary)))

(define-minor-mode html-editing-mode
  "HTML editing minor mode based on sgml-mode."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map ">" #'zmacs-html-insert-gt)
            map))

(add-hook 'html-mode-hook 'html-editing-mode)

;;;; Tree-Splitter:

(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))

;;;; Provide package:

(provide 'zmacs-prog-html)

;;; zmacs-prog-html.el ends here.
