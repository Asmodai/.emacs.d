;;; zmacs-prog-js.el --- JavaScript packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    28 Oct 2024 10:21:34
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

;;;; Vue package:

(use-package vue-mode
  :defer t
  :ensure t
  :custom
  (js-indent-level 2))

;;;; TypeScript:

(use-package typescript-ts-mode
  :ensure nil
  :hook (tsx-ts-mode . sgml-electric-tag-pair-mode))

;;;; Tree-Splitter:

(add-to-list 'major-mode-remap-alist '(js2-mode        . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))

;;;; Provide package:

(provide 'zmacs-prog-js)

;;; zmacs-prog-js.el ends here.
