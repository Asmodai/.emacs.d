;;; zmacs-search.el --- Search packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:14:00
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

;;;; Deadgrep:

;; XXX revisit this
(use-package deadgrep)

;;;; Ripgrep:

(use-package rg
  :commands rg)

;;;; XRef:

(use-package xref
  :ensure nil
  :defer 1)

;;;; Visual Regexp:

(use-package visual-regexp
  :commands (vr/query-replace)
  :config
  (use-package visual-regexp-steroids
    :commands (vr/select-query-replace)))

;;;; Swiper:

(use-package swiper
  :ensure t
  :config
  (global-unset-key (kbd "C-s"))
  (global-set-key (kbd "C-s") #'swiper-isearch))

;;;; Provide package:

(provide 'zmacs-search)

;;; zmacs-search.el ends here.
