;;; zmacs-early.el --- Load early  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 13:04:02
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

(use-package cl-lib
  :ensure nil
  :demand t)
(require 'cl-lib)

(use-package macroexp
  :ensure nil
  :demand t)
(require 'macroexp)

(use-package subr-x
  :ensure nil
  :demand t)
(require 'subr-x)

(use-package async
  :defer
  :commands (dired-async-mode
             dired-async--modeline-mode)
  :config
  (progn
    (dired-async-mode 1)
    (setq dired-async--modeline-mode nil)))

(use-package dash
  :defer 2)

(use-package s
  :defer 2)

(use-package f
  :defer 2)


(provide 'zmacs-early)

;;; zmacs-early.el ends here.
