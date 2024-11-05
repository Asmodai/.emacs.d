;;; zmacs-prog-ruby.el --- Ruby packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    05 Nov 2024 09:08:39
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

(eval-when-compile
  (require 'cl-lib))

;;;; Enhanced Ruby Mode:

(use-package enh-ruby-mode
  :ensure t
  :defer t
  :mode (("\\.rb$"      . enh-ruby-mode)
         ("\\.rake$"    . enh-ruby-mode)
         ("Rakefile$"   . enh-ruby-mode)
         ("\\.gemspec$" . enh-ruby-mode)
         ("\\.ru$"      . enh-ruby-mode)
         ("Gemfile$"    . enh-ruby-mode)
         ("Capfile$"    . enh-ruby-mode)
         ("\\.thor$"    . enh-ruby-mode))
  :custom
  (enh-ruby-bounce-deep-indent         t)
  (enh-ruby-hanging-brace-indent-level 2)
  (enh-ruby-check-syntax               nil)
  :config
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))

;;;; Robe

(use-package robe
  :ensure t
  :after enh-ruby-mode
  :demand t
  :hook ((ruby-mode         . robe-mode)
         (ruby-ts-mode-hook . robe-mode)))

;;;; Provide package:

(provide 'zmacs-prog-ruby)

;;; zmacs-prog-ruby.el ends here.
