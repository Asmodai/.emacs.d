;;; zmacs-dark-theme.el --- ZMACS dark theme  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    20 Oct 2024 22:18:50
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

(require 'zmacs-themes)

(deftheme zmacs-dark
  "ZMACS Dark theme.")

(zmacs-themes-create 'dark 'zmacs-dark)
(run-hooks 'zmacs-themes-after-load-theme-hook)
(provide-theme 'zmacs-dark)

(provide 'zmacs-dark-theme)

;;; zmacs-dark-theme.el ends here.
