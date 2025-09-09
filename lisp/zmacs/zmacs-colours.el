;;; zmacs-colours.el --- Colour-related packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 15:28:50
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

(require 'cl-lib)
(require 'zlisp-platform)

;;;; Colourspace:

;; Does this work outside of macOS?
(zlisp/when-macos
 (setq-default ns-use-srgb-colorspace t))

;;;; Rainbow mode:

(use-package rainbow-mode
  :defer nil
  :commands rainbow-mode)

;;;; Colorful mode:

(use-package colorful-mode
  :defer nil
  :custom
  (colorful-use-prefix   t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors    nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

;;;; Provide package:

(provide 'zmacs-colours)

;;; zmacs-colours.el ends here.
