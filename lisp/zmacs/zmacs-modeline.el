;;; zmacs-modeline.el --- Modeline-related packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 15:40:01
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

(use-package zmacs-line
  :ensure nil
  :custom
  (display-time-mode t)
  (zmacs-line-abbrev t)
  (zmacs-line-position 'bottom)
  (zmacs-line-hspace "  ")
  (zmacs-line-prefix t)
  (zmacs-line-prefix-padding nil)
  (zmacs-line-status-invert nil)
  (zmacs-line-gui-ro-symbol  " ⨂") ;- ⬤◯⨂
  (zmacs-line-gui-mod-symbol " ⬤") ;; ⨀⬤
  (zmacs-line-gui-rw-symbol  " ◯")  ;; ◉ ◎ ⬤◯
  (zmacs-line-vc-symbol "")
  (zmacs-line-space-top +.50)
  (zmacs-line-space-bottom -.50)
  (zmacs-line-symbol-position 0.1)
  :custom-face
  (zmacs-line-visual-bell ((t (:bcakground "red3"))))
  :config
  (zmacs-line-clockface-update-fontset "ClockFaceRectSolid")
  (zmacs-line-mode)
  (zmacs-line-visual-bell-config)
  (when (eq zmacs-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

(provide 'zmacs-modeline)

;;; zmacs-modeline.el ends here.
