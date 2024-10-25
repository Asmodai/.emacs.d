;;; zlisp-platform-windows.el --- Windows-specific stuff  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    23 Oct 2024 17:31:38
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

(setq-default tramp-use-ssh-controlmaster-options nil
              w32-pass-lwindow-to-system          nil
              w32-pass-rwindow-to-system          nil
              w32-lwindow-modifier                'super
              w32-rwindow-modifier                'super
              w32-apps-modifier                   'hyper)

(when (fboundp 'w32-register-hot-key)
  (w32-register-hot-key [s-]))

(provide 'zlisp-platform-windows)

;;; zlisp-platform-windows.el ends here.
