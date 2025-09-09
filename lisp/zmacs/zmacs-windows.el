;;; zmacs-windows.el --- Window-related packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 14:54:52
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

;;;; Window setup:

(use-package window
  :ensure nil
  :custom
  (display-buffer-base-action nil))

;;;; Window division:

(use-package frame
  :ensure nil
  :custom
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))

(add-hook 'before-make-frame-hook 'window-divider-mode)

;;;; Window movement:
;;;;; Ace:

(use-package ace-window
  :commands (ace-window
             ace-swap-window
             aw-flip-window))

;;;;; Windmove:

(use-package windmove
  :ensure nil
  :commands (windmove-up
             windmove-down
             windmove-left
             windmove-right)
  :bind (("C-c C-h"       . #'windmove-left)
         ("C-c C-l"       . #'windmove-right)
         ("C-c C-j"       . #'windmove-down)
         ("C-c C-k"       . #'windmove-up)
         ("C-c C-<left>"  . #'windmove-left)
         ("C-c C-<right>" . #'windmove-right)
         ("C-c C-<down>"  . #'windmove-down)
         ("C-c C-<up>"    . #'windmove-up))
  :config
  (windmove-default-keybindings))

;; TODO: move this to zlisp.
(defun zmacs-other-window ()
  "Move to another window."
  (interactive)
  (other-window 1))

(bind-key* "C-c C-o" 'zmacs-other-window)

;;;; Window restore:

(use-package winner
  :ensure nil
  :commands (winner-mode)
  :hook (after-init . winner-mode))

;;;; Provide package:

(provide 'zmacs-windows)

;;; zmacs-windows.el ends here.
