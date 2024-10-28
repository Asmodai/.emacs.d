;;; zmacs-buffers.el --- Buffer-related packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 15:04:56
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

;;;; Hooks:

;; TODO: Move this to ZLISP.

(defvar zmacs-switch-buffer-hook nil
  "A list of hooks to run after changing the current buffer.")

(defun zmacs-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'zmacs-switch-buffer-hook)))

(add-hook 'window-buffer-change-functions #'zmacs-run-switch-buffer-hooks-h)
(add-hook 'server-visit-hook              #'zmacs-run-switch-buffer-hooks-h)

;;;; Scrolling:

(use-package emacs
  :ensure nil
  :config
  (setq auto-window-vscroll          nil
        scroll-step                  1
        scroll-margin                3
        scroll-conservatively        101
        scroll-up-aggressively       0.01
        scroll-down-aggressively     0.01
        fast-but-imprecise-scrolling nil
        hscroll-step                 1
        hscroll-margin               1))

(use-package mwheel
  :ensure nil
  :config
  (setq mouse-wheel-follow-mouse      't
        mouse-wheel-progressive-speed nil
        mwheel-coalesce-scroll-events t
        mouse-wheel-scroll-amount     '(1 ((shift) . 2))))

(use-package pixel-scroll-mode
  :ensure nil
  :disabled)

(use-package mouse
  :ensure nil
  :config
  (setq mouse-autoselect-window nil
        focus-follows-mouse     nil
        context-menu-functions  '(context-menu-ffap
                                  occur-context-menu
                                  context-meun-region
                                  context-menu-undo
                                  context-menu-dictionary)))

;;;; Buffer switching point preservation:

(setq switch-to-buffer-preserve-window-point t)

;;;; Uniquify:

(use-package uniquify
  :ensure nil
  :defer 3
  :config
  (setq uniquify-buffer-name-style   'reverse
        uniquify-separator           " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re   "^\\*"))

;;;; Buffer modes:

(setq-default major-mode (lambda ()
                           (if buffer-file-name
                               (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))

;;;; Autorevert:

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-interval 0.5)
  (global-auto-revert-non-file-buffers t)
  :config
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
  (add-to-list 'global-auto-revert-ignore-modes 'dired-mode))

;;;; Revert buffers:

(use-package revert-buffer-all
  :commands (revert-buffer-all))

;;;; Popper:

(use-package popper
  :hook (after-init . popper-mode)
  :bind (("M-`"   . popper-mode)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-window-height 20)
  (popper-display-control t)
  (popper-display-function #'popper-select-popup-at-top)
  (popper-group-function #'popper-group-by-directory)
  (popper-reference-buffers '("\\*Messages\\*"
                              "Output\\*$"
                              "\\*Async Shell Command\\*"
                              help-mode
                              compilation-mode))
  :config
  (defun popper-select-popup-at-top (buffer &optional _alist)
    "Display and switch to popup-buffer BUFFER at the top of the screen."
    (let ((window (popper-display-popup-at-top buffer)))
      (select-window window)))

  (defun popper-display-popup-at-top (buffer &optional _alist)
    "Display popup-buffer BUFFER at the top of the screen."
    (display-buffer-in-side-window
     buffer
     `((window-height . ,popper-window-height)
       (side . top)
       (slot . 1)))))

;;;; Xwidget browser:

(use-package xwidget
  :ensure nil
  :defer 1
  :config
  (remove-hook 'kill-buffer-query-functions
               #'xwidget-kill-bfufer-query-function)

  (defun xwidget-webkit-estimated-load-progress (session)
    1.0))

;;;; xwwp:

(use-package xwwp-follow-link
  :ensure nil
  :commands (xwwp-follow-link)
  :custom
  (xwwp-follow-link-completion-system 'default)
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwwp-follow-link)))

;;;; Fringe:

(use-package fringe
  :ensure nil
  :custom
  (fringe-mode '(8 . 8)))

;;;; Provide package:

(provide 'zmacs-buffers)

;;; zmacs-buffers.el ends here.
