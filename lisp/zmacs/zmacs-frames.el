;;; zmacs-frames.el --- Frame-related packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 14:57:14
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

(require 'cl-lib)
(require 'zlisp-platform)

;;;===================================================================
;;;{{{ Frame defaults:

(use-package frame
  :ensure nil
  :config
  (setq-default default-frame-alist
                (append (list
                         '(frame-title-format    . nil)
                         '(internal-border-width . 0)
                         '(tool-bar-lines        . 0)
                         '(vertical-scroll-bar   . t)
                         '(horizontal-scroll-bar . nil))))

    ;; Resize pixel-wise.

  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise  t)

    ;; Don't show icon in frame.

  (setq-default ns-use-proxy-icon nil))

;;; TODO: move this to ZLISP.
(defun zmacs-frame-recenter (&optional frame)
  "Center FRAME on the screen."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame
     '((user-position . t)
       (top           . 0.5)
       (left          . 0.5)))))

;;; Un/comment this if you want it.
;;(add-hook 'after-make-frame-functions #'zmacs-frame-recenter)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Titlebar hacks for macOS:

(use-package ns-auto-titlebar
  :if (zlisp-macos-p)
  :commands ns-auto-titlebar-mode
  :config (ns-auto-titlebar-mode))

;;;}}}
;;;===================================================================

(provide 'zmacs-frames)

;;; zmacs-frames.el ends here.
