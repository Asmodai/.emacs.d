;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Syntax checking packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 20:49:04
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY  WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

(setq syntax-checking-packages
      '(flycheck
        flycheck-pos-tip
        (redspace-mode :location local)
        popwin))

(defun syntax-checking:init-flycheck ()
  (use-package flycheck
    :defer t
    :init
    (progn
      (setq flycheck-standard-error-navigation nil))
    :config
    (progn
      (bootstrap:diminish flycheck-mode  " ⓢ" " s")

      (defun bootstrap:defface-flycheck-mode-line-color (state)
        (let* ((fname (intern (format "*bootstrap-mode-line-flycheck-%s-face*"
                                      (symbol-name state))))
               (foreground (face-foreground
                            (intern (format "flycheck-fringe-%s" state)))))
          (eval `(defface ,fname '((t ()))
                   ,(format "Colour for Flycheck %s feedback in mode line."
                            (symbol-name state))
                   :group 'bootstrap))
          (set-face-attribute fname nil
                              :foreground foreground
                              :box (face-attribute 'mode-line :box))))

      (defun bootstrap:set-flycheck-mode-line-faces ()
        (mapcar 'bootstrap:defface-flycheck-mode-line-color
                '(error warning info)))
      (bootstrap:set-flycheck-mode-line-faces)

      (defmacro bootstrap:custom-flycheck-lighter (error)
        `(let* ((error-counts (flycheck-count-errors
                               flycheck-current-errors))
                (errorp (flycheck-has-current-errors-p ',error))
                (err (or (cdr (assq ',error error-counts))
                         "?"))
                (running (eq flycheck-last-status-change 'running)))
           (if (or errorp running)
               (format "•%s " err))))

      (when (fboundp 'define-fringe-bitmap)
        (define-fringe-bitmap 'my-flycheck-fringe-indicator
          (vector #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00011100
                  #b00111110
                  #b00111110
                  #b00111110
                  #b00011100
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b01111111)))

      (flycheck-define-error-level 'error
                                   :overlay-category 'flycheck-error-overlay
                                   :fringe-bitmap 'my-flycheck-fringe-indicator
                                   :fringe-face 'flycheck-fringe-error)

      (flycheck-define-error-level 'warning
                                   :overlay-category 'flycheck-warning-overlay
                                   :fringe-bitmap 'my-flycheck-fringe-indicator
                                   :fringe-face 'flycheck-fringe-warning)

      (flycheck-define-error-level 'info
                                   :overlay-category 'flycheck-info-overlay
                                   :fringe-bitmap 'my-flycheck-fringe-indicator
                                   :fringe-face 'flycheck-fringe-info)

      ;; toggle flycheck window
      (defun bootstrap:toggle-flycheck-error-list ()
        "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
        (interactive)
        (-if-let (window (flycheck-get-error-list-window))
            (quit-window nil window)
          (flycheck-list-errors))))))

(defun syntax-checking:init-flycheck-pos-tip ()
  (use-package flycheck-pos-tip
    :if *syntax-checking-enable-tooltips*
    :defer t
    :init
    (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

(defun syntax-checking:init-redspace-mode ()
  (require 'redspace-mode))

(defun syntax-checking:post-init-popwin ()
  (push '("^\*Flycheck.+\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config))

;;; packages.el ends here.
