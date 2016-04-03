;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; keybindings.el --- Distribution key bindings.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 06:17:07
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

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on return
(define-key global-map (kbd "RET") 'newline-and-indent)

;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
;; can do the same thing and with fuzzy matching and other features.
(eval-after-load 'dired
  '(define-key dired-mode-map "j" 'bootstrap:helm-find-files))

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; linum margin bindings-------------------------------------------------------
(global-set-key (kbd "<left-margin> <down-mouse-1>")
                'bootstrap:md-select-linum)

(global-set-key (kbd "<left-margin> <mouse-1>")
                'bootstrap:mu-select-linum)

(global-set-key (kbd "<left-margin> <double-mouse-1>")
                'bootstrap:select-current-block)

(global-set-key (kbd "<left-margin> <drag-mouse-1>")
                'bootstrap:mu-select-linum)

(eval-after-load "shell"
  '(progn
    (define-key comint-mode-map [(shift) (up)] 'comint-previous-input)
    (define-key comint-mode-map [(shift) (down)] 'comint-next-input)))

;; ^c-w - What line am I on?
(global-set-key "\C-cw" 'what-line)

;; ^c-d - Insert current date and time.
(global-set-key "\C-cp" 'bootstrap:insert-date-string)

;; Nudge window sizes.
(when (not (nextstep-p))
  ;;
  ;; ^c-<up> - Shrink/Grow window up.
  (global-set-key [(control c) (up)]
                  (lambda ()
                    (interactive)
                    (shrink-window -1)))
  ;;
  ;; ^c-<down> - Shrink/Shrink window down.
  (global-set-key [(control c) (down)]
                  (lambda ()
                    (interactive)
                    (shrink-window 1)))
  ;;
  ;; ^c-<left> - Shrink/Grow window left.
  (global-set-key [(control c) (left)]
                  (lambda ()
                    (interactive)
                    (shrink-window-horizontally -1)))
  ;;
  ;; ^c-<right> - Shrink/grow window left.
  (global-set-key [(control c) (right)]
                  (lambda ()
                    (interactive)
                    (shrink-window-horizontally 1))))

;; ^c-e - Insert a Euro symbol.
(global-set-key "\C-ce"
                (lambda ()
                  (interactive)
                  (ucs-insert #x20ac)))

;; Windows keys
(when (windows-p)
  (setq w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'hyper))

;;; keybindings.el ends here.
