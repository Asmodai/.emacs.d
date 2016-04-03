;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Racket packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    12 Mar 2016 05:01:23
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

(setq racket-packages
      '((company
         company-quickhelp
         racket-mode)))

(defun racket:post-init-company ()
  (add-hook 'racket-mode-hook 'company-mode))

(defun racket:post-init-company-quickhelp ()
  (add-hook 'company-mode-hook
            '(lambda ()
               (when (and (equal major-mode 'racket-mode)
                          (bound-and-true-p company-quickhelp-mode))
                 (company-quickhelp-mode -1)))
            t))

(defun racket:init-racket-mode ()
  (use-package racket-mode
    :defer t
    :config
    (progn
      (with-eval-after-load 'smartparens
        (add-to-list 'sp--lisp-modes 'racket-mode)
        (when (fboundp 'sp-local-pair)
          (sp-local-pair 'racket-mode "'" nil :actions nil)
          (sp-local-pair 'racket-mode "`" nil :actions nil)))

      (defun bootstrap:racket-test-with-coverage ()
        "Call `racket-test' with universal argument."
        (interactive)
        (racket-test t))

      (defun bootstrap:racket-send-last-sexp-focus ()
        "Call `raclet-send-last-sexp' and switch to REPL buffer."
        (interactive)
        (racket-send-last-sexp)
        (racket-repl))

      (defun bootstrap:racket-send-definition-focus ()
        "Call `racket-send-definition' and switch to the REPL buffer."
        (interactive)
        (racket-send-definition)
        (racket-repl))

      (defun bootstrap:racket-send-region-focus (start end)
        "Call `racket-send-region' and switch to the REPL buffer."
        (interactive "r")
        (repl-send-region start end)
        (racket-repl))

      (define-key racket-mode-map (kbd "H-r") 'racket-run)
      (define-key racket-mode-map ")" 'self-insert-command)
      (define-key racket-mode-map "]" 'self-insert-command)
      (define-key racket-mode-map "}" 'self-insert-command))))

;;; packages.el ends here.
