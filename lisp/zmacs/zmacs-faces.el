;;; zmacs-faces.el --- Face-related packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 15:14:46
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
;; Common Lisp has its own rules regarding comments:
;;    ;;;;   File description comment block -- entirely optional.
;;    ;;;    Top-level comment block -- aligned to column 0.
;;    ;;     Commemt block -- aligned to current code block indentation.
;;    ;      Inline comment block -- aligned to column 50 usually
;;
;; e.g.:
;;
;;          ;;; -*- Mode: Common-Lisp: Syntax: ANSI-Common-Lisp; -*-
;;          ;;;
;;
;;          ;;; Print some foo
;;          (defun print-me-some-foo ()
;;            (let ((foo (random)))               ; Randomness!
;;              ;; Print it out now
;;              (princ foo)))
;;
;; So, all this is a long-winded way to say:  Outline won't be used in Lisp mode
;; on a whole, only Emacs Lisp mode.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;; Outline faces:
;;;;; Outline package:

(use-package outline
  :ensure nil
  :commands (outline-minor-mode)
  :hook ((emacs-lisp-mode       . outline-minor-mode)))

;;;;; Outline minor faces package:

(use-package outline-minor-faces
  :after outline
  :commands (outline-minor-faces-mode)
  :hook (outline-minor-mode . outline-minor-faces-mode))

;;;;; Bicycle package:

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

;;;;; Outli packages:

(use-package outli
  :vc (:fetcher github
                :repo jdtsmith/outli)
  :after outline
  :bind (:map outli-mode-map
              ("C-c C-p" . (lambda ()
                             (interactive)
                             (outline-back-to-heading))))
  :hook ((emacs-lisp-mode . outli-mode)))

;;;; What face?

;; TODO: move to ZLISP
(defun what-face (pos)
  "State the face at the point POS."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face
        (message "Face: %s" face)
      (message "No face at %d" pos))))

;;;; Underlines:

(customize-set-variable 'x-underline-at-descent-line t)

;;;; Cursor:

(customize-set-variable 'cursor-in-non-selected-windows nil)

;;;; Reveal:

(use-package reveal
  :ensure nil
  :defer 1
  :commands (global-reveal-mode)
  :config
  (setq reveal-auto-hide nil)
  (global-reveal-mode))

;;;; SVG:
;;;;; Library:

(use-package svg-lib)

;;;;; SVG tags:

(use-package svg-tag-mode
  :after svg-lib
  :when (image-type-available-p 'svg)
  :commands (svg-tag-make)
  :hook (prog-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(("\\(:[A-Z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag
                                               :face 'success
                                               :inverse t
                                               :beg 1
                                               :end -1))))
          ;; other tags
          ("DONE:"  . ((lambda (tag)
                         (svg-tag-make "DONE:"  :face 'fringe  :inverse t ))))
          ("FIXME:" . ((lambda (tag)
                         (svg-tag-make "FIXME:" :face 'error   :inverse t))))
          ("HACK:"  . ((lambda (tag)
                         (svg-tag-make "HACK:"  :face 'warning :inverse t))))
          ("NOTE:"  . ((lambda (tag)
                         (svg-tag-make "NOTE:"  :face 'warning :inverse t))))
          ("TODO:"  . ((lambda (tag)
                         (svg-tag-make "TODO:"  :face 'warning :inverse t))))
          ("XXX"    . ((lambda (tag)
                         (svg-tag-make "XXX"    :face 'warning :inverse t))))
          ("BUG:"   . ((lambda (tag)
                         (svg-tag-make "BUG:"   :face 'error   :inverse t)))))))

;;;; Emacs widgets:

(use-package wid-ed
  :ensure nil
  :defer 1
  :custom
  (widget-image-enable nil))

;;;; Highlights:
;;;;; Highlight numbers:

(use-package highlight-numbers
  :defer t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;;;;; Highlight todo:

(use-package hl-todo
  :defer t
  :commands (hl-todo-mode
             hl-todo-text
             hl-todo-previous
             hl-todo-occur)
  :init
  (add-hook 'prog-mode-hook     #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

(with-eval-after-load 'hydra
  (defhydra zmacs-hydra-todo
    (:pre
     (hl-todo-mode 1)
     :post
     (hl-todo-mode -1))
    "Todo"
    ("n" h1-todo-text     "Next")
    ("p" h1-todo-previous "Previous")
    ("o" h1-todo-occur    "Occur")
    ("q" nil              "Quit" :color blue :exit t)))

;;;;; Pulse:

(use-package pulse
  :bind ("C-<return>" . pulse-line)
  :commands (pulse-line pulse-momentary-highlight-one-line)
  :config
  (setq pulse-delay 0.08)

  (defun pulse-line (&rest _)
    "Pulse the current line."
    (interactive)
    (pulse-momentary-highlight-one-line (point)))

  (dolist (command '(scroll-up-command
                     scroll-down-command
                     recenter-top-bottom))
    (advice-add command :after #'pulse-line))

  (push #'pulse-line window-selection-change-functions))

;;;;; Goggles:

(use-package goggles
  :hook ((prog-mode . goggles-mode)
         (text-mode . goggles-mode))
  :config
  (setq-default goggles-pulse t))

(setq-default indicate-empty-lines nil)

;;;; Define package:

(provide 'zmacs-faces)

;;; zmacs-faces.el ends here.
