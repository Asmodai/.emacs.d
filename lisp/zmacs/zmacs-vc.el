;;; zmacs-vc.el --- Version control packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:14:32
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
(require 'ediff)

;;;; VC:

(use-package vc
  :ensure nil
  :hook (emacs-startup . vc-mode)
  :custom
  (vc-follow-symlinks t)
  (vc-log-short-cycle '(file)))

(use-package vc-git
  :ensure nil
  :after vc
  :config
  (setq vc-git-diff-switches    "--patch-with-stat"
        vc-git-print-log-follow t))

(use-package vc-annotate
  :ensure nil
  :after vc
  :config
  (setq vc-annotate-display-mode 'scale))

;;;; Magit:

(use-package magit
  :commands (magit-blame-mode
             magit-commit
             magit-diff
             magit-log
             magit-status
             magit-run-post-commit-hook)
  :hook (git-commit-mode . turn-on-flyspell)
  :bind ((:map magit-log-mode-map
               ("Q" . #'exit-recursive-edit)))
  :init
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  :config
  (setq magit-log-margin '(t "%Y-%m-%d.%H:%M:%S "
                             magit-log-margin-width
                             nil
                             18)
        magit-refresh-status-buffer t
        magit-diff-refine-hunk t
        magit-section-initial-visibility-alist
        '((stashes           . hide)
          (untrached         . hide)
          (unpushed          . hide)
          ([unpulled status] . show)))
  (global-git-commit-mode t)

  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (advice-add 'magit-set-header-line-format :override #'ignore)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;;; Difference highlighting:

(use-package diff-hl
  :defer nil
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode. diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'left)
  (diff-hl-fringe-bmp-function #'cpm--diff-hl-fringe-bmp-from-type)
  (diff-hl-fringe-face-function #'cpm--diff-hl-fringe-face-from-type)
  (diff-hl-margin-symbols-alist '((insert    . "┃")
                                  (delete    . "┃")
                                  (change    . "┃")
                                  (unknown   . "?")
                                  (ignored   . "i")
                                  (reference . " ")))
  :init
  (defun cpm--diff-hl-fringe-face-from-type (type _pos)
    (intern (format "cpm--dif-hl-%s" type)))

  (defun cpm--diff-hl-fringe-bmp-from-type (type _pos)
    (intern (format "cpm--diff-hl-%s" type)))

  (defun cpm--diff-hl-set-render-mode ()
    (diff-hl-margin-mode (if window-system
                             -1
                           1)))
  :config
  (diff-hl-margin-mode 1)
  (define-fringe-bitmap 'diff-hl-insert
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-change
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-delete
    [#b00000011] nil nil '(center repeated)))

;;;; Diff files with Vdiff:

(use-package vdiff-magit
  :defer t
  :init
  (with-eval-after-load 'magit
    (define-key magit-mode-map "e" #'vdiff-magit-dwim)
    (define-key magit-mode-map "E" #'vdiff-magit)
    (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
    (transient-suffix-put 'magit-dispatch "e" :command #'vdiff-magit-dwim)
    (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
    (transient-suffix-put 'magit-dispatch "E" :command #'vdiff-magit)))

(setq ediff-window-setup-function #'ediff-setup-windows-plain)

;;;; Provide package:

(provide 'zmacs-vc)

;;; zmacs-vc.el ends here.
