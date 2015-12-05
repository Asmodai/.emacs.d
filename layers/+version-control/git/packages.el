;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Git packages.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2015 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    05 Dec 2015 15:44:46
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; Licenseas published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program isdistributed in the hope that it will be
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

(setq git-packages '(gitattributes-mode
                     gitconfig-mode
                     gitignore-mode
                     git-commit
                     git-messenger
                     git-timemachine
                     helm-gitignore
                     magit
                     magit-gitflow
                     smeargle))

(defun git:init-helm-gitignore ()
  (use-package helm-gitignore
    :defer t))

(defun git:init-git-commit ()
  (use-package git-commit
    :defer t))

(defun git:init-git-messenger ()
  (use-package git-messenger
    :defer t))

(defun git:init-git-timemachine ()
  (use-package git-timemachine
    :defer t))

(defun git:init-gitattributes-mode ()
  (use-package gitattributes-mode
    :defer t))

(defun git:init-gitconfig-mode ()
  (use-package gitconfig-mode
    :defer t))

(defun git:init-gitignore-mode ()
  (use-package gitignore-mode
    :defer t))

(defun git:init-magit ()
  (use-package magit
    :commands (magit-status
               magit-blame-mode
               magit-log
               magit-commit)
    :init
    (progn
      (setq magit-completing-read-function 'magit-builtin-completing-read)
      (add-hook 'git-commit-mode-hook 'fci-mode)

      (when (eq window-system 'w32)
        (setenv "GIT_ASKPASS" "git-gui--askpass"))

      (defun bootstrap:magit-diff-head ()
        (interactive)
        (magit-diff "HEAD")))
    :config
    (progn
      (require 'git-rebase)

      (when *git-magit-status-fullscreen*
        (setq magit-restore-window-configuration t)
        (setq magit-status-buffer-switch-function
              (lambda (buffer)
                (pop-to-buffer buffer)
                (delete-other-windows))))

      (defun magit-toggle-whitespace ()
        (interactive)
        (if (member "-w"
                    (if (derived-mode-p 'magit-diff-mode)
                        magit-refresh-args
                      magit-diff-section-arguments))
            (magit-dont-ignore-whitespace)
          (magit-ignore-whitespace)))

      (defun magit-ignore-whitespace ()
        (interactive)
        (add-to-list (if (derived-mode-p 'magit-diff-mode)
                         'magit-refresh-args
                       'magit-diff-section-arguments)
                     "-w")
        (magit-refresh))

      (defun magit-dont-ignore-whitespace ()
        (interactive)
        (setq magit-diff-options
              (remove "-w"
                      (if (derived-mode-p 'magit-diff-mode)
                          magit-refresh-args
                        magit-diff-section-arguments)))
        (magit-refresh))

      (define-key magit-status-mode-map (kbd "C-S-w")
        'magit-toggle-whitespace))))

(defun git:init-magit-gitflow ()
  (use-package magit-gitflow
    :commands turn-on-magit-gitflow
    :init (progn
            (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

            (eval-after-load 'magit
              '(progn
                 (define-key magit-mode-map "#f" 'magit-gitflow-popup))))
    :config (bootstrap:diminish magit-gitflow-mode "Flow")))

(defun git:init-smeargle ()
  (use-package smeargle
    :defer t))

;;; packages.el ends here
