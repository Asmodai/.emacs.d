;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Version control packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Dec 2015 16:03:37
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

(setq version-control-packages '(diff-mode
                                 diff-hl
                                 git-gutter
                                 git-gutter+
                                 git-gutter-fringe
                                 git-gutter-fringe+))

(defun version-control:init-diff-mode ()
  (use-package diff-mode
    :defer t))

(defun version-control:init-diff-hl ()
  (use-package diff-hl
    :init
    (progn
      (setq diff-hl-side 'right)

      (when (eq *version-control-diff-tool* 'diff-hl)
        (when *version-control-global-margin*
          (global-diff-hl-mode))

        (unless (display-graphic-p)
          (setq diff-hl-side 'left)
          (diff-hl-margin-mode))))))

(defun version-control:init-git-gutter ()
  (use-package git-gutter
    :commands global-git-gutter-mode
    :init
    (progn
      (when (and (eq *version-control-diff-tool* 'git-gutter)
                 *version-control-global-margin*)
        (global-git-gutter-mode t))

      (git-gutter:linum-setup)

      (setq git-gutter:update-interval    2
            git-gutter:modified-sign      " "
            git-gutter:added-sign         "+"
            git-gutter:deleted-sign       "-"
            git-gutter:diff-option        "-w"
            git-gutter:hide-gutter        t
            git-gutter:ask-p              nil
            git-gutter:verbosity          0
            git-gutter:handled-backends   '(git hg bzr svn)))
    :config (bootstrap:hide-lighter git-gutter-mode)))

(defun version-control:init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :commands git-gutter-mode
    :init
    (progn
      (when (display-graphic-p)
        (with-eval-after-load 'git-gutter
          (require 'git-gutter-fringe)))
      (setq git-gutter-fr:side 'right-fringe))
    :config
    (progn
      ;; custom graphics that works nice with half-width fringes
      (fringe-helper-define 'git-gutter-fr:added nil
                            "..X...."
                            "..X...."
                            "XXXXX.."
                            "..X...."
                            "..X....")
      (fringe-helper-define 'git-gutter-fr:deleted nil
                            "......."
                            "......."
                            "XXXXX.."
                            "......."
                            ".......")
      (fringe-helper-define 'git-gutter-fr:modified nil
                            "..X...."
                            ".XXX..."
                            "XX.XX.."
                            ".XXX..."
                            "..X...."))))

(defun version-control:init-git-gutter+ ()
  (use-package git-gutter+
    :commands global-git-gutter+-mode
    :init
    (progn
      (when (and (eq *version-control-diff-tool* 'git-gutter+)
                 *version-control-global-margin*)
        (add-hook 'magit-pre-refresh-hook 'git-gutter+-refresh)
        (global-git-gutter+-mode t))

      (setq git-gutter+-modified-sign   " "
            git-gutter+-added-sign      "+"
            git-gutter+-deleted-sign    "-"
            git-gutter+-diff-option     "-w"
            git-gutter+-hide-gutter     t))
    :config (bootstrap:hide-lighter git-gutter+-mode)))

(defun version-control:init-git-gutter-fringe+ ()
  (use-package git-gutter-fringe+
    :commands git-gutter+-mode
    :init
    (progn
      (when (display-graphic-p)
        (with-eval-after-load 'git-gutter+
          (require 'git-gutter-fringe+)))
      (setq git-gutter-fr+-side 'right-fringe))
    :config
    (progn
      ;; custom graphics that works nice with half-width fringes
      (fringe-helper-define 'git-gutter-fr+-added nil
                            "..X...."
                            "..X...."
                            "XXXXX.."
                            "..X...."
                            "..X....")
      (fringe-helper-define 'git-gutter-fr+-deleted nil
                            "......."
                            "......."
                            "XXXXX.."
                            "......."
                            ".......")
      (fringe-helper-define 'git-gutter-fr+-modified nil
                            "..X...."
                            ".XXX..."
                            "XX.XX.."
                            ".XXX..."
                            "..X...."))))

;;; packages.el ends here
