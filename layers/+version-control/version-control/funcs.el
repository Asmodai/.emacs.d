;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; funcs.el --- Version control functions.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    12 Mar 2016 04:28:22
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

(defun version-control/next-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     'diff-hl-next-hunk)
       (git-gutter  'git-gutter:next-hunk)
       (git-gutter+ 'git-gutter+-next-hunk)))))

(defun version-control/previous-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     'diff-hl-previous-hunk)
       (git-gutter  'git-gutter:previous-hunk)
       (git-gutter+ 'git-gutter+-previous-hunk)))))

(defun version-control/revert-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     'diff-hl-revert-hunk)
       (git-gutter  'git-gutter:revert-hunk)
       (git-gutter+ 'git-gutter+-revert-hunks)))))

(defun version-control/stage-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     (message "staging not available."))
       (git-gutter  'git-gutter:stage-hunk)
       (git-gutter+ 'git-gutter+-stage-hunks)))))

(defun version-control/show-hunk ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     'diff-hl-diff-goto-hunk)
       (git-gutter  'git-gutter:popup-hunk)
       (git-gutter+ 'git-gutter+-show-hunk-inline-at-point)))))

(defun version-control/enable-margin ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun version-control/disable-margin ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     'diff-hl-mode)
       (git-gutter  'git-gutter-mode)
       (git-gutter+ 'git-gutter+-mode)))))

(defun version-control/enable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun version-control/disable-margin-globally ()
  (interactive)
  (let ((current-prefix-arg nil))
    (call-interactively
     (cl-case *version-control-diff-tool*
       (diff-hl     'global-diff-hl-mode)
       (git-gutter  'global-git-gutter-mode)
       (git-gutter+ 'global-git-gutter+-mode)))))

(defun version-control/show-help ()
  (interactive)
  (setq version-control--ms-doc-toggle
        (logxor version-control--ms-doc-toggle 1)))

(defun version-control/margin-p ()
  (interactive)
  (cl-case *version-control-diff-tool*
    (diff-hl     diff-hl-mode)
    (git-gutter  git-gutter-mode)
    (git-gutter+ git-gutter+-mode)))

(defun version-control/margin-global-p ()
  (interactive)
  (cl-case *version-control-diff-tool*
    (diff-hl     global-diff-hl-mode)
    (git-gutter  global-git-gutter-mode)
    (git-gutter+ global-git-gutter+-mode)))

;;; funcs.el ends here
