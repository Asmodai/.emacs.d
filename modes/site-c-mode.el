;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-c-mode.el --- C mode hacks.
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 00:40:53 asmodai>
;;; Revision:   6
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    16 Feb 2011 07:24:29
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

;;; Comment editing.
(when emacs>=21-p
  (require 'c-comment-edit)
  (if (featurep 'c-comment-edit)
      (setq c-comment-leader "  ")))

;;; C-family autoload support
(when (and emacs>=21-p
           (or running-on-yorktown-p
               running-on-lisp-machine-p)) 
  (autoload 'csharp-mode "csharp-mode"
    "Major mode for editing C# code." t)
  (autoload 'lsl-mode "lsl-mode"
    "Major mode for editing LSL code." t)

  (setq auto-mode-alist
        (append '(("\\.cs$" . csharp-mode)
                  ("\\.lsl$" . lsl-mode))
                auto-mode-alist)))

;;;
;;; ctags
(setq path-to-ctags "C:/Program Files/Emacs-23.2/bin/ctags")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s"
           path-to-ctags
           dir-name
           (directory-file-name dir-name))))

;;;
;;; The rest of this file is meant for Emacs >= 19.
;;;

(when emacs>=19-p
  ;; We quite like `cc-mode'.
  (require 'cc-mode)

  ;; Now, set up a custom indentation style.
  (defconst hackers-c-style
    '((c-tab-always-indent . t)
      (c-comment-only-line-offset . 0)
      (c-basic-offset . 2)
      (c-hanging-braces-alist . ((substatement-open after)
                                 (brace-list-open)))
      (c-hanging-colons-alist . ((member-init-intro before)
                                 (inher-intro)
                                 (case-label after)
                                 (label after)
                                 (access-label after)))
      (c-cleanup-list . (scope-operator
                         empty-defun-braces
                         defun-close-semi))
      (c-offsets-alist . ((statement-block-intro . +)
                          (knr-argdecl-intro . +)
                          (substatement-open . 0)
                          (label . 0)
                          (statement-cont . +)
                          (case-label . 2)))
      (c-echo-syntactic-information-p . t))
    "Custom BSD-derived C syntax style.")

  ;; Add the new style to the available styles.
  (c-add-style "hackers" hackers-c-style)

  ;; Set up the default styles
  (if (or running-on-yorktown-p
          running-on-lisp-machine-p
          running-on-magellan-p)
      (setq-default c-default-style
                    '((c-mode . "hackers")
                      (c++-mode ."hackers")
                      (objc-mode . "hackers")
                      (java-mode . "hackers")
                      (cperl-mode . "hackers")
                      (other . "gnu")
                      (lsl-mode . "hackers")
                      (csharp-mode . "hackers")))
      (setq-default c-default-style
                    '((c-mode . "hackers")
                      (c++-mode ."hackers")
                      (objc-mode . "hackers")
                      (java-mode . "hackers")
                      (other . "gnu"))))

  ;; Add some hooks
  (defun my-common-c-mode-hooks ()
    (when (or emacs=20-p emacs=21-p)
      (turn-on-font-lock)
      (font-lock-mode t))
    (setq indent-tabs-mode nil)
    (auto-fill-mode t))

  (add-hook 'c-mode-common-hook 'my-common-c-mode-hooks))

;;; site-c-mode.el ends here
