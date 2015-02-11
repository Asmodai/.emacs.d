;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic-docstrings: t -*-
;;;
;;; bindings.el --- Generic key bindings.
;;;
;;; Time-stamp: <Saturday Jan 31, 2015 18:39:28 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2013 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    17 Jul 2013 14:12:41
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

;;;==================================================================
;;;{{{ Functions:

(defun insert-date-string (&optional time-value)
  "Insert the current date/time at the cursor."
  (interactive)
  (let ((time-string (if (emacs=18-p)
                         (current-time-string)
                         (current-time-string time-value))))
    (insert time-string)
    time-string))

;;;}}}
;;;==================================================================

;;;
;;; ^c-w - What line am I on?
(global-set-key "\C-cw" 'what-line)

;;;
;;; ^c-d - Insert current date and time.
(global-set-key "\C-cp" 'insert-date-string)

;;;
;;; Nudge window sizes.
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

;;;
;;; Keys specirfic to Emacs 21 and above.
(when (or (xemacs>=21-p)
          (emacs>=21-p))

  ;;
  ;; ^c-e - Insert a Euro symbol.
  (global-set-key "\C-ce"
                  (lambda ()
                    (interactive)
                    (ucs-insert #x20ac)))
  
  ;;
  ;; Windows keys
  (when (windows-p)
    (setq w32-pass-lwindow-to-system nil
          w32-pass-rwindow-to-system nil
          w32-lwindow-modifier 'super
          w32-rwindow-modifier 'hyper))
  
  ;;
  ;; MacOS X keys
  (when (mac-os-x-p)
    (global-unset-key (kbd "M-3"))      ; Unbind M-3 first.
    (global-set-key (kbd "M-3")         ; Bind so the UK keyboard can
                    (lambda ()          ; generate the hash symbol.
                      (interactive)
                      (insert "#")))))

;;;
;;; OS/2 seems to have something against |.
(when (and (emx-p)
           (presentation-manager-p))
  (global-set-key "\C-c1"
                  (lambda ()
                    (interactive)
                    (insert "|"))))

;;; bindings.el ends here
