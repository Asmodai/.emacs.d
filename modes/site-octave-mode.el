;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-octave-mode.el --- Support for GNU Octave.
;;;
;;; Time-stamp: <Wednesday Sep  5, 2012 02:52:03 asmodai>
;;; Revision:   7
;;;
;;; Copyright (c) 2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Sep 2012 02:06:36
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

;;;
;;; Configure paths for Octave
(setq inferior-octave-program
      "/Applications/Octave.app/Contents/Resources/bin/octave-3.4.0")

;;;
;;; Set up autoloading.
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;;;
;;; Custom Octave mode hook.
(defun my-octave-mode-hook ()
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (font-lock-mode 1)
  (define-key octave-mode-map "\C-m"
    'octave-reindent-then-newline-and-indent)
  (define-key octave-mode-map [(control tab)]
    'octave-complete-symbol))

;;;
;;; Add our custom hook function to the hook.
(add-hook 'octave-mode-hook 'my-octave-mode-hook)

(defvar +gnu-octave-wants-x11+ nil)

(defun start-macos-x11-server ()
  (unless (eq (process-status "macos-x11") 'run)
    (start-process "macos-x11"
                   nil
                   "/Applications/Utilities/X11.app/Contents/MacOS/X11")))

(defun stop-macos-x11-server ()
  (unless (null (process-status "macos-x11"))
    (kill-process "macos-x11")))

;;;
;;; Custom inferior Octave mode hook.
(defun my-inferior-octave-mode-hook ()
  (when +gnu-octave-wants-x11+
    (start-macos-x11-server)
    (setenv "DISPLAY" "127.0.0.1:0.0"))
  (turn-on-font-lock)
  (define-key inferior-octave-mode-map [up] 'comint-previous-input)
  (define-key inferior-octave-mode-map [down] 'comint-next-input))

;;;
;;; Add our custom hook function to the hook.
(add-hook 'inferior-octave-mode-hook 'my-inferior-octave-mode-hook)

;;; site-octave-mode.el ends here
