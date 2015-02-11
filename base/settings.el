;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic-docstrings: t -*-
;;;
;;; settings.el --- Various settings.
;;;
;;; Time-stamp: <Saturday Jan 31, 2015 18:41:03 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2013 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    17 Jul 2013 13:57:15
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

;;; Owner information.
(setq user-url-address          "http://lisp.gotgeeks.com/"
      user-full-name            "Paul Ward"
      user-mail-address         "asmodai@gmail.com"
      url-personal-mail-address "asmodai@gmail.com")

;;; Enable `erase-buffer'.
(put 'erase-buffer 'disabled nil)

;;; Configure modeline scrolling.
(setq modeline-scrolling-method 'scrollbar)

;;; Info-specific settings
(setq Info-default-directory-list
      (append '("~/.emacs.d/info/") Info-default-directory-list))
(setq Info-additional-directory-list Info-default-directory-list)

;;; Put mouse selection in the kill buffer.
(when (or (windows-p)
          (x-windows-p)
          (nextstep-p)
          (presentation-manager-p))
  (defun mouse-track-drag-copy-to-kill (event count)
    "Copy the dragged region to the kill ring."
    (let ((region (default-mouse-track-return-dragged-selection
                   event)))
      (when region
        (copy-region-as-kill (car region) (cdr region)))
      nil))
  (add-hook 'mouse-track-drag-up-hook 'mouse-track-drag-copy-to-kill))

;;; Avoid deactivation of a region when the buffer end or beginning is
;;; reached.
(when (or (emacs>=19-p)
          (emacs-version>= 21 4 10))
  (defadvice line-move (around catch-buffer-border-error activate)
    "Catch errors `beginning-of-buffer' and `end-of-buffer' to avoid
deactivation of the region."
    (condition-case ()
         ad-do-it
       ((beginning-of-buffer end-of-buffer)))))

;;; Don't invert colours for passwords.
(when (or (emacs>=20-p)
          (xemacs>=19-p))
  (setq passwd-invert-frame-when-keyboard-grabbed nil))

;;; Change modeline indicators.
(when (emacs>=21-p)
  (setq pending-delete-modeline-string " PD"
        filladept-mode-line-string " Fa")
  (add-minor-mode 'abbrev-mode " Ab"))

;;; Line and column numbers.
(if (and (xemacs-p)
         (emacs-version>= 21 5 6))
    (column-number-mode 1)
    (line-number-mode 1))

;;; Tell abbrev to shut up.
(when (emacs>=19-p)
  (quietly-read-abbrev-file))

;;; Latin unity. (requires XEmacs and Mule)
(when (and (featurep 'mule)
           (xemacs-p))
  (latin-unity-install)
  (add-to-list 'latin-unity-preapproved-coding-system-list
               'iso-8859-1))

;;; Workaround for an encoding bug in XEmacs 21.5.
(when (and (featurep 'mule)
           (xemacs-p)
           (emacs-version>= 21 5))
  (set-coding-priority-list '(iso-8-1
                              iso-8-2
                              iso-7
                              no-conversion
                              iso-8-designate
                              iso-lock-shift
                              shift-jis
                              big5
                              ucs-4
                              utf-8)))

;;; Windowing system hacks
(when (and (not (terminal-p))
           (emacs>=20-p))
  (require 'msb))

;;; Buffer tabs. (XEmacs only)
(when (xemacs>=21-p)
  (customize-set-variable 'gutter-buffer-tabs-visible-p t)
  (customize-set-variable 'gutters-buffers-tab-visible-p t))

;;; Various `missing' features.
(when (emacs>=20-p)
  (eval-and-compile

    ;; `line-beginning-position'. (used by newcomment.el)
    (unless (fboundp 'line-beginning-position)
      (defun line-beginning-position ()
        "Return the point of the beginning of the current line."
        (save-excursion
          (beginning-of-line)
          (point))))

    ;; `line-end-position'. (used by newcomment.el)
    (unless (fboundp 'line-end-position)
      (defun line-end-position ()
        "Return the point of the end of the current line."
        (save-excursion
          (end-of-line)
          (point))))))

;;; Various common settings.
(setq abbrev-mode t                     ; `abbrevs' needs to exist
      complex-buffers-menu-p t
      zmacs-regions t
      buffers-menu-grouping-function
         'group-buffers-menu-by-mode-then-alphabetically
      buffers-menu-sort-function 
         'sort-buffers-menu-by-mode-then-alphabetically
      buffers-menu-submenus-for-groups-p t
      case-fold-search t
      case-replace t
      get-frame-for-buffer-default-instance-limit nil
      mouse-yank-at-point 1
      next-line-adds-newlines nil
      overwrite-mode nil
      require-final-newline t
      teach-extended-commands-p t
      teach-extended-commands-timeout t)

;;; Buffer boundaries and empty lines.
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)

;;; settings.el ends here
