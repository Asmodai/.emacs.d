;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; keys.el --- Custom key bindings
;;;
;;; Time-stamp: <Tuesday Jan 24, 2012 12:52:58 asmodai>
;;; Revision:   10
;;;
;;; Copyright (c) 2005-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Mon Jun 06 00:40:01 2005
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
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
;;; }}}
;;; {{{ Commentary:
;;;
;;; }}}

;;; ==================================================================
;;; {{{ Key bindings that work in all versions of Emacs:

(global-set-key "\C-xw" 'what-line)

(defun insert-date-string (&optional time-value)
  (interactive)
  (let ((tstr (if emacs=18-p
                  (current-time-string)
                  (current-time-string time-value))))
    (insert time-string)
    time-string))

(global-set-key "\C-cd" 'insert-date-string)

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Key bindings for Emacs 19 and above:

(when emacs>=19-p
  (global-set-key [(f2)] 'undo)
  (global-set-key [(f3)] 'find-file)

  ;; The following fail miserably on NeXTSTEP
  (when (not nextstep-p)
    (global-set-key [(control x) (up)]
                    (lambda ()
                      (interactive)
                      (shrink-window 1)))
    (global-set-key [(control x) (down)]
                    (lambda ()
                      (interactive)
                      (shrink-window 2)))
    (global-set-key [(control x) (left)]
                    (lambda ()
                      (interactive)
                      (shrink-window-horizontally -1)))
    (global-set-key [(control x) (right)]
                    (lambda ()
                      (interactive)
                      (shrink-window-horizontally 1)))

    (defun match-paren (arg)
      "Go to the matching parenthesis if the current point is on a
      parenthesis."
      (interactive "P")
      (cond
       ((looking-at "[([{]")
        (forward-sexp 1))
       ((looking-at "[)]}]")
        (forward-char)
        (backward-sexp 1))))
    (global-set-key "\C-xp" 'match-paren))

  ;; If ibuffer is loaded, replace the buffer menu key binding.
  (if (featurep 'ibuffer)
      (global-set-key "\C-x\C-b" 'ibuffer)))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Key bindings for Emacs 21 and above:

(when (or xemacs>=21-p
          emacs>=21-p)

  ;; Translations for numeric keypads
  (define-key key-translation-map [kp-divide]     [?/])
  (define-key key-translation-map [kp-multiply]   [?*])
  (define-key key-translation-map [kp-subtract]   [?-])
  (define-key key-translation-map [kp-add]        [?+])
  (define-key key-translation-map [kp-enter]     [?\r])
  (define-key key-translation-map [kp-decimal]    [?.])
  (define-key key-translation-map [kp-begin]   [begin])
  (define-key key-translation-map [kp-home]     [home])
  (define-key key-translation-map [kp-end]       [end])
  (define-key key-translation-map [kp-next]     [next])
  (define-key key-translation-map [kp-prior]   [prior])
  (define-key key-translation-map [kp-left]     [left])
  (define-key key-translation-map [kp-right]   [right])
  (define-key key-translation-map [kp-up]         [up])
  (define-key key-translation-map [kp-down]     [down])
  (define-key key-translation-map [kp-insert] [insert])
  (define-key key-translation-map [kp-delete] [delete])
  (define-key key-translation-map [kp-0]          [?0])
  (define-key key-translation-map [kp-1]          [?1])
  (define-key key-translation-map [kp-2]          [?2])
  (define-key key-translation-map [kp-3]          [?3])
  (define-key key-translation-map [kp-4]          [?4])
  (define-key key-translation-map [kp-5]          [?5])
  (define-key key-translation-map [kp-6]          [?6])
  (define-key key-translation-map [kp-7]          [?7])
  (define-key key-translation-map [kp-8]          [?8])
  (define-key key-translation-map [kp-9]          [?9])

  ;; Mouse wheel support
  (when (featurep 'mwheel)
    (global-set-key [(button4)] 'mwheel-scroll)
    (global-set-key [(button5)] 'mwheel-scroll)
    (global-set-key [(shift button4)] 'mwheel-scroll)
    (global-set-key [(shift button4)] 'mwheel-scroll))

  ;; Euro support for keyboards that might lack a euro key.
  (global-set-key "\C-ce"
                  (lambda ()
                    (interactive)
                    (ucs-insert #x20ac)))

  ;; Windows keys
  (when windows-p
    (setq w32-pass-lwindow-to-system nil
          w32-pass-rwindow-to-system nil
          w32-lwindow-modifier 'super
          w32-rwindow-modifier 'hyper))

  ;;
  ;; Special keys on my Acer laptop
  (when running-on-yorktown-p
    (global-set-key [(meta kp-0)        ; This is just stupid really...
                     (meta kp-1)        ; What were Acer thinking?
                     (meta kp-2)
                     (meta kp-8)]
                    (lambda ()
                      (interactive)
                      (ucs-insert #x20ac)))
    (global-set-key [(meta kp-0)
                     (meta kp-0)
                     (meta kp-3)
                     (meta kp-6)]
                    (lambda ()
                      (interactive)
                      (insert "$")))))

;;; }}}
;;; ==================================================================

;;; ==================================================================
;;; {{{ Special cases:

;;;
;;; OS/2 seems to have something against |.  It uses a broken bar
;;; glyph instead, which usually results in various compilers and
;;; interpreters just pointing and laughing at me.
(when (and emx-p
           presentation-manager-p)
  (global-set-key "\C-c1"
                  (lambda ()
                    (interactive)
                    (insert "|"))))

;;; }}}
;;; ==================================================================

;;; keys.el ends here
