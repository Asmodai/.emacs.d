;;; zlisp-vtkeyboard-setup.el --- VT keyboard setup.  -*- mode: emacs-lisp; lexical-binding: t; -*-
 ;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    29 Nov 2025 03:34:32
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

(eval-when-compile
  (require 'cl-lib)
  (require 'zlisp-features)
  (require 'zlisp-vtkeyboard-goldmap))

(defun zlisp-vtkeyboard//setup ()
  "Set up a VT-style keyboard for use in TTY Emacs.

By `VT-style', we mean DEC `LK' keyboards such as the `LK401'."
  (when (and (getenv "VT_KEYBOARD")
             (not (zlisp/feature-p :vt-keyboard)))
    (progn
      (zlisp/add-feature :vt-keyboard)
      ;; On some DEC keyboards, PF1 is known as the "gold key", because VMS
      ;; applications often used it as a modifier for action shortcuts.
      (define-key input-decode-map "\eOP" [gold])

      ;; Set up decode map entries for PF1, PF2, and PF3.
      (zlisp-vtkeyboard//setup-other-pf-keys)

      ;; Set up decode map entries for the editing keypad.


      ;; Set up the key mapping for the `gold' key.
      (zlisp-vtkeyboard//setup-goldmap))))


(defun zlisp-vtkeyboard//setup-other-pf-keys ()
  "Set up the key bindings for the other `PF' keys."
  (define-key input-decode-map "\eOQ" [pf2])
  (define-key input-decode-map "\eOR" [pf3])
  (define-key input-decode-map "\eOS" [pf4]))

;; This is incomplete.  I need to check all the modifiers on a VT (because of
;; Windows interpreting CUA shortcuts before the app gets them.
(defun zlisp-vtkeyboard//setup-editing-keypad ()
  "Set up the key bindings for the editing keypad."
  (define-key input-decode-map "\e[1~" [find])
  ;; \e[2~ ==> [insert] or [insertchar]
  ;; \e[3~ ==> [delete] or [deletechar]
  (define-key input-decode-map "\e[4~" [select])
  (define-key input-decode-map "\e[5~" [prior]) ; Pixel scrolling can use this.
  (define-key input-decode-map "\e[6~" [next])) ; Pixel scrolling can use this.

(defun zlisp-vtkeyboard//setup-cursor-keypad ()
  "Set up the key bindings for the cursor keypad."
  (define-key input-decode-map "\eOp" [kp-0])
  (define-key input-decode-map "\eOq" [kp-1])
  (define-key input-decode-map "\eOr" [kp-2])
  (define-key input-decode-map "\eOs" [kp-3])
  (define-key input-decode-map "\eOt" [kp-4])
  (define-key input-decode-map "\eOu" [kp-5])
  (define-key input-decode-map "\eOv" [kp-6])
  (define-key input-decode-map "\eOw" [kp-7])
  (define-key input-decode-map "\eOx" [kp-8])
  (define-key input-decode-map "\eOy" [kp-9])
  )




;;(global-set-key [pf2] #'recenter-top-bottom) ; "Page"
;;(global-set-key [pf3] #'backward-kill-word)  ; "Del Word"
;;(global-set-key [pf4] #'delete-char)        ; "Del Char"



;; Add to hook.
(add-hook 'tty-setup-hook #'zlisp-vtkeyboard//setup)

(provide 'zlisp-vtkeyboard-setup)

;;; zlisp-vtkeyboard-setup.el ends here.
