;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; symbolics.el --- Symbolics keyboard
;;;
;;; Copyright (c) 2011-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    31 Oct 2011 17:32:09
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation,
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
;;; Symbolics keyboard scan code table
;;;
;;; This is the proposed scancode table for the Symbolics keyboard USB hack.
;;; Where possible it uses keyboard usage values taken from the USB HID
;;; usage tables v1.12.
;;;
;;; +---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+
;;; |Function |Escape   |Refresh  |Square   |Circle   |Triangle |Clear    |Suspend  |Resume   |Abort    |
;;; |(F6)     |◊        |(F5)     |(F2)     |(F3)     |(F4)     |(F10)    |(F7)     |(F9)     |(F8)     |
;;; |3F       |29       |3E       |3B       |3C       |3D       |43       |40       |42       |41       |
;;; +---------+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+---------+
;;; |Network  |:   |1 ! |2 @ |3 # |4 $ |5 % |6 ^ |7 & |8 * |9 ( |0 ) |- _ |= + |` ~ |\ { || } |Help     |
;;; |(F12)    |    |    |    |    |    |    |    |    |    |    |    |¬   |≠ ⊕ |≡   |    |    |(F1)     |
;;; |45       |CB  |1E  |1F  |20  |21  |22  |23  |24  |25  |26  |27  |2D  |2E  |35  |31  |32  |3A       |
;;; +---------+----+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+----+---------+
;;; |Local    |Tab    |q Q |w W |e E |r R |t T |y Y |u U |i I |o O |p P |( [ |) ] |B-S |Page  |Complete |
;;; |(F13)    |       |∧   |∨   |∩ ε |∪   |⊂   |⊃   |∀   |∞   |∃   |∂ π |    |    |    |(F15) |(F11)    |
;;; |(68)     |2B     |14  |1A  |08  |15  |17  |1C  |18  |0C  |12  |13  |2F  |30  |4C  |6A    |44       |
;;; +---------+-------+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+-+--+----+-+----+---------+
;;; |Select   |Rubout   |a A |s S |d D |f F |g G |h H |j J |k K |l L |; : |' " |Return   |Line| End     |
;;; |(F14)    |         |  α |    |  δ |    |↑ γ |↓   |←   |→   |⇄ λ |  ± |⋅   |         |(F16|         |
;;; |69       |2A       |04  |16  |07  |09  |0A  |0B  |0D  |0E  |0F  |33  |34  |28       |6B  |58       |
;;; +----+------+----------+----+----+----+----+----+----+----+----+----+----+------+------+------+-----+
;;; |Caps|Symbol|Shift     |z Z |x X |c C |v V |b B |n N |m M |, < |. > |/ ? |Shift |Symbol|Repeat|Mode |
;;; |    |(F17) |          |    |    |    |    |  β |    |    |≤   |≥   |∫   |      |(F18) |(F19) |     |
;;; |39  |6C    |E1        |1D  |1B  |06  |19  |05  |11  |10  |36  |37  |38  |E5    |6D    |6E    |     |
;;; +----+----+-+--+-------+-+--+----+----+----+----+----+----+----+----+-+--+---+--+-+----+----+-+-----+
;;; |Hpr |Spr |Meta|Control  |Space                                       |Ctrl  |Meta|Spr |Hpr |Scroll |
;;; |(F20|(F21|    |         |                                            |      |    |(F22|(F23|(F24)  |
;;; |6F  |70  |E2  |E0       |2C                                          |E4    |E6  |71  |72  |73     |
;;; +----+----+----+---------+--------------------------------------------+------+----+----+----+-------+
;;;
;;; Clear = Clear-Input
;;; Caps  = Caps Lock
;;; Mode  = Mode Lock
;;; Hpr   = Hyper
;;; Spr   = Super
;;; Ctrl  = Control
;;; B-S   = Back-Space
;;;
;;; Notes:
;;;
;;; With no Emacs hacks, the keyboard produces the following:
;;;
;;;
;;; Key sequences produced by the LOCAL key for the parens et al:
;;;       ( - B6              = (
;;; shift ( - 2F (unshifted)  = [
;;;       ) - B7              = )
;;; shift ) - 30 (unshifted)  = ]
;;;       \ - 31 (unshifted)  = \
;;; shift \ - 2F (shifted)    = {
;;;       | - 31 (shifted)    = |
;;; shift | - 30 (shifted)    = }
;;;
;;; The `Function' key is bound to the `menu' key.
;;;
;;; The `Local' key will trigger F13 with a single press.  The true
;;; functionality of the key is implemented in the firmware and
;;; outside the control of Emacs.
;;;
;;; The `Clear-Input' key could probably be mapped to some sort of
;;; delete function.
;;;
;;; The `Square', `Triangle', and `Circle' keys are left unbound so
;;; that they might be used elsewhere.
;;;
;;; The `Suspend', `Abort', and `Resume' keys could be used by Emacs
;;; processes.
;;;
;;; The `Network' key could probably be mapped to telnet or IRC stuff.
;;;
;;; TODO: Find out if it's possible to create a key symbols for
;;; square, triangle, circle, select, etc.
;;;
;;;}}}

(eval-when-compile
  (require 'cl)
  (require 'cl-lib)
  (require 'help+)
  (require 'help-fns+))

(defgroup symbolics nil
  "Symbolics customisations."
  :group 'convenience
  :prefix 'symbolics-)

(defcustom *symbolics-debug* nil
  "Non-NIL if debugging is required.")

(defvar *symbolics-map* (make-sparse-keymap)
  "Symbolics key map.")

(defun symbolics-display-keyboard-mapping ()
  (interactive)
  (with-help-window (help-buffer)
    (princ (format (concat "Symbolics keyboard mapping:\n\n"
                           "  Key   \tMapping\n"
                           "  ------\t-------------------------\n")))
    (mapcar (lambda (x)
              (princ (format "  %6s\t%s\n"
                             (car x)
                             (cdr x))))
            (reverse (cdr *symbolics-map*)))
    nil))

(defun symbolics-display-function-bindings ()
  (interactive)
  (describe-bindings [function]))

(defun symbolics-describe-function-keymap ()
  (interactive)
  (describe-keymap *symbolics-function-map*))

(defun symbolics-display-select-bindings ()
  (interactive)
  (describe-bindings [select]))

(defun symbolics-describe-select-keymap ()
  (interactive)
  (describe-keymap *symbolics-select-map*))

(defmacro symbolics-define-function-key (key fun)
  `(define-key *symbolics-function-map* ,key ,fun))

(defmacro symbolics-define-select-key (key fun)
  `(define-key *symbolics-select-map* ,key ,fun))

;; Define a new symbol key.
(defmacro define-symbol-key (key symbol charname)
  "Define a key sequence that results with KEY, when pressed with the
`symbol' key, prints the given unicode SYMBOL to the buffer."
  (declare (indent 1))
  (let* ((docstr (concat "Inserts a(n) " charname " at the current point."))
         (str (if (symbolp symbol)
                  (symbol-name symbol)
                symbol))
         (mname (intern (concat "ucs-insert-"
                                (replace-regexp-in-string
                                 "\\([[:space:]\n]\\)"
                                 "-"
                                 (downcase charname))))))
    `(progn
       (defun ,mname (&rest ignore)
         ,docstr
         (interactive "P")
         (ucs-insert ,symbol))
       (define-key *symbolics-symbol-map* ,key ',mname))))

(defun symbolics-display-symbol-bindings ()
  (interactive)
  (describe-bindings [symbol]))

(defun symbolics-describe-symbol-keymap ()
  (interactive)
  (describe-keymap *symbolics-symbol-map*))

(defun symbolics::install-prefix-maps ()
  (define-prefix-command '*symbolics-hyper-map*)
  (define-prefix-command '*symbolics-function-map*)
  (define-prefix-command '*symbolics-select-map*)
  (define-prefix-command '*symbolics-square-map*)
  (define-prefix-command '*symbolics-circle-map*)
  (define-prefix-command '*symbolics-triangle-map*)
  (define-prefix-command '*symbolics-symbol-map*))

(defun symbolics::install-fkey-mapping ()
  (define-key *symbolics-map* (kbd "<f1>")    [select])   ; meta
  (define-key *symbolics-map* (kbd "<f2>")    [function]) ; meta
  (define-key *symbolics-map* (kbd "<f3>")    [symbol])   ; meta
  (define-key *symbolics-map* (kbd "<f4>")    [hyper])    ; meta
  (define-key *symbolics-map* (kbd "<f5>")    [square])   ; meta
  (define-key *symbolics-map* (kbd "<f6>")    [circle])   ; meta
  (define-key *symbolics-map* (kbd "<f7>")    [triangle]) ; meta
  (define-key *symbolics-map* (kbd "<f8>")    [cut])
  (define-key *symbolics-map* (kbd "<f9>")    [copy])
  (define-key *symbolics-map* (kbd "<f10>")   [paste])
  (define-key *symbolics-map* (kbd "<f11>")   [help])
  (define-key *symbolics-map* (kbd "<f12>")   [complete])
  
  (define-key *symbolics-map* (kbd "S-<f1>")  [abort])
  (define-key *symbolics-map* (kbd "S-<f2>")  [undo])
  (define-key *symbolics-map* (kbd "S-<f3>")  [redo])
  ;;(define-key *symbolics-map* (kbd "S-<f4>")  [])
  ;;(define-key *symbolics-map* (kbd "S-<f5>")  [])
  ;;(define-key *symbolics-map* (kbd "S-<f6>")  [])
  (define-key *symbolics-map* (kbd "S-<f7>")  [find])
  (define-key *symbolics-map* (kbd "S-<f8>")  [insert])
  (define-key *symbolics-map* (kbd "S-<f9>")  [home])
  (define-key *symbolics-map* (kbd "S-<f10>") [end])
  (define-key *symbolics-map* (kbd "S-<f11>") [symbol-help])
  (define-key *symbolics-map* (kbd "S-<f12>") [clear-input]))

(defun symbolics::install-keys ()
  (define-key function-key-map [super] 'event-apply-super-modifier)
  (define-key function-key-map [hyper] 'event-apply-hyper-modifier)
  (global-set-key [copy]        'kill-ring-save)
  (global-set-key [cut]         'kill-ring)
  (global-set-key [paste]       'yank)
  (global-set-key [abort]       'keyboard-quit)
  (global-set-key [clear-input] 'backward-kill-sentence))

(defun symbolics::install-meta-maps ()
  (global-set-key [hyper]    *symbolics-hyper-map*)
  (global-set-key [function] *symbolics-function-map*)
  (global-set-key [select]   *symbolics-select-map*)
  (global-set-key [square]   *symbolics-square-map*)
  (global-set-key [circle]   *symbolics-circle-map*)
  (global-set-key [triangle] *symbolics-triangle-map*)
  (global-set-key [symbol]   *symbolics-symbol-map*))

(defun symbolics::install-function-map ()
  (symbolics-define-function-key "e"    'eval-defun)
  (symbolics-define-function-key "r"    'redraw-display)
  (symbolics-define-function-key "="    'symbolics-display-function-bindings)
  (symbolics-define-function-key [help] 'symbolics-describe-function-keymap))

(defun symbolics::install-select-map ()
  (symbolics-define-select-key "l"    'ielm)
  (symbolics-define-select-key "s"    'eshell)
  (symbolics-define-select-key "="    'symbolics-display-select-bindings)
  (symbolics-define-select-key [help] 'symbolics-describe-select-keymap))


(defun symbolics::install-symbol-map ()
  (global-set-key [symbol-help] 'symbolics-display-symbol-bindings)
  (define-key *symbolics-symbol-map* [help] 'symbolics-describe-symbol-keymap)
  (define-symbol-key "'" #x22C5 "dot operator")
  (define-symbol-key "A" #x03B1 "Greek small letter alpha")
  (define-symbol-key "q" #x2227 "logical AND")
  (define-symbol-key "E" #x03B5 "Greek small letter epsilon")
  (define-symbol-key "L" #x03BB "Greek small letter lambda")
  (define-symbol-key "D" #x03B4 "Greek small letter delta")
  (define-symbol-key ":" #x00B1 "plus-minus sign")
  (define-symbol-key "i" #x221E "infinity")
  (define-symbol-key "t" #x2282 "subset of")
  (define-symbol-key "e" #x2229 "intersection")
  (define-symbol-key "u" #x2200 "for all")
  (define-symbol-key "*" #x2297 "circled times")
  (define-symbol-key "j" #x2190 "leftwards arrow")
  (define-symbol-key "=" #x2260 "not equal to")
  (define-symbol-key "," #x2264 "less-than or equal to")
  (define-symbol-key "`" #x2261 "identical to")
  (define-symbol-key "/" #x222B "integral")
  (define-symbol-key "h" #x2193 "downwards arrow")
  (define-symbol-key "B" #x03B2 "Greek small letta beta")
  (define-symbol-key "-" #x00AC "not")
  (define-symbol-key "P" #x03C0 "Greek small letter pi")
  (define-symbol-key "G" #x03B3 "Greek small letter gamma")
  (define-symbol-key "g" #x2191 "upwards arrow")
  (define-symbol-key "+" #x2295 "circled plus")
  (define-symbol-key "p" #x2202 "partial differential")
  (define-symbol-key "y" #x2283 "superset of")
  (define-symbol-key "r" #x222A "union")
  (define-symbol-key "o" #x2203 "there exists")
  (define-symbol-key "l" #x21C4 "rightwards arrow over leftwards arrow")
  (define-symbol-key "k" #x2192 "rightwards arrow")
  (define-symbol-key [escape] #x25CA "lozenge")
  (define-symbol-key "." #x2265 "greater-than or equal to")
  (define-symbol-key "w" #x2228 "logical OR"))

(defun symbolics:install-keymap ()
  (symbolics::install-fkey-mapping)
  (let ((map (copy-keymap *symbolics-map*)))
    (set-keymap-parent map (keymap-parent key-translation-map))
    (set-keymap-parent key-translation-map map))
  (symbolics::install-prefix-maps)
  (symbolics::install-keys)
  (symbolics::install-meta-maps)
  (symbolics::install-function-map)
  (symbolics::install-select-map)
  (symbolics::install-symbol-map))

(provide 'symbolics)

;;; symbolics.el ends here
