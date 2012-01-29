;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; symbolics.el --- Symbolics keyboard
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 17:33:27 asmodai>
;;; Revision:   59
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
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
  (require 'cl))

;;;==================================================================
;;;{{{ Variables:

(defvar +symbolics-help-key+ 'f1
  "The key code for the Symbolics `Help' key.")

(defvar +symbolics-function-key+ 'f6
  "The key code for the Symbolics `Function' key.")

;;; NOTE: F12 for debugging
(defvar +symbolics-select-key+ 'f12
  "The key code for the Symbolics `Select' key.")

;;; NOTE: F2 for debugging
(defvar +symbolics-left-symbol-key+ 'f2
  "The key code for the Symbolics left-hand `Symbol' key.")

;;; NOTE: F3 for debugging
(defvar +symbolics-right-symbol-key+ 'f3
  "The key code for the Symbolics right-hand `Symbol' key.")

(defvar +symbolics-refresh-key+ 'f5
  "The key code for the Symbolics `Refresh' key.")

(defvar +symbolics-complete-key+ 'f11
  "The key code for the Symbolics `Complete' key.")

;;;}}}
;;;==================================================================

;;; ==================================================================
;;;{{{ Utility functions:

(defun insert-emacs-page-break ()
  "Inserts a page break character."
  (interactive)
  (insert ?\f)
  (newline-and-indent))

(defun revisit-scratch ()
  "Visit (or revisit if closed) the *scratch* buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun symbolise-charname (str)
  "Takes a string and replaces all space characters with a `-'."
  (let ((s (if (symbolp str)
               (symbol-name str)
               str)))
    (replace-regexp-in-string "\\([[:space:]\n]\\)" "-" s)))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Simple mapping:

;;;
;;; Remove existing mappings for F2 through F24.
;;;
;;; We keep F1 so things don't require heaps of work to have a working
;;; `help' key.
(let ((keys '(f2  f3  f4  f5  f6  f7  f8  f9  f10 f11 f12
              f13 f14 f15 f16 f17 f18 f19 f20 f21 f22 f23 f24)))
  (dolist (key keys)
    (global-unset-key (vector key))))

;;;
;;; Define both `Hyper' and `Super' keys.
(define-key function-key-map [f20] 'event-apply-hyper-modifier)
(define-key function-key-map [f21] 'event-apply-super-modifier)
(define-key function-key-map [f22] 'event-apply-super-modifier)
(define-key function-key-map [f23] 'event-apply-hyper-modifier)

;;;
;;; Define `Suspend' so it does the same as C-z (or C-x C-z).
(global-set-key [(f7)] 'suspend-frame)

;;;
;;; Define `Abort' so it does the same as C-g.
(global-set-key [(f8)] 'keyboard-quit)

;;;
;;; Map the `Complete' key to whatever completion function we want to
;;; use.
(global-set-key [(f11)] 'hippie-expand)

;;;
;;; The `Page' key will insert a pagebreak character (^L).
(global-set-key [(f15)] 'insert-emacs-page-break)
;;;
;;; With Zmacs, the `Line' key will perform an operation similar to
;;; `NEWLINE-AND-INDENT'.
(global-set-key [(f16)] 'newline-and-indent)

;;;}}}
;;; ==================================================================

;;;==================================================================
;;;{{{ Symbolics keys:

;;;------------------------------------------------------------------
;;;{{{ Function key:

;;;
;;; Prefix command.
(define-prefix-command '+symbolics-function-map+)

;;;
;;; Prefix key.
(global-set-key (vector +symbolics-function-key+)
                +symbolics-function-map+)

;;;
;;; Display our bindings.
(defun display-function-bindings ()
  "Emacs version of `Function-Help'."
  (interactive)
  (describe-bindings (vector +symbolics-function-key+)))

;;;
;;; Define a new `Function' operation using this.
(defmacro define-function-key (key fname)
  "Define a `Function' key binding to function FNAME bound on KEY." 
  `(define-key +symbolics-function-map+ ,key ,fname))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Select key:

;;;
;;; Prefix command.
(define-prefix-command '+symbolics-select-map+)

;;;
;;; Prefix key.
(global-set-key (vector +symbolics-select-key+)
                +symbolics-select-map+)

;;;
;;; Display our bindings.
(defun display-select-bindings ()
  "Emacs version of `Select-Help'."
  (interactive)
  (describe-bindings (vector +symbolics-select-key+)))

;;;
;;; Define a new `Select' operation using this.
(defmacro define-select-key (key fname)
  "Bind a `Select-' KEY combination to FNAME."
  `(define-key +symbolics-select-map+ ,key ,fname))

;;;
;;; Initial Symbolics-like map
(when (fboundp 'define-select-key)
  (define-select-key "e" 'revisit-scratch)
  (define-select-key "t" 'eshell))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Symbol keys:

;;; The left and right symbol key modify the next keypress to display
;;; a symbol, if the key is mapped to display one.
;;;
;;; The mappings are as follows:
;;;
;;; Glyph  Code   Unicode Name                             Sequence
;;; -----  ----   ------------                             --------
;;;  ⋅     22C5   DOT OPERATOR                             Symbol-'
;;;  α     03B1   GREEK SMALL LETTER ALPHA                 Symbol-A
;;;  ∧     2227   LOGICAL AND                              Symbol-q
;;;  ε     03B5   GREEK SMALL LETTER EPSILON               Symbol-E
;;;  λ     03BB   GREEK SMALL LETTER LAMBDA                Symbol-L
;;;  δ     03B4   GREEK SMALL LETTER DELTA                 Symbol-D
;;;  ±     00B1   PLUS-MINUS SIGN                          Symbol-:
;;;  ∞     221E   INFINITY                                 Symbol-i
;;;  ⊂     2282   SUBSET OF                                Symobl-t
;;;  ∩     2229   INTERSECTION                             Symbol-e
;;;  ∀     2200   FOR ALL                                  Symbol-u
;;;  ⊗     2297   CIRCLED TIMES                            Symbol-*
;;;  ←     2190   LEFTWARDS ARROW                          Symbol-j
;;;  ≠     2260   NOT EQUAL TO                             Symbol-equal
;;;  ≤     2264   LESS-THAN OR EQUAL TO                    Symbol-,
;;;  ≡     2261   IDENTICAL TO                             Symbol-`
;;;  ∫     222B   INTEGRAL                                 Symbol-/
;;;  ↓     2193   DOWNWARDS ARROW                          Symbol-h
;;;  β     03B2   GREEK SMALL LETTER BETA                  Symbol-B
;;;  ¬     00AC   NOT SIGN                                 Symbol-minus
;;;  π     03C0   GREEK SMALL LETTER PI                    Symbol-P
;;;  γ     03B3   GREEK SMALL LETTER GAMMA                 Symbol-G
;;;  ↑     2191   UPWARDS ARROW                            Symbol-g
;;;  ⊕     2295   CIRCLED PLUS                             Symbol-plus
;;;  ∂     2202   PARTIAL DIFFERENTIAL                     Symbol-p
;;;  ⊃     2283   SUPERSET OF                              Symbol-y
;;;  ∪     222A   UNION                                    Symbol-r
;;;  ∃     2203   THERE EXISTS                             Symbol-o
;;;  ⇄     21C4   RIGHTWARDS ARROW OVER LEFTWARDS ARROW    Symbol-l
;;;  →     2192   RIGHTWARDS ARROW                         Symbol-k
;;;  ◊     25CA   LOZENGE                                  Symbol-escape
;;;  ≥     2265   GREATER-THAN OR EQUAL TO                 Symbol-.
;;;  ∨     2228   LOGICAL OR                               Symbol-w
;;; 

;;;
;;; Prefix command.
(define-prefix-command '+symbolics-symbol-map+)

;;;
;;; Prefix key, left.
(global-set-key
 (vector +symbolics-left-symbol-key+) +symbolics-symbol-map+)

;;;
;;; Prefix key, right.
(global-set-key
 (vector +symbolics-right-symbol-key+) +symbolics-symbol-map+)

;;;
;;; Display our bindings.
(defun display-symbol-bindings (&optional buffer)
  "Emacs version of `Symbol-Help'."
  (interactive)
  (describe-bindings (vector +symbolics-symbol-map+)))

;;;
;;; Define a new symbol key.
(defmacro define-symbol-key (key symbol charname)
  "Define a key sequence that results with KEY, when pressed with the
  `Symbol' key, prints the given unicode SYMBOL to the buffer."
  (let ((docstr (concat "Inserts a(n) " charname " at the current point."))
        (mname (intern (concat "ucs-insert-"
                               (symbolise-charname charname)))))
    `(progn
       (defun ,mname (&rest ignore)
         ,docstr
         (interactive "P")
         (ucs-insert ,symbol))
       (define-key +symbolics-symbol-map+ ,key ',mname))))

;;;
;;; Emacs-Lisp indentation.
(put 'define-symbol-key 'lisp-indent-function 1)

;;;
;;; Symbolics Genera `Symbol' key mappings:
(when (fboundp 'define-symbol-key)
  (define-symbol-key "'" "22C5" "dot operator")
  (define-symbol-key "A" "03B1" "Greek small letter alpha")
  (define-symbol-key "q" "2227" "logical AND")
  (define-symbol-key "E" "03B5" "Greek small letter epsilon")
  (define-symbol-key "L" "03BB" "Greek small letter lambda")
  (define-symbol-key "D" "03B4" "Greek small letter delta")
  (define-symbol-key ":" "00B1" "plus-minus sign")
  (define-symbol-key "i" "221E" "infinity")
  (define-symbol-key "t" "2282" "subset of")
  (define-symbol-key "e" "2229" "intersection")
  (define-symbol-key "u" "2200" "for all")
  (define-symbol-key "*" "2297" "circled times")
  (define-symbol-key "j" "2190" "leftwards arrow")
  (define-symbol-key "=" "2260" "not equal to")
  (define-symbol-key "," "2264" "less-than or equal to")
  (define-symbol-key "`" "2261" "identical to")
  (define-symbol-key "/" "222B" "integral")
  (define-symbol-key "h" "2193" "downwards arrow")
  (define-symbol-key "B" "03B2" "Greek small letta beta")
  (define-symbol-key "-" "00AC" "not")
  (define-symbol-key "P" "03C0" "Greek small letter pi")
  (define-symbol-key "G" "03B3" "Greek small letter gamma")
  (define-symbol-key "g" "2191" "upwards arrow")
  (define-symbol-key "+" "2295" "circled plus")
  (define-symbol-key "p" "2202" "partial differential")
  (define-symbol-key "y" "2283" "superset of")
  (define-symbol-key "r" "222A" "union")
  (define-symbol-key "o" "2203" "there exists")
  (define-symbol-key "l" "21C4" "rightwards arrow over leftwards arrow")
  (define-symbol-key "k" "2192" "rightwards arrow")
  (define-symbol-key [escape] "25CA" "lozenge")
  (define-symbol-key "." "2265" "greater-than or equal to")
  (define-symbol-key "w" "2228" "logical OR"))

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Key definitions:

;;;
;;; Function key definitions:
(define-function-key "r" 'redraw-display)
(define-function-key (vector +symbolics-refresh-key+) 'redraw-display)
(define-function-key "q" 'print-buffer)

;;;
;;; Select key definitions:
(define-select-key "e" 'revisit-scratch)
(define-select-key "t" 'eshell)

;;;
;;; There will probably be more definitions in site-lisp-mode.
;;;

;;;}}}
;;;==================================================================

(provide 'symbolics)

;;; symbolics.el ends here
