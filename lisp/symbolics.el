;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; symbolics.el --- Symbolics keyboard
;;;
;;; Time-stamp: <Monday Jan 30, 2012 03:42:10 asmodai>
;;; Revision:   65
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

(defvar !symbolics-debug! nil)

;;;==================================================================
;;;{{{ Symbolics input decode map:

(defvar *symbolics-input-decode-map* (make-sparse-keymap)
  "Key definitions for Symbolics lisp machine keyboards.")

(when !symbolics-debug!
  (setq *symbolics-input-decode-map* (make-sparse-keymap))
  
  (define-key *symbolics-input-decode-map* [f1]  [help])
  (define-key *symbolics-input-decode-map* [f2]  [super])
  (define-key *symbolics-input-decode-map* [f3]  [hyper])
  (define-key *symbolics-input-decode-map* [f4]  [select])
  (define-key *symbolics-input-decode-map* [f5]  [refresh])
  (define-key *symbolics-input-decode-map* [f6]  [function])
  (define-key *symbolics-input-decode-map* [f7]  [symbol])
  (define-key *symbolics-input-decode-map* [f8]  [page])
  (define-key *symbolics-input-decode-map* [f9]  [line])
  (define-key *symbolics-input-decode-map* [f10] [clear])
  (define-key *symbolics-input-decode-map* [f11] [complete])
  (define-key *symbolics-input-decode-map* [f12] [network]))

(unless !symbolics-debug!
  (define-key *symbolics-input-decode-map* [f1]  [help])
  (define-key *symbolics-input-decode-map* [f2]  [square])
  (define-key *symbolics-input-decode-map* [f3]  [circle])
  (define-key *symbolics-input-decode-map* [f4]  [triangle])
  (define-key *symbolics-input-decode-map* [f5]  [refresh])
  (define-key *symbolics-input-decode-map* [f6]  [function])
  (define-key *symbolics-input-decode-map* [f7]  [suspend])
  (define-key *symbolics-input-decode-map* [f8]  [abort])
  (define-key *symbolics-input-decode-map* [f9]  [resume])
  (define-key *symbolics-input-decode-map* [f10] [clear])
  (define-key *symbolics-input-decode-map* [f11] [complete])
  (define-key *symbolics-input-decode-map* [f12] [network])
  (define-key *symbolics-input-decode-map* [f13] [local])
  (define-key *symbolics-input-decode-map* [f14] [select])
  (define-key *symbolics-input-decode-map* [f15] [page])
  (define-key *symbolics-input-decode-map* [f16] [line])
  (define-key *symbolics-input-decode-map* [f17] [symbol])
  (define-key *symbolics-input-decode-map* [f18] [symbol])
  (define-key *symbolics-input-decode-map* [f19] [repeat])
  (define-key *symbolics-input-decode-map* [f20] [hyper])
  (define-key *symbolics-input-decode-map* [f21] [super])
  (define-key *symbolics-input-decode-map* [f22] [super])
  (define-key *symbolics-input-decode-map* [f23] [hyper]))

(let ((m (copy-keymap *symbolics-input-decode-map*)))
  (set-keymap-parent m (keymap-parent input-decode-map))
  (set-keymap-parent input-decode-map m))

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

(defun display-symbolics-keyboard-mapping ()
  (interactive)
  (with-help-window (help-buffer)
    (princ (format "Mapping for the Symbolics keyboard:\n\n"))
    (princ (format "  PC\t   Symbolics\n  --\t   ---------\n"))
    (mapcar (lambda (x)
              (princ (format "  %s\t-> %s\n"
                             (car x)
                             (cdr x))))
            (reverse (cdr *symbolics-input-decode-map*)))))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Simple mapping:

;;;
;;; Clear existing function key mappings.
(let ((keys '(f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16
              f17 f18 f19 f20 f21 f22 f23 f24 square circle triangle
              refresh function suspend abort resume complete network
              local select page line symbol repeat)))
  (dolist (key keys)
    (global-unset-key (vector key))))

;;;
;;; Define both `Hyper' and `Super' keys.
(define-key function-key-map [super] 'event-apply-hyper-modifier)
(define-key function-key-map [hyper] 'event-apply-super-modifier)

;;;
;;; Define `Suspend' so it does the same as C-z (or C-x C-z).
(global-set-key [(suspend)] 'suspend-frame)

;;;
;;; Define `Abort' so it does the same as C-g.
(global-set-key [(abort)] 'keyboard-quit)

;;;
;;; Map the `Complete' key to whatever completion function we want to
;;; use.
(global-set-key [(complete-in-turn)] 'hippie-expand)

;;;
;;; The `Page' key will insert a pagebreak character (^L).
(global-set-key [(page)] 'insert-emacs-page-break)
;;;
;;; With Zmacs, the `Line' key will perform an operation similar to
;;; `NEWLINE-AND-INDENT'.
(global-set-key [(line)] 'newline-and-indent)

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
(global-set-key [function] +symbolics-function-map+)

;;;
;;; Display our bindings.
(defun display-function-bindings ()
  "Emacs version of `Function-Help'."
  (interactive)
  (describe-bindings [function]))

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
(global-set-key [select] +symbolics-select-map+)

;;;
;;; Display our bindings.
(defun display-select-bindings ()
  "Emacs version of `Select-Help'."
  (interactive)
  (describe-bindings [select]))

;;;
;;; Define a new `Select' operation using this.
(defmacro define-select-key (key fname)
  "Bind a `Select-' KEY combination to FNAME."
  `(define-key +symbolics-select-map+ ,key ,fname))

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
;;; Prefix key.
(global-set-key [symbol] +symbolics-symbol-map+)

;;;
;;; Display our bindings.
(defun display-symbol-bindings (&optional buffer)
  "Emacs version of `Symbol-Help'."
  (interactive)
  (describe-bindings [symbol]))

;;;
;;; Define a new symbol key.
(defmacro define-symbol-key (key symbol charname)
  "Define a key sequence that results with KEY, when pressed with the
  `Symbol' key, prints the given unicode SYMBOL to the buffer."
  (let* ((docstr (concat "Inserts a(n) " charname " at the current point."))
 (str (if (symbolp symbol)
  (symbol-name symbol)
  symbol))
 (mname (intern (concat "ucs-insert-"
(replace-regexp-in-string
 "\\([[:space:]\n]\\)"
 "-"
 str)))))
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
(define-function-key [refresh] 'redraw-display)
(define-function-key "q" 'print-buffer)

;;;
;;; Select key definitions:
(define-select-key "e" 'revisit-scratch)
(define-select-key "i" 'ielm)
(define-select-key "t" 'eshell)

;;;
;;; There will probably be more definitions in site-lisp-mode.
;;;

;;;}}}
;;;==================================================================

(provide 'symbolics)

;;; symbolics.el ends here
