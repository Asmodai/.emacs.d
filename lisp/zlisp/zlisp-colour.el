;;; zlisp-colour.el --- Colour hacks.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    28 Aug 2025 09:27:07
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

;;; Code:
;;;; Requirements:

(eval-when-compile
  (require 'cl-lib)
  (require 'zlisp-math))

;;;; Custom variables:

(defgroup zlisp-colour nil
  "Variables used for colour functions in ZLISP."
  :group 'zlisp)

(defcustom zlisp-grey-chroma-threshold 0.03
  "Anything with an OKLCh chroma value below this is treated as neutral.

That is, it is considered to have no hue."
  :type 'number
  :group 'zlisp-colour)

(defcustom zlisp-palette-dL-bright +0.08
  "ΔL for computation of `bright' colours."
  :type 'number
  :group 'zlisp-colour)

(defcustom zlisp-palette-dL-dim -0.12
 "ΔL for computation of `dim' colours."
 :type 'number
 :group 'zlisp-colour)

(defcustom zlisp-palette-kC-bright 1.06
  "×C for computation of `bright' colours."
  :type 'number
  :group 'zlisp-colour)

(defcustom zlisp-palette-kC-dim 0.86
  "×C for computation of `dim' colours."
  :type 'number
  :group 'zlisp-colour)

(defcustom zlisp-palette-anchors '((red     . 20)
                                   (orange  . 50)
                                   (yellow  . 90)
                                   (green   . 150)
                                   (cyan    . 200)
                                   (blue    . 240)
                                   (violet  . 280)
                                   (magenta . 320))
  "Canonical OKLCh hue anchors (degrees) used to derive named colours."
  :type '(alist :key-type symbol :value-type number)
  :group 'zlisp-colour)

;;;; Conversion:

(defun zlisp/hex->srgb01 (hex)
  "Return sRGB (r g b) in 0..1 from HEX.

HEX may be a string in the form of \"#RRGGBB\", \"RRGGBB\", or \"#RGB\"."
  (when (string-prefix-p "#" hex)
    (setq hex (substring hex 1)))
  (when (= (length hex) 3)
    (setq hex (apply #'concat (mapcar (lambda (c) (make-string 2 c)) hex))))
  (let* ((r (/ (string-to-number (substring hex 0 2) 16) 255.0))
         (g (/ (string-to-number (substring hex 2 4) 16) 255.0))
         (b (/ (string-to-number (substring hex 4 6) 16) 255.0)))
    (list r g b)))

(defsubst zlisp/srgb01->hex (red green blue)
  "Convert sRGB 0..1 values in RED, GREEN, and BLUE to hexadecimal."
  (format "#%02X%02X%02X"
          (round (* 255 (zlisp/clamp red   0.0 1.0)))
          (round (* 255 (zlisp/clamp green 0.0 1.0)))
          (round (* 255 (zlisp/clamp blue  0.0 1.0)))))

(defsubst zlisp/srgb->linear (channel)
  "Convert sRGB channel CHANNEL (0..1, gamma-encoded) into linear-light sRGB."
  (if (<= channel 0.03928 ;0.04045
          )
      (/ channel 12.92)
    (expt (/ (+ channel 0.055) 1.055) 2.4)))

(defsubst zlisp/srgb01->linear (rgb)
  "Convert list RGB (R G B) in sRGB 0..1 (gamma-encoded) to linear-light sRGB."
  (cl-mapcar (lambda (channel)
               (zlisp/srgb->linear channel))
             rgb))

(defsubst zlisp/linear->srgb (linear)
  "Convert linear-light LINEAR into sRGB channels."
  (if (<= linear 0.0031308)
      (* 12.92 linear)
    (- (* 1.055 (expt linear (/ 1.0 2.4))) 0.055)))

(defsubst zlisp/linear->srgb01 (linear)
  "Convert linear-light LINEAR into sRGB 0..1."
  (cl-mapcar (lambda (channel)
               (zlisp/linear->srgb channel))
             linear))

;;;; Utilities:

(defsubst zlisp/rel-luminance (hex)
  "Releative luminance for HEX (#rrggbb)."
  (pcase-let* ((`(,r ,g ,b) (zlisp/hex->srgb01 hex))
               (R (zlisp/srgb->linear r))
               (G (zlisp/srgb->linear g))
               (B (zlisp/srgb->linear b)))
    (+ (* 0.2126 R)
       (* 0.7152 G)
       (* 0.0722 B))))

(defsubst zlisp/contrast (hex-a hex-b)
  "WCAG contrast ratio (>=1) between HEX-A and HEX-B."
  (let* ((la (zlisp/rel-luminance hex-a))
         (lb (zlisp/rel-luminance hex-b))
         (L1 (max la lb))
         (L2 (min la lb)))
    (/ (+ L1 0.05)
       (+ L2 0.05))))

;;;; OKLab <-> sRGB:

(defun zlisp/linear->oklab (rgb)
  "RGB is linear sRGB 0..1; return (L a b) in OKLab space."
  (cl-destructuring-bind (r g b) rgb
    (let* ((l (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b)))
           (m (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b)))
           (s (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)))
           (l_ (expt l (/ 1.0 3.0)))
           (m_ (expt m (/ 1.0 3.0)))
           (s_ (expt s (/ 1.0 3.0)))
           (L (+ (* 0.2104542553 l_) (* 0.7936177850 m_) (* -0.0040720468 s_)))
           (A (+ (* 1.9779984951 l_) (* -2.4285922050 m_) (* 0.4505937099 s_)))
           (B (+ (* 0.0259040371 l_) (* 0.7827717662 m_) (* -0.8086757660 s_))))
      (list L A B))))

(defun zlisp/oklab->linear (Lab)
  "Return linear sRGB 0..1 from LAB in OKLab space."
  (cl-destructuring-bind (L A B) Lab
    (let* ((l_ (+ L (* 0.3963377774 A) (* 0.2158037573 B)))
           (m_ (+ L (* -0.1055613458 A) (* -0.0638541728 B)))
           (s_ (+ L (* -0.0894841775 A) (* -1.2914855480 B)))
           (l (* l_ l_ l_))
           (m (* m_ m_ m_))
           (s (* s_ s_ s_))
           (r (+ (* 4.0767416621 l) (* -3.3077115913 m) (* 0.2309699292 s)))
           (g (+ (* -1.2684380046 l) (* 2.6097574011 m) (* -0.3413193965 s)))
           (b (+ (* -0.0041960863 l) (* -0.7034186147 m) (* 1.7076147010 s))))
      (list r g b))))

(defun zlisp/hex->oklab (hex)
  "Convert the hexadecimal string in HEX to L a b values in OKLab space."
  (zlisp/linear->oklab (zlisp/srgb01->linear (zlisp/hex->srgb01 hex))))

(defun zlisp/oklab->hex (L a b)
  "Return L A B (in OKLab space) to a hexadecimal RGB string."
  (apply #'zlisp/srgb01->hex
         (zlisp/linear->srgb01 (zlisp/oklab->linear (list L a b)))))

;;;; OKLCh helpers:

(defun zlisp/oklab->oklch (L a b)
  "Return L A B (in OKLab space) to values in OKLCh."
  (let* ((C (sqrt (+ (* a a) (* b b))))
         (h (zlisp/wrap-360 (zlisp/rad->deg (atan b a)))))
    (list L C h)))

(defun zlisp/oklch->oklab (L C h)
  "Return L C H (in OKLCh) to L a b in OKLab."
  (let* ((hr (zlisp/deg->rad h))
         (a (* C (cos hr)))
         (b (* C (sin hr))))
    (list L a b)))

(defun zlisp/oklch->srgb01-raw (L C h)
  "OKLCh L C H -> sRGB 0..1 WITHOUT clamping (channels may be <0 or >1)."
  (zlisp/linear->srgb01
   (zlisp/oklab->linear
    (apply #'zlisp/oklch->oklab
           (list L C h)))))

(defun zlisp/in-srgb-gamut-p (rgb)
  "Return non-nil if RGB=(r g b) all lie in [0,1] (with tiny epsilon)."
  (cl-destructuring-bind (r g b) rgb
    (and (<= -1e-6 r 1.0)
         (<= -1e-6 g 1.0)
         (<= -1e-6 b 1.0))))

(defun zlisp/oklch->hex-safe (L C h)
  "Convert L C H (in OKLCh) to hex.  Reduce C until in gamut."
  (let ((c C) rgb)
    (cl-loop repeat 64 do
             (setq rgb (zlisp/oklch->srgb01-raw L c h))
             (when (zlisp/in-srgb-gamut-p rgb) (cl-return))
             (setq c (* c 0.96)))
    (apply #'zlisp/srgb01->hex rgb)))

(defun zlisp/hex->oklch (hex)
  "Return OKLCh values for HEX (h in degrees 0..360)."
  (apply #'zlisp/oklab->oklch
         (zlisp/hex->oklab hex)))

(defun zlisp/hue (hex)
  "OKLCh hue (0..360) for HEX.  NIL if under the grey threshold."
  (cl-destructuring-bind (L C h)
      (zlisp/hex->oklch hex)
    (when (>= C zlisp-grey-chroma-threshold)
      h)))

(defsubst zlisp/anchor-hue (hex)
  "OKLCh hue of HEX; defaults to 0 if HEX is neutral."
  (or (zlisp/hue hex) 0.0))

;;;; Complementary colours:
;;;;; Helper functions:

(defun zlisp--emit-rotations-oklab (hex degrees &optional safe)
  "From HEX, rotate hue by each element of DEGREES (list), preserve L,C.
Return list of hex. If SAFE non-nil, use `zlisp/oklch->hex-safe`."
  (cl-destructuring-bind (L C h0)
      (zlisp/hex->oklch hex)
    (let ((emit (if safe
                    (lambda (L C h)
                      (zlisp/oklch->hex-safe L C h))
                  (lambda (L C h)
                    (apply #'zlisp/oklab->hex (zlisp/oklch->oklab L C h))))))
      (mapcar (lambda (d)
                (funcall emit L C (zlisp/wrap-360 (+ h0 (float d)))))
              degrees))))

;;;;; Plain compliment:

(defun zlisp/complement-oklab (hex &rest opts)
  "Single complementary of HEX (rotate hue 180°). Return a list of one hex.

Keyword OPTS: :safe (non-nil => gamut-safe)."
  (let ((safe (plist-get opts :safe)))
    (zlisp--emit-rotations-oklab hex (list 180.0) safe)))

(defun zlisp/complement-alist (hex &rest opts)
  "Single complementary of HEX.

As `zlisp/complement-oklab' but labeled: '((:complement . \"#…\")).

Keyword OPTS: :safe (non-nil => gamut-safe)."
  (let* ((safe (plist-get opts :safe))
         (c    (car (zlisp/complement-oklab hex :safe safe))))
    (list (cons :complement c))))

;;;;; Analogous:

(defun zlisp/analogous-oklab (hex &rest opts)
  "Analogous colors of HEX at ±DELTA degrees (default 30°). Return (cw ccw).

Keyword OPTS: :delta (number), :safe."
  (let* ((delta (or (plist-get opts :delta) 30.0))
         (safe  (plist-get opts :safe)))
    (zlisp--emit-rotations-oklab hex (list delta (- delta)) safe)))

(defun zlisp/analogous-alist (hex &rest opts)
  "Analogous colors of HEX at ±DELTA degrees (default 30°).

As `zlisp/analogous-oklab' but labeled: (:analogous+Δ . \"#…\") etc.

Keyword OPTS: :delta (number), :safe."
  (let* ((delta (or (plist-get opts :delta) 30.0))
         (safe  (plist-get opts :safe))
         (pair  (zlisp/analogous-oklab hex :delta delta :safe safe))
         (d     (truncate delta)))
    (list (cons (intern (format ":analogous+%d" d)) (nth 0 pair))
          (cons (intern (format ":analogous-%d" d)) (nth 1 pair)))))

;;;;; Split complements:

(defun zlisp/split-complements-oklab (hex &rest opts)
  "Split-complements of HEX at ±(180-DELTA) (default Δ=30 → 150°).

Keyword OPTS: :delta (number), :safe."
  (let* ((delta  (or (plist-get opts :delta) 30.0))
         (spread (- 180.0 delta))
         (safe   (plist-get opts :safe)))
    (zlisp--emit-rotations-oklab hex (list spread (- spread)) safe)))

(defun zlisp/split-complements-alist (hex &rest opts)
  "Split-complements of HEX at ±(180-DELTA) (default Δ=30 → 150°).

As `zlisp/split-complements-oklab' but labeled.

Keyword OPTS: :delta (number), :safe."
  (let* ((delta (or (plist-get opts :delta) 30.0))
         (safe  (plist-get opts :safe))
         (pair  (zlisp/split-complements-oklab hex :delta delta :safe safe))
         (d     (truncate (- 180.0 delta))))
    (list (cons (intern (format ":split+%d" d)) (nth 0 pair))
          (cons (intern (format ":split-%d" d)) (nth 1 pair)))))

;;;;; Triad:

(defun zlisp/triad-complements-oklab (hex &rest opts)
  "Return the two triad complements of HEX in OKLab/OKLCh.

Keeps L and C, rotates hue by ±120°. Returns a list of two hexes.

Keyword OPTS:
  :safe  non-nil -> gamut-safe emit
  :order 'cw | 'ccw | 'both  (default 'both; same as 'cw here)"
  (let* ((safe  (plist-get opts :safe))
         (order (or (plist-get opts :order) 'both))
         (pair  (zlisp--emit-rotations-oklab hex (list 120.0 -120.0) safe)))
    (pcase order
      ('ccw (nreverse pair))
      (_    pair))))

(defun zlisp/triad-complements-alist (hex &rest opts)
  "Return the two triad complements of HEX in OKLab/OKLCh.

As `zlisp/triad-complements-oklab' but labeled.

Keyword OPTS:
  :safe  non-nil -> gamut-safe emit
  :order 'cw | 'ccw | 'both  (default 'both; same as 'cw here)"
  (let* ((safe  (plist-get opts :safe))
         (order (or (plist-get opts :order) 'both))
         (pair  (zlisp/triad-complements-oklab hex :safe safe :order order)))
    (list (cons :triad+120 (nth 0 pair))
          (cons :triad-120 (nth 1 pair)))))

;;;;; Square:

(defun zlisp/square-complements-oklab (hex &rest opts)
  "Square harmony companions of HEX: +90°, +180°, +270°.

Keyword OPTS: :safe."
  (let ((safe (plist-get opts :safe)))
    (zlisp--emit-rotations-oklab hex (list 90.0 180.0 270.0) safe)))

(defun zlisp/square-complements-alist (hex &rest opts)
  "Square harmony companions of HEX: +90°, +180°, +270°.

As `zlisp/square-complements-oklab' but labeled.

Keyword OPTS: :safe."
  (let* ((safe (plist-get opts :safe))
         (xs   (zlisp/square-complements-oklab hex :safe safe)))
    (list (cons :square+90  (nth 0 xs))
          (cons :square+180 (nth 1 xs))
          (cons :square+270 (nth 2 xs)))))

;;;;; Rectangle:

(defun zlisp/rectangle-complements-oklab (hex &rest opts)
  "Rectangle (tetradic) companions of HEX: +DELTA, +180, +180+DELTA.

The default DELTA=60°.

Keyword OPTS: :delta (number), :safe."
  (let* ((delta (or (plist-get opts :delta) 60.0))
         (safe  (plist-get opts :safe)))
    (zlisp--emit-rotations-oklab hex (list delta 180.0 (+ 180.0 delta)) safe)))

(defun zlisp/rectangle-complements-alist (hex &rest opts)
  "Rectangle (tetradic) companions of HEX: +DELTA, +180, +180+DELTA.

As `zlisp/rectangle-complements-oklab' but labeled.

Keyword OPTS: :delta (number), :safe".
  (let* ((delta (or (plist-get opts :delta) 60.0))
         (safe  (plist-get opts :safe))
         (xs    (zlisp/rectangle-complements-oklab hex :delta delta :safe safe))
         (d     (truncate delta)))
    (list (cons (intern (format ":rect+%d" d))       (nth 0 xs))
          (cons :rect+180                             (nth 1 xs))
          (cons (intern (format ":rect+%d+180" d))   (nth 2 xs)))))

;;;; Gradients:
;;;;; Normal gradients:

(defun zlisp/gradient-oklab (base-hex target-hex step &rest opts)
  "Generate a gradient of STEP steps from BASE-HEX to TARGET-HEX in OKLab.

OPTS can be one of:
  :inclusive  (default nil)         Include endpoints BASE and TARGET in
                                    the output.
  :easing     (default #'identity)  Function of t in [0,1] -> [0,1] to
                                    shape the ramp.
  :safe       (default nil)         If non-nil, emit with gamut safety via
                                    OKLCh chroma shrink.

Returns a list of #RRGGBB strings."
  (unless (and (stringp base-hex) (stringp target-hex))
    (user-error "The base/target must be hex strings"))
  (let* ((inclusive (or (plist-get opts :inclusive) nil))
         (easing    (or (plist-get opts :easing) #'identity))
         (safe      (plist-get opts :safe))
         (n         (max 2 (prefix-numeric-value step)))
         (L1a1b1    (zlisp/hex->oklab base-hex))
         (L2a2b2    (zlisp/hex->oklab target-hex))
         (L1        (nth 0 L1a1b1))
         (a1        (nth 1 L1a1b1))
         (b1        (nth 2 L1a1b1))
         (L2        (nth 0 L2a2b2))
         (a2        (nth 1 L2a2b2))
         (b2        (nth 2 L2a2b2))
         (ts        (cl-loop for i from 0 below n
                             collect (if inclusive
                                         (if (= n 1)
                                             0.0
                                           (/ (float i) (1- n)))
                                       ;; No endpoints: space inside (0,1).
                                       (/ (float (1+ i)) (1+ n))))))
    (cl-loop for t0 in ts
             for T = (funcall easing t0)
             for L = (zlisp/lerp L1 L2 T)
             for a = (zlisp/lerp a1 a2 T)
             for b = (zlisp/lerp b1 b2 T)
             collect (if safe
                         ;; Convert to OKLCh and emit with chroma shrink to
                         ;; fit gamut.
                         (cl-destructuring-bind (L* C* h*)
                             (zlisp/oklab->oklch L a b)
                           (zlisp/oklch->hex-safe L* C* h*))
                       ;; Fast path: direct OKLab -> sRGB
                       ;; channels are clamped.
                       (zlisp/oklab->hex L a b)))))

;;;;; Laddered gradients:

(defun zlisp/gradient-ladder-step-oklab (colours step &optional amount)
  "Create a colour that matches STEP within alternating gradients of COLOURS."
  (let* ((modulo (mod step (length colours)))
         (base   (nth modulo colours))
         (steps  (1- step))
         (amount (or amount -4)))
    (zlisp/adjust-brightness-oklab base (* amount steps))))

(defun zlisp/gradient-ladder-oklab (colours count &optional amount)
  "Create a laddered gradient of COUNT colours using COLOURS."
  (cl-loop for idx from 0 to count
           collect (zlisp/gradient-ladder-step-oklab colours idx amount)))

;;;;; Complementary laddered gradients:

(defun zlisp/gradient-ladder-analogous-oklab (primary count)
  "Create a laddered gradient of COUNT colours with PRIMARY and its analogues."
  (let ((colours (append (list primary)
                         (zlisp/analogous-oklab primary))))
    (zlisp/gradient-ladder-oklab colours count)))

(defun zlisp/gradient-ladder-split-complements-oklab (primary count)
  "Create a laddered gradient of COUNT colours with PRIMARY and its splits."
  (let ((colours (append (list primary)
                         (zlisp/split-complements-oklab primary))))
    (zlisp/gradient-ladder-oklab colours count)))

(defun zlisp/gradient-ladder-triad-oklab (primary count)
  "Create a laddered gradient of COUNT colours with PRIMARY and its triads."
  (let ((colours (append (list primary)
                         (zlisp/triad-complements-oklab primary))))
    (zlisp/gradient-ladder-oklab colours count)))

(defun zlisp/gradient-ladder-square-complements-oklab (primary count
                                                               &optional amount)
  "Create a laddered gradient of COUNT colours with PRIMARY and its squares."
  (let ((colours (append (list primary)
                         (zlisp/square-complements-oklab primary))))
    (zlisp/gradient-ladder-oklab colours count amount)))

(defun zlisp/gradient-ladder-rectangle-complements-oklab (primary count
                                                              &optional amount)
  "Create a laddered gradient of COUNT colours with PRIMARY and its rectangles."
  (let ((colours (append (list primary)
                         (zlisp/rectangle-complements-oklab primary))))
    (zlisp/gradient-ladder-oklab colours count amount)))

;;;; Sorting:

(defun zlisp/colours-sort (alist &rest opts)
  "Sort ALIST of (NAME . \"#HEX\") using OKLCh.

OPTS keywords:
  :primary   'hue | 'lightness    (default 'hue)
  :h-order   'asc | 'desc         (default 'asc)
  :l-order   'asc | 'desc         (default 'asc)  ; asc = dark→light
  :anchor    HEX or nil           (rotate hue so ANCHOR → 0°)
  :neutrals  'front | 'back | nil (default nil = mingle at 0° bucket)"
  (let* ((primary    (or (plist-get opts :primary) 'hue))
         (h-order    (or (plist-get opts :h-order) 'asc))
         (l-order    (or (plist-get opts :l-order) 'asc))
         (anchor-hex (plist-get opts :anchor))
         (neutrals   (plist-get opts :neutrals))
         (anchor     (and anchor-hex (zlisp/anchor-hue anchor-hex)))
         ;; Normalize numeric order by flipping sign when we want 'desc.
         (hkey       (lambda (h)
                       (if (eq h-order 'asc)
                           h
                         (- h))))
         (lkey       (lambda (L)
                       (if (eq l-order 'asc)
                           L
                         (- L))))
         (bucket     (lambda (L)
                       (pcase neutrals
                         ('front -1)
                         ('back 1)
                         (_ 0)))))
    (cl-stable-sort
     (copy-sequence alist)
     (lambda (ka kb)           ; Lexicographic over 3-tuple.
       (cond ((< (nth 0 ka) (nth 0 kb)) t)
             ((> (nth 0 ka) (nth 0 kb)) nil)
             ((< (nth 1 ka) (nth 1 kb)) t)
             ((> (nth 1 ka) (nth 1 kb)) nil)
             (t (< (nth 2 ka) (nth 2 kb)))))
     :key (lambda (kv)
            (cl-destructuring-bind (L C h)
                (zlisp/hex->oklch (cdr kv))
              (if (>= C zlisp-grey-chroma-threshold)
                  (let* ((hc (mod (- h (or anchor 0.0)) 360.0))
                         (hk (funcall hkey hc))
                         (lk (funcall lkey L)))
                    (pcase primary
                      ('hue        (list 0 hk lk))
                      ('lightness  (list 0 lk hk))
                      (_           (list 0 hk lk))))
                ;; neutral: give them a bucket apart from colours, then sort by L
                (list (funcall bucket L) (funcall lkey L) 0)))))))

(defun zlisp/colours-sort-by-hue (alist &optional anchor-hex neutrals-position)
  (zlisp/colours-sort alist
                      :primary  'hue
                      :h-order  'asc
                      :l-order  'asc
                      :anchor   anchor-hex
                      :neutrals neutrals-position))

(defun zlisp/colours-sort-by-lightness (alist &optional anchor-hex neutrals-position)
  (zlisp/colours-sort alist
                      :primary  'lightness
                      :l-order  'asc
                      :h-order  'asc
                      :anchor   anchor-hex
                      :neutrals neutrals-position))

;;;; Palette logic:

(defun zlisp//anchor-shift/magenta (magenta-hue)
  "Return an alist of (NAME . HUE) shifted so magenta maps to MAGENTA-HUE."
  (let* ((canon (alist-get 'magenta zlisp-palette-anchors))
         (delta (- magenta-hue canon)))
    (mapcar (lambda (kv)
              (cons (car kv) (zlisp/wrap-360 (+ (cdr kv) delta))))
            zlisp-palette-anchors)))

(defun zlisp//mk-variants (L C h)
  "Return (base bright dim) hexes at hue h from L C and H."
  (let* ((Lb     (zlisp/clamp L 0.0 1.0))
         (Cb     (max 0.0 C))
         (base   (zlisp/oklch->hex-safe Lb Cb h))
         (bright (zlisp/oklch->hex-safe
                  (zlisp/clamp (+ Lb  zlisp-palette-dL-bright) 0.0 1.0)
                  (* Cb zlisp-palette-kC-bright) h))
         (dim    (zlisp/oklch->hex-safe
                  (zlisp/clamp (+ Lb zlisp-palette-dL-dim) 0.0 1.0)
                  (* Cb zlisp-palette-kC-dim) h)))
    (list base bright dim)))

(defun zlisp/palette-generate (magenta-hex &rest opts)
  "Generate a palette from MAGENTA-HEX.

OPTS can contain any of the following options:
  :L         override base lightness (default = L of magenta)
  :C-scale   multiply base chroma (default 1.0)
  :include-extras  non-nil -> add teal/rose/turquoise/umber/steel"
  (let* ((LCh     (apply #'zlisp/oklab->oklch (zlisp/hex->oklab magenta-hex)))
         (Lm      (or (plist-get opts :L) (nth 0 LCh)))
         (Cm0     (nth 1 LCh))
         (Cm      (* (or (plist-get opts :C-scale) 1.0) Cm0))
         (hm      (nth 2 LCh))
         (anchors (zlisp//anchor-shift/magenta hm))
         (out ()))
    ;; Named primaries
    (dolist (nm '(red orange yellow green cyan blue violet magenta))
      (let* ((h (alist-get nm anchors))
             (trip (zlisp//mk-variants Lm Cm h)))
        (push (cons nm trip) out)))
    ;; Extras
    (when (plist-get opts :include-extras)
      (cl-labels
          ((h-of (k) (alist-get k anchors))
           (h-mid (a b)
                  (let* ((ha (zlisp/deg->rad (h-of a)))
                         (hb (zlisp/deg->rad (h-of b)))
                         (x (/ (+ (cos ha) (cos hb)) 2.0))
                         (y (/ (+ (sin ha) (sin hb)) 2.0)))
                    (zlisp/wrap-360 (zlisp/rad->deg (atan y x))))))
        (let ((extras
               `((teal      . ,(h-mid 'green 'cyan))
                 (rose      . ,(h-mid 'red   'magenta))
                 (turquoise . ,(h-mid 'cyan  'blue))
                 ;; Umber: skew toward orange, lower L and C a touch
                 (umber     . ,(h-of 'orange))
                 ;; Silver: near green, lower C a tad
                 (silver      . ,(h-of 'green))
                 ;; Steel: near blue, but tiny chroma later
                 (steel     . ,(h-of 'blue)))))
          (dolist (kv extras)
            (pcase (car kv)
              ('umber
               (push (cons 'umber (zlisp//mk-variants (* Lm 0.80)
                                                      (* Cm 0.70)
                                                      (cdr kv)))
                     out))
              ('silver
               (push (cons 'silver (zlisp//mk-variants (* Lm 0.99)
                                                       (* Cm 0.25)
                                                       (cdr kv)))
                     out))
              ('steel
               (let ((trip (zlisp//mk-variants Lm (* Cm 0.25) (cdr kv))))
                 (push (cons 'steel trip) out)))
              (_
               (push (cons (car kv) (zlisp//mk-variants Lm Cm (cdr kv)))
                     out)))))))
    (nreverse out)))

(defun zlisp/palette-to-list (alist)
  "Convert the generated palette in ALIST to an alist of (NAME . COLOUR)."
  (let ((brights nil)
        (dims    nil)
        (norms   nil))
    (dolist (kv alist)
      (cl-destructuring-bind (base bright dim) (cdr kv)
        (let* ((sym-name     (symbol-name (car kv)))
               (base-keyword (intern (format ":%s"        sym-name)))
               (bri-keyword  (intern (format ":bright-%s" sym-name)))
               (dim-keyword  (intern (format ":dim-%s"    sym-name))))
          (setq norms   (append norms   (list (cons base-keyword base))))
          (setq brights (append brights (list (cons bri-keyword  bright))))
          (setq dims    (append dims    (list (cons dim-keyword  dim)))))))
    (append brights norms dims)))

(defun zlisp/adjust-brightness-oklab (hex percent &rest opts)
  "Return HEX with brightness adjusted by PERCENT in OKLab.

PERCENT is a signed number (e.g. +10, -15).

OPTS can be one of:
  :mode  one of:
         'scale   -> L' = L * (1 + p)         (default)
         'offset  -> L' = L + p
         'toward  -> L' = L + (1-L)*p  if p>0,  or L' = L - L*|p| if p<0
         where p = PERCENT/100.0
  :safe  non-nil -> emit with gamut safety (convert via OKLCh and shrink
                    chroma if needed)

Returns a \"#RRGGBB\" string."
  (let* ((p (/ (float percent) 100.0))
         (mode (or (plist-get opts :mode) 'scale))
         (safe (plist-get opts :safe))
         (Lab  (zlisp/hex->oklab hex))
         (L    (nth 0 Lab))
         (a    (nth 1 Lab))
         (b    (nth 2 Lab))
         (Lc   (zlisp/clamp
                (pcase mode
                  ('scale  (* L (+ 1.0 p))) ; relative scale.
                  ('offset (+ L p))         ; absolute offset.
                  ('toward (if (>= p 0)
                               (+ L (* (- 1.0 L) p)) ; move toward 1.
                             (- L (* L (abs p)))))   ; move toward 0.
                  (_       (* L (+ 1.0 p))))         ; default: scale.
                0.0 1.0)))
    (if safe
        ;; keep chroma/hue identical in OKLCh; adjust only L, then
        ;; gamut-safe emit.
        (cl-destructuring-bind (_L C h) (zlisp/oklab->oklch L a b)
          (zlisp/oklch->hex-safe Lc C h))
      ;; fast path: direct OKLab -> sRGB (channels will be clamped).
      (zlisp/oklab->hex Lc a b))))

(defun zlisp/adjust-brightness-oklab-list (alist percent &rest opts)
  "Map `zlisp/adjust-brightness-oklab' over ALIST of (NAME . \"#HEX\").
Returns a NEW alist with colours by PERCENT.

For a description of OPTS see `zlisp/adjust-brightness-oklab'.

Example:
  (zlisp/oklab-adjust-brightness-list my-colours +8 :mode 'toward :safe t)"
  (mapcar (lambda (kv)
            (cons (car kv)
                  (apply #'zlisp/adjust-brightness-oklab
                         (cdr kv) percent opts)))
          alist))

;; +12% brighter, proportional:
;;
;;     (zlisp/oklab-adjust-brightness "#AC94E1" 12)
;;
;;
;; -15% darker, but move percent toward black/white (nicer on extremes),
;; gamut-safe:
;;
;;     (zlisp/oklab-adjust-brightness "#94E1D3" -15 :mode 'toward :safe t)
;;
;;
;; Batch-adjust an alist by +8% toward white:
;;
;;     (zlisp/oklab-adjust-brightness-list zmacs-ui-colours 8 :mode 'toward)

(defun zlisp/rotate-hue-oklab (hex degrees &optional safe)
  "Rotate HEX by DEGREES around the OKLCh hue circle.

Keeps lightness (L) and chroma (C) fixed; only hue (h°) changes.
If SAFE is non-nil, emit via `zlisp/oklch->hex-safe' (shrinks chroma if needed).
Otherwise, convert directly back to sRGB (faster, may clamp in sRGB)."
  (cl-destructuring-bind (L C h0) (zlisp/hex->oklch hex)
    (let ((h (zlisp/wrap-360 (+ h0 (float degrees)))))
      (if safe
          (zlisp/oklch->hex-safe L C h)
        (apply #'zlisp/oklab->hex (zlisp/oklch->oklab L C h))))))

(defun zlisp/adjust-chroma-oklab (hex percent &rest opts)
  "Adjust chroma of HEX by PERCENT in OKLCh; return \"#RRGGBB\".

PERCENT is signed (e.g., +20 or -15). Only chroma (C) changes; L and h are held.

Keyword OPTS:
  :mode  one of:
         'scale  -> C' = C * (1 + p)      (default)
         'offset -> C' = C + p
         'toward -> C' = C * (1 - |p|)    (reduces chroma toward neutral)
         where p = PERCENT/100.
  :safe  non-nil -> use `zlisp/oklch->hex-safe' for gamut-safe output.

Notes:
- 'scale increases/decreases proportionally.
- 'offset adds a fixed delta in chroma units.
- 'toward ignores the sign and pulls toward neutral (C=0) by |p| fraction."
  (let* ((p    (/ (float percent) 100.0))
         (mode (or (plist-get opts :mode) 'scale))
         (safe (plist-get opts :safe)))
    (cl-destructuring-bind (L C h) (zlisp/hex->oklch hex)
      (let* ((Cc
              (max 0.0
                   (pcase mode
                     ('scale  (* C (+ 1.0 p)))
                     ('offset (+ C p))
                     ('toward (* C (max 0.0 (- 1.0 (abs p)))))
                     (_       (* C (+ 1.0 p)))))))
        (if safe
            (zlisp/oklch->hex-safe L Cc h)
          (apply #'zlisp/oklab->hex (zlisp/oklch->oklab L Cc h)))))))

(defun zlisp/pick-contrast (bg &rest candidates)
  "Given BG and CANDIDATES, return the hex with max WCAG contrast.

Usage:
  (zlisp/pick-contrast \"#100d13\" \"#e2daf6\" \"#131019\")
  (zlisp/pick-contrast bg '(\"#fff\" \"#ddd\" \"#bbb\"))
  (zlisp/pick-contrast bg '((light . \"#e2daf6\") (dark . \"#131019\")))

Returns the chosen hex string. Use `zlisp/contrast' if you need the ratio."
  ;; Normalize candidates: allow varargs, list, or alist.
  (let* ((list
          (cond
           ;; single list or alist argument.
           ((and (= (length candidates) 1)
                 (listp (car candidates)))
            (let ((x (car candidates)))
              (if (and x (consp (car x))) ; alist → take cdrs.
                  (mapcar #'cdr x)
                x)))
           ;; plain varargs.
           (t candidates))))
    (unless list (user-error "No candidates given"))
    (car (cl-reduce
          (lambda (best hex)
            (let ((r-best (zlisp/contrast bg (car best)))
                  (r-new  (zlisp/contrast bg hex)))
              (if (> r-new r-best)
                  (cons hex r-new)
                best)))
          (cdr list)
          :initial-value (cons (car list) (zlisp/contrast bg (car list)))
          :from-end nil))))

(defun zlisp/contrast-foreground (bg)
  "Return a foreground hex suitable for BG.

Chooses between the foregrounds of `zmacs-dark-text` and `zmacs-light-text`
by comparing WCAG contrast ratios."
  (let* ((fg-dark  zmacs-text-dark)
         (fg-light zmacs-text-light)
         (c-dark   (zlisp/contrast bg fg-dark))
         (c-light  (zlisp/contrast bg fg-light)))
    (if (> c-dark c-light)
        fg-dark
      fg-light)))

;;;; Org export:

(defun zlisp/palette-to-org (alist)
  "Return an Org table string for ALIST of (name . (base bright dim))."
  (let ((rows
         (mapconcat
          (lambda (kv)
            (cl-destructuring-bind (base bright dim) (cdr kv)
              (format "| %s | %s | %s | %s |"
                      (capitalize (symbol-name (car kv)))
                      base bright dim)))
          alist "\n")))
    (concat
     "#+CAPTION: Generated palette\n"
     "| Name | Base | Bright | Dim |\n|-\n" rows "\n")))

;;;; Provide package:

(provide 'zlisp-colour)

;;; zlisp-colour.el ends here.
