;;; zlisp-math.el --- Math stuff  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    31 Oct 2024 07:33:15
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

(require 'cl-lib)

;;;; Utilities:

(defsubst zlisp/clamp (value lo hi)
  "Clamp VALUE to a value between LO and HI."
  (max lo (min hi value)))

(defsubst zlisp/deg->rad (degs)
  "Convert DEGS from degrees to radians."
  (* pi (/ degs 180.0)))

(defsubst zlisp/rad->deg (rads)
  "Convert RADS from radians to degrees."
  (* 180.0 (/ rads pi)))

(defsubst zlisp/wrap-360 (degs)
  "Wrap the given DEGS value (in degrees) to always be within 0..360."
  (let ((x (mod degs 360.0)))
    (if (< x 0)
        (+ x 360.0)
      x)))

(defsubst zlisp/lerp (start end t)
  "Return the linear interpolation between START and END.

T should be a value between 0 and 1 that represents how far along the result
should be."
  (+ start (* (- end start) t)))

(defsubst zlisp/ease-in (value)
  "Return an `ease-in' curve for VALUE.

The curve starts slowly, then accelerates.

Mathematically, this is just t^2, so values near 0 are even smaller, and
values near 1 catch up quickly.

Good for gradients that should start gentle and finish strong."
  (* value value))

(defsubst zlisp/ease-out (value)
  "Return an `ease-out' curve for VALUE.

The curve starts fast, then decelerates smoothly.

This is 1 - (1 - t)^2, so values near 0 jump quickly, and values near 1
flatten out. Great if you want the gradient to rush into the target and then
settle."
  (- 1.0 (expt (- 1.0 value) 2)))

(defsubst zlisp/ease-in-out (value)
  "Return an `ease-in-and-out' curve for VALUE.

Ease-in-out curve: slow start, speeds up in the middle, then slows down again
before reaching the end.

This is built by combining ease-in for the first half and ease-out for the
second half. Useful for 'natural-feeling' gradients where neither end feels
abrupt."
  (if (< value 0.5)
      (/ (zlisp/ease-in (* 2 value)) 2.0)
    (/ (+ 1.0 (zlisp/ease-out (* 2 (- value 0.5)))) 2.0)))

;;;; Basic math:

;;;; Squares:

(defsubst zlisp/square (val)
  "Return the square of VAL."
  (expt val 2))

(cl-defun zlisp/square-root (x &optional (tolerance 0.00001))
  "Return the square root of X using Newton's Method.

If TOLERANCE is provided it will be used as a tolerance when guessing."
  (let ((good-enough-p (lambda (guess)
                         (< (abs (- (* guess guess) x)) tolerance)))
        (improve (lambda (guess)
                   (/ (+ guess (/ x guess)) 2))))
    (cl-labels ((sqrt-iter (guess)
                           (if (funcall good-enough-p guess)
                               guess
                             (sqrt-iter (funcall improve guess)))))
      (sqrt-iter 1.0))))

;;;; Cubes:

(defsubst zlisp/cube (val)
  "Return the cube of VAL."
  (expt val 3))

(cl-defun zlisp//cube-root (x &optional (tolerance 0.00001))
  "Return the cube root of X using Newton's Method.

If TOLERANCE is provided it will be used as a tolerance when guessing."
  (let ((good-enough-p (lambda (guess)
                         (< (abs (- (* guess guess guess) x)) tolerance)))
        (improve (lambda (guess)
                   (/ (+ (* 2 guess) (/ x (* guess guess))) 3))))
    (cl-labels ((cube-iter (guess)
                           (if (funcall good-enough-p guess)
                               guess
                             (cube-iter (funcall improve guess)))))
      (cube-iter 1.0))))

(defsubst zlisp/sign (val)
  "Return a value indicating the sign of VAL.

1 is returned for positive numbers, -1 for negative, and 0 for zero."
  (cond ((cl-plusp val)   1)
        ((cl-minusp val) -1)
        (t                0)))

;;;; SI symbols:

(defvar zlisp--si-symbols
  '(("radian"    . "rad")
    ("steradian" . "sr")
    ("hertz"     . "Hz")
    ("newton"    . "N")
    ("pascal"    . "Pa")
    ("joule"     . "J")
    ("watt"      . "W")
    ("coulomb"   . "C")
    ("volt"      . "V")
    ("farad"     . "F")
    ("ohm"       . "Ω")
    ("siemens"   . "S")
    ("weber"     . "Wb")
    ("tesla"     . "T")
    ("henry"     . "H")
    ("celcius"   . "°C")
    ("lumen"     . "lm")
    ("lux"       . "lx")
    ("becquerel" . "Bq")
    ("gray"      . "Gy")
    ("sievert"   . "Sv")
    ("katal"     . "kat")
    ("second"    . "s")
    ("metre"     . "m")
    ("meter"     . "m")                 ; For Americans :)
    ("gram"      . "g")
    ("ampere"    . "A")
    ("kelvin"    . "K")
    ("mole"      . "mol")
    ("candela"   . "cd"))
  "SI units and their symbols.")

(defun zlisp//si-pluralise (name)
  "Pluralise an SI unit given in NAME."
  (if (string-match "s$" name)
      name
    (concat name "s")))

(defun zlisp//si-lookup-symbol (name)
  "Look up an SI symbol for NAME.  Return NIL if none are found."
  (let ((name (downcase name)))
    (cdr (assoc name zlisp--si-symbols #'string=))))

;;;; SI units:

(defvar zlisp--si-units
  '((1e+30 >= "Quetta" "Q")
    (1e+27 >= "Ronna"  "R")
    (1e+24 >= "Yotta"  "Y")
    (1e+21 >= "Zetta"  "Z")
    (1e+18 >= "Exa"    "E")
    (1e+15 >= "Peta"   "P")
    (1e+12 >= "Tera"   "T")
    (1e+9  >= "Giga"   "G")
    (1e+6  >= "Mega"   "M")
    (1e+3  >= "Kilo"   "k")
    (1e+2  >= "Hecto"  "h")
    (1e+1  >= "Deca"   "da")
    (1e-30 <= "Quecto" "q")
    (1e-27 <= "Ronto"  "r")
    (1e-24 <= "Yocto"  "y")
    (1e-21 <= "Zepto"  "z")
    (1e-18 <= "Atto"   "a")
    (1e-15 <= "Femto"  "f")
    (1e-12 <= "Pico"   "p")
    (1e-9  <= "Nano"   "n")
    (1e-6  <= "Micro"  "μ")
    (1e-3  <= "Milli"  "m")
    (1e-2  <= "Centi"  "c")
    (1e-1  <= "Deci"   "d")
    (0     >= ""       ""))
  "SI quantity prefixes.")

(defun zlisp/si-units (val unit &optional short)
  "Convert an SI VAL of UNIT into proper form.

If SHORT is non-NIL then the returned string will use short SI form, e.g. km
instead of kilometres."
  (let ((si-symbol (or (zlisp//si-lookup-symbol unit)
                       unit)))
    (cl-loop for ent in zlisp--si-units
             for limit = (cl-first ent)
             for adjusted = (if (zerop limit)
                                val
                              (/ val limit))

             if (funcall (cl-second ent) val limit)
             do (cl-return
                 (let* ((plural (/= adjusted 1.0))
                        (symbol (if (and short si-symbol)
                                    si-symbol
                                  (if plural
                                      (zlisp//si-pluralise unit)
                                    unit))))
                   (format "%0.6f %s"
                           adjusted
                           (if short
                               (concat (cl-fourth ent) symbol)
                             (concat (cl-third ent) symbol))))))))

;;;; Binary units:

(defvar zlisp--iec-binary-units
  `((,(expt 1024 8) "Yobibyte" "YiB")
    (,(expt 1024 7) "Zebibyte" "ZiB")
    (,(expt 1024 6) "Exbibyte" "EiB")
    (,(expt 1024 5) "Pebibyte" "PiB")
    (,(expt 1024 4) "Tebibyte" "TiB")
    (,(expt 1024 3) "Gibibyte" "GiB")
    (,(expt 1024 2) "Mebibyte" "MiB")
    (1024           "Kibibyte" "KiB")
    (0              "byte"      "B"))
  "IEC binary units.")

(defun zlisp/iec-binary-units (val &optional short)
  "Convert VAL bytes into IEC binary notation.

If SHORT is non-nil, short form will be used.

e.g. 1024 bytes becomes 1 kikibytes."
  (cl-loop for unit in zlisp--iec-binary-units
           for limit = (cl-first unit)
           for adjusted = (if (zerop limit)
                              val
                            (/ val limit))

           if (>= val limit)
           do (cl-return
               (format "%.2f %s"
                       adjusted
                       (if short
                           (cl-third unit)
                         (if (= adjusted 1.0)
                             (cl-second unit)
                           (concat (cl-second unit) "s")))))))

;;;; Provide package:

(provide 'zlisp-math)

;;; zlisp-math.el ends here.
