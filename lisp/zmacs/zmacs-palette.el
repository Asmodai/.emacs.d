;;; zmacs-palette.el --- Palette hacks.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    28 Aug 2025 11:52:12
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
;;;; Requirements:

(require 'cl-lib)
(require 'tabulated-list)
(require 'subr-x)
(require 'zlisp-colour)

;;;; Customize:

(defgroup zmacs-colour-display nil
  "Display a list of colours with swatches and contrast."
  :group 'zmacs)

(defcustom zmacs-colour-display-example-string "AaBbCcDdEeFfGgHhIi"
  "A sample string that will be displayed in the colours of a swatch."
  :type 'string
  :group 'zmacs-colour-display)

;;;; Utilities:

(defun zmacs--swatch (hex &optional width height border)
  "Return a swatch string propertised with HEX background.

The swatche's width and height shall be set to WIDTH and HEIGHT.

If BORDER is non-nil, then the string shall be enclosed in a box."
  (let* ((w   (or width 0))
         (h   (or height 1))
         (pad (make-string w ?\s))
         (line (propertize pad
                           'face `(:background ,hex ,@(when border `(:box t))))))
    (string-join (make-list h line) "\n")))

(defsubst zmacs--sample (hex bg)
  "Return a sample string with HEX foreground on BG background."
  (propertize zmacs-colour-display-example-string
              'face `(:foreground ,hex :background ,bg :weight semibold)))

(defsubst zmacs--fmt (n)
  "Format number N to 2 decimals."
  (format "%.2f" n))

;;;; Sorting:



;;;; Derived mode:

(define-derived-mode zmacs-colour-display-mode tabulated-list-mode
  "Zmacs-Colours"
  "Major mode for displaying a table of colours."
  (setq tabulated-list-format
        ;; name, hex, contrast vs bg, vs fg, swatch, sample
        [("Name"   20 t)
         ("Hex"    10 t)
         ("C/BG"   6  t)
         ("C/FG"   6  t)
         ("Swatch" 10 nil)
         ("Sample" 8  nil)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;; Display functions:

(defun zmacs-colour-display ()
  (interactive)
  (zmacs--colour-display zmacs-ui-colours))

(defun zmacs-colour-display-sorted ()
  (interactive)
  (zmacs--colour-display-sorted zmacs-ui-colours))

(defun zmacs--colour-display (colours &optional bg fg buffer-name)
  "Display COLOURS in a table.

COLOURS may be a list of hex strings or an alist of (NAME . HEX).

if non-nil, BUFFER-NAME specifies the name of the buffer used.
If non-NIL, BG specifies the background colour.  It defaults to `unspecified'.
If non-NIL, FG overrides the foreground colour.  It defaults to `default'."
  (interactive
   (list (read (read-string "Colours (Lisp expr): "))
         (read-string (format "Background (default %s): "
                              (face-background 'default))
                      nil
                      nil
                      (face-background 'default))
         (read-string (format "Foreground (default %s): "
                              (face-foreground 'default))
                      nil
                      nil
                      (face-foreground 'default))))
  (let* ((bg* (or bg (face-background 'default)))
         (fg* (or fg (face-foreground 'default)))
         (buf (get-buffer-create (or buffer-name "*ZMACS-Colors*"))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (zmacs-colour-display-mode)
        (setq tabulated-list-entries
              (mapcar
               (lambda (it)
                 (let* ((name (if (consp it)
                                  (car it)
                                ""))
                        (hex  (if (consp it)
                                  (cdr it)
                                it))
                        (c-bg (zmacs--fmt (zlisp/contrast hex bg*)))
                        (c-fg (zmacs--fmt (zlisp/contrast hex fg*)))
                        (sw   (zmacs--swatch hex 8 1))
                        (smp  (zmacs--sample hex bg*))
                        (nm   (if (symbolp name)
                                  (symbol-name name)
                                (if (string-empty-p name)
                                    hex
                                  name))))
                   (list nm
                         (vector
                          (truncate-string-to-width nm 40)
                          hex
                          c-bg
                          c-fg
                          sw
                          smp))))
               colours))
        (tabulated-list-print)))
    (pop-to-buffer buf)))

(defun zmacs--colour-display-sorted (alist &optional
                                           anchor-hex
                                           neutrals-position
                                           bg
                                           fg)
  "Sort ALIST by hue and display.

For descriptions of ANCHOR-HEX and NEUTRALS-POSITION please see the docstring
for `zlisp/colours-sort-by-hue'.

If non-NIL, BG will be used as the background colour.
If non-NIL, FG will be used as the foreground colour."
  (interactive
   (list (read (read-string "Colours (Lisp expr): "))
         (read-string (format "Background (default %s): "
                              (face-background 'default))
                      nil
                      nil
                      (face-background 'default))
         (read-string (format "Foreground (default %s): "
                              (face-foreground 'default))
                      nil
                      nil
                      (face-foreground 'default))))
  (let* ((sorted (zlisp/colours-sort-by-hue alist anchor-hex neutrals-position)))
    (zmacs--colour-display sorted
                          (or bg (face-background 'default))
                          (or fg (face-foreground 'default)))))

(provide 'zmacs-palette)

;;; zmacs-palette.el ends here.
