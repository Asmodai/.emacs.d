;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; bootstrap-font-support.el --- Font support routines.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 06:17:07
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
;;; This program is distributed in the hope that it will be
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

(require 'bootstrap-funs)
(require 'bootstrap-buffer)

(defvar *bootstrap-default-font*
  (list "Source Code Pro for Powerline"
        :size (if (linux-p)
                  12
                10)
        :weight 'normal
        :width 'normal
        :powerline-scale 1.0))

(defvar *bootstrap-diminished-minor-modes* nil
  "List of diminished minor modes.")

(defun bootstrap:set-default-font (plist)
  "Set the font given the passed PLIST.

PLIST has the form (\"fontname\" :prop1 val1 :prop2 val2 ...)"
  (let* ((font (car plist))
         (props (cdr plist))
         (scale (plist-get props :powerline-scale))
         (font-props (bootstrap:mplist-remove
                      (bootstrap:mplist-remove props :powerline-scale)
                      :powerline-offset))
         (fontspec (apply 'font-spec :name font font-props)))
    (bootstrap-buffer:message "Setting font \"%s\"..." font)
    (set-default-font fontspec nil t)
    (setq-default powerline-scale scale)
    (setq-default powerline-height (bootstrap:compute-powerline-height))
    ;; fallback font for unicode characters used in spacemacs
    (pcase system-type
      (`gnu/linux
       (setq fallback-font-name  "Ubuntu Mono")
       (setq fallback-font-name2 "Ubuntu Mono"))
      (`darwin
       (setq fallback-font-name  "Menlo")
       (setq fallback-font-name2 "Menlo"))
      (`windows-nt
       (setq fallback-font-name  "MS Gothic")
       (setq fallback-font-name2 "Lucida Sans Unicode"))
      (`cygwin
       (setq fallback-font-name  "MS Gothic")
       (setq fallback-font-name2 "Lucida Sans Unicode"))
      (other
       (setq fallback-font-name nil)
       (setq fallback-font-name2 nil)))
    (when (and fallback-font-name fallback-font-name2)
      ;; remove any size or height properties in order to be able to
      ;; scale the fallback fonts with the default one (for zoom-in/out
      ;; for instance)
      (let* ((fallback-props (bootstrap:mplist-remove
                              (bootstrap:mplist-remove font-props :size)
                              :height))
             (fallback-spec (apply 'font-spec
                                   :name fallback-font-name
                                   fallback-props))
             (fallback-spec2 (apply 'font-spec
                                    :name fallback-font-name2
                                    fallback-props)))
        ;; window numbers
        (set-fontset-font "fontset-default"
                          '(#x2776 . #x2793) fallback-spec nil 'prepend)
        ;; mode-line circled letters
        (set-fontset-font "fontset-default"
                          '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
        ;; mode-line additional characters
        (set-fontset-font "fontset-default"
                          '(#x2295 . #x22a1) fallback-spec nil 'prepend)
        ;; new version lighter
        (set-fontset-font "fontset-default"
                          '(#x2190 . #x2200) fallback-spec2 nil 'prepend)))
    fontspec))

(defun bootstrap:compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale)
                        powerline-scale)
                   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defmacro bootstrap:diminish (mode unicode &optional ascii)
  `(add-to-list '*bootstrap-diminished-minor-modes* '(,mode ,unicode ,ascii)))

(defmacro bootstrap:hide-lighter (mode)
  `(eval-after-load 'diminish '(diminish ',mode)))

(provide 'bootstrap-font-support)

;;; bootstrap-font-support.el ends here.
