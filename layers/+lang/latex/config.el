;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; config.el --- LaTeX configuration.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 21:01:49
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

(bootstrap:defvar-company-backends LaTeX-mode)

(defvar *latex-build-command*
  (if (executable-find "latexmk")
      "LatexMk"
    "LaTeX")
  "The default command to use with `SPC m b'.")

(defvar *latex-enable-auto-fill* t
  "Whether to use `auto-fill-mode' with TeX files.")

(defvar *latex-enable-folding* nil
  "Whether to use `TeX-fold-mode' with TeX buffers.")

(defvar *latex-nofill-env* '("equation"
                             "equation*"
                             "align"
                             "align*"
                             "tabular"
                             "tikzpicture")
  "List of environment names in which `auto-fill-mode' will be inhibited.")

;;; config.el ends here
