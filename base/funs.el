;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic-docstrings: t; byte-compile-dynamic: t -*-
;;;
;;; funs.el --- Various utility functions.
;;;
;;; Time-stamp: <Saturday Jan 31, 2015 18:10:27 asmodai>
;;; Revision:   2
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    31 Jan 2015 14:01:10
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

;;;==================================================================
;;;{{{ Ported from XEmacs:

;;; Port of `emacs-program-name', `emacs-program-version', and
;;; `construct-emacs-version-name' for GNU Emacs and XEmacs 19 and 20.
(when (or (not (xemacs-p))
          (xemacs=19-p)
          (xemacs=20-p))
  
  (defconst emacs-program-name
    (if (xemacs-p)
        "xemacs"
        "emacs")
    "The name of this Emacs program.")
  
  (defconst emacs-program-version
    emacs-version
    "The version of this Emacs program.")
  
  (defun construct-emacs-version-name ()
    "Constructs an Emacs version string from the name and version."
    (concat emacs-program-name
            "-"
            emacs-program-version)))

;;; Port of `emacs-version>=' for GNU Emacs and XEmacs 19 and 20.
(when (or (not (xemacs-p))
          (xemacs=19-p)
          (xemacs=20-p))
  
  (defun emacs-version>= (major &optional minor patch)
    "Returns T if the Emacs version number is greater or equal to
the given MAJOR, MINOR and PATCH version numbers.

Only the MAJOR version number is required, the MINOR and PATCH
arguments are optional.

Only non-NIL arguments are used in the test, and PATCH is ignored but
present for compatability reasons."
    (cond
      ((> emacs-major-version major))  ; T if major is >
      ((< emacs-major-version major)   ; NIL if major is <
       nil)
      ((null minor))                   ; T if minor is NIL
      ((> emacs-minor-version minor))  ; T if minor is >
      ((< emacs-minor-version minor)   ; NIL if minor is <
       nil))))

;;;}}}
;;;==================================================================

;;; funs.el ends here
