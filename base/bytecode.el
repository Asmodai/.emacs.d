;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; bytecode.el --- Bytecode utilities
;;;
;;; Time-stamp: <Tuesday Jan 24, 2012 12:52:09 asmodai>
;;; Revision:   7
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    09 Mar 2011 14:43:26
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
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
;;; }}}
;;; {{{ Commentary:
;;;
;;; }}}

(defun bytecode-filename (file)
  "Return the name of the bytecode filename in the cache directory."
  (expand-file-name
   (concat file ".elc")
   +bc-cache-directory+))

(defun bytecode-is-stale-p (file)
  "Returns T if the given `file' has stale bytecode; otherwise NIL is
returned.

In order to determine whether bytecode is stale, we compare the Emacs
Lisp creation date to the bytecode creation date.  If the Emacs Lisp
file is newer, then the bytecode is stale."
  (let* ((sourcefile (locate-library file))
         (bytefile (bytecode-filename file)))
    (and (file-exists-p bytefile)
         (file-newer-than-file-p sourcefile bytefile))))

(defun byte-compile-to (input output)
  "Like byte-compile-file, but puts the result in `output'. Returns
the result of BYTE-COMPILE-FILE."
  ;; Require byte-compile here instead of letting the call to
  ;; byte-compile-file autoload it. We need byte-compile-dest-file to
  ;; be defined; if we define it ourselves and then let byte-compile
  ;; load, byte-compile will notice that byte-compile-dest-file is
  ;; alreay defined and not define its version.
  (require 'bytecomp)
  (let ((saved-dest-func (symbol-function 'byte-compile-dest-file))
        (byte-compile-verbose nil)
        (font-lock-verbose nil)
        (byte-compile-warnings '()))
    (unwind-protect
        (progn
          (fset 'byte-compile-dest-file
                '(lambda (src)
                  (if (equal src input)
                      output
                      (funcall saved-dest-func src))))
          (byte-compile-file input))
      (fset 'byte-compile-dest-file saved-dest-func))))

(defun compile-load (file)
  "Load in the given `file' bytecode.  If the given `file' has no
bytecode, then compile the file and then load the bytecode."
  (let ((cache-file (bytecode-filename file)))
    (when (or (bytecode-is-stale-p file)
              (not (file-exists-p cache-file)))
      (let ((el-file (locate-library file)))
        (if el-file
            (byte-compile-to el-file cache-file)
            (error "Could not locate %s for library %s"
                   el-file
                   file))))
    (load cache-file)))

;;; bytecode.el ends here
