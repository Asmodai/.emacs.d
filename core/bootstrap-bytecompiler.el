;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; bootstrap-bytecompiler.el --- Bytecode compiler hacks.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
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
;;; Licenseas published by the Free Software Foundation,
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

(defun bootstrap::bytecode-filename (file)
  "Returns the name of the bytecode filename in the cache directory."
  (expand-file-name
   (concat file ".elc")
   +bootstrap-bytecode-cache-directory+))

(defun bootstrap::bytecode-is-stale-p (file)
  "Returns T if the given FILE has state bytecode; otherwise NIL is returned.

In order to determine whether bytecode is stale, we compare the Emacs Lisp file
creation timestamp with that of the bytecode file.  If the Emacs Lisp file is
newer, then the bytecode is stale."
  (let* ((sourcefile (locate-library file))
         (bytefile (bootstrap::bytecode-filename file)))
    (and (file-exists-p bytefile)
         (file-newer-than-file-p sourcefile bytefile))))

(defun bootstrap::byte-compile-to (input output)
  "Like `byte-compile-file', but puts the result in OUTPUT.  Returns the result
of `byte-compile-file'."
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

(defun bootstrap:compile-load (file)
  "Load in the given FILE's bytecode.  If the given FILE has no bytecode, then
invoke the  compier and load the resulting bytecode."
  (let ((cache-file (bootstrap::bytecode-filename file)))
    (when (or (bootstrap::bytecode-is-stale-p file)
              (not (file-exists-p cache-file)))
      (let ((el-file (locate-library file)))
        (if el-file
            (bootstrap::byte-compile-to el-file cache-file)
          (error "Could not locate %s for library %s."
                 el-file
                 file))))
    (load cache-file)))

(provide 'bootstrap-bytecompiler)

;;; bootstrap-bytecompiler.el ends here
