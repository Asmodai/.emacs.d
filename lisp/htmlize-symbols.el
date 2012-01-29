;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; htmlize-symbols.el --- Htmlize symbol hacks
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 00:37:28 asmodai>
;;; Revision:   29
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2007, Volkan YAZICI <volkan.yazici@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Wed Jan 19 08:52:13 2011
;;; Keywords:   
;;; URL:        http://unixware.kicks-ass.org/
;;;
;;;{{{ License:
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are  permitted provided that the following conditions are
;;; met:
;;;
;;; Redistributions of source code must retain the above copyright notice,
;;; this list of conditions and the following disclaimer. 
;;;
;;; Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;; This is is a modification of the functions in `htmlize.sh'.
;;;
;;; This does not do any font-lock clobbering, we should let Emacs determine
;;; what is a keyword, builtin, variable etc.
;;;
;;;}}}

(defun symbols->font-lock-keywords (symbols face)
  (list
   (concat "\\((\\|\\s+\\|\\<\\)" (regexp-opt symbols t) "\\>")
   2
   face))

(defmacro symbols->font-lock-add-keywords (mode &rest specs)
  `(font-lock-add-keywords
    (quote ,mode)
    (mapcar (lambda (spec)
              (symbols->font-lock-keywords
               (mapcar 'car (first spec))
               (second spec)))
            (list ,@(mapcar (lambda (spec)
                              `(list ,(first spec) (quote ,(third spec))))
                            specs)))))

(defmacro symbols->hash-tables (&rest specs)
  `(progn
     ,@(mapcar (lambda (spec)
                 `(defvar ,(second spec)
                      (let ((table (make-hash-table
                                    :test 'equal
                                    :size (length ,(first spec)))))
                        (dolist (pair ,(first spec))
                          (puthash (car pair) (cdr pair) table))
                        table)))
               specs)))

(defmacro symbols->uri-lookup-hook (base-uri &rest specs)
  `(lambda (sym)
     (let ((res (or ,@(mapcar (lambda (spec)
                                `(gethash sym ,(second spec)))
                              specs))))
       (when res
         (concat ,base-uri res)))))

(defmacro make-symbol-lookup-data (mode base-uri &rest spec)
  `(progn
     (symbols->hash-tables ,@spec)
     (puthash (quote ,mode)
              (cons (symbols->uri-lookup-hook ,base-uri ,@spec)
                    (gethash (quote ,mode) *avail-uri-lookup-hooks-table*))
              *avail-uri-lookup-hooks-table*)))

(defmacro make-symbol-lookup-data-with-font-lock (mode base-uri &rest spec)
  `(progn
     (symbols->font-lock-add-keywords ,mode ,@spec)
     (symbols->hash-tables ,@spec)
     (puthash (quote ,mode)
              (cons (symbols->uri-lookup-hook ,base-uri ,@spec)
                    (gethash (quote ,mode) *avail-uri-lookup-hooks-table*))
              *avail-uri-lookup-hooks-table*)))

;;; htmlize-symbols.el ends here
