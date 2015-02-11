;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic-docstrings: t -*-
;;;
;;; macros.el --- Various macros.
;;;
;;; Time-stamp: <Wednesday Feb  4, 2015 12:36:01 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    02 Feb 2015 07:06:26
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
;;;{{{ Bootstrap macro:

;;; This macro allows us to define a `bootstrap' form for various
;;; Emacs flavours.

(defvar %bootstrap-args%)               ; `define-bootstrap' arguments.
(defvar %bootstrap-body%)               ; Resulting form body.
(defvar %bootstrap-ands%)               ; `and' groups.
(defvar %bootstrap-vers%)               ; Main Emacs version predicate.
(defvar %bootstrap-clds%)               ; `compile-load' forms.
(defvar %bootstrap-load%)               ; `load' forms.
(defvar %bootstrap-rqrs%)               ; `require' forms.

(defmacro define-bootstrap (&rest options)
  "Defines a boostrap conditional form.

OPTIONS is a list of clauses.

Valid clauses are:
  version PRED
  require [ FILE | (LIST-OF-FILES) ]
  load [ FILE | (LIST-OF-FILES) ]
  compile-load [ FILE | (LIST-OF-FILES) ]
  if COND-CLAUSE [and CLAUSE].. else CLAUSE [and CLAUSE...] [end]
  unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...] [end]
  when COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...] [end]
  do EXPRS...

VERSION      - Defines which version of Emacs this boostrap will operate on.
REQUIRE      - Load in an Emacs package via `require'.
LOAD         - Load a file via `load'.
COMPILE-LOAD - Compile a file and then load it via `compile-load'.
IF           - Conditional form.
UNLESS       - Conditional form.
WHEN         - Conditional form.
DO           - Perform expressions.
END          - End a conditional form.

For example:

  (define-bootstrap
    version (emacs=19-p)
 
    compile-load (\"19-extensions\"
                  \"19-template\"
                  \"19-folding\")

    require ('paren
             'time-stamp)

    when (or (running-on-nova-p)
             (running-on-dauntless-p)
             (running-on-galaxy-p))
      compile-load (\"gcl\"
                    \"dbl\"
                    \"lisp-complete\")
    end

    do
      (initialize-template-binding)
      (set 'time-stamp-active t))"
  (declare (indent 0))
  (let ((%bootstrap-args% nil)
        (%bootstrap-body% nil)
        (%bootstrap-ands% nil)
        (%bootstrap-vers% nil)
        (%bootstrap-clds% nil)
        (%bootstrap-load% nil)
        (%bootstrap-rqrs% nil))
    (setq %bootstrap-args% (append options '(end-bootstrap)))
    (while (not (eq (car %bootstrap-args%) 'end-bootstrap))
      (%bootstrap-parse-clause))
    `(if ,(or %bootstrap-vers% t)
         (progn
           ,@(nreverse %bootstrap-body%)))))

(defmacro %bootstrap-make-cons (thing with)
  `(cons 'progn         
	 (mapcar (function
		  (lambda (x)
		    (list ,thing x)))
		 ,with)))

(defun %bootstrap-parse-clause ()
  (let ((word (pop %bootstrap-args%)))
    (cond
      ((null %bootstrap-args%)
       (error "Malformed `define-bootstrap' macro"))
      ((eq word 'version)
       (setq %bootstrap-vers% (pop %bootstrap-args%)))
      ((eq word 'compile-load)
       (let ((to-load (pop %bootstrap-args%)))
         (if (not (listp to-load))
             (setq to-load (list to-load)))
         (push (%bootstrap-make-cons 'compile-load to-load) %bootstrap-body%)))
      ((eq word 'load)
       (let ((to-load (pop %bootstrap-args%)))
         (if (not (listp to-load))
             (setq to-load (list to-load)))
         (push (%bootstrap-make-cons 'load to-load) %bootstrap-body%)))
      ((eq word 'require)
       (let ((to-load (pop %bootstrap-args%)))
         (if (not (listp to-load))
             (setq to-load (list to-load)))
         (setq to-load (remove 'quote to-load))
         (push (cons 'progn
                     (mapcar
                      (function
                       (lambda (thing)
                         (list 'require 
                               (list 'quote (if (listp thing)
                                                (car (remove 'quote thing))
                                              (remove 'quote thing))))))
                      to-load))
               %bootstrap-body%)))
      ((eq word 'do)
       (let ((body nil))
         (or (consp (car %bootstrap-args%))
             (error "Syntax error on `do' clause."))
         (while (consp (car %bootstrap-args%))
           (push (pop %bootstrap-args%) body))
         (push (cons 'progn (nreverse (cons t body))) %bootstrap-body%)))
      ((memq word '(if when unless))
       (let* ((cond (pop %bootstrap-args%))
              (then (let ((%bootstrap-body% nil))
                      (%bootstrap-parse-clause)
                      (bootstrap-build-ands (nreverse %bootstrap-body%))))
              (else (let ((%bootstrap-body% nil))
                      (if (eq (car %bootstrap-args%) 'else)
                          (progn
                            (pop %bootstrap-args%)
                            (%bootstrap-parse-clause)))
                      (bootstrap-build-ands (nreverse %bootstrap-body%))))
              (simple (and (eq (car then) t)
                           (eq (car else) t))))
         (if (eq (car %bootstrap-args%) 'end)
             (pop %bootstrap-args%))
         (if (eq word 'unless)
             (setq then (prog1
                          else
                          (setq else then))))
         (let ((form (cons (if simple
                               (cons 'progn (nth 1 then))
                               (nth 2 then))
                           (if simple
                               (list (cons 'progn (nth 1 else)))
                               (list (nth 2 else))))))
           (setq form `(if ,cond ,@form))
           (push (if simple
                     `(progn ,form t)
                     form)
                 %bootstrap-body%)))))
    (if (eq (car %bootstrap-args%) 'and)
        (progn
          (pop %bootstrap-args%)
          (%bootstrap-parse-clause)))))

(defun bootstrap-build-ands (clauses)
  (let ((ands nil)
        (body nil))
    (while clauses
      (if (and (eq (car-safe (car clauses)) 'progn)
               (eq (car (last (car clauses))) t))
          (if (cdr clauses)
              (setq clauses (cons
                             (nconc (butlast (car clauses))
                                    (if (eq (car-safe (cadr clauses)) 'progn)
                                        (cdadr clauses)
                                        (list (cadr clauses))))
                             (cddr clauses)))
              (setq body (cdr (butlast (pop clauses)))))
          (push (pop clauses) ands)))
    (setq ands (or (nreverse ands)
                   (list t)))
    (list (if (cdr ands)
              (cons 'and ands)
              (car ands))
          body
          (let ((full (if body
                          (append ands (list (cons 'progn (append body '(t)))))
                          ands)))
            (if (cdr full)
                (cons 'and full)
                (car full))))))

;;;}}}
;;;==================================================================

;;; macros.el ends here
