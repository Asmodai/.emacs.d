;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; (>>>FILE<<<) --- (>>>TITLE<<<)
;;;
;;; Copyright (c) (>>>YEAR<<<) (>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)
;;;
;;; Author:     (>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)
;;; Maintainer: (>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)
;;; Created:    (>>>DATE<<<) (>>>TIME<<<)
;;; Keywords:   (>>>1<<<)
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This code is free software; you can redistribute it and/or modify
;;; it under the terms of the version 2.1 of the GNU Lesser General
;;; Public License as published by the Free Software Foundation, as
;;; clarified by the Franz preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful, but
;;; without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be found
;;; at http://opensource.franz.com/license.html. If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt
;;; (until superseded by a newer  version) or write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite  330, Boston, MA
;;; 02111-1307  USA
;;;
;;;}}}
;;;{{{ Commentary:
;;;
;;;}}}

#+genera
(error "Please do not load this file into a Symbolics system.
This is only for Common Lisp systems that support ASDF.")

(in-package #:common-lisp-user)

(defpackage (>>>PACKAGE<<<)-system
  (:use #:asdf
        #:common-lisp))

(in-package #:(>>>PACKAGE<<<)-system)

(defsystem (>>>PACKAGE<<<)
    :name "(>>>TITLE<<<)"
    :author "(>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)"
    :version "1.0"
    :maintainer "(>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)"
    :license "Lisp Lesser General Public License (LLGPL)"
    :description "<fill this in>"
    :long-description "<fill this in>"

    :depends-on ()

    :components
    ((:module :src
              :components
	      ())))

;;; (>>>FILE<<<) ends here.

>>>TEMPLATE-DEFINITION-SECTION<<<
("TITLE" "Enter a description for this file: " "" "" "")
("PACKAGE" "Package to define (in UPPER CASE): " "" "" "")
