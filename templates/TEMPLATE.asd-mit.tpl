;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-USER; Base: 10; Lowercase: Yes -*-
;;;
;;; (>>>FILE<<<) --- (>>>TITLE<<<)
;;;
;;; Copyright (c) (>>>YEAR<<<) (>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)
;;;
;;; Author:     (>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)
;;; Maintainer: (>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)
;;; Created:    (>>>DATE<<<) (>>>TIME<<<)
;;;
;;;{{{ License:
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the “Software”),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
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
  :version          "1.0"
  :author           "(>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)"
  :maintainer       "(>>>USER_NAME<<<) (>>>LITERAL<<<)<(>>>/LITERAL<<<)(>>>AUTHOR<<<)(>>>LITERAL<<<)>(>>>/LITERAL<<<)"
  :license          "MIT License"
  :description      "<fill this in>"
  :long-description "<fill this in>"

  :depends-on ()

  :components
  ((:module :src
    :components (()))))

;;; (>>>FILE<<<) ends here.

>>>TEMPLATE-DEFINITION-SECTION<<<
("TITLE" "Enter a description for this file: " "" "" "")
("PACKAGE" "Package to define (in UPPER CASE): " "" "" "")
