;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; flycheck-erlang.el --- Erlang checker for Flycheck.
;;;
;;; Copyright (c) 2018 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    21 May 2018 15:19:57
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

(require 'flycheck)

(defmacro make-find-predicate (fmt root &rest body)
  (let ((fmt-symb  (gensym))
        (path-symb (gensym))
        (root-symb (gensym)))
    `(let* ((,path-symb ,root)
            (,fmt-symb  (format ,fmt ,path-symb)))
       (if (file-exists-p ,fmt-symb)
           (let ((it ,fmt-symb))
             ,@body)
         " "))))

(defun find-lib-directory (&optional directory)
  (let (it)
    (make-find-predicate "%s/_build/default/lib"
                         directory
                         it)))

(defun find-include-directory (&optional directory)
  (let (it)
    (make-find-predicate "%s/include"
                         directory
                         it)))

(defun find-checkouts-directory (&optional directory)
  (let (it)
    (make-find-predicate "%s/_checkouts"
                         directory
                         it)))

(defun find-rebar1-deps (&optional directory)
  (let (it)
    (make-find-predicate "%s/deps"
                         directory
                         it)))

(defun find-plts (&optional directory)
  (let ((path (make-find-predicate "%s/_build/default"
                                   directory
                                   it)))
    (first
     (remove-if (lambda (x) (null x))
                (cl-loop for fspec in (directory-files path)
                         collect (when (not (null (string-match "_plt" fspec)))
                                   (format "%s/%s" path fspec)))))))

(defun %have-logic (result then else)
    (if (not (eql "" result))
        then
      else))

(defun have-include-directory (&optional path)
  (%have-logic (find-include-directory path) "-I" " "))

(defun have-lib-directory (&optional path)
  (%have-logic (find-lib-directory path) "-I" " "))

(defun have-checkouts-directory (&optional path)
  (%have-logic (find-checkouts-directory path) "-I" " "))

(defun have-rebar1-deps (&optional path)
  (%have-logic (find-rebar1-deps path) "-I" " "))

(defun have-plts (&optional path)
  (%have-logic (find-plts path) "--plts" " "))

(flycheck-define-checker erlang
  "An Erlang syntax checker using the Erlang interpreter."
  :command ("erlc"
            (eval (have-include-directory (projectile-project-root)))
            (eval (find-include-directory (projectile-project-root)))
            
            (eval (have-rebar1-deps (projectile-project-root)))
            (eval (find-rebar1-deps (projectile-project-root)))

            (eval (have-lib-directory (projectile-project-root)))
            (eval (find-lib-directory (projectile-project-root)))

            (eval (have-checkouts-directory (projectile-project-root)))
            (eval (find-checkouts-directory (projectile-project-root)))
            "-o" temporary-directory
            (option-list "-I" flycheck-erlang-include-path)
            (option-list "-pa" flycheck-erlang-library-path)
            "-Wall"
            source)
  :error-patterns
  ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start (file-name) ":" line ": " (message) line-end))
  :modes erlang-mode
  :next-chekers (erlang-dialyzer))
(add-to-list 'flycheck-checkers 'erlang)

(flycheck-define-checker erlang-dialyzer
  "Erlang synta checker that uses `dialyzer'."
  :command ("dialyzer"
            (eval (have-plts (projectile-project-root)))
            (eval (find-plts (projectile-project-root)))
            "--"
            (eval (have-include-directory (projectile-project-root)))
            (eval (find-include-directory (projectile-project-root)))
            (eval (have-lib-directory (projectile-project-root)))
            (eval (find-lib-directory (projectile-project-root)))
            (option-list "-I" flycheck-erlang-include-path)
            (option-list "-pa" flycheck-erlang-library-path)
            source-original)
  :error-patterns
  ((error line-start (file-name) ":" line ":" (message) line-end))
  :modes erlang-mode)
(add-to-list 'flycheck-checkers 'erlang-dialyzer)

(provide 'flycheck-erlang)

;;; flycheck-erlang.el ends here.
