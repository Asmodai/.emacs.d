;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; 19-template.el --- Emacs 19 template insertion
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 00:47:34 asmodai>
;;; Revision:   4
;;;
;;; Copyright (c) 1998, 1999 Stefan Hornburg <racke@gundel.han.de>
;;; Copyright (c) 2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    16 Feb 2011 14:47:46
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

(eval-when-compile
  (require 'cl))

(defvar *template-directory*
  (expand-file-name
   ".emacs.d/templates-19/"
   (home-path-for-system))
  "Default directory for templates.")

(defvar *template-wrapper-regexp* "@\\\([^\n@]*\\\)@"
  "Regular expression matching a template instruction.")

(defvar *template-query-regexp* "^>"
  "Regular expression matching a query directive.")

(defvar *template-message-regexp* "&\\(.*\\)\\'"
  "Regular expression matching a message directive.  Applies the
function found in the first submatch to the replacement before
  inserting.")

(defvar *template-instruction-alist*
  '(("address" . user-mail-address)
    ("basename" . (file-name-sans-extension (buffer-file-basename)))
    ("changelogcap" . (concat (current-time-string)
                       "  "
                       (user-full-name)
                       "  <" user-mail-address ">\n"))
    ("copyright" . (template-from-string
                    "Copyright (c) @year@ @fullname@"))
    ("emacs" . (emacs-version))
    ("file" . (buffer-file-basename))
    ("fullname" . (user-full-name))
    ("loginname" . (user-login-name))
    ("snapshot" . (format-snapshot))
    ("system" . (system-name))
    ("time" . (format-time))
    ("year" . (format-year)))
  "Association list with predefined template instructions.
The values can be of several types:

- A function symbol. The return value of the function call is inserted.
- A symbol. The symbol value is inserted.
- A string. The string is inserted.
- A list. Evaluated as lisp expression. The result is inserted")

(defvar *template-history* nil
"A history list for templates.")

(defvar *template-inserted-region* nil
  "Used to record the inserted region.")

(defvar *template-vars-alist* nil
  "Temporary alist with template instructions and their value\(s).")

(defun template-horse ()
  "Replaces buffer part of instruction is defined."
  (let ((instruction (buffer-substring
                      (match-beginning 1)
                      (match-end 1)))
        (begin (match-beginning 0))
        (end (match-end 0))
        (message "identity")
        replacement
        query)
    ;;
    ;; Check for query directive.
    (save-match-data
      (if (string-match *template-query-regexp* instruction)
          (setq query t
                instruction (replace-match "" t t instruction))))
    ;;
    ;; Check for message directive.
    (save-match-data
      (if (string-match *template-message-regexp* instruction)
          (setq message (substring instruction
                                   (match-beginning 0)
                                   (match-end 0))
                instruction (replace-match "" t t instruction))))
    ;;
    ;; Determine value for instruction.
    (setq replacement (template-instruction-value instruction))
    (if (or replacement
            (and query
                 (setq replacement (read-from-minibuffer
                                    (format "Value for `%s': "
                                            instruction)))))
        (progn
          ;;
          ;; Add variable to alist.
          (setq *template-vars-alist*
                (cons (cons instruction replacement)
                      *template-vars-alist*))
          ;;
          ;; Replace buffer part.
          (delete-region begin end)
          (insert (funcall (intern-soft message) replacement))))))

(defun template-from-file (fspec)
  "Replaces variable parts of a template stored in `fspec' and inserts
the results."
  (interactive
   (list (read-file-name "Template file: " *template-directory*)))
  (let ((begin (point))
        end
        endmarker
        result
        range
        instruction
        *template-vars-alist*)
    ;;
    ;; Insert template.
    (setq result (insert-file-contents file))
    (setq end (+ begin (car (cdr result))))
    (setq endmarker (copy-marker end))
    ;;
    ;; Search for potential replacements
    (goto-char begin)
    (while (and (<= (point) endmarker)
                (re-search-forward *template-wrapper-regexp*
                                   endmarker
                                   t))
      (template-horse))
    (goto-char endmarker)
    ;;
    ;; Record the resulting region for further processing.
    (setq *template-inserted-region*
          (list begin endmarker)))
  ;;
  ;; Return empty string to use this function as an instruction
  ;; value.
  "")

(defun template-from-string (str)
  "Replaces variable parts of a template `str' and inserts the
results."
  (interactive
   (list (read-string "Template: " nil '*template-history*)))
  (let ((begin (point))
        end
        endmarker
        result
        range
        instruction
        *template-vars-alist*)
    ;;
    ;; Insert the template.
    (insert str)
    (setq end (point))
    (setq endmarker (copy-marker end))
    ;;
    ;; Search for potential replacements
    (goto-char begin)
    (while (and (<= (point) endmarker)
                (re-search-forward *template-wrapper-regexp*
                                   endmarker
                                   t))
      (template-horse))
    (goto-char endmarker)
    ;;
    ;; Record resulting region for further processing.
    (setq *template-inserted-region*
          (list begin endmarker)))
  ;; Return an empty string to use this function as an instruction
  ;; value.
  "")

(defun template-add-instruction (name value)
  "Adds instruction to `*template-instructions-alist*'."
  (interactive "sInstruction name: \nxValue as Lisp expression: ")
  (setq *template-instructions-alist*
        (cons (cons name value)
              *template-instructions-alist*)))

(defun initialise-template-bindings ()
  "Installs global key bindings for the user functions in this
module."
  (interactive)
  (define-key ctl-x-map "ti" 'template-add-instruction)
  (define-key ctl-x-map "tf" 'template-from-file)
  (define-key ctl-x-map "ts" 'template-from-string)
  (define-key ctl-x-map "tv" 'template-insert-value))

(defun template-instruction-value (name)
  "Returns a value for an instruction named `name'; or nil if no such
instruction exists."
  (let ((definition (cdr-safe (assoc name
                                     *template-instructions-alist*))))
    (cond ((fboundp 'definition)
           (prin1-to-string (funcall definition) t))
          ((not definition)
           (cdr-safe (assoc name *template-vars-alist*)))
          ((symbolp definition)
           definition)
          ((listp definition)
           (prin1-to-string (eval definition) t)))))

(defun format-snapshot ()
  (let ((str (current-time-string (current-time))))
    (format "%s%02d%s"
            (substring str 20 24)
            (get-month-number (substring str 4 7))
            (substring str 8 10))))

(defun format-year ()
  (let ((str (current-time-string (current-time))))
    (format "%s" (substring str 20 24))))

(provide 'template)

;;; template.el ends here

