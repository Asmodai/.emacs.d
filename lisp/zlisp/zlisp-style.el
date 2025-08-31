;;; zlisp-style.el --- Org theme.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    31 Aug 2025 17:43:14
;; URL:        not distributed yet
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY  WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;

;;; Code:
;;;; Requirements:

(require 'cl-lib)
(require 'org)


;;;; Variables:

(defvar-local zlisp--org-style-current nil
  "The current Org style in use.")

(defvar-local zlisp--org-style-remaps nil
  "An alist of face remaps.")

(defvar zlisp--org-style-registry nil
  "List of style objects in precedence order.")

;;;; Custom variables:

(defgroup zlisp-org-style nil
  "ZLISP Org style."
  :group 'zlisp)

(defcustom zlisp-org-style-file-variable 'zmacs-org-style
  "File-local variable name used to determine Org style."
  :group 'zlisp-org-style)

;;;; Classes:

(defclass zlisp/org-style ()
  ((name
    :initarg :name
    :type symbol)
   (pred
    :initarg :pred
    :initform (lambda () t))
   (remaps
    :initarg :remaps
    :type list)
   (enter
    :initarg :enter
    :initform nil)
   (exit
    :initarg :exit
    :initform nil))
  (:documentation
   "An Org style."))

;;;; Generics:

(cl-defgeneric zlisp/org-style-enter (style)
  "Invoke the `enter' hook for the Org style STYLE.")

(cl-defgeneric zlisp/org-style-exit (style)
  "Invoke the `exit' hook for the Org style STYLE.")

(cl-defgeneric zlisp/org-style-style (style)
  "String used to identify Org style STYLE.")

;;;; Remap functions:

(defun zlisp--org-style-apply-remaps (pairs)
  "Apply the styles in PAIRS to `face-remapping-alist'."
  (let ((new (mapcar (lambda (pair)
                       (cons (car pair) (list (cdr pair))))
                     pairs)))
    (setq-local face-remapping-alist
                (append new
                        (cl-remove-if (lambda (pair)
                                        (assq (car pair) pairs))
                                      face-remapping-alist)))
    (setq zlisp--org-style-remaps pairs)))

(defun zlisp--org-style-clear-remaps ()
  "Clear Org styles from `face-remapping-alist'."
  (when zlisp--org-style-remaps
    (setq-local face-remapping-alist
                (cl-remove-if (lambda (pair)
                                (assq (car pair) zlisp--org-style-remaps))
                              face-remapping-alist))
    (setq zlisp--org-style-remaps nil)))

;;;; Methods:

(cl-defmethod zlisp/org-style-enter ((obj zlisp/org-style))
  "Invoke the `enter' hook for the Org style OBJ."
  (when-let ((remaps (oref obj remaps)))
    (zlisp--org-style-apply-remaps remaps))
  (when-let ((fn (oref obj enter)))
    (funcall fn)))

(cl-defmethod zlisp/org-style-exit ((obj zlisp/org-style))
  "Invoke the `exit' hook for the Org style OBJ."
  (when (not (null (oref obj remaps)))
    (zlisp--org-style-clear-remaps))
  (when-let ((fn (oref obj exit)))
    (funcall fn)))

;;;; Public functions:

(defun zlisp/org-style-activate (style)
  "Activate the Org style STYLE."
  (when (ignore-errors
          (object-of-class-p zlisp--org-style-current 'zlisp/org-style))
    (zlisp/org-style-exit zlisp--org-style-current))
  (setq zlisp--org-style-current style)
  (when style
    (zlisp/org-style-enter style)))

(defun zlisp/org-style-auto ()
  "Activate the first registered Org style whose style name matches."
  (when (derived-mode-p 'org-mode)
    (let ((chosen (cl-find-if (lambda (s)
                                (when-let ((fn (oref s pred)))
                                  (funcall (oref s pred))))
                              zlisp--org-style-registry)))
      (zlisp/org-style-activate chosen))))

;;;###autoload
(defun zlisp/org-style-switch ()
  "Interactively switch styles in this Org buffer."
  (interactive)
  (let* ((names (mapcar (lambda (style)
                          (symbol-name (oref sytyle name)))
                        zlisp--org-style-registry))
         (pick  (completing-read "Org style: " (cons "none" names) nil t)))
    (zlisp/org-style-activate
     (and (not (string= pick "none"))
          (cl-find-if (lambda (style)
                        (string= (symbol-name (oref style name)) pick))
                      zlisp--org-style-registry)))))

;;;; Org styles:
;;;;; Helpers:

(defun zlisp/in-denote-p ()
  "Return T if the buffer refers to a file within the Denote directory."
  (when (and buffer-file-name *zmacs-notes-directory*)
    (string-prefix-p (file-truename *zmacs-notes-directory*)
                     (file-truename buffer-file-name))))

;;;;; Default `writing' style:

(defvar zlisp/org-style-writing
  (zlisp/org-style :name 'writing
                   :pred (lambda () t)
                   :remaps nil
                   :enter (lambda ()
                            (when (fboundp 'mixed-pitch-mode)
                              (mixed-pitch-mode 1))
                            (visual-line-mode 1)
                            (hl-line-mode 1))
                   :exit (lambda ()
                           (visual-line-mode -1)))
  "Generic Org style.")

;;;;; `org-present' style:

(defvar zlisp/org-style-present
  (zlisp/org-style :name 'present
                   :pred nil
                   :remaps '((default              . zmacs-org-style-present-default)
                             (fixed-pitch          . zmacs-org-style-present-fixed-pitch)
                             (org-document-info    . zmacs-org-style-present-document-info)
                             (org-document-title   . zmacs-org-style-present-document-title)
                             (org-level-1          . zmacs-org-style-present-level-1)
                             (org-level-2          . zmacs-org-style-present-level)
                             (org-level-3          . zmacs-org-style-present-level)
                             (org-level-4          . zmacs-org-style-present-level)
                             (org-level-5          . zmacs-org-style-present-level)
                             (org-level-6          . zmacs-org-style-present-level)
                             (org-level-7          . zmacs-org-style-present-level)
                             (org-level-8          . zmacs-org-style-present-level)
                             (org-code             . zmacs-org-style-present-code)
                             (org-verbatim         . zmacs-org-style-present-verbatim)
                             (org-block            . zmacs-org-style-present-block)
                             (org-block-begin-line . zmacs-org-style-present-block-begin-line)
                             (org-block-end-line   . zmacs-org-style-present-block-end-line))
                   :enter (lambda ()
                            (setq-local line-spacing 0.2))
                   :exit (lambda ()
                           (setq-local line-spacing nil)))
  "`org-present' style.")

;;;;; `org-agenda' style:

(defvar zlisp/org-style-agenda
  (zlisp/org-style :name 'agenda
                   :pred (let ((root (expand-file-name *zmacs-org-directory*)))
                           (lambda ()
                             (and buffer-file-name
                                  (string-prefix-p
                                   root
                                   (file-truename buffer-file-name)))))
                   :remaps '((org-level-1 . zmacs-org-style-agenda-level-1)
                             (org-level-2 . zmacs-org-style-agenda-level-2)
                             (org-level-3 . zmacs-org-style-agenda-level-3)
                             (org-level-4 . zmacs-org-style-agenda-level)
                             (org-level-5 . zmacs-org-style-agenda-level)
                             (org-level-6 . zmacs-org-style-agenda-level)
                             (org-level-7 . zmacs-org-style-agenda-level)
                             (org-level-8 . zmacs-org-style-agenda-level)))
  "`org-agenda' style.")

;;;;; `denote' style:

(defvar zlisp/org-style-denote
  (zlisp/org-style :name 'denote
                   :pred #'zlisp/in-denote-p
                   :remaps '((org-level-1 . zmacs-org-style-denote-level-1)
                             (org-level-2 . zmacs-org-style-denote-level-2)
                             (org-level-3 . zmacs-org-style-denote-level-3)
                             (org-level-4 . zmacs-org-style-denote-level)
                             (org-level-5 . zmacs-org-style-denote-level)
                             (org-level-6 . zmacs-org-style-denote-level)
                             (org-level-7 . zmacs-org-style-denote-level)
                             (org-level-8 . zmacs-org-style-denote-level)))
  "`denote' style.")

;;;; Registry:

(setq zlisp--org-style-registry (list zlisp/org-style-agenda
                                      zlisp/org-style-denote
                                      zlisp/org-style-writing))

;;;; Determining style:

(defun zlisp--org-style-from-file-local ()
  "Attempt to extract the Org style from the file-local var."
  (let ((sym (make-local-variable zlisp-org-style-file-variable)))
    (when (boundp zlisp-org-style-file-variable)
      (let ((val (symbol-value zlisp-org-style-file-variable)))
        (and val
             (intern (format "%s" (downcase val))))))))

(defun zlisp--org-style-from-keywords ()
  "Attempt to extract the Org style from the Org properties."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((kw   (org-collect-keywords '("ZORGSTYLE" "PROPERTY")))
               (zkw  (cdr (assoc "ZORGSTYLE" kw)))
               (pkw  (cdr (assoc "PROPERTY" kw)))
               (prop (when pkw
                       (car (seq-filter (lambda (s)
                                          (string-match-p (rx bos
                                                              "Z_ORG_STYLE"
                                                              (+ space))
                                                          s))
                                        pkw))))
               (raw  (cond (zkw  (car zkw))
                           (prop (cadr (split-string prop "[ \t]+" t))))))
          (and raw
               (intern (downcase raw))))))))

(defun zlisp/org-style-lookup (name)
  "Find a style object by NAME (symbol) in the registry."
  (cl-find-if (lambda (s)
                (eq (oref s name) name))
              zlisp--org-style-registry))

(defun zlisp/org-style-activate-from-buffer ()
  "Activate a style chosen by buffer hints."
  (let* ((choice (or (zlisp--org-style-from-file-local)
                     (zlisp--org-style-from-keywords)))
         (obj    (and choice
                      (zlisp/org-style-lookup choice))))
    (if obj
        (zlisp/org-style-activate obj)
      (zlisp/org-style-auto))))

;;;; Minor mode:

;;;###autoload
(define-minor-mode zlisp/org-styles-auto-mode
  "Auto-select a ZMACS Org style in Org buffers."
  :init-value t
  :global     t
  :lighter    ""
  (if zlisp/org-styles-auto-mode
      (add-hook 'org-mode-hook #'zlisp/org-style-auto)
    (remove-hook 'org-mode-hook #'zlisp/org-style-auto)))

;;;; Provide package:

(provide 'zlisp-style)

;;; zlisp-style.el ends here.
