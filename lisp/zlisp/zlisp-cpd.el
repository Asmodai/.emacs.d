;;; zlisp-cpd.el --- Continuing Professional Development stuff.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    01 Sep 2025 09:59:21
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
(require 'hydra)

;;;; Custom variables:

(defgroup zlisp-cpd nil
  "CPD Capture."
  :group 'zlisp)

(defcustom zlisp-cpd-file (expand-file-name
                           (concat zmacs-org-directory "cpd.org"))
  "File for CPD entries."
  :type 'file
  :tag "CPD portfolio file"
  :group 'zlisp-cpd)

(defcustom zlisp-cpd-heading "CPD Log"
  "Top-level heading under which CPD entries are filed."
  :type 'string
  :tag "CPD heading"
  :group 'zlisp-cpd)

(defcustom zlisp-cpd-tags "go|lisp|computer-science|first-aid|c|cplusplus|dart|rust|html|reading|research"
  "A list of tags used for CPD entries."
  :type 'string
  :tag "CPD tags"
  :group 'zlisp-cpd)

;;;; Utilities:

(defun zlisp--cpd-tags ()
  "Return a |-delimited list of tags."
  (concat "Tags|" zlisp-cpd-tags))

;;;; Template generation:

(defun zlisp--cpd-template (&optional default-type default-workload)
  "Return a CPD capture template string.

If provided, DEFAULT-TYPE will specify the type.
If provided, DEFAULT-WORKLOAD will specify the workload."
  (format
   (concat
    "** TODO %%^{Title} :cpd:%%^{" (zlisp--cpd-tags) "}:\n"
    ":PROPERTIES:\n"
    ":type:     %%^{Type|%s|Course/Workshop|Conference|Mentoring|Teaching|Reading}\n"
    ":workload: %%^{Workload|%s|Research|Design|Implementation|Study}\n"
    ":created:  %%U\n"
    ":skills:   %%^{Skills|Go|Performance Optimisation}\n"
    ":category: %%^{Category|Software Engineering}\n"
    ":evidence: %%^{Evidence (URL/file)|}\n"
    ":END:\n"
    ":LOGBOOK:\n"
    ":END:\n\n"
    "*** Description\n"
    "%%?\n\n"
    "*** Outcomes and Objectives\n"
    " - \n"
    " - \n\n"
    "*** Next Steps\n"
    " - Apply to: %%^{Apply where|current project|blog post|team talk|tooling}\n"
    " - Explore: %%^{Explore next|benchmarks|alternatives|papers}\n")
   (or default-type "Self-Directed")
   (or default-workload "Design & Implementation")))

;;;; `org-capture' templates:

(defun zlisp/cpd-install-templates ()
  "Install CPD templates to `org-capture'."
  (add-to-list 'org-capture-templates
               `("p" "CPD" entry
                 (file+heading ,zlisp-cpd-file ,zlisp-cpd-heading)
                 ,(zlisp--cpd-template)
                 :empty-lines 1
                 :jump-to-captured t
                 :clock-in nil
                 :clock-resume nil))
  (add-to-list 'org-capture-templates
               `("ps" "CPD: Self-Directed" entry
                 (file+headline ,zlisp-cpd-file ,zlisp-cpd-heading)
                 ,(zlisp--cpd-template "Self-Directed")
                 :empty-lines 1
                 :jump-to-captured t
                 :clock-in nil
                 :clock-resume nil))
  (add-to-list 'org-capture-templates
               `("pc" "CPD: Course/Workshop" entry
                 (file+headline ,zlisp-cpd-file ,zlisp-cpd-heading)
                 ,(zlisp--cpd-template "Course/Workshop")
                 :empty-lines 1
                 :jump-to-captured t
                 :clock-in nil
                 :clock-resume nil))
  (add-to-list 'org-capture-templates
               `("pf" "CPD: Conference/Talk" entry
                 (file+headline ,zlisp-cpd-file ,zlisp-cpd-heading)
                 ,(zlisp--cpd-template "Conference")
                 :empty-lines 1
                 :jump-to-captured t
                 :clock-in nil
                 :clock-resume nil))
  (add-to-list 'org-capture-templates
               `("pr" "CPD: Reading" entry
                 (file+headline ,zlisp-cpd-file ,zlisp-cpd-heading)
                 ,(zlisp--cpd-template "Reading")
                 :empty-lines 1
                 :jump-to-captured t
                 :clock-in nil
                 :clock-resume nil)))

;;;; Hydra:

(defhydra zlisp-cpd-hydra (:color (zmacs--get-ui-colour :bright-teal)
                           :hint  nil)
  "
CPD capture:

 _s_ Self-directed  _c_ Course/Workshop  _f_ Conference/Talk
 _q_ Quit
"
  ("s" (org-capture nil "ps"))
  ("c" (org-capture nil "pc"))
  ("f" (org-capture nil "pf"))
  ("q" nil "quit"))

(global-set-key (kbd "C-c c p") #'zlisp-cpd-hydra/body)

;;;; Provide package:

(provide 'zlisp-cpd)

;;; zlisp-cpd.el ends here.
