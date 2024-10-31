;;; zlisp-consult.el --- Consult functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    31 Oct 2024 17:19:03
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

(require 'cl-lib)

(defun zlisp/consult-info-emacs ()
  "Search through Emacs info pages."
  (interactive)
  (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

(defun zlisp/consult-info-org ()
  "Search through the Org info page."
  (interactive)
  (consult-info "org"))

(defun zlisp/consult-info-completion ()
  "Search through completion info pages."
  (interactive)
  (consult-info "vertico"
                "consult"
                "marginalia"
                "orderless"
                "embark"
                "corfu"
                "cape"
                "tempel"))

(defun zlisp/consult-line-symbol-at-point ()
  "Evaluate `consult-line' on the symbol at current point."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(provide 'zlisp-consult)

;;; zlisp-consult.el ends here.
