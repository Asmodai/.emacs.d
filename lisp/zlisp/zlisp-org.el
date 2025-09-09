;;; zlisp-org.el --- Org mode stuff  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    31 Oct 2024 13:34:56
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
(require 'org-mode)
(require 'markdown-mode)

(defun zlisp/toggle-display-markup ()
  "Toggle the display of markup in markdown and org modes"
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-toggle-link-display)
    (if markdown-hide-markup
        (markdown-toggle-markup-hiding 0)
      (markdown-toggle-markup-hiding))))

(defun zlisp/fill-paragraph ()
  "If in an org buffer use `org-fill-paragraph'; else use `fill-paragraph'."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (call-interactively #'org-fill-paragraph)
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'zlisp/fill-paragraph)

(defun zlisp/unfill-paragraph (&optional region)
  "Take a multi line paragraph and make it into a single line of text.

If REGION is provided, it shall be the region that is unfilled."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(global-set-key (kbd "M-Q") #'zlisp/unfill-paragraph)


(provide 'zlisp-org)

;;; zlisp-org.el ends here.
