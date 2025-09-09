;;; zmacs-prog-json.el --- JSON packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    28 Oct 2024 10:32:12
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

(eval-when-compile
  (require 'cl-lib))

(defun zmacs-json-prettify-dwim ()
  "Prettify JSON."
  (interactive)
  (if (use-region-p)
      (json-pretty-print-ordered (use-region-beginning)
                                 (use-region-end))
    (json-pretty-print-buffer-ordered)))

(use-package json-ts-mode
  :ensure nil
  :bind (:map json-ts-mode-map
         ([remap fill-paragraph] . zmacs-json-prettify-dwim)))

(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

(provide 'zmacs-prog-json)

;;; zmacs-prog-json.el ends here.
