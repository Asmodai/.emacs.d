;;; zmacs-lang.el --- Non-English language tools and support.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2026 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    30 Jan 2026 05:55:30
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
;;;; Requirements.
(eval-when-compile
  (require 'cl-lib))

;;;; Kanji:

(use-package kanji-mode
  ;; Please note this requires Kakasi to be installed.
  :if (executable-find "kakasi")
  :ensure t
  :defer nil)

;;;; StarDict:

(use-package sdcv
  :if (executable-find "sdcv")
  :ensure t
  :defer nil)

(use-package quick-sdcv
  :if (executable-find "sdcv")
  :ensure t
  :defer nil
  :custom
  (quick-sdcv-dictionary-data-dir (expand-file-name
                                   (concat user-emacs-directory "stardict")))
  :bind (("C-c d" . quick-sdcv-search-at-point)
         ("C-c D" . quick-sdcv-search-input)))

(provide 'zmacs-lang)

;;; zmacs-lang.el ends here.
