;;; zmacs-citation.el --- Citation/bibliography packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:28:49
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

(eval-when-compile
  (require 'cl-lib))

;;;; Variables:

(defcustom zmacs-bibliography-files
  (list (expand-file-name (concat zmacs-storage-directory "user.bib"))
        (expand-file-name (concat zmacs-storage-directory "books.bib")))
  "User bibliography files."
  :group 'zmacs-emacs
  :tag "Bibliography databases")

(defvar *zmacs-bib-notes-directory*
  (expand-file-name (concat zmacs-storage-directory "bib/")))

(defvar *zmacs-citar-note-template* ""
  "Template for Citar notes.")

(defvar *zmacs-csl-directory*
  (expand-file-name (concat zmacs-storage-directory "csl/")))

;;;; Org-Cite:

(use-package oc
  :ensure nil
  :after org
  :config
  (setq org-cite-global-bibliography `(,zmacs-bibliography-files)
        org-cite-export-processors   '((beamer csl)
                                       (latex  csl)
                                       (t      csl))))

(use-package oc-csl
  :ensure nil
  :after oc
  :config
  (setq org-cite-csl-styles-dir  (concat *zmacs-csl-directory* "styles/")
        org-cite-csl-locales-dir (concat *zmacs-csl-directory* "locales/")))

(use-package citeproc
  :after (oc oc-csl))

;;;; Citar:

(use-package citar
  :commands (citar-open-brief
             citar-open-notes
             citar-insert-citation)
  :hook ((LaTeX-mode . citar-capf-setup)
         (org-mode   . citar-capf-setup))
  :bind (:map citar-map
         ("b" . #'citar-open-brief))
  :custom
  (org-cite-global-bibliography `(,zmacs-bibliography-files))
  (citar-bibliography           `(,zmacs-bibliography-files))
  (org-cite-insert-processor    'citar)
  (org-cite-follow-processor    'citar)
  (org-cite-activate-processor  'citar)
  :config
  (setq citar-at-point-function 'embark-act
        citar-additional-fields '("doi" "url")
        citar-templates
        `((main . " ${=key= id:15} ${title:48}")
          (suffix . "${author editor:30}  ${=type=:12}  ${=beref=:12} ${tags keywords:*}")
          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . ,*zmacs-citar-note-template*)))

  (when (display-graphic-p)
    (setq citar-symbols
          `((file ,(all-the-icons-octicon "file-pdf"
                                          :face 'error)
                  . " ")
            (note ,(all-the-icons-octicon "file-text"
                                          :face 'warning)
                  . " ")
            (link ,(all-the-icons-octicon "link-external"
                                          :face 'org-link)
                  . " "))))

  (setq citar-notes-paths `(,*zmacs-bib-notes-directory*)))

;;;; Provide package:

(provide 'zmacs-citation)

;;; zmacs-citation.el ends here.
