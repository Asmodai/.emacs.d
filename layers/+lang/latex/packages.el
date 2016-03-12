;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- LaTeX packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 21:15:52
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

(setq latex-packages
      '(auctex
        auctex-latexmk
        company
        company-auctex
        (reftex :location built-in)
        (bibtex :location built-in)
        flycheck
        flyspell
        smartparens
        typo
        yasnippet
        which-key))

(defun latex:init-auctex ()
  (use-package tex
    :defer t
    :init
    (progn
      (setq TeX-command-default               *latex-build-command*
            TeX-auto-save                     t
            TeX-parse-self                    t
            TeX-syntactic-comment             t
            TeX-source-correlate-start-server nil
            LaTeX-fill-break-at-separators    nil)

      (when *latex-enable-auto-fill*
        (add-hook 'LaTeX-mode-hook 'latex:auto-fill-mode))
      (when *latex-enable-folding*
        (add-hook 'LaTeX-mode-hook 'TeX-fold-mode))

      (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
      (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
      (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode))))

(defun latex:init-reftex ()
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX '(nil nil t t t)
        reftex-use-fonts t))

(defun latex:init-company-auxtex ()
  (use-package company-auctex
    :if (bootstrap-layer:package-used-p 'company)
    :defer t
    :init
    (progn
      (push 'company-auctex-labels *company-backends-LaTeX-mode*)
      (push 'company-auctex-bibs *company-backends-LaTeX-mode*)
      (push '(company-auctex-macros
              company-auctex-symbols
              company-auctex-environments)
            *company-backends-LaTeX-mode*))))

(defun latex:post-init-flycheck ()
  (bootstrap:add-flycheck-hook 'LaTeX-mode-hook))

(defun latex:post-init-flyspell ()
  (bootstrap:add-flycheck-hook 'LaTeX-mode-hook))

(defun latex:post-init-type ()
  (defun bootstrap::disable-typo-mode ()
    (typo-mode -1))
  (add-hook 'LaTeX-mode-hook 'bootstrap:load-yasnippet))

(defun latex:post-init-which-key ()
  (push '("\\`latex/font-\\(.+\\)\\'" . "\\1")
        which-key-description-replacement-alist))

;;; packages.el ends here
