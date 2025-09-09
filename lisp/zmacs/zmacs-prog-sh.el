;;; zmacs-prog-sh.el --- Shell scripting packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    28 Oct 2024 05:31:56
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

;;;; Main pachage:

(use-package sh-script
  :ensure nil
  :commands sh-script-mode
  :init
  ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
  (dolist (pattern '("\\.zsh\\'"
                     "zlogin\\'"
                     "zlogout\\'"
                     "zpreztorc\\'"
                     "zprofile\\'"
                     "zshenv\\'"
                     "zshrc\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'sh-mode))))

;;;; Flymake via shellcheck:

(use-package flymake-shellcheck
  :if (executable-find "shellcheck")
  :defer t
  :hook (sh-mode . flymake-shellcheck-load)
  :custom
  (flymake-shellcheck-use-file nil))

;;;; Automatically set up the right mode:

(defun zmacs--setup-shell ()
  (when (and buffer-file-name
             (string-match-p "\\.zsh\\'" buffer-file-name))
    (sh-set-shell "zsh")))

(add-hook 'sh-mode-hook 'zmacs--setup-shell)

;;;; Provide package:

(provide 'zmacs-prog-sh)

;;; zmacs-prog-sh.el ends here.
