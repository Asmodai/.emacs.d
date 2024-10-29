;;; zmacs-prog-common-lisp.el --- Common Lisp packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:36:10
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
(require 'zmacs-prog-lisp)

;;;; Auto-Highlight symbol:

(use-package auto-highlight-symbol
  :defer t
  :hook ((common-lisp-mode      . auto-highlight-symbol-mode)
         (emacs-lisp-mode       . auto-highlight-symbol-mode)
         (lisp-interaction-mode . auto-highlight-symbol-mode))
  :config
  (require 'auto-highlight-symbol)
  (add-to-list 'ahs-plugin-bod-modes 'lisp-mode))

;;;; Snippets:

(use-package common-lisp-snippets
  :defer t
  :after yasnippet)

;;;; Paredit:

(use-package paredit
  :ensure t
  :hook (common-lisp-mode . paredit-mode))

;;;; Tags:

(use-package ggtags
  :defer t)

(use-package counsel-gtags
  :defer t)

;;;; Rainbow delimiters:

(use-package rainbow-identifiers
  :defer t
  :config
  (add-hook 'lisp-mode-hook #'colors//rainbow-identifiers-ignore-keywords))

;;;; Sly:

(use-package sly
  :ensure nil
  :load-path "~/.emacs.d/lisp/extensions/sly"
  :bind (:map sly-mrepl-mode-map
         ("M-<up>"   . #'sly-mrepl-previous-input-or-button)
         ("M-<down>" . #'sly-mrepl-next-input-or-button))
  ;;:custom
  ;;(sly-complete-symbol-function #'sly-flex-completions)
  :init
  (setq sly-contribs '(sly-autodoc
                       sly-fancy
                       sly-fancy-inspector
                       sly-fancy-trace
                       sly-indentation
                       sly-profiler
                       sly-stickers
                       sly-trace-dialog
                       sly-mrepl)
        inferior-lisp-program "sbcl")
  :config
  (sly-setup)
  (setq-default sly-mrepl-pop-sylvester nil)
  (add-hook 'sly-mrepl-mode-hook      #'zmacs-deactivate-smartparens)
  (add-hook 'sly-mrepl-mode-hook      #'zmacs-deactivate-paredit)
  (add-hook 'sly-inspector-mode-hook  'visual-line-mode))

(use-package sly-asdf             :after sly :ensure t)
(use-package sly-named-readtables :after sly :ensure t)
(use-package sly-macrostep        :after sly :ensure t)
(use-package sly-stepper
  :after sly
  :ensure nil
  :load-path "~/.emacs.d/lisp/extensions/sly-stepper")

;;;; Provide package:

(provide 'zmacs-prog-common-lisp)

;;; zmacs-prog-common-lisp.el ends here.
