;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- HTML packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 18:37:36
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

(setq html-packages
      '(company
        company-web
        css-mode
        emmet-mode
        flycheck
        haml-mode
        helm-css-scss
        jade-mode
        less-css-mode
        rainbow-delimiters
        sass-mode
        scss-mode
        slim-mode
        smartparens
        tagedit
        web-mode
        yasnippet))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun html:post-init-company ()
    (bootstrap:add-company-hook css-mode)
    (bootstrap:add-company-hook jade-mode)
    (bootstrap:add-company-hook slim-mode)
    (bootstrap:add-company-hook web-mode))

  (defun html:init-company-web ()
    (use-package company-web)))

(defun html:init-company-web ()
  (use-package company-web))

(defun html:init-css-mode ()
  (use-package css-mode
    :defer t
    :init
    (progn
      (push 'company-css *company-backends-css-mode*)
      (put 'css-indent-offset 'safe-local-variable #'integerp)

      (defun css-expand-statement ()
        "Expand a CSS block."
        (interactive)
        (save-excursion
          (end-of-line)
          (search-backeard "{")
          (forward-char 1)
          (while (or (eobp)
                     (not (looking-at "}")))
            (let ((beg (point)))
              (newline)
              (search-forward ";")
              (indent-region beg (point))))
          (newline)))

      (defun css-contract-statement ()
        "Contract a CSS block."
        (interactive)
        (end-of-line)
        (search-backward "}")
        (while (not (looking-at "}"))
          (join-line -1))))))

(defun htnl:init-emmet-mode ()
  (use-package emmet-mode
    :defer t
    :init (bootstrap:add-to-hooks 'emmet-mode '(css-mode-hook
                                                html-mode-hook
                                                web-mode-hook))
    :config
    (bootstrap:hide-lighter emmet-mode)))

(defun html:post-init-flycheck ()
  (dolist (hook '(html-mode-hook
                  jade-mode-hook
                  less-mode-hook
                  sass-mode-hook
                  scss-mode-hook
                  slim-mode-hook
                  web-mode-hook))
    (bootstrap:add-flycheck-hook hook)))

(defun html:init-haml-mode ()
  (use-package haml-mode
    :defer t))

(defun html:init-helm-css-scss ()
  (use-package helm-css-scss
    :defer t))

(defun html:init-jade-mode ()
  (use-package jade-mode
    :defer t))

(defun html:init-less-css-mode ()
  (use-package less-css-mode
    :defer t
    :mode ("\\.less\\'" . less-css-mode)))


(defun html:init-sass-mode ()
  (use-package sass-mode
    :defer t
    :mode ("\\.scss\\'" . sass-mode)))

(defun html:init-slim-mode ()
  (use-package slim-mode
    :defer t))

(defun html:post-init-smartparens ()
  (bootstrap:add-to-hooks
   (if *bootstrap-smartparens-strict-mode*
       'smartparens-strict-mode
     'smartparens-mode)
   '(css-mode-hook
     scss-mode-hook
     sass-mode-hook
     less-css-mode-hook))

  (with-eval-after-load 'smartparens
    (setq web-mode-enable-auto-pairing nil)
    (sp-local-pair 'web-mode "<%= " " %>")
    (sp-local-pair 'web-mode "<%# " " %>")
    (sp-local-pair 'web-mode "<%$ " " %>")
    (sp-local-pair 'web-mode "<%@ " " %>")
    (sp-local-pair 'web-mode "<%: " " %>")
    (sp-local-pair 'web-mode "<% "  " %>")
    (sp-local-pair 'web-mode "{ "   " }")
    (sp-local-pair 'web-mode "{{ "  " }}")
    (sp-local-pair 'web-mode "{% "  " %}")
    (sp-local-pair 'web-mode "{%- " " %}")
    (sp-local-pair 'web-mode "{# "  " #}")))

(defun html:init-tagedit ()
  (use-package tagedit
    :defer t
    :config
    (progn
      (tagedit-add-experimental-features)
      (add-hook 'html-mode-hook (lambda ()
                                  (tagedit-mode 1)))
      (bootstrap:diminish tagedit-mode " Ⓣ" " T"))))

(defun html:post-init-rainbow-delimiters ()
  (bootstrap:add-to-hooks 'rainbow-delimiters-mode '(haml-mode-hook
                                                     jade-mode-hook
                                                     less-css-mode-hook
                                                     scss-mode-hook
                                                     slim-mode-hook)))

(defun html:init-web-mode ()
  (use-package web-mode
    :defer t
    :init (push '(company-web-html company-css) *company-backends-web-mode*)
    :mode (("\\.phtml\\'"      . web-mode)
           ("\\.tpl\\.php\\'"  . web-mode)
           ("\\.twig\\'"       . web-mode)
           ("\\.html\\'"       . web-mode)
           ("\\.[gj]sp\\'"     . web-mode)
           ("\\.as[cp]x?\\'"   . web-mode)
           ("\\.eex\\'"        . web-mode)
           ("\\.erb\\'"        . web-mode)
           ("\\.mustache\\'"   . web-mode)
           ("\\.handlebars\\'" . web-mode)
           ("\\.hbs\\'"        . web-mode)
           ("\\.eco\\'"        . web-mode)
           ("\\.ejs\\'"        . web-mode)
           ("\\.djhtml\\'"     . web-mode))))

(defun html:post-init-yasnippet ()
  (bootstrap:add-to-hooks 'bootstrap:load-yasnippet '(css-mode-hook
                                                      jade-mode
                                                      slim-mode)))

;;; packages.el ends here
