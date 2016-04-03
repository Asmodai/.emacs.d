;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- JavaScript layer packages.
;;;
;;; Copyright (c) 2016 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    02 Apr 2016 20:44:49
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

(setq javascript-packages '(coffee-mode
                            company
                            company-tern
                            flycheck
                            redspace-mode
                            rainbow-mode
                            indent-guide
                            js-doc
                            js2-mode
                            js2-refactor
                            js-comint
                            json-mode
                            json-snatcher
                            tern
                            web-beautify))

(defun javascript:init-coffee-mode ()
  (use-package coffee-mode
    :defer t
    :init
    (progn
      (defun javascript:coffee-indent ()
        (if (coffee-line-wants-indent)
            (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
          (coffee-insert-spaces (coffee-previous-indent))))

      (add-hook 'coffee-mode-hook
                '(lambda ()
                   (setq indent-line-function 'javascript:coffee-indent))))))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun javascript:post-init-conpany ()
    (bootstrap:add-company-hook js2-mode))

  (defun javascript:init-company-tern ()
    (use-package company-tern
      :if (and (bootstrap-layer:package-used-p 'company)
               (bootstrap-layer:package-used-p 'tern))
      :defer t
      :init
      (push 'company-tern *company-backends-js2-mode*))))

(defun javascript:post-init-flycheck ()
  (dolist (hook '(coffee-mode-hook js2-mode-hook json-mode-hook))
    (bootstrap:add-flycheck-hook hook)))

(defun javascript:init-js-doc ()
  (use-package js-doc
    :defer t
    :init
    (progn
      (defun bootstrap:js-doc-require ()
        "Lazy-load js-doc"
        (require 'js-doc))

      (add-hook 'js2-mode-hook 'bootstrap:js-doc-require))))

(defun javascript:init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      (add-hook 'js2-mode-hook 'js2-imenu-extras-mode))))

(defun javascript:init-js2-refactor ()
  (use-package js2-refactor
    :defer t
    :init
    (progn
      (defun bootstrap:js2-refactor-require ()
        "Lazy-load js2-refactor"
        (require 'js2-refactor))

      (add-hook 'js2-mode-hook 'bootstrap:js2-refactor-require))))

(defun javascript:init-json-mode ()
  (use-package json-mode
    :defer t))

(defun javascript:init-json-snatcher ()
  (use-package json-snatcher
    :defer t))

(defun javascript:init-tern ()
  (use-package tern
    :defer t
    :init (add-hook 'js2-mode-hook 'tern-mode)
    :config
    (progn
      (when *javascript-disable-tern-port-files*
        (add-to-list 'tern-command "--no-port-file" 'append)))))

(defun javascript:init-web-beautify ()
  (use-package web-beautify
    :defer t))

(defun javascript:init-js-comint ()
  (use-package js-comint
    :defer t
    :init
    (let ((node (if (mac-os-x-p)
                    "/opt/local/bin/node"
                  "node")))
      (setq inferior-js-program-command node))))

(defun javascript:post-init-rainbow-mode ()
  (bootstrap:add-to-hooks 'rainbow-mode '(js2-mode-hook)))

(defun javascript:post-init-indent-guide ()
  (bootstrap:add-to-hooks 'indent-guide-mode '(js2-mode-hook
                                               javascript-mode-hook)))

(defun javascript:post-init-redspace-mode ()
  (bootstrap:add-to-hooks 'redspace-mode '(js2-mode-hook
                                           javascript-mode-hook)))

;;; packages.el ends here
