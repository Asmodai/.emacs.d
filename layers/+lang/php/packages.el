;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- PHP packages.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2015 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    05 Dec 2015 16:25:17
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
;;; This program isdistributed in the hope that it will be
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

(setq php-packages '(company
                     drupal-mode
                     eldoc
                     flycheck
                     php-auto-yasnippets
                     (php-extras :location (recipe
                                            :fetcher github
                                            :repo  "arnested/php-extras"))
                     php-mode
                     phpcbf
                     phpunit))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun php:post-init-company ()
    (bootstrap:add-company-hook php-mode)))

(defun php:init-drupal-mode ()
  (use-package drupal-mode
    :defer t))

(defun php:post-init-flycheck ()
  (add-hook 'php-mode-hook 'flycheck-mode))

(defun php:init-php-auto-yasnippets ()
  (use-package php-auto-yasnippets
    :defer t))

(defun php:init-php-extras ()
  (use-package php-extras
    :defer t))

(defun php:init-php-mode ()
  (use-package php-mode
    :defer t
    :mode ("\\.php\\'" . php-mode)
    :init (bootstrap:add-to-hooks 'redspace-mode
                                  '(php-mode))))

(defun php:init-phpcbf ()
  (use-package phpcbf
    :defer t))

(defun php:init-phpunit ()
  (use-package phpunit
    :defer t))

;;; packages.el ends here
