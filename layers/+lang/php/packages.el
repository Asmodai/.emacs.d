;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- PHP packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Dec 2015 16:25:17
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation,
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

(setq php-packages '(company
                     eldoc
                     flycheck
                     indent-guide
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

(defun php:post-init-flycheck ()
  (add-hook 'php-mode-hook 'flycheck-mode))

(defun php:init-php-auto-yasnippets ()
  (use-package php-auto-yasnippets
    :defer t))

(when (not (windows-p))
  (defun php:init-php-extras ()
    (use-package php-extras
      :defer t)))

(defun php:init-php-mode ()
  (use-package php-mode
    :defer t
    :mode ("\\.php\\'" . php-mode)
    :init
    (progn
      (defun unindent-php-closure ()
        "Fix `php-mode' indentation for closures."
        (let ((syntax (mapcar 'car c-syntactic-context)))
          (if (and (member 'arglist-cont-nonempty syntax)
                   (or (member 'statement-block-intro syntax)
                       (member 'brace-list-intro syntax)
                       (member 'block-close syntax)))
              (save-excursion
                (beginning-of-line)
                (delete-char (* (count 'arglist-cont-nonempty syntax)
                                c-basic-offset))))))

      (bootstrap:add-to-hook 'php-mode-hook
                             (lambda ()
                               (setq c-basic-offset 4)
                               (bootstrap:add-to-hook 'c-special-indent-hook
                                                      'unindent-php-closure)))

      (bootstrap:add-to-hook 'php-mode-hook 'php-enable-psr2-coding-style))))

(defun php:init-phpcbf ()
  (use-package phpcbf
    :defer t))

(defun php:init-phpunit ()
  (use-package phpunit
    :defer t))

(defun perl:post-init-indent-guide ()
  (bootstrap:add-to-hooks 'indent-guide-mode '(php-mode-hook)))

;;; packages.el ends here.
