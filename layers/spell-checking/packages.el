;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Spell checking packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 16:20:40
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

(setq spell-checking-packages
      '(auto-dictionary
        flyspell
        helm-flyspell))

(defun spell-checking:init-auto-dictionary ()
  (use-package auto-dictionary
    :defer t
    :if *spell-checking-enable-auto-dictionary*
    :init
    (progn
      (add-hook 'flyspell-mode-hook 'auto-dictionary-mode)

      (defun bootstrap::adict-set-local-dictionary ()
        "Set the local dictionary."
        (when (and (fboundp 'adict-change-dictionary)
                   ispell-local-dictionary)
          (adict-change-dictionary ispell-local-dictionary)))

      (add-hook 'auto-dictionary-mode-hook
                'bootstrap::adict-set-local-dictionary 'append))))

(defun spell-checking:init-flyspell ()
  (use-package flyspell
    :defer t
    :commands (spell-checking:change-dictionary)
    :init
    (progn
      (spell-checking:add-flyspell-hook 'text-mode-hook)

      (when *spell-checking-enable-by-default*
        (add-hook 'prog-mode-hook 'flyspell-prog-mode)))
    :config
    (bootstrap:diminish flyspell-mode " Ⓢ" " S")))

(defun spell-checking:init-helm-flyspell ()
  (use-package helm-flyspell
      :commands helm-flyspell-correct))

;;; packages.el ends here
