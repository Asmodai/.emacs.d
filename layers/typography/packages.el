;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Typography packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 21:30:05
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

(setq typography-packages
      '(typo))

(when (version<= "25" emacs-version)
  (push 'tildify typography-packages))

(setq typography-excluded-packages '())

(defun typography:init-type ()
  (use-package typo
    :defer t
    :init
    (progn
      (when *typography-enable-editing*
        (add-hook 'text-mode-hook 'typo-mode))

      (bootstrap:diminish typo-mode " ☺" " T"))
    :config (setq-default typo-language "English")))

(defun typography:init-tildify ()
  (use-package tildify
    :defer t
    :init
    (progn
      (when *typography-enable-editing*
        (add-hook 'text-hook-mode 'tildify-mode))

      (defun typography:tildify-latex-space ()
        "Set tildify for LaTeX."
        (setq-local tildify-space-string "~"))

      (add-hook 'LaTeX-mode-hook 'typography:tildify-latex-space)

      (bootstrap:diminish tildify-mode " ~" " ~"))))

;;; packages.el ends here
