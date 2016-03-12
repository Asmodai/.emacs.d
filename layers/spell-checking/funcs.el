;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; funcs.el --- Spell checking functions.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 16:18:23
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

(defun spell-checking:add-flyspell-hook (hook)
  "Add `flyspell-mode' to the given HOOK if `*spell-checking-enable-by-default*'
is non-NIL."
  (when *spell-checking-enable-by-default*
    (add-hook hook 'flyspell-mode)))

(defun spell-checking:change-dictionary ()
  "Change the dictionary.

Use the ispell version if `auto-dictionary' is not be used."
  (interactive)
  (if (fboundp 'adict-change-dictionary)
      (adict-change-dictionary)
      (call-interactively 'ispell-change-dictionary)))

;;; funcs.el ends here
