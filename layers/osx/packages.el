;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Support for Mac OS X
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 18:04:07
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

(setq osx-packages
      '(pbcopy
        reveal-in-osx-finder))

(when (mac-os-x-p)
  (if (executable-find "trash")
      (defun system-move-file-to-trash (file)
        "Use the `trash' utility to move FILE to the system trash can."
        (call-process (executable-find "trash") nil 0 nil file))
    (setq *mac-system-move-file-to-trash-use-finder* t))

  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-listing-switches "-aBhl --group-directories-first")))

(defun osx:no-init-pbcopy ()
  (use-package pbcopy
    :if (and (mac-os-x-p)
             (not (display-graphic-p)))
    :init (turn-on-pbcopy)))

(defun osx:no-init-reveal-in-finder ()
  (use-package reveal-in-osx-finder
    :if (mac-os-x-p)
    :commands reveal-in-osx-finder))

;;; packages.el ends here
