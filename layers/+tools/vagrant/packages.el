;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Packages for Vagrant.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2015 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    05 Dec 2015 15:32:17
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

(setq vagrant-packages (if (not (windows-p))
                           '(vagrant
                             vagrant-tramp)))

(defun vagrant:init-vagrant ()
  (when (not (windows-p))
    (use-package vagrant
      :defer t)))

(defun vagrant:init-vagrant-tramp ()
  (when (not (windows-p))
    (use-package vagrant-tramp
      :defer t
      :init
      (progn
        (defvar *bootstrap-vagrant-tramp-loaded* nil)
        
        (defadvice vagrant-tramp-term (before bootstrap::load-vagrant activate)
          (unless *bootstrap-vagrant-tramp-loaded*
            (vagrant-tramp-add-method)
            (setq *bootstrap-vagrant-tramp-loaded* t)))))))

;;; packages.el ends here
