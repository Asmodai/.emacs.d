;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; config.el --- Ruby configuration.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2016 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    11 Mar 2016 00:28:29
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

(bootstrap:defvar-company-backends enh-ruby-mode)
(bootstrap:defvar-company-backends ruby-mode)

(defvar *ruby-enable-enh-ruby-mode* t
  "If non-NIL, use `enh-ruby-mode' rather than the built-in Ruby mode.")

(defvar *ruby-version-manager* nil
  "If non-NIL, defines the Ruby version manager.

Possible values are `rbenv', `rvm', or `chruby'.")

(defvar *ruby-test-runner* 'rspec'
  "Test runner to use.

Possible values are `rspec', or `ruby-test'.")

;;; config.el ends here
