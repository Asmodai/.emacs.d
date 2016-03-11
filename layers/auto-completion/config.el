;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; config.el --- Auto-completion configuration.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 14:14:39
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

;;;============================================================================
;;;{{{ Company:

(defvar *auto-completion-return-key-behavior* 'complete
  "What the RET key should do when auto-completion menu is active.

Possible values are `complete' or `nil'.")

(defvar *auto-completion-tab-key-behavior* 'cycle
  "What the TAB key should do when auto-completion menu is active.

Possible values are `complete', `cycle', or `nil'.")

(defvar *auto-completion-complete-with-key-sequence* nil
  "Provide a key sequence (string) to complete the current selection.")

(defvar *auto-completion-enable-snippets-in-popup* nil
  "If non-NIL, then show snippets in the auto-completion popup.")

(defvar *auto-completion-enable-sort-by-usage* nil
  "If non-NIL, then suggestions are sorted by usage.")

(defvar *auto-completion-enable-help-tooltip* nil
  "If non-NIL, then the docstring appears in a tooltip.")

(defvar *company-mode-completion-cancel-keywords*
  '("do"
    "then"
    "begin"
    "case")
  "Keywords on which to cancel completion so that you can use RET to complete
without blocking common line endings.")

(defvar *auto-completion-private-snippets-directory* nil
  "Configurable private snippets directory.")

;;;}}}
;;;============================================================================

;;; config.el ends here
