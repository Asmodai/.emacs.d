;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; toolbar.el --- Toolbar hacks
;;;
;;; Time-stamp: <Tuesday Feb 15, 2011 17:23:06 asmodai>
;;; Revision:   1
;;;
;;; Copyright (c) 2005-2011 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Mon Jun 06 02:52:50 2005
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
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
;;; }}}
;;; {{{ Commentary:
;;;
;;; }}}

;;; Set up the toolbar
(when (featurep 'toolbar)
  (setq toolbar-info-use-separate-frame nil
        toolbar-mail-reader 'gnus
        toolbar-news-use-separate-frame nil)
  (customize-set-variable 'toolbar-captioned-p nil))

;;; Location of toolbar icons
(defvar my-toolbar-icon-directory
  (expand-file-name ".emacs.d/icons/"
                    (home-path-for-system))
  "Location of XEmacs toolbar icons.")

(defun tb-make-button-action (file)
  "Create a toolbar button action."
  (setq file (expand-file-name file my-toolbar-icon-directory))
  (if (file-readable-p file)
      (toolbar-make-button-list file)
      (error "Cannot find pixmap %s" file)))

;;; List of toolbar buttons.
(setq edit-toolbar-added-buttons-alist 'nil)

;;; Initial values
(mapcar #'(lambda (c)
            (setf (symbol-value (car c))
                  (toolbar-make-button-list (cdr c))))
        edit-toolbar-added-buttons-alist)

;; New toolbar redo function
(defun toolbar-redo ()
  (interactive)
  (call-int$eractively 'redo))

;;; Toolbar button layout

(defconst toolbar-file-icon (tb-make-button-action "filenew.xpm"))
(defconst toolbar-folder-icon (tb-make-button-action "fileopen.xpm"))
(defconst toolbar-disk-icon (tb-make-button-action "filesave.xpm"))
(defconst toolbar-printer-icon (tb-make-button-action "fileprint.xpm"))
(defconst toolbar-cut-icon (tb-make-button-action "editcut.xpm"))
(defconst toolbar-copy-icon (tb-make-button-action "editcopy.xpm"))
(defconst toolbar-paste-icon (tb-make-button-action "editpaste.xpm"))
(defconst toolbar-undo-icon (tb-make-button-action "undo.xpm"))
(defconst toolbar-redo-icon (tb-make-button-action "redo.xpm"))
(defconst toolbar-spell-icon (tb-make-button-action "spellcheck.xpm"))
(defconst toolbar-search-icon (tb-make-button-action "find.xpm"))
(defconst toolbar-compile-icon (tb-make-button-action "compile.xpm"))
(defconst toolbar-debug-icon (tb-make-button-action "debug.xpm"))
(defconst toolbar-info-icon (tb-make-button-action "help.xpm"))

;;; Toolbar structure
(set-specifier
 default-toolbar
 '([toolbar-file-icon toolbar-open t "Open a file"] 
   [toolbar-folder-icon toolbar-dired t "Edit a directory"] 
   [toolbar-disk-icon toolbar-save t "Save buffer"] 
   [toolbar-printer-icon toolbar-print t "Print buffer"] 
   [toolbar-cut-icon toolbar-cut t "Kill region"] 
   [toolbar-copy-icon toolbar-copy t "Copy region"] 
   [toolbar-paste-icon toolbar-paste t "Paste from clipboard"] 
   [toolbar-undo-icon toolbar-undo t "Undo"]
   [toolbar-redo-icon toolbar-redo t "Redo"]
   [toolbar-spell-icon toolbar-ispell t "Check spelling"] 
   [toolbar-search-icon toolbar-replace t "Search & Replace"] 
   [toolbar-compile-icon toolbar-compile t "Start a compilation"] 
   [toolbar-debug-icon toolbar-debug t "Start a debugger"] 
   nil
   [toolbar-info-icon toolbar-info t "Don't Panic!!"]))

;;; toolbar.el ends here

