;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; funcs.el --- Auto-completion functions.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 17:27:35
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

(defun bootstrap::auto-completion-set-RET-key-behavior (package)
  "Bind RET key appropriately for the given PACKAGE and value of
`auto-completion-return-key-behavior'."
  (cond ((eq package 'company)
         (let ((map company-active-map))
           (cond ((eq 'complete *auto-completion-return-key-behavior*)
                  (define-key map [return] 'company-complete-selection)
                  (define-key map (kbd "RET") 'company-complete-selection))
                 (t
                  (define-key map [return] 'nil)
                  (define-key map (kbd "RET") 'nil)))))
        (t (message "Not yet implemented for package %S" package))))

(defun bootstrap::auto-completion-set-TAB-key-behavior (package)
  "Bind TAB key appropriately for the given PACKAGE and value of
`auto-completion-tab-key-behavior'."
  (cond ((eq 'company package)
         (let ((map company-active-map))
           (cond
            ((eq 'complete *auto-completion-tab-key-behavior*)
             (define-key map (kbd "TAB") 'company-complete-selection)
             (define-key map (kbd "<tab>") 'company-complete-selection))
            ((eq 'cycle *auto-completion-tab-key-behavior*)
             (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
             (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
             (define-key map (kbd "<S-tab>")
               'bootstrap::company-complete-common-or-cycle-backward)
             (define-key map (kbd "<backtab>")
               'bootstrap::company-complete-common-or-cycle-backward))
            (t
             (define-key map (kbd "TAB") nil)
             (define-key map (kbd "<tab>") nil)))))
        (t (message "Not yet implemented for package %S" package))))

;;; funcs.el ends here
