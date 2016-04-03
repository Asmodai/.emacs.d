;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; bootstrap-funs.el --- Various functions.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    03 Apr 2016 06:17:07
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

(defvar *bootstrap-init-redisplay-count* 0
  "The number of calls to `redisplay'")

(defun bootstrap:redisplay ()
  "`redisplay' wrapper."
  (incf *bootstrap-init-redisplay-count*)
  (redisplay))

(defun bootstrap:list-variables (&optional prefix suffix)
  (setq prefix (or prefix
                   "*bootstrap-"))
  (setq suffix (or suffix
                   ""))
  (all-completions "" obarray
                   (lambda (x)
                     (let ((name (symbol-name x)))
                       (and (boundp x)
                            (not (keywordp x))
                            (string-prefix-p prefix name)
                            (string-suffix-p suffix name))))))

(defun bootstrap:paths ()
  (let ((buffer "*bootstrap-paths*"))
    (with-current-buffer (get-buffer-create buffer)
      (interactive)
      (erase-buffer)
      (insert
       (string-join
        (mapcar (lambda (x)
                  (format "%s = %s\n"
                          (symbol-name (intern x))
                          (symbol-value (intern x))))
                (bootstrap:list-variables "+bootstrap-"
                                          "-directory+")))))
    (switch-to-buffer buffer)))

(defun bootstrap:load-or-install-protected-package
    (pkg &optional log file-to-load)
  (push pkg *bootstrap-layer-protected-packages*)
  (bootstrap:load-or-install-package pkg log file-to-load))

(defun bootstrap:load-or-install-package
    (pkg &optional log file-to-load)
  (let ((warning-minimum-level :error))
    (condition-case nil
        (require pkg)
      (error
       (require 'cl)
       (let ((pkg-elpa-dir (bootstrap::get-package-directory pkg)))
         (if pkg-elpa-dir
             (add-to-list 'load-path pkg-elpa-dir)
           (when log
             (bootstrap-buffer:append
              (format "(Bootstrap) Installing %s...\n" pkg))
             (bootstrap:redisplay))
           (package-refresh-contents)
           (package-install pkg)
           (setq pkg-elpa-dir (bootstrap::get-package-directory pkg)))
         (require pkg nil 'noerror)
         (when file-to-load
           (load-file (concat pkg-elpa-dir file-to-load)))
         pkg-elpa-dir)))))

(defun bootstrap::get-package-directory (pkg)
  (let ((elpa-dir (concat user-emacs-directory "elpa/")))
    (when (file-exists-p elpa-dir)
      (let ((dir (reduce (lambda (x y)
                           (if x
                               x
                             y))
                         (mapcar (lambda (x)
                                   (when (string-match (concat "/"
                                                               (symbol-name pkg)
                                                               "-[0-9]+")
                                                       x)
                                     x))
                                 (directory-files elpa-dir 'full))
                         :initial-value nil)))
        (when dir
          (file-name-as-directory dir))))))

(defun bootstrap:mplist-get (plist prop)
    "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail)
                (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail)
                (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun bootstrap:mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.

If there are multiple properties with the same keyword, only the first property
and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail)
                (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail)
                  (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

(defmacro bootstrap:symbol-value (symbol)
  "Return the value of SYMBOL corresponding to a dotspacemacs variable.
If SYMBOL value is `display-graphic-p' then return the result of
 `(display-graphic-p)', otherwise return the value of the symbol."
  `(if (eq 'display-graphic-p ,symbol)
       (display-graphic-p)
       ,symbol))

(defun bootstrap::image-p (object)
  "Tests whether the given object is an image (a list whose
first element is the symbol `image')."
  (and (listp object)
       object
       (eq 'image (car object))))

(defun bootstrap::intersperse (seq separator)
  "Returns a list with `SEPARATOR' added between each element
of the list `SEQ'."
  (cond
   ((not seq) nil)
   ((not (cdr seq)) seq)
   (t (append (list (car seq) separator)
              (bootstrap::intersperse (cdr seq) separator)))))

(defun bootstrap::mode-line-nonempty (seg)
  "Checks whether a modeline segment (classical Emacs style)
is nonempty."
  (let ((val (format-mode-line seg)))
    (cond ((listp val) val)
          ((stringp val) (< 0 (length val)))
          (t))))

(defun bootstrap:add-to-hooks (fun hooks)
  "Add a function to the given hooks."
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun bootstrap:add-to-hook (hook &rest funs)
  "Add all given functions to a hook."
  (dolist (fun funs)
    (add-hook hook fun)))

(defun bootstrap:add-all-to-hook (hook funs)
  "Add a list of functions to a hook"
  (bootstrap:add-to-hook hook funs))

(defun bootstrap:echo (msg &rest args)
  "Display MSG in echo area without logging it."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(provide 'bootstrap-funs)

;;; bootstrap-funs.el ends here
