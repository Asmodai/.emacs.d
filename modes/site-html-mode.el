;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-html-mode.el --- HTML mode hacks.
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 00:41:37 asmodai>
;;; Revision:   11
;;;
;;; Copyright (c) 2006-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Mar 2006 17:18:10
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This code is free software; you can redistribute it and/or modify
;;; it under the terms of the version 2.1 of the GNU Lesser General
;;; Public License as published by the Free Software Foundation, as
;;; clarified by the Franz preamble to the LGPL found in
;;; http://opensource.franz.com/preamble.html.
;;;
;;; This code is distributed in the hope that it will be useful, but
;;; without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; Version 2.1 of the GNU Lesser General Public License can be found
;;; at http://opensource.franz.com/license.html. If it is not present,
;;; you can access it from http://www.gnu.org/copyleft/lesser.txt
;;; (until superseded by a newer  version) or write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite  330, Boston, MA
;;; 02111-1307  USA
;;;
;;;}}}
;;;
;;;{{{ Commentary:
;;;
;;;}}}

(autoload 'css-mode "css-mode")

(add-hook 'html-mode-hook #'(lambda ()
                              (setq ispell-extra-args "-H")))

(add-hook 'html-helper-timestamp-hook 'html-helper-default-insert-timestamp)

;;; ==================================================================
;;;{{{ HTML Tidy (Unix only):

(if unix-p
    (defun tidy-buffer ()
      "Run HTML Tidy on the current buffer."
      (interactive)
      (if (get-buffer "tidy-errors")
          (kill-buffer "tidy-errors"))
      (shell-command-on-region
       (point-min) (point-max)
       "/usr/bin/tidy -f /tmp/tidy-errors -q -i -wrap 72 -c" t)
      (find-file-other-window "/tmp/tidy-errors")
      (other-window 1)
      (delete-file "/tmp/tidy-errors")
      (message "HTML Tidy applied to buffer.")))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ htmlize/htmlfontify:

(eval-and-compile
  (progn
    (require 'htmlize)
    (require 'cl)))

(defvar *avail-uri-lookup-hooks-table*
  (make-hash-table)
  "Associated list of all available URI lookup hooks.")

(defvar *uri-lookup-hooks* nil
  "URI lookup hooks for current mode.")

;;; We overwrite `htmlize-css-insert-text' to extract and place proper links for
;;; builtins.
(defun htmlize-css-insert-text (text fstruct-list buffer)
  ;; Insert TEXT colored with FACES into BUFFER.  In CSS mode, this is
  ;; easy: just nest the text in one <span class=...> tag for each
  ;; face in FSTRUCT-LIST.
  (let (keyword-uri)
    (dolist (fstruct fstruct-list)
      (princ "<span class=\"" buffer)
      (princ (htmlize-fstruct-css-name fstruct) buffer)
      (let ((hooks *uri-lookup-hooks*)
            (text (downcase text)))
        (while (and (null keyword-uri) hooks)
          (setq keyword-uri (funcall (first hooks) text)
                hooks (rest hooks))))
      (princ "\">" buffer))
    (when keyword-uri
      (princ "<a href=\"" buffer)
      (princ keyword-uri buffer)
      (princ "\">" buffer))
    (princ text buffer)
    (when keyword-uri
      (princ "</a>" buffer)))
  (dolist (fstruct fstruct-list)
    (ignore fstruct)                    ; shut up the byte-compiler
    (princ "</span>" buffer)))

;;;
;;; Compile/load htmlize symbols.
(compile-load "htmlize-symbols")
(compile-load "htmlize-symbols-cl-mop")
(compile-load "htmlize-symbols-common-lisp")
(compile-load "htmlize-symbols-emacs-lisp")

(defun htmlize-buffer-with-hyperlinks (&optional buffer)
  (interactive)
  (let* ((*uri-lookup-hooks* (gethash major-mode
                                      *avail-uri-lookup-hooks-table*))
         (htmlbuf (with-current-buffer (or buffer (current-buffer))
                    (htmlize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer htmlbuf))
    htmlbuf))

(setq htmlize-html-charset "iso-8859-1"
      htmlize-html-major-mode 'html-mode
      htmlize-head-tags 
       "<meta http-equiv=\"author\" content=\"Paul Ward\" />
<meta http-equiv=\"generator\" content=\"Emacs + htmlize.el\" />")

(add-hook 'htmlize-before-hook 'font-lock-fontify-buffer)
(add-hook 'htmlize-after-hook '(lambda ()
                                (font-lock-fontify-buffer)))

;;;}}}
;;; ==================================================================

;;; site-html-mode.el ends here
