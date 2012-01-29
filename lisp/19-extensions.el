;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; 19-extensions.el --- Emacs Lisp extensions for Emacs 19.
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 00:50:19 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    16 Feb 2011 10:48:05
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
;;; This package provides some extra functionality for our custom
;;; Emacs 19 packages.  Some of this could be used in Emacs 20 and
;;; higher, but there are probably better versions already hidden away
;;; somewhere :)
;;;
;;;}}}

(eval-when-compile
  (require 'cl))

;;; ==================================================================
;;;{{{ Buffer utilities:

(unless (fboundp 'buffer-file-basename)
  (defun buffer-file-basename (&optional buffer)
    "Return base name of file `buffer' is visiting, or nil if none.
  No argument or nil as argument means use the current buffer."
    (let ((fnam (buffer-file-name buffer)))
      ;; TODO: support Windows.
      (if fnam
          (car (reverse (split-string fnam "/" t)))))))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ String utilities:

(defconst split-string-default-separators "[ \f\t\n\r\v]+"
  "The default value of separators for `split-string'.

A regexp matching strings of whitespace. Should not match non-breaking 
spaces.

Warning: binding this to a different value and using it as default is 
likely to have undesired semantics.")

(defun split-string (string &optional separators omit-nulls)
  "Split STRING into substrings bounded by matches for SEPARATORS.

The beginning and end of STRING, and each match for SEPARATORS, are
splitting points.  The substrings matching SEPARATORS are removed, and the
substrings between the splitting points are collected as a list, which is
returned.

If SEPARATORS is non-nil, it should be a regular expression matching text
which separates, but is not part of, the substrings.  If nil it defaults to
`split-string-default-separators', normally \"[ \\f\\t\\r\\n\\v]+\", and
OMIT-NULLS is forced to t.

If OMIT-NULLS is t, zero-length substrings are omitted from the list \(so
that for the default value of SEPARATORS leading and trailing whitespace
are effectively trimmed).  If nil, all zero-length substrings are retained,
which correctly parses CSV format, for example.

Note that the effect of `(split-string STRING)' is the same as
`(split-string STRING split-string-default-separators t)'.  In the rare case
that you wish to retain zero-length substrings when splitting on whitespace,
use `(split-string STRING split-string-default-separators)'.

Modifies the match data; use `save-match-data' if necessary."
  (let ((keep-nulls (not (if separators omit-nulls t)))
        (rexp (or separators split-string-default-separators))
        (start 0)
        notfirst
        (list nil))
    (while (and (string-match rexp string
                              (if (and notfirst
                                       (= start (match-beginning 0))
                                       (< start (length string)))
                                  (1+ start)
                                  start))
                (< start (length string)))
      (setq notfirst t)
      (if (or keep-nulls (< start (match-beginning 0)))
          (setq list
                (cons (substring string start (match-beginning 0))
                      list)))
      (setq start (match-end 0)))
    (if (or keep-nulls (< start (length string)))
        (setq list
              (cons (substring string start) list)))
    (nreverse list)))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Time/Date utilities:

(defun get-month-number (month)
  "Returns the number of the given `month'; or 0 if the `month' is
somehow unknown."
  (let* ((mnums '(("Jan" . 1)
                  ("Feb" . 2)
                  ("Mar" . 3)
                  ("Apr" . 4)
                  ("May" . 5)
                  ("Jun" . 6)
                  ("Jul" . 7)
                  ("Aug" . 8)
                  ("Sep" . 9)
                  ("Oct" . 10)
                  ("Nov" . 11)
                  ("Dec" . 12)))
         (num (assoc month mnums)))
    (if num
        (cdr num)
        0)))

(defun time-zone-offset (&optional time)
  "Returns the time zone offset of a given `time'.

If `time' is not passed, then the function defaults to the value of
`current-time'."
  (and (fboundp 'current-time-zone)
       (let* ((sec (or (car (current-time-zone
                             (if time
                                 time
                                 (current-time))))
                       0))
              (absmin (/ (abs sec) 60)))
         (format "%c%02d%02d"
                 (if (< sec 0) ?- ?+)
                 (/ absmin 60)
                 (% absmin 60)))))

(defun time-zone-name (&optopnal time)
  "Returns the name of the time zone of a given `time'.

If `time' is not specified, then the function defaults to the value of
`current-time'."
  (cond ((fboundp 'current-time-zone)
         (nth 1 (current-time-zone
                 (if time
                     time
                     (current-time))))
         ((getenv "TZ")
          (substring (getenv "TZ") 0 3)))))

(defun format-time ()
  "Format the time in the RFC-822 format, e.g. \"Thu, 01 Jan 1970
00:00:00 \(UTF\)\"."
  (let* ((time (current-time))
         (datestr
          (let ((str (current-time-string time)))
            (format "%s, %s %s %s %s"
                    (substring str 0 3)
                    (substring str 8 10)
                    (substring str 4 7)
                    (substring str 20 24)
                    (substring str 11 19))))
         (tzoff (time-zone-offset time))
         (tznam (time-zone-name time)))
    (cond ((and tzoff tznam)
           (format "%s %s (%s)" datestr tzoff tznam))
          ((or tzoff tznam)
           (format "%s %s" datestr (or tzoff tznam)))
          (t datestr))))

;;;}}}
;;; ==================================================================

(provide 'extensions)

;;; extensions.el ends here
