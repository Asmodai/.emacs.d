;;; zlisp-gc.el --- Garbage collection stuff  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    01 Nov 2024 08:01:23
;; URL:        not distributed yet
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY  WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;

;;; Code:

(require 'cl-lib)
(require 'zlisp-timing)


;;;; Variables:

(defvar zlisp-report-gc nil
  "If T, display GC information.")

;;;; GC stats collector:

;; Add a post-GC hook that displays GC information.
;;
;; To enable this at run-time, set `*zmacs-report-gc*' to t.
(let ((gc-time-elapsed 0))

  (defun zlisp/gc-elapsed-time ()
    "Return the total elapsed time spent collecting garbage."
    gc-time-elapsed)

  (defun zlisp/reset-gc-elapsed-time ()
    "Reset the total elapsed time."
    (setq gc-time-elapsed 0))

  (defun zlisp/gc-info ()
    "Report garbage collection metrics."
    (let ((inhibit-message t)
          (garbage-collection-messages zlisp-report-gc)
          (time (if (zerop gc-time-elapsed)
                    0
                  (- gc-elapsed gc-time-elapsed))))
      (when (bound-and-true-p zmacs-report-gc)
        (message "GC Info:  %d (%.03fs) collections so far, %.06fs this run."
                 gcs-done
                 gc-elapsed
                 time))
      (setf gc-time-elapsed gc-elapsed))))

;;;; GC timer:

(defun zlisp/timed-garbage-collect (&optional source)
  "Perform a garbage collection and report on the time taken.

If SOURCE is non-nil, then it is listed as the trigger for the collection."
  (let ((inhibit-message t)
        results)
    (message "GC Info: [%-12s] Collector ran for %.06f seconds"
             (if (not (null source))
                 source
               "system")
             (zlisp/simple-measure-time
              (setq results (garbage-collect))))
    results))

;;;; Functions:

(defun zlisp/after-focus-change-gc ()
  "Perform a garbage collection after focus change."
  (unless (frame-focus-state)
    (run-with-timer 3.0
                    nil
                    (lambda ()
                      (zlisp/timed-garbage-collect "focus-change")))))

;;;; Hooks:
;;;;; Post GC:

;; Have GC print information.
(add-hook 'post-gc-hook #'zlisp/gc-info)

;;;;; Focus change

;; Force a GC on focus change.
(add-function :after after-focus-change-function #'zlisp/after-focus-change-gc)

;;;; Timers:

;; When idle for 15 sec run the GC no matter what.
(defvar zlisp-gc-timer
  (run-with-idle-timer 15
                       t
                       (lambda ()
                         (zlisp/timed-garbage-collect "idle-timer"))))

;;;; Provide package:

(provide 'zlisp-gc)

;;; zlisp-gc.el ends here.
