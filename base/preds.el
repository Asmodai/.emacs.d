;;; -*- Mode: Emacs-Lisp; byte-compile-dynamic-docstrings:t; byte-compile-dynamic: t -*-
;;;
;;; preds.el --- Predicates.
;;;
;;; Time-stamp: <Wednesday Feb  4, 2015 12:36:37 asmodai>
;;; Revision:   5
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    31 Jan 2015 11:05:35
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

;;;==================================================================
;;;{{{ Emacs flavour predicates:

(defsubst xemacs-p ()
  "T if we are running in XEmacs."
  (featurep 'xemacs))

(defsubst emacs-p ()
  "T if we are running in GNU Emacs."
  (not (xemacs-p)))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Version predicates:

;;;------------------------------------------------------------------
;;;{{{ GNU Emacs:

(defsubst emacs=18-p ()
  "T if we are running in GNU Emacs 18."
  (and (not (xemacs-p))
       (= emacs-major-version 18)))

(defsubst emacs=19-p ()
  "T if we are running in GNU Emacs 19."
  (and (not (xemacs-p))
       (= emacs-major-version 19)))

(defsubst emacs=20-p ()
  "T if we are running in GNU Emacs 20."
  (and (not (xemacs-p))
       (= emacs-major-version 20)))

(defsubst emacs=21-p ()
  "T if we are running in GNU Emacs 21."
  (and (not (xemacs-p))
       (= emacs-major-version 21)))

(defsubst emacs=22-p ()
  "T if we are running in GNU Emacs 22."
  (and (not (xemacs-p))
       (= emacs-major-version 22)))

(defsubst emacs=23-p ()
  "T if we are running in GNU Emacs 23."
  (and (not (xemacs-p))
       (= emacs-major-version 23)))

(defsubst emacs=24-p ()
  "T if we are running in GNU Emacs 24."
  (and (not (xemacs-p))
       (= emacs-major-version 24)))

;;; The follwing are for major groupings of versions.  e.g. code that
;;; works in Emacs 21, 22, 23... etc.

(defsubst emacs>=19-p ()
  "T if we are running in GNU Emacs 19 or higher."
  (and (not (xemacs-p))
       (>= emacs-major-version 19)))

(defsubst emacs>=20-p ()
  "T if we are running in GNU Emacs 20 or higher."
  (and (not (xemacs-p))
       (>= emacs-major-version 20)))

(defsubst emacs>=21-p ()
  "T if we are running in GNU Emacs 21 or higher."
  (and (not (xemacs-p))
       (>= emacs-major-version 21)))

(defsubst emacs>=22-p ()
  "T if we are running in GNU Emacs 22 or higher."
  (and (not (xemacs-p))
       (>= emacs-major-version 22)))

(defsubst emacs>=23-p ()
  "T if we are running in GNU Emacs 23 or higher."
  (and (not (xemacs-p))
       (>= emacs-major-version 23)))

(defsubst emacs>=24-p ()
  "T if we are running in GNU Emacs 24 or higher."
  (and (not (xemacs-p))
       (>= emacs-major-version 24)))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ XEmacs:

(defsubst xemacs=19-p ()
  "T if we are running in XEmacs 19."
  (and (xemacs-p)
       (= emacs-major-version 19)))

(defsubst xemacs=20-p ()
  "T if we are running in XEmacs 20."
  (and (xemacs-p)
       (= emacs-major-version 20)))

(defsubst xemacs=21-p ()
  "T if we are running in XEmacs 21."
  (and (xemacs-p)
       (= emacs-major-version 21)))

(defsubst xemacs>=19-p ()
  "T if we are running in XEmacs 19 or higher."
  (and (xemacs-p)
       (>= emacs-major-version 19)))

(defsubst xemacs>=20-p ()
  "T if we are running in XEmacs 20 or higher."
  (and (xemacs-p)
       (>= emacs-major-version 20)))

(defsubst xemacs>=21-p ()
  "T if we are running in XEmacs 21 or higher."
  (and (xemacs-p)
       (>= emacs-major-version 21)))

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Windowing system predicates:

(defsubst x-windows-p ()
  "T if we are running inside the X Window System."
  (eq window-system 'x))

(defsubst presentation-manager-p ()
  "T if we are running inside OS/2's Presentation Manager."
  (eq window-system 'pm))

(defsubst nextstep-p ()
  "T if we are running inside NeXTSTEP, GNUstep, or Cocoa."  
  (eq window-system 'ns))

(defsubst terminal-p ()
  "T if we are running in a text-based terminal of some sort."
  (eq window-system nil))

(defsubst 256-colour-p ()
  "T if we're running on a terminal or display system capable of
displaying 256 colours or more."
  (>= (display-color-cells) 256))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ UI toolkit predicates:

(defsubst motif-p ()
  "T if this Emacs was built using the Motif UI toolkit."
  (featurep 'motif))

(defsubst xt-p ()
  "T if this Emacs was built using the X Toolkit."
  (featurep 'x-toolkit))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Host predicates:

;;;------------------------------------------------------------------
;;;{{{ Generation macro:

(defsubst %running-on (host)
  "Returns T if the string given in HOST matches the string contained
in `system-name'."
  (eq (string-match (concat "^" host) (system-name)) 0))

;;; TODO: Does this work with Emacs 18?
(defmacro define-host (host)
  "Defines a local host.  HOST is the name of the host to define, for
which we create a predicate named RUNNING-ON-<host>-P."
  (declare (indent 0))
  (let* ((host-str (symbol-name host)) 
         (pred (intern (concat "running-on-" host-str "-p")))
         (doc-str (concat "T if Emacs is running on `" host-str "'.")))
    `(defsubst ,pred ()
       ,doc-str 
       (%running-on ,host-str))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Hosts:

;;; Some hostnames.
(define-host enterprise)                ; GNU/Linux.
(define-host exeter)                    ; GNU/Linux.
(define-host hubble)                    ; FreeBSD.
(define-host yorktown)                  ; Windows.
(define-host challenger)                ; GNU/Linux.
(define-host voyager)                   ; GNU/Linux.
(define-host lynx)                      ; HP-UX.
(define-host farragut)                  ; AIX.
(define-host magellan)                  ; OS X.
(define-host nova)                      ; OPENSTEP.
(define-host galaxy)                    ; NeXTSTEP.
(define-host dauntless)                 ; NeXTSTEP.
(define-host saratoga)                  ; Rhapsody.
(define-host paradox)                   ; Work laptop, GNU/Linux.

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

;;; preds.el ends here
