;;; -*- Mode: Emacs-Lisp; Mode: auto-fill -*-
;;;
;;; init.el --- Emacs initialisation file.
;;;
;;; Copyright (c) 2005-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Sun Jun 05 21:44:38 2005
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation,
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
;;; NOTICE:  This file is never bytecompiled.
;;;
;;; Emacsen this has been tested on:
;;;   GNU Emacs 22.1.1       Mac OS X 10.5.8
;;;   GNU Emacs 23.1.1       GNU/Linux
;;;   GNU Emacs 23.2.3       Windows 7
;;;   GNU Emacs 23.3.1       Windows 7
;;;   GNU Emacs 23.3.1       GNU/Linux
;;;   GNU Emacs 23.4.1       Mac OS X 10.5.8
;;;   GNU Emacs 24.1.1       Mac OS X 10.5.8
;;;   GNU Emacs 24.2.1       GNU/Linux
;;;   GNU Emacs 24.3.1       GNU/Linux
;;;   GNU Emacs 24.5.1       GNU/Linux
;;;   GNU Emacs 24.5.1       Mac OS X 10.10 and 10.11
;;;
;;; PLEASE BE AWARE THAT SUPPORT FOR EMACSEN OLDER THAN 24 HAS BEEN DROPPED.
;;; Use the `legacy' branch of the configuration repo for older Emacsen.
;;;
;;;}}}

;;; Some preliminary variables.
(setq-default debug-on-error t)         ; Debug on errors please.
(setq initial-buffer-choice nil         ; No thanks.
      inhibit-splash-screen t           ; I don't care about it.
      inhibit-startup-screen t          ; New version of above.
      inhibit-startup-message t)        ; Another alias for above.

;;; Copy this at your peril.
(setq inhibit-startup-echo-area-message "asmodai")

;; Set GC threshold.
;;(setq gc-cons-threshold 100000000)

;; Set this to T if you want verbose messages.
(defvar *bootstrap-verbose* nil)

(defconst +bootstrap-emacs-min-version+ "24.3"
  "Minimum required version of Emacs.")

(defun bootstrap:emacs-version-ok ()
  (version<= +bootstrap-emacs-min-version+ emacs-version))

;; Load it all.
(when (bootstrap:emacs-version-ok)
  ;; Load in paths.
  (load-file (concat user-emacs-directory "core/bootstrap-paths.el"))

  ;; Require core packages.
  (require 'bootstrap-core)

  ;; Start the ball rolling.
  (bootstrap-init)

  ;; Load in custom file.
  (load +bootstrap-custom-file+)

  (require 'server)
  (unless (server-running-p)
    (server-start)))

;;; init.el ends here.
