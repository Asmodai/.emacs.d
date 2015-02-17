;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-erlang-mode.el --- Erlang mode.
;;;
;;; Time-stamp: <Tuesday Feb 17, 2015 08:45:09 asmodai>
;;; Revision:   11
;;;
;;; Copyright (c) 2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    24 Jan 2012 14:37:02
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

(when (emacs>=23-p) 
  ;;
  ;; Start by setting up Erlang variables
  (setq erlang-root-dir 
        (cond ((windows-p)
               "c:/Progra~1/erl5.9")
              ((running-on-paradox-p)   ; Special case for work laptop.
               "/opt/erlang/16b03")
              ((running-on-hubble-p)    ; Special case for monitoring host.
               "/home/promon/")
              (t
               "/usr/local")))
  
  ;;
  ;; Tell emacs where to find the Erlang binary
  (setq exec-path
        (cond ((windows-p)
               (cons "c:/Progra~1/erl5.9/bin" exec-path))
              ((running-on-paradox-p)   ; Special case for work laptop.
               (cons "/opt/erlang/16b03/bin" exec-path))
              ((running-on-hubble-p)    ; Special case for monitoring host.
               (cons "/home/promon/bin" exec-path))
              (t
               (cons "/usr/local/bin" exec-path))))
  
  ;;
  ;; Deal with Erlang hanging
  (setf inferior-erlang-prompt-timeout t)
  
  ;;
  ;; Load erlang-mode
  (require 'erlang-start))

;;; site-erlang-mode.el ends here
