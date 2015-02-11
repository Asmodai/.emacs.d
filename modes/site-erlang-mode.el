;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-erlang-mode.el --- Erlang mode.
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 00:41:08 asmodai>
;;; Revision:   6
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
  (setq erlang-root-dir (if (windows-p)
                            "c:/Progra~1/erl5.9"
                            "/usr/local/otp"))
  
  ;;
  ;; Tell emacs where to find the Erlang binary
  (setq exec-path (if (windows-p)
                      (cons "c:/Progra~1/erl5.9/bin" exec-path)
                      (cons "/usr/local/otp/bin" exec-path)))
  
  ;;
  ;; Deal with Erlang hanging
  (setf inferior-erlang-prompt-timeout t)
  
  ;;
  ;; Load erlang-mode
  (require 'erlang-start))

;;; site-erlang-mode.el ends here
