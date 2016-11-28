;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; distel-loader.el --- A simple loader for Distel.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    15 Oct 2016 16:45:15
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

(eval-when-compile
  (require 'cl))

(defun load-distel ()
  (when (featurep 'erlang)
    (push "~/.emacs.d/private/distel/elisp/" load-path)
    (require 'distel)
    (distel-setup)
    (setq inferior-erlang-prompt-timeout t)
    (setq inferior-erlang-machine-options '("-sname" "emacs"))
    (setq erl-nodename-cache
          (make-symbol
           (concat
            "emacs@"
            (car (split-string (shell-command-to-string "hostname"))))))))

(provide 'distel-loader)

;;; distel-loader.el ends here.
