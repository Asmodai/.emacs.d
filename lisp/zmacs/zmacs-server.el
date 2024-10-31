;;; zmacs-server.el --- Emacs server  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    26 Oct 2024 10:21:52
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
(require 'zlisp-platform)

;;;; Emacs server

(use-package server
  :ensure nil
  :if window-system
  :defer 2
  :config
  (setq server-client-instructions nil)
  (or (server-running-p)
      (server-start)))

;;;; Functions

;; BUG: This isn't really portable yet.
(defun zmacs/kill-all-emacsen ()
  "Kill all running Emacsen."
  (interactive)
  (progn
    (save-buffers-kill-emacs)
    (shell-command-to-string (concat "pkill -i "
                                     (cond ((zlisp/macos-p   "Emacs"))
                                           ((zlisp/windows-p "Emacs.exe"))
                                           (t                "emacs"))))))
(provide 'zmacs-server)

;;; zmacs-server.el ends here.
