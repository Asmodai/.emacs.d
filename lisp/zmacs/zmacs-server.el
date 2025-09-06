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
(require 'zmacs-dashboard)

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

(defun zmacs-server-shutdown ()
  "Kill all Emacs clients and the server."
  (interactive)
  (if (server-running-p)
      (save-buffers-kill-emacs)
    (message "Not running as daemon")))

(defun zmacs/new-frame-file-or-dashboard ()
  "Show dashboard if FRAME is `*scratch*'."
  (if (= (zmacs/server-gui-client-count) 1)
      (zlisp/initial-frame-size)
    (zlisp/initial-server-frame-size))
  (zmacs-line-clockface-update-fontset "ClockFaceRectSolid")
  (when (or (string= (buffer-name) "*scratch*")
            (eq (current-buffer) (get-buffer "*scratch*")))
    (zmacs-dashboard)
    (zmacs-goto-dashboard)))

;; Hook into emacsclient:
(add-hook 'server-after-make-frame-hook #'zmacs/new-frame-file-or-dashboard)

(defun zmacs//terminal-is-initial (terminal)
  "Return T if TERMINAL is the initial terminal."
  (string= (terminal-name terminal) "initial_terminal"))

(defun zmacs/server-client-count ()
  "Return the number of emacsclient connections."
  (length (cl-remove-if #'zmacs//terminal-is-initial (terminal-list))))

(defun zmacs/server-gui-client-count ()
  "Return the number of GUI emacsclient connections."
  (length
   (cl-remove-if-not (lambda (client)
                       (let ((live (terminal-live-p client)))
                         (or (equal live 'x)
                             (equal live 'pgtk))))
                     (cl-remove-if #'zmacs//terminal-is-initial
                                   (terminal-list)))))

;;; Provide package:

(provide 'zmacs-server)

;;; zmacs-server.el ends here.
