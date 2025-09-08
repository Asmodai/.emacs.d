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
;;;; Requirements:

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

;;;; Server shutdown:

(defun zmacs-server-shutdown ()
  "Kill all Emacs clients and the server."
  (interactive)
  (if (server-running-p)
      (save-buffers-kill-emacs)
    (message "Not running as daemon")))

;;;; Utilities:

(defun zmacs//terminal-is-initial (terminal)
  "Return T if TERMINAL is the initial terminal."
  (string= (terminal-name terminal) "initial_terminal"))

(defun zmacs//frame-in-initial (frame)
  "Return T if FRAME is parented to the initial terminal."
  (let ((parent (frame-parent frame)))
    (if (not (null parent))
        (zmacs//frame-in-initial parent)
      (zmacs//terminal-is-initial (frame-terminal frame)))))

(defun zmacs//frame-in-terminal (frame)
  "Return T if FRAME is in a terminal."
  (let ((parent   (frame-parent frame)))
    (if (not (null parent))
        (zmacs//frame-in-terminal parent)
      (let* ((terminal (frame-terminal frame))
             (live     (terminal-live-p terminal)))
        (cond ((zmacs//terminal-is-initial terminal) t)
              ((equal live 't)                       t)
              (t                                     nil))))))

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

(defun zmacs/frame-count ()
  "Return the number of frames."
  (length (cl-remove-if (lambda (frame)
                          (or (zmacs//frame-in-initial frame)
                              (not (null (frame-parent frame)))))
                        (frame-list))))

(defun zmacs/server-gui-frame-count ()
  "Return the number of GUI frames that the Emacs server has.

This will ignore any frame that has a parent, or any frame that is in a
CLI Emacs session."
  (length
   (cl-remove-if (lambda (frame)
                   (or (zmacs//frame-in-terminal frame)
                       (not (null (frame-parent frame)))))
                 (frame-list))))

;;;; New frame:

(defun zmacs/new-frame-file-or-dashboard ()
  "Show dashboard if FRAME is `*scratch*'.

If the new frame is the only frame, and there is no filename given, then
Emacs will treat this similarly to a plain client.  It will open using as
much as the screen as configured, and will switch to the ZMACS dashboard.

If it is either not the first frame, or a filename was given, then it will
be treated as a plain `emacsclient' frame."
  (if (= (zmacs/server-gui-frame-count) 1)
      (zlisp/initial-frame-size)
    (zlisp/initial-server-frame-size))
  (zmacs-line-clockface-update-fontset "ClockFaceRectSolid")
  (when (or (string= (buffer-name) "*scratch*")
            (eq (current-buffer) (get-buffer "*scratch*")))
    (zmacs-dashboard)
    (zmacs-goto-dashboard)))

;; Hook into emacsclient:
(add-hook 'server-after-make-frame-hook #'zmacs/new-frame-file-or-dashboard)

;;; zmacs-server.el ends here.
