;;; zlisp-frame.el --- Frame functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 14:28:36
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

(defun zlisp/make-split-windows-for-frame ()
  "Evenly divide the current frame into 90-column windows.

This function aims to put enough 90-column windows to use up the entire frame
real-estate."
  (interactive)
  (let ((cols (1- (floor (/ (frame-width) 90)))))
    (when (> cols 0)
      (balance-windows)
      (delete-other-windows)
      (dotimes (_ cols)
        (other-window 1)
        (split-window-horizontally))
      (other-window -1)
      (balance-windows))))

(defun zlisp/initial-frame-size (&optional max-width max-height)
  "Set initial frame and new frame sizes.

The default size is max screen dimensions - 200.

If MAX-WIDTH or MAX-HEIGHT are non-NIL, those values shall be used for their
respective dimension."
  (when (display-graphic-p)
    (let* ((max-w (or max-width 200))
           (max-h (or max-height 200))
           (dpy-w (display-pixel-width))
           (dpy-h (display-pixel-height))
           (new-w (/ (- dpy-w max-w) (frame-char-width)))
           (new-h (/ (- dpy-h max-h) (frame-char-height))))
      (modify-frame-parameters
       nil
       `((user-position . t)
         (top           . 0.5)
         (left          . 0.5)
         (width         . ,new-w)
         (height        . ,new-h)))
      (add-to-list 'default-frame-alist
                   (cons 'height (/ (- dpy-h max-h)
                                    (frame-char-height))))
      (add-to-list 'default-frame-alist
                   (cons 'width (/ (- dpy-h max-w)
                                   (frame-char-width)))))))

(defun zlisp/initial-server-frame-size ()
  "Set the initial frame size for frames created by `emacsclient'."
  (zlisp/initial-frame-size 1000 400))

(defun zlisp/recenter-frame ()
  "Recenter the focused frame to the user's screen."
  (interactive)
  (modify-frame-parameters nil
                           '((user-position . t)
                             (top           . 0.5)
                             (left          . 0.5))))

(defun zlisp/delete-frame-or-quit ()
  "Delete the selected frame and kill terminal buffers.

If it is the last frame, then Emacs is also killed."
  (interactive)
  (kill-matching-buffers "*vterm" nil t)
  (when (condition-case nil (delete-frame)
          (error (save-buffers-kill-emacs))))
  (select-frame-set-input-focus (selected-frame)))

(defun zlisp/activate-capture-frame ()
  "Run `org-capture' in a capture frame."
  (progn
    (require 'org)
    (select-frame-by-name "capture")
    (switch-to-buffer (get-buffer-create "new"))
    (org-capture)))

(provide 'zlisp-frame)

;;; zlisp-frame.el ends here.
