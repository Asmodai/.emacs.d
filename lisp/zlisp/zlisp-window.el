;;; zlisp-window.el --- Window-specific stuff  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    31 Oct 2024 16:40:34
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
(require 'ace-window)

(defun zlisp/toggle-window-detected ()
  "Toggle whether the current active window is detected or not."
  (interactive)
  (message
   (if (let ((window (get-buffer-window (current-buffer))))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated."
     "Window '%s' is normal.")
   (current-buffer)))

(defun zlisp/window-exchange-buffer ()
  "Swap buffer in windows leaving focus in the original window."
  (interactive)
  (ace-swap-window)
  (aw-flip-window))

(defun zlisp/rotate-windows (count)
  "Rotate your windows by COUNT.

Dedicated windows are left untouched.
Giving a negative prefix will result in rotation in the other direction."
  (interactive "p")
  (let* ((non-dedicated (cl-remove-if #'window-dedicated-p
                                      (window-list)))
         (num-windows (length non-dedicated))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (_ (- num-windows 1))
             (let* ((next-i (% (+ step 1) num-windows))
                    (w1 (elt non-dedicated i))
                    (w2 (elt non-dedicated next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun zlisp/rotate-windows-backward (count)
  "Rotate your windows the other direction by COUNT."
  (interactive "p")
  (zlisp-rotate-windows (* -1 count)))

(defun zlisp/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-right)
  (windmove-right))

(defun zlisp/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-below)
  (windmove-down))

(defun zlisp/toggle-window-split ()
  "Move from a horizontal to a vertical split and vice versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun zlisp/other-window ()
  "Switch to another window."
  (interactive)
  (other-window 1))

(provide 'zlisp-window)

;;; zlisp-window.el ends here.
