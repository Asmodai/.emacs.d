;;; zlisp-buffer.el --- Buffer functions  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 14:29:22
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

(defun zlisp-goto-minibuffer-window ()
  "Locate point to minibuffer window if it is active."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(bind-key "C-c m" #'zlisp-goto-minibuffer-window)

(defun zlisp-new-buffer-new-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))

(defun zlisp-create-new-buffer ()
  "Create a new buffer in the default major mode."
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (funcall (default-value 'major-mode)))))

(defun zlisp-create-new-elisp-buffer ()
  "Create a new buffer in `emacs-lisp' mode."
  (interactive)
  (let ((buffer (generate-new-buffer "*new-elisp*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (emacs-lisp-mode))))

(defun zlisp-tmp-buffer ()
  "Make a temporary buffer and switch to it."
  (interactive)
  (switch-to-buffer
   (get-buffer-create
    (concat "tmp-" (format-time-string "%Y.%m%dT%H.%M.%S"))))
  (delete-other-windows))

(defun zlisp-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted. They
will be reverted though if they were modified outside Emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s"
                     filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun zlisp-user-buffer-p ()
  "Return T if the current buffer is a user buffer."
  (interactive)
  (if (string-equal "*" (cl-subseq (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t)))

(defun zlisp-next-user-buffer ()
  "Switch to the next user buffer."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (zlisp-user-buffer-p))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun zlisp-eval-buffer-until-error ()
  "Evaluate a buffer until an error occurs."
  (interactive)
  (goto-char (point-min))
  (while t
    (eval (read (current-buffer)))))

(defun zlisp-kill-this-buffer ()
  "Kill this buffer."
  (interactive)
  (kill-buffer))

(defun zlisp-show-and-copy-buffer-full-filename ()
  "Show the full path to the current file  and copy it to the clipboard."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (message filename)
          (kill-new filename))
      (error "Buffer is not visiting a file"))))

(defun switch-to-previous-buffer ()
  "Switch to the previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'zlisp-buffer)

;;; zlisp-buffer.el ends here.
