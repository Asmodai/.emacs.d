;;; inf-perl.el --- Run Inferior Perl process

;; Copyright 2005 Wenbin Ye
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Hacked up by Paul Ward <asmodai@gmail.com>
;; Version: 0.01
;; Keywords: tools, processes
;; 
;; This file is part of PDE (Perl Development Environment).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(eval-when-compile
  (require 'cl))

(require 'comint)
(require 'cperl-mode)
(require 'shell)

(defgroup inf-perl nil
  "*Inferior perl for Emacs."
  :group 'processes
  :group 'unix
  :group 'perl)

(defcustom inf-perl-tool-path (file-name-directory load-file-name)
  "*Directory where the Perl tools are."
  :type 'directory
  :group 'inf-perl)

(defcustom inf-perl-prompt-pattern "^[^#$%>\n]*[#$%>] *"
  "*Regexp to match prompts in a shell."
  :type 'regexp
  :group 'inf-perl)

(defcustom inf-perl-command-regexp "[^>\n]*>+ *"
  "*Regexp to match a single command."
  :type 'regexp
  :group 'inf-perl)

(defcustom inf-perl-input-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "*Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'inf-perl)

(defcustom inf-perl-program "perl"
  "*Name of Perl binary."
  :type 'string
  :group 'inf-perl)

(defcustom inf-perldoc-program "perldoc"
  "*Name of `perldoc' program."
  :type 'string
  :group 'inf-perl)

(defcustom inf-perl-shell-program (expand-file-name
                                   "tools/psh.pl"
                                   inf-perl-tool-path)
  "*The Perl shell program location."
  :type 'file
  :group 'inf-perl)

(defcustom inf-perl-options nil
  "*The Perl shell program options."
  :type 'list
  :group 'inf-perl)

(defcustom inf-perl-start-file "~/.pshrc"
  "*The file to load when `run-perl' is invoked."
  :type 'file
  :group 'inf-perl)

(defcustom inf-perl-buffer-name "Inf-Perl"
  "*Buffer name of inf-perl REPL."
  :type 'string
  :group 'inf-perl)

(defvar inf-perl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    map)
  "*Keymap for inf-perl buffer.")

(defcustom inf-perl-mode-hook '()
  "Hook for customising `inf-perl' mode."
  :type 'hook
  :group 'inf-perl)

(defcustom inf-perl-remove-my t
  "If non-NIL, any `my' declaration will be removed for lexical scope."
  :type 'boolean
  :group 'inf-perl)

(defvar inf-perl-font-lock-keywords)

(put 'inf-perl-mode 'mode-class 'special)

(define-derived-mode inf-perl-mode comint-mode "Perl-Interaction"
  "In cperl-mode, you can send text to the inferior perl process.
     `inf-perl-switch-to-perl'   switches to perl process buffer
     `inf-perl-send-line'   send current line to the perl process
     `inf-perl-send-region' send the current region to the perl process
     `inf-perl-send-region-and-go'  send the current region to the perl process
                              and switch to the perl process buffer

Something uncomfortable is that, the subroutine can't redefine in perl.
So the provided perl shell program translate the code \"sub func { body }\"
to \"*func = sub { body };\". Evaluate this code has a warnning, but it
does work."
  (setq comint-prompt-regexp "[^>\n]*>+ *"
        comint-prompt-regexp inf-perl-prompt-pattern
        comint-input-filter (function inf-perl-input-filter))
  (modify-syntax-entry ?: "_")
  (modify-syntax-entry ?> ".")
  (when (ring-empty-p comint-input-ring)
    (set-process-sentinel
     (get-buffer-process (current-buffer))
     'shell-write-history-on-exit)
    (comint-read-input-ring)))

(defun inf-perl-input-filter (str)
  "Don't save anything matching `inf-perl-input-filter-regexp'."
  (not (string-match inf-perl-input-filter-regexp str)))

;;;###autoload
(defalias 'run-perl 'inf-perl-start)

;;;###autoload
(defun inf-perl-start (&optional buffer)
  "Run an inferior Perl process, input and output via buffer `Inf-Perl'.

If there is a process already running in `Inf-Perl', we will switch to that
buffer."
  (interactive)
  (setq buffer (get-buffer-create (or buffer
                                      inf-perl-buffer-name)))
  (setq inf-perl-buffer buffer)
  (pop-to-buffer buffer)
  (unless (comint-check-proc buffer)
    (apply 'make-comint-in-buffer "perl" buffer inf-perl-program
           (and inf-perl-start-file
                (file-exists-p inf-perl-start-file)
                inf-perl-start-file)
           (cons inf-perl-shell-program inf-perl-options))
    (inf-perl-mode))
  buffer)

(defun inf-perl-proc ()
  "Return the current Perl process.

See the variable `inf-perl-buffer'."
  (let ((proc (get-buffer-process
               (if (eq major-mode 'inf-perl-mode)
                   (current-buffer)
                 inf-perl-buffer))))
    (or proc
        (error "No current Perl process to switch to."))))

(defun inf-perl-switch-to-perl (display)
  "Switch to Perl process buffer.

If DISPLAY is non-NIL, then the buffer is just displayed."
  (interactive "P")
  (unless (and (buffer-live-p inf-perl-buffer)
               (get-buffer-process inf-perl-buffer)
               (eq (process-status (get-buffer-process inf-perl-buffer)) 'run))
    (save-window-excursion
      (inf-perl-start)))
  (if display
      (display-buffer inf-perl-buffer)
    (pop-to-buffer inf-perl-buffer)))

(defun inf-perl-switch-to-end-perl (display)
  "Switch to the end of the Perl process buffer.

If DISPLAY is non-NIL, then the buffer is just displayed."
  (interactive "P")
  (if (get-buffer inf-perl-buffer)
      (progn
        (inf-perl-switch-to-perl display)
        (with-selected-window (get-buffer-window inf-perl-buffer)
          (push-mark)
          (goto-char (point-max))))
    (error "No current Perl process to switch to.")))

(defun inf-perl-send-region-and-go (start end)
  "Send a region to the Perl process buffer and switch to it."
  (interactive "r")
  (inf-perl-send-region start ent)
  (inf-perl-switch-to-perl t))

(defun inf-perl-put-empty-input ()
  "Use an empty input for movement around the buffer."
  (if (get-buffer inf-perl-buffer)
      (save-excursion
        (let* ((proc (get-buffer-process inf-perl-buffer))
               (marker (process-mark proc)))
          (set-buffer inf-perl-buffer)
          (goto-char marker)
          (insert (propertize "\n" 'field 'input))
          (set-marker marker (point))))
    (error "No current Perl process.")))

(defun inf-perl-send ()
  "If the mark is active then send a region, otherwise send the current line."
  (interactive)
  (if (and mark-active transient-mark-mode)
      (inf-perl-send-region (region-beginning)
                            (region-end))
    (inf-perl-send-line)))

(defun inf-perl-send-line ()
  "Send the current line to the Perl process."
  (interactive)
  (let ((string (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
    (if (and (string-match "^\\s-*my\\s-*" string)
             inf-perl-remove-my)
        (setq string (replace-match "" nil nil string 0)))
    (inf-perl-put-empty-input)
    (comint-send-string (inf-perl-proc) (concat string "\n"))))

(defun inf-perl-send-region (start end)
  "Send a region to the Perl interaction buffer."
  (interactive "r")
  (let ((string (concat "do {\n"
                        (buffer-substring-no-properties start end)
                        "\n} while (0);")))
    (setq string
          (replace-regexp-in-string "\n" "\\\n" string nil '\\))
    (inf-perl-put-empty-input)
    (comint-send-string (inf-perl-proc) (concat string "\n"))))

(defun inf-perl-load-file ()
  "Send a whole buffer to the Perl process."
  (interactive)
  (if (and (buffer-modified-p)
           (y-or-n-p "The buffer is modified, do you want to save it? "))
      (save-buffer))
  (comint-send-string (inf-perl-proc)
                      (format "do '%s' || ($@ && die $@)\n"
                              (buffer-file-name))))

(defun inf-perl-set-cwd ()
  "Set the working directory of the Perl interpreter."
  (interactive)
  (let ((dir default-directory))
    (with-current-buffer inf-perl-buffer
      (setq default-directory dir)
      (comint-send-string (inf-perl-buffer)
                          (format "print \"Change to %s\n\"; chdir '%s';\n"
                                  dir dir)))))

(provide 'inf-perl)

;;; inf-perl-mode.el ends here
