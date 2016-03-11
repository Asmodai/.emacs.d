;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Shell packages.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2015 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    04 Dec 2015 21:00:10
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

(setq shell-packages
      '(company
        helm
        multi-term
        (comint :location built-in)
        xterm-color
        shell
        shell-pop
        term
        eshell
        eshell-prompt-extras
        esh-help
        magit))

(defun shell:init-comint ()
  (setq comint-prompt-read-only t))

(defun shell:init-xterm-color ()
  (use-package xterm-color
    :defer t
    :init
    (progn
      (require 'xterm-color)
      (add-hook 'comint-preoutput-filter-functions
                'xterm-color-filter)
      (setq comint-output-filter-functions
            (remove 'ansi-color-process-output
                    comint-output-filter-functions))
      (setq font-lock-unfontify-region-function
            'xterm-color-unfontify-region)
      (with-eval-after-load 'esh-mode
        (add-hook 'eshell-mode-hook
                  (lambda ()
                    (setq xterm-color-preserve-properties t)))
        (add-hook 'eshell-preoutput-filter-functions
                  'xterm-color-filter)
        (setq eshell-output-filter-functions
              (remove 'eshell-handle-ansi-color
                      eshell-output-filter-functions))))))

(defun shell:pre-init-company ()
  (bootstrap:use-package-add-hook eshell
    :post-init
    (progn
      (push 'company-capf company-backends-eshell-mode)
      (bootstrap:add-company-hook eshell-mode))
    :post-config
    (progn
      (defun bootstrap::toggle-shell-auto-completion-based-on-path ()
        (if (file-remote-p default-directory)
            (setq-local company-idle-delay nil)
          (setq-local company-idle-delay 0.2)))
      (add-hook 'eshell-directory-change-hook
                'bootstrap::toggle-shell-auto-completion-based-on-path)

      (defun bootstrap::eshell-switch-company-frontend ()
        (setq-local company-frontends '(company-preview-frontend)))
      (add-hook 'eshell-mode-hook
                'bootstrap::eshell-switch-company-frontend))))

(defun shell:init-eshell ()
  (use-package ehell
    :defer t
    :init
    (progn
      (setq eshell-cmpl-cycle-completions nil
            ;; auto truncate after 20k lines
            eshell-buffer-maximum-lines 20000
            ;; history size
            eshell-history-size 350
            ;; buffer shorthand -> echo foo > #'buffer
            eshell-buffer-shorthand t
            ;; my prompt is easy enough to see
            eshell-highlight-prompt nil
            ;; treat 'echo' like shell echo
            eshell-plain-echo-behavior t)

      (defun bootstrap::eshell-auto-end ()
        (when (and (eq major-mode 'eshell-mode)
                   (not (eq (line-end-position) (point-max))))
          (end-of-buffer)))

      (when *shell-protect-eshell-prompt*
        (defun bootstrap::protect-eshell-prompt ()
          (let ((inhibit-field-text-motion t))
            (add-text-properties
             (point-at-bol)
             (point)
             '(rear-nonsticky t
                              inhibit-line-move-field-capture t
                              field output
                              read-only t
                              front-sticky (field inhibit-line-move-field-capture)))))
        (add-hook 'eshell-after-prompt-hook 'bootstrap::protect-eshell-prompt))

       (defun bootstrap::init-eshell ()
        "Stuff to do when enabling eshell."
        (setq pcomplete-cycle-completions nil)
        (unless *shell-enable-smart-eshell*
          ;; we don't want auto-jump to prompt when smart eshell is enabled.
          ;; Idea: maybe we could make auto-jump smarter and jump only if the
          ;; point is not on a prompt line
          (add-hook 'evil-insert-state-entry-hook
                    'bootstrap::eshell-auto-end nil t))
        (when (bootstrap-layer:package-used-p 'semantic)
          (semantic-mode -1)))
       (add-hook 'eshell-mode-hook 'bootstrap::init-eshell))
    :config
    (progn
      (require 'esh-opt)

      (defalias 'e 'find-file-other-window)
      (defalias 'd 'dired)
      (setenv "PAGER" "cat")

      (when *shell-enable-smart-eshell*
        (require 'em-smart)
        (setq eshell-where-to-jump 'begin
              eshell-review-quick-commands nil
              eshell-smart-space-goes-to-end t)
        (add-hook 'eshell-mode-hook 'eshell-smart-initialize))

      (require 'em-term)
      (mapc (lambda (x)
              (push x eshell-visual-commands))
            '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

      (when (boundp 'eshell-output-filter-functions)
        (push 'eshell-truncate-buffer eshell-output-filter-functions)))))

(defun shell:init-esh-help ()
  (use-package esh-help
    :defer t
    :init (add-hook 'eshell-mode-hook 'eldoc-mode)
    :config (setup-esh-help-eldoc)))

(defun shell:init-eshell-prompt-extras ()
  (use-package eshell-prompt-extras
    :commands epe-theme-lambda
    :init
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(defun shell:init-multi-term ()
  (use-package multi-term
    :defer t
    :init
    (progn

      (defun multiterm (_)
        "Wrapper to be able to call multi-term from shell-pop"
        (interactive)
        (multi-term)))
    :config
    (progn
      (defun term-send-tab ()
        "Send tab in term mode."
        (interactive)
        (term-send-raw-string "\t"))
      (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))

      (when (bootstrap-layer:package-used-p 'projectile)
        (defun projectile-multi-term-in-root ()
          "Invoke `multi-term' in the project's root."
          (interactive)
          (projectile-with-default-dir (projectile-project-root)
                                       (multi-term)))))))

(defun shell:init-shell ()
  (defun shell-comint-input-sender-hook ()
    "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
    (setq comint-input-sender
          (lambda (proc command)
            (cond
             ;; Check for clear command and execute it.
             ((string-match "^[ \t]*clear[ \t]*$" command)
              (comint-send-string proc "\n")
              (erase-buffer))
             ;; Check for man command and execute it.
             ((string-match "^[ \t]*man[ \t]*" command)
              (comint-send-string proc "\n")
              (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
              (setq command (replace-regexp-in-string "[ \t]+$" "" command))
              (funcall 'man command))
             ;; Send other commands to the default handler.
             (t (comint-simple-send proc command))))))
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook))

(defun shell:init-shell-pop ()
  (use-package shell-pop
    :defer t
    :init
    (progn
      (setq shell-pop-window-position shell-default-position
            shell-pop-window-height   shell-default-height
            shell-pop-term-shell      shell-default-term-shell
            shell-pop-full-span t)

      (defmacro make-shell-pop-command (type &optional shell)
        (let* ((name (symbol-name type)))
          `(defun ,(intern (concat "shell-pop-" name)) (index)
             (interactive "P")
             (require 'shell-pop)
             (shell-pop--set-shell-type
              'shell-pop-shell-type
              (backquote (,name
                          ,(concat "*" name "*")
                          (lambda nil (funcall ',type ,shell)))))
             (shell-pop index))))

      (make-shell-pop-command eshell)
      (make-shell-pop-command shell)
      (make-shell-pop-command term shell-pop-term-shell)
      (make-shell-pop-command multiterm)
      (make-shell-pop-command ansi-term shell-pop-term-shell)

      (defun bootstrap::term-kill-buffer-hook ()
        "Function that hook `kill-buffer-hook'."
        (when (eq major-mode 'term-mode)
          (when (term-check-proc (current-buffer))
            (term-quit-subjob))))
      (add-hook 'kill-buffer-hook 'bootstrap::term-kill-buffer-hook)

      (defun ansi-term-handle-close ()
        "Close current term buffer when `exit' from term buffer."
        (when (ignore-errors (get-buffer-process (current-buffer)))
          (set-process-sentinel
           (get-buffer-process (current-buffer))
           (lambda (proc change)
             (when (string-match "\\(finished\\|exited\\)" change)
               (kill-buffer (process-buffer proc))
               (delete-window))))))
      (add-hook 'term-mode-hook 'ansi-term-handle-close)

      (defun bootstrap:default-pop-shell ()
        "Open the default shell in a popup."
        (interactive)
        (let ((shell (if (eq 'multi-term shell-default-shell)
                         'multiterm
                       shell-default-shell)))
          (call-interactively (intern (format "shell-pop-%S" shell))))))))

(defun shell:init-term ()
  (defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t")))

(defun shell:pre-init-magit ()
  (bootstrap:use-package-add-hook magit
    :post-init
    (defalias 's 'magit-status)))

;;; packages.el ends here
