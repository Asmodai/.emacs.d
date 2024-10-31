;;; zmacs-shell.el --- Shell packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:17:33
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

;;;; Compilation:

(use-package compile
  :ensure nil
  :defer 2
  :bind (:map project-prefix-map
         ("C" . recompile))
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(add-hook 'minibuffer-exit-hook #'(lambda ()
                                    (let ((buffer "*Completions*"))
                                      (and (get-buffer buffer)
                                           (kill-buffer buffer)))))

(defun async-shell-command-no-window (command)
  "Don't pop up a buffer for async commands."
  (interactive)
  (let ((display-buffer-alist
         (list (cons "\\*Async Shell Command\\*.*"
                     (cons #'dislpay-buffer-no-window nil)))))
    (async-shell-command command)))

;;;; Exec path:

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-arguments nil)
  :config
  (zlisp/when-macos
    (exec-path-from-shell-initialize)))

;;;; Terminal:

(setq-default term-suppress-hard-newline t)

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
 (if (memq (process-status proc) '(signal exit))
     (let ((buffer (process-buffer proc)))
       ad-do-it
       (kill-buffer buffer))
   ad-do-it))
(ad-activate 'term-sentinel)

(defun zmacs-term-hook ()
  (progn
    (goto-address-mode)
    (hl-line-mode 0)
    (setq comint-buffer-maximum-size most-positive-fixnum)))

(add-hook 'term-mode-hook #'zmacs-term-hook)
(add-hook 'eshell-mode-hook #'zmacs-term-hook)

(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

;; Ensure EDITOR is sane.
(or (getenv "EDITOR")
    (progn
      (setenv "EDITOR" "emacsclient")
      (setenv "VISUAL" (getenv "EDITOR"))))

;; Ensure PAGER is sane.
(or (getenv "PAGER")
    (setenv "PAGER" "cat"))

;;;; EAT:

(use-package eat
  :config
  :custom
  (eat-kill-buffer-on-exit            t)
  (eat-enable-yank-to-terminal        t)
  (eat-enable-directory-tracking      t)
  (eat-enable-shell-command-history   t)
  (eat-enable-shell-prompt-annotation t)
  (eat-term-scrollback-size           nil)
  :hook (eshell-load . eat-eshell-mode))

;;;; Tramp:

(use-package tramp
  :ensure nil
  :defer 1
  :config
  (setq tramp-persistency-file-name (concat *zmacs-cache-directory* "tramp")
        tramp-default-method "ssh"
        tramp-copy-size-limit nil
        tramp-use-ssh-controlmaster-options nil))

;; ~/.ssh/config settings:
;; Host *
;; ForwardAgent yes
;; AddKeysToAgent yes
;; ControlMaster auto
;; ControlPath ~/.ssh/master-%r@%h:%p
;; ControlPersist yes
;; ServerAliveInterval 10
;; ServerAliveCountMax 10

;;;; Provide package:

(provide 'zmacs-shell)

;;; zmacs-shell.el ends here.
