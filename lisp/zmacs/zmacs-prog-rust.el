;;; zmacs-prog-rust.el --- Rust packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Sep 2025 14:27:31
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
;;;; Requirements

(require 'cl-lib)
(require 'zlisp-platform)

;;;; Rust mode:

(use-package rust-mode
  :hook (rust-mode . eglot-ensure))

;;;; Configure Treesitter mode:

(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
(add-to-list 'major-mode-remap-alist '(conf-toml-mode . toml-ts-mode))

;;;; Eglot setup:

(let* ((cargo-ra     (concat user-home-directory "/.cargo/bin/rust-analyzer"))
       (rustanalyzer (cond ((file-exists-p cargo-ra)
                            cargo-ra)
                           ((file-exists-p "/usr/bin/rust-analyzer")
                            "/usr/bin/rust-analyzer")
                           (t
                            (error "Rust has not been configured!")))))
  (when (file-exists-p rustanalyzer)
    (message (format "ZMACS/RUST: Configured '%s' as LSP backend." rustanalyzer))
    (add-to-list 'eglot-server-programs `(rust-mode ,rustanalyzer))
    (add-to-list 'eglot-server-programs `(rust-ts-mode ,rustanalyzer))))

;;;; Rust TS mode hooks:

(defun zmacs//ra-config ()
  "Per-buffer configuration for `rust-analyzer'."
  (let* ((has-cargo (locate-dominating-file default-directory "Cargo.toml"))
         (check     (if has-cargo
                        '((command . "clippy"))
                      '((enable . :json-false)))))
    `((cargo . ((allFeatures . t)))
      (procMacro . ((enable . t)))
      (checkOnSave . ,check))))

(add-hook 'rust-ts-mode-hook
          (lambda ()
            (let* ((old (or eglot-workspace-configuration '()))
                   (cfg (assq-delete-all :rust-analyzer (copy-sequence old))))
              (setq-default eglot-workspace-configuration
                            (cons (cons :rust-analyzer (zmacs//ra-config)) cfg)))
            (eglot-ensure)))

(add-hook 'rust-ts-mode-hook #'eglot-inlay-hints-mode)

;;;; Save hook:

(defun zmacs//rust-on-save ()
  "Add on-save hooks for Rust."
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'rust-ts-mode-hook #'zmacs//rust-on-save)

(provide 'zmacs-prog-rust)

;;; zmacs-prog-rust.el ends here.
