;;; zmacs-prog-c.el --- C/C++/Objective-C packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:37:19
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

(eval-when-compile
  (require 'cl-lib))

(when (and (not (executable-find "ccls"))
           (not (executable-find "clangd")))
  (error "You need `ccls' or `clangd' for this to work."))

;;; C/C++ mode.
(use-package cc-mode
  :defer t)

(defconst c-c++-modes '(c-mode c++-mode c-ts-mode c++-ts-mode)
  "List of known C/C++ modes.")

(defconst c-c++-maps '(c-mode-map c++-mode-map)
  "List of known C/C++ keymaps.")

;;;===================================================================
;;;{{{ LSP detection:

(eval-when-compile

  (defvar *zmacs-c-backend-mode* nil
    "Which backend for C/C++ are we using?")

;;;
;;; For some reason, ccls continuously causes Emacs to lock up.
;;; For this reason, and this reason alone, prefer clangd over ccls.
;;;

;;;-------------------------------------------------------------------
;;;{{{ `clangd':

  (when (and (executable-find "clangd")
             (null *zmacs-c-backend-mode*))
    (setq *zmacs-c-backend-mode* "clangd"))

;;;}}}
;;;-------------------------------------------------------------------

;;;-------------------------------------------------------------------
;;;{{{ `ccls':

  (when (and (executable-find "ccls")
             (null *zmacs-c-backend-mode*))
    (setq *zmacs-c-backend-mode* "ccls")

    (use-package ccls :ensure t)
    (require 'ccls))

;;;}}}
;;;-------------------------------------------------------------------

  (when (not (null *zmacs-c-backend-mode*))
    (require 'eglot)
    (message (format "Configured '%s' as LSP backend." *zmacs-c-backend-mode*))
    (when (not (string= *zmacs-c-backend-mode* "ccls"))
      (add-to-list 'eglot-server-programs `(c-c++-modes
                                            ,*zmacs-c-backend-mode*)))
    (dolist (mode c-c++-modes)
      (add-hook mode #'eglot-ensure))))

;;;}}}
;;;===================================================================

;;; Clang format.
(use-package clang-format
  :defer t
  :if (executable-find "clang-format"))

;;; Insert and delete C++ headers automatically.
(use-package cpp-auto-include
  :defer t)

;;; Disassemble C, C++, or Fortran.
(use-package disaster
  :defer t
  :config
  (require 'disaster)
  (dolist (map c-c++-maps)
    (define-key map   (kbd "C-c d") #'disaster))
  (when (boundp 'fortran-mode-map)
    (define-key fortran-mode-map (kbd "C-c d") #'disaster)))

(use-package gdb-mi
  :defer t
  :init
  (setq gdb-many-windows t
        gdb-show-main    t))

(use-package ppindent
  :defer t
  :vc (:fetcher github
       :repo emacsmirror/ppindent)
  :config
  (require 'ppindent))

;;;===================================================================
;;;{{{ Mode hooks:

(defun zmacs--cc-mode-hook ()
  "ZMACS C/C++ mode hook."
  (when (not (null *zmacs-c-backend-mode*))
    (message "Ensuring that C/C++ modes use Eglot!")
    (setq-local eglot-ensure t))
  (setq-local indent-tabs-mode nil
              visual-line-mode nil
              auto-fill-mode   t
              flycheck-mode    t))

(add-hook 'makefile-mode-hook (lambda ()
                                (setq-local indent-tabs-mode t)))

(dolist (mode c-c++-modes)
  (add-hook 'c-mode-hook #'zmacs--cc-mode-hook)
  (zmacs--add-realgud-debugger mode "gdb"))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Indentation hacks:

;;;###autoload
(defun google-c-lineup-expression-plus-4 (langelem)
  "Indents to the beginning of the current C expression plus 4 spaces.

This implements title \"Function Declarations and Definitions\" of the Google
C++ Style Guide for the case where the previous line ends with an open
parenthese.

\"Current C expression\", as per the Google Style Guide and as clarified by
subsequent discussions,
means the whole expression regardless of the number of nested parentheses, but
excluding non-expression material such as \"if(\" and \"for(\" control
structures.

Suitable for inclusion in `c-offsets-alist'."
  (save-excursion
    (back-to-indentation)
    (c-backward-syntactic-ws)
    (back-to-indentation)
    (if (looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
        (goto-char (match-end 1)))
    (vector (+ 4 (current-column)))))

;;;###autoload
(defconst google-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . nil)
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (brace-list-open after)
                               (brace-list-intro after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label after)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . t)
    (comment-column . 40)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       one-liner-defun
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                        (func-decl-cont . ++)
                        (member-init-intro . +)
                        (inher-intro . +)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . -)
                        (innamespace . 2))))
  "Google C/C++ Programming Style")

;;;###autoload
(defun google-set-c-style ()
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "google" google-c-style t))

;;;###autoload
(defun google-make-newline-indent ()
  (interactive)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent))

;;;###autoload
(setq-default c-default-style '((c-mode      . "google")
                                (c++-mode    . "google")
                                (objc-mode   . "google")
                                (java-mode   . "google")
                                (cperl-mode  . "google")
                                (csharp-mode . "google")
                                (other       . "gnu"))
              c-basic-offset  2)

(when (or (boundp 'align-exclude-key-list)
          (boundp 'align-custom-rules-list))
  (makunbound 'align-exclude-key-list)
  (makunbound 'align-custom-rules-list))

(defvar align-exclude-key-list '(c-variable-declaration
                                 c-assignment))

(defvar align-custom-rules-list
  `((c-assignment
     (regexp . ,(concat "[^-=!^&*+<>/| \t\n]\\(\\s-*[-=!^&*+<>/|]*\\)"
                        "=\\(\\s-*\\)\\([^= \t\n]\\|$\\)"))
     (group . (1 2))
     (justify . t)
     (modes . align-c++-modes))
    (c-variable-declaration
     (regexp . ,(concat "[*&0-9A-Za-z_]\\(?:\\s-*>\\)?[&*]*\\(\\s-+[*&]*\\)"
                        "[A-Za-z_][0-9A-Za-z:_]*\\s-*\\(\\()\\|"
                        "=[^=\n].*\\|(.*)\\|\\(\\[.*\\]\\)*\\)?"
                        "\\s-*[;,]\\|)\\s-*$\\)"))
     (group . 1)
     (modes . align-c++-modes)
     (justify . t)
     (valid
      . ,(function
          (lambda ()
            (not (or (save-excursion
                       (goto-char (match-beginning 1))
                       (backward-word 1)
                       (looking-at
                        "\\(goto\\|return\\|new\\|delete\\|throw\\)"))
                     (if (and (boundp 'font-lock-mode)
                              font-lock-mode)
                         (eq (get-text-property (point) 'face)
                             'font-lock-comment-face)
                       (eq (caar (c-guess-basic-syntax)) 'c))))))))))

(defvar cc-mode::align-mode-rules-list (append align-custom-rules-list))

(dolist (key align-exclude-key-list)
  (setq cc-mode::align-mode-rules-list
        (remove (assq key cc-mode::align-mode-rules-list)
                cc-mode::align-mode-rules-list)))

(setq cc-mode::align-mode-rules-list
      (append align-custom-rules-list
              cc-mode::align-mode-rules-list))

(add-hook 'c-mode-common-hook
          #'(lambda ()
              (setq align-mode-rules-list
                    cc-mode::align-mode-rules-list)))

(defun perform-c-alignment ()
  (unless (or (eq (caar c-syntactic-context) 'c)
              (eq (caar c-syntactic-context) 'topmost-intro)
              (eq (caar c-syntactic-context) 'brace-list-close))
    (save-excursion
      (let ((place (if (and (listp (cadr c-syntactic-context))
                            (not (null (cadr c-syntactic-context)))
                            (not (null (cadadr c-syntactic-context))))
                       (cadadr c-syntactic-context)
                     (cadar c-syntactic-context)))
            (saved-point (point)))
        (if (not (null place))
            (align place (point) "\\({$\\|^\\s-*}\\|^\\(\\s-*\\)$\\)"))))))

(add-hook 'c-special-indent-hook 'perform-c-alignment)

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]*"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Set up `c++-mode' to better handle \"enum class\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(use-package google-c-style
  :defer t)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;;;}}}
;;;===================================================================

(provide 'zmacs-prog-c)

;;; zmacs-prog-c.el ends here.
