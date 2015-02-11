;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-c-mode.el --- C mode hacks.
;;;
;;; Time-stamp: <Monday Dec 23, 2013 19:58:51 asmodai>
;;; Revision:   148
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    16 Feb 2011 07:24:29
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

;;;==================================================================
;;;{{{ Comment editing.:

(when (emacs>=21-p)
  (require 'autopair)
  (require 'c-comment-edit)
  (compile-load "cmake-mode")
  (compile-load "andersl-cmake-font-lock")
  
  (if (featurep 'c-comment-edit)
      (setq c-comment-leader "  ")))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Support for C-based languages:

(when (emacs>=21-p)
  (autoload 'csharp-mode "csharp-mode"
    "Major mode for editing C# code." t)
  
  (setq auto-mode-alist
        (append '(("\\.cs$" . csharp-mode))
                auto-mode-alist)))
;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ CTAGS:

(when (emacs>=21-p)
  (setq path-to-ctags (if (windows-p)
                          "C:/Program Files/Emacs-23.2/bin/ctags"
                          "/usr/bin/ctags"))
  
  (defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s"
           path-to-ctags
           dir-name
           (directory-file-name dir-name)))))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Configure cc-mode:

(when (emacs>=19-p)
  
  ;;
  ;; Require some stuff.
  (require 'cc-mode)
  (require 'align)
  (require 'ppindent)
  
;;; ------------------------------------------------------------------
;;;{{{ Lining up expressions the Google way:

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
  
;;;}}}
;;;------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ An indentation based on the one Google have:

  (defconst google-c-style
    `((c-recognize-knr-p . nil)
      (c-enable-xemacs-performance-kludge-p . t)
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
  
;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Utilities for setting cc-mode:

  (defun google-set-c-style ()
    (interactive)
    (make-local-variable 'c-tab-always-indent)
    (setq c-tab-always-indent t)
    (c-add-style "google" google-c-style t))
  
  (defun google-make-newline-indent ()
    (interactive)
    (define-key c-mode-base-map "\C-m" 'newline-and-indent)
    (define-key c-mode-base-map [ret] 'newline-and-indent))
  
;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Explicitly set the indentation style:

  (setq-default c-default-style
                    '((c-mode      . "google")
                      (c++-mode    . "google")
                      (objc-mode   . "google")
                      (java-mode   . "google")
                      (cperl-mode  . "google")
                      (other       . "gnu")
                      (csharp-mode . "google")))
  
;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Clean up one-liner definitions:

  (if (emacs=22-p)
      ;;
      ;; Emacs 22 seems to have a problem with c-cleanup-list.
      (if (listp c-cleanup-list)
          (add-to-list 'c-cleanup-list 'one-liner-defun)
          (setf c-cleanup-list (append (list c-cleanup-list)
                                       'one-liner-defun)))
      ;;
      ;; Works on everything else.
      (add-to-list 'c-cleanup-list 'one-liner-defun))
  
;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ cc-mode hooks:
  
  (defun my-common-c-mode-hooks ()
    (when (or (emacs=20-p)
	      (emacs=21-p))
      (turn-on-font-lock)
      (font-lock-mode 1))
    (setq indent-tabs-mode nil
          c-max-one-liner-length 80)
    (google-set-c-style)
    (google-make-newline-indent)
    (when (featurep 'company)
      (company-mode 1))
    (when (featurep 'highlight-parentheses)
      (hi-parens-autopair))
    (when (featurep 'srecode)
      (srecode-minor-mode))
    (when (featurep 'semantic)
      (semantic-mode)
      (ede-minor-mode 1))
    (show-paren-mode 1)
    (eldoc-mode 1)
    (c-toggle-auto-state 1)
    (c-toggle-syntactic-indentation 1)
    (c-toggle-electric-state 1)
    (c-toggle-hungry-state 1)
    (c-toggle-auto-hungry-state 1)
    (c-toggle-auto-newline 1)
    (when (emacs>=23-p)
      (subword-mode 1))
    (autopair-mode 1)
    (auto-fill-mode 1))
  
    (add-hook 'c-mode-common-hook 'my-common-c-mode-hooks)
  
;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Set up the semantic bovinator:

    (when (featurep 'semantic)
      (defun my-c-mode-cedet-hook ()
        (local-set-key [(control tab)] 'semantic-ia-complete-symbol))
      
      (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook))
    
;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Doxygen stuff:

    (when (emacs>=23-p)
      (require 'doc-mode)
      (add-hook 'c-mode-common-hook 'doc-mode))

;;;}}}
;;;------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Set up alignment:

  ;;
  ;; Clean up any existing stuff
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
  
  (defvar cc-mode::align-mode-rules-list (append align-rules-list))
  
  (dolist (key align-exclude-key-list)
    (setq cc-mode::align-mode-rules-list
          (remove (assq key cc-mode::align-mode-rules-list)
                  cc-mode::align-mode-rules-list)))
  
  (setq cc-mode::align-mode-rules-list
        (append align-custom-rules-list
                cc-mode::align-mode-rules-list))
  
  (add-hook 'c-mode-common-hook
            '(lambda ()
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
  
;;;}}}
;;;------------------------------------------------------------------
  
;;; ------------------------------------------------------------------
;;;{{{ Fix for C++11's `enum class':
  
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

;;;}}}
;;;------------------------------------------------------------------


  )

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Configure cmake:

(if (featurep 'cmake-mode)
    (setq auto-mode-alist
          (append
           '(("CMakeLists\\.txt\\'" . cmake-mode))
           '(("\\.cmake\\'" . cmake-mode))
           auto-mode-alist))
    
    (add-hook 'cmake-mode-hook 'andersl-cmake-font-lock-activate))

;;;}}}
;;;==================================================================

;;; site-c-mode.el ends here
