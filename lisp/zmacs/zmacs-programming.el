;;; zmacs-programming.el --- Generic programming packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:38:21
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

;;;; Show pretty symbols:

(use-package prog-mode
  :ensure nil
  :defer t
  :custom
  (prettify-symbols-unprettify-at-point t))

;;;; Delimiters and identifiers:
;;;;; Rainbow Delimiters:

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;; Raimbow Identifiers:

(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode)

;;;;; Highlight Parentheses:

(use-package highlight-parentheses
  :defer t
  :custom
  (highlight-parentheses-delay 0.2)
  (highlight-parentheses-colors '("#dcff75"
                                  "#f3cc62"
                                  "#fd964f"
                                  "#ff543d"))
  :custom-face (highlight-parentheses-highlight ((nil (:weight ultra-bold))))
  :commands highlight-parentheses-minibuffer-setup
  :init
  (add-hook 'prog-mode-hook #'highlight-parentheses-mode)
  (add-hook 'minibuffer-setup-hook #'highlight-parentheses-minibuffer-setup)
  :config
  (zmacs-diminish highlight-parentheses-mode " ⫈" " hP"))

;;;;; Embrace:

(use-package embrace
  :bind (("C-M-s-#" . embrace-commander))
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))

(use-package highlight-numbers
  :defer t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1))))

;;;; Smart parentheses.

(use-package smartparens
  :defer t
  :commands (show-smartparens-global-mode
             smart-parens-mode
             smartparens-global-strict-mode
             smartparens-strict-mode
             smartparens-mode)
  :custom
  (sp-show-pair-delay (or (bound-and-true-p sp-show-pair-delay) 0.2))
  (sp-show-pair-from-inside t)
  (sp-cancel-autoskip-on-backward-movement nil)
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  :config
  (progn
    (require 'smartparens-config)
    (show-smartparens-global-mode +1)
    (zmacs-diminish smartparens-mode " 🄪" " SP")))

(defun zmacs/deactivate-smartparens (&optional global)
  "Deactivate `smartparens-mode' and `smartparens-strict-mode'.

If GLOBAL is non-NIL then we work on the global modes."
  (if global
      (progn
        (when smartparens-global-strict-mode
          (smartparens-global-strict-mode -1))
        (smartparens-global-mode -1))
    (when (and (boundp 'smartparens-strict-mode)
               smartparens-strict-mode)
      (smartparens-strict-mode -1))
    (smartparens-mode -1)))

;;;; Paredit:

(use-package paredit
  :defer t
  :commands (enable-paredit-mode
             paredit-mode)
  :hook ((emacs-lisp-mode  . enable-paredit-mode)
         (lisp-mode-hook   . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode))
  :config
  (zmacs-diminish paredit-mode " ⊚" "P"))

(defun zmacs/deactivate-paredit ()
  (paredit-mode -1))

;;;; Tree Sitter:

(setq treesit-extra-load-path `(,(concat *zmacs-cache-directory*
                                         "tree-sitter/")))

;; List languages for install
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                    "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
             "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                    "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Install all languages
(defun zmacs-install-treesit-lang-grammar ()
  "Install tree-sitter language grammars in alist."
  (interactive)
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist)))

(use-package tree-sitter
  :defer t)

(setq major-mode-remap-alist
      '((yaml-mode       . yaml-ts-mode)
        (bash-mode       . bash-ts-mode)
        (js2-mode        . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode       . json-ts-mode)
        (css-mode        . css-ts-mode)
        (python-mode     . python-ts-mode)))

;;;; Multiple cursors:

(use-package iedit
  :bind (:map lem+search-keys
         ("c" . iedit-mode)))

;;;; Indentation:

;; (use-package aggressive-indent
;;   :preface
;;   (defun lem-aggressive-indent-mode-off ()
;;     (aggressive-indent-mode 0))
;;   :hook ((css-mode              . aggressive-indent-mode)
;;          (emacs-lisp-mode       . aggressive-indent-mode)
;;          (lisp-interaction-mode . aggressive-indent-mode)
;;          (lisp-mode             . aggressive-indent-mode)
;;          (js-mode               . aggressive-indent-mode)
;;          (sgml-mode             . aggressive-indent-mode))
;;   :config
;;   (progn
;;     (setq-default aggressive-indent-comments-too t)
;;     (add-to-list 'aggressive-indent-protected-commands 'comment-dwim)))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-method 'character
                ;;highlight-indent-guides-character ?\|
                ;; default is \x2502 but it is very slow on Mac
                ;; highlight-indent-guides-character ?\xFFE8
                highlight-indent-guides-responsive 'top
                highlight-indent-guides-auto-odd-face-perc 100
                highlight-indent-guides-auto-even-face-perc 100
                highlight-indent-guides-auto-character-face-perc 100
                highlight-indent-guides-auto-enabled t
                highlight-indentation-blank-lines t))

;;;; Flymake and Flycheck:

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout nil)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  (flymake-mode-line-counter-format '("" flymake-mode-line-error-counter
                                      flymake-mode-line-warning-counter
                                      flymake-mode-line-note-counter
                                      ""))
  (flymake-mode-line-format '(" "
                              flymake-mode-line-exception
                              flymake-mode-line-counters)))

(use-package flymake-collection
  :hook (after-init . flymake-collection-hook-setup))

(use-package consult-flymake
  ;; comes with consult
  :ensure nil
  :bind (:map lem+flymake-keys
         ("c" . consult-flymake)))

;;;; Compilation:

(use-package multi-compile
  :commands (compile multi-compile-run)
  :custom
  (multi-compile-history-file (concat *zmacs-cache-directory*
                                      "multi-compile.cache"))
  (multi-compile-completion-system 'default)
  :config
  ;; Use for book compiling
  (defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (stringp string)
         (string-match (rx-to-string `(: bos ,prefix) t) string))))

(defun zmacs-upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
manage to find it, return the containing directory. Else if we
get to the toplevel directory and still can't find it, return
nil. Start at startdir or . if startdir not given"
  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get to / so that we only check it once
                    ; While we've neither been at the top last time nor have we
                    ; found  the file.
    (while (not (or found top))
      ;; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
          (setq top t))
      ;; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
        ;; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
    (if found
        dirname
      nil)))

(defun zmacs-compile-next-makefile ()
  (interactive)
  (let* ((default-directory (or (zmacs-upward-find-file "Makefile") "."))
         (compile-command (concat "cd " default-directory " && "
                                  compile-command)))
    (compile compile-command)))

;;;; Emacs Lisp:

(use-package lisp-mode
  :ensure nil
  :commands lisp-mode)

(use-package emacs-lisp-mode
  :ensure nil
  :mode (("\\.el$" . emacs-lisp-mode))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package eldoc
  :ensure nil
  :commands eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  :diminish eldoc-mode
  :config
  (add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)
    (setq eldoc-idle-delay 0))

(use-package elisp-def
  :commands (elisp-def
             elisp-def-mode)
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  ielm-mode-hook
                  lisp-interaction-mode-hook))
    (add-hook hook #'elisp-def-mode)))

(use-package inspector
  :defer t)

(use-package macrostep
  :defer t
  :mode (("\\*.el\\'" . emacs-lisp-mode)
         ("Cask\\'"   . emacs-lisp-mode)))

(use-package edebug
  :defer t)

(use-package bug-hunter
  :defer 2)

(use-package elisp-def
  :defer t)

(use-package emr
  :defer t)

(use-package esup
  :ensure nil
  :init
  (unless (package-installed-p 'esup)
    (package-vc-install "https://github.com/kiennq/esup.git"))
  :commands esup)

(use-package flycheck-elsa
  :defer t
  :hook (emacs-lisp-mode . flycheck-elsa-setup))

(use-package ggtags
  :defer t)

(use-package nameless
  :defer t
  :init
  (setq nameless-separator nil
        nameless-prefix    ">")

  (put 'nameless-current-name 'safe-local-variable #'stringp))

(use-package overseer
  :defer t)

;;;; Auto Fill Mode

(add-hook 'prog-mode #'auto-fill-mode)

;;;; Useful functions:

(defun zmacs-eval-current-form ()
    "Looks for the current def* or set* command then evaluates, unlike
`eval-defun', does not go to topmost function."
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun zmacs-nav-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))

;;;; Emacs indentation shuffle:

(with-eval-after-load 'el-patch
  (el-patch-defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (el-patch-let
        (($cond (and (elt state 2)
                     (el-patch-wrap 1 1
                       (or (not (looking-at "\\sw\\|\\s_"))
                           (looking-at ":")))))
         ($then (progn
                  (if (not (> (save-excursion (forward-line 1) (point))
                              calculate-lisp-indent-last-sexp))
                      (progn (goto-char calculate-lisp-indent-last-sexp)
                             (beginning-of-line)
                             (parse-partial-sexp
                              (point)
                              calculate-lisp-indent-last-sexp 0 t)))
                  ;; Indent under the list or under the first sexp on the same
                  ;; line as calculate-lisp-indent-last-sexp.  Note that first
                  ;; thing on that line has to be complete sexp since we are
                  ;; inside the innermost containing sexp.
                  (backward-prefix-chars)
                  (current-column)))
         ($else (let ((function (buffer-substring
                                 (point)
                                 (progn (forward-sexp 1) (point))))
                      method)
                  (setq method (or (function-get (intern-soft function)
                                                 'lisp-indent-function)
                                   (get (intern-soft function)
                                        'lisp-indent-hook)))
                  (cond ((or (eq method 'defun)
                             (and (null method)
                                  (> (length function) 3)
                                  (string-match "\\`def" function)))
                         (lisp-indent-defform state indent-point))
                        ((integerp method)
                         (lisp-indent-specform method state
                                               indent-point normal-indent))
                        (method
                         (funcall method indent-point state))))))
      (let ((normal-indent (current-column))
            (el-patch-add
              (orig-point (point))))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (el-patch-swap
          (if $cond
              ;; car of form doesn't seem to be a symbol
              $then
            $else)
          (cond
           ;; car of form doesn't seem to be a symbol, or is a keyword
           ($cond $then)
           ((and (save-excursion
                   (goto-char indent-point)
                   (skip-syntax-forward " ")
                   (not (looking-at ":")))
                 (save-excursion
                   (goto-char orig-point)
                   (looking-at ":")))
            (save-excursion
              (goto-char (+ 2 (elt state 1)))
              (current-column)))
           (t $else)))))))

;;;; GraphQL:

(use-package graphql
  :defer t)

;;;; Protobuf:

(use-package protobuf-mode
  :defer t
  :hook ((protobuf-mode . outline-minor-mode)
         (protobuf-mode . outli-mode))
  :init
  (require 'outline)
  (require 'outline-minor-faces))

;;;; Bison:

(use-package bison-mode
  :defer t
  :hook ((bison-mode . outline-minor-mode)
         (bison-mode . outli-mode)
         (flex-mode  . outline-minor-mode)
         (flex-mode  . outli-mode))
  :mode (("\\.l\\'" . bison-mode)
         ("\\.y\\'" . flex-mode))
  :init
  (require 'outline)
  (require 'outline-minor-faces))

;;;; Provide package:

(provide 'zmacs-programming)

;;; zmacs-programming.el ends here.
