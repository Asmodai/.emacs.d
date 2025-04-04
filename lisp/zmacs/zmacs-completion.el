;;; zmacs-completion.el --- Completion-related packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 15:30:11
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

;;;; EShadow:

(use-package rfn-eshadow
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :custom
  (resize-mini-windows t)
  (read-answer-short   t)
  (echo-keystrokes     0.25)
  (kill-ring-max       60)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;;;; Vertico:
;;;;; Main package:

(use-package vertico
  :commands (vertico-map
             vertico-exit
             vertico-mode)
  :bind (:map vertico-map
         ("<escape>"  . #'zmacs/minibuffer-keyboard-quit)
         ("M-RET"     . #'vertico-exit))
  :hook (emacs-startup . vertico-mode)
  :custom
  (vertico-cycle         nil)
  (vertico-resize        t)
  (vertico-scroll-margin 0)
  (vertico-count         10)
  :config
  (progn

    (defun zmacs/minibuffer-keyboard-quit ()
      "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it; then it
takes a second \\[keyboard-quit] to abort the minibuffer."
      (interactive)
      (if (and delete-selection-mode transient-mark-mode mark-active)
          (setq deactivate-mark  t)
        (when (get-buffer "*Completions*")
          (delete-windows-on "*Completions*"))
        (abort-recursive-edit)))

    (with-eval-after-load 'rfn-eshadow
      (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

    (defun zmacs/directories-before-files (files)
      "Put directories before files."
      (setq files (vertico-sort-history-length-alpha files))
      (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
             (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

    (defvar zmacs/completion-category-sort-function-overrides
      '((file . zmacs/directories-before-files))
      "Completion category-specific sorting function overrides.")

    (defun zmacs/completion-category-sort-function ()
      "Sort completion by category."
      (alist-get (vertico--metadata-get 'category)
                 zmacs/completion-category-sort-function-overrides))

    (advice-add #'vertico--sort-function :before-until
                #'zmacs/completion-category-sort-function)))

;;;; Vertico repeat:

(use-package vertico-repeat
  :ensure nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :commands (vertico-repeat-save))

;;;; Vertico directory:

(use-package vertico-directory
  :ensure nil
  :after vertico
  :commands (vertico-directory-enter
             vertico-directory-delete-char
             vertico-directory-delete-word
             vertico-directory-tidy)
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(defun zmacs/crm-indicator (args)
  "Append a CRM indicator to ARGS."
  (cons (concat "[CRM] " (car args))
        (cdr args)))

(advice-add #'completing-read-multiple :filter-args #'zmacs/crm-indicator)

;;;; Minibuffer settings:
;;;;; Grow and shrink minibuffer:

(setq resize-mini-windows t)

;;;;; Do not allow the cursor in the minibuffer prompt:

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;;;; Enable recursive minibuffers.

(setf enable-recursive-minibuffers t)

;;;; Prescient:
;;;;; Main mode:

(use-package prescient
  :after vertico
  :config
  (require 'prescient)
  (push 'prescient completion-styles)
  (prescient-persist-mode))

;;;;; Vertico Presceient:

(use-package vertico-prescient
  :after vertico
  :config
  (vertico-prescient-mode))

;;;; Orderless:

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file
                                         (styles . (partial-completion))))))

;;;; Marginalia:

(use-package marginalia
  :ensure t
  :commands (marginalia-cycle
             marginalia-mode)
  :bind (:map minibuffer-local-map
         ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-align 'center))

;;;; Consult:
;;;;; Main package:

(use-package consult
  :commands (consult-line
             consult-line-multi
             consult-buffer
             consult-project-buffer
             consult-find
             consult-apropos
             consult-yank-pop
             consult-goto-line
             consult-org-agenda
             consult-org-heading
             consult-flymake)
  :bind (:map project-prefix-map
              ("b" . consult-project-buffer)
              ("m" . consult-bookmark))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (fset 'multi-occur #'consult-multi-occur)
  :config
  (progn

    (consult-customize consult-ripgrep
                       consult-git-grep
                       consult-grep
                       consult-bookmark
                       consult-recent-file
                       consult-xref
                       consult--source-bookmark
                       consult--source-recent-file
                       consult--source-project-recent-file
                       consult-theme
                       :preview-key '(:debounce 0.2 any))

    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    (advice-add #'register-preview :override #'consult-register-window)

    (setq xref-show-xrefs-function       #'consult-xref
          xref-show-definitions-function #'consult-xref)

    (setq consult-ripgrep-args (mapconcat #'identity
                                          (list "rg"
                                                "--null"
                                                "--line-buffered"
                                                "--color=never"
                                                "--max-columns=1000"
                                                "--path-separator /"
                                                "--smart-case"
                                                "--no-heading"
                                                "--with-filename"
                                                "--line-number"
                                                "--search-zip")
                                          " ")
          consult-async-min-input 2)

    (when (zlisp/macos-p)
      (setq consult-locate-args "mdfind -name"))

    (bind-key "C-h i" #'consult-info)))

;;;;; Consult directory:

(use-package consult-dir
  :commands (consult-dir
             consult-dir-jump-file)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;;;; ZLisp consult:

(use-package zlisp-consult
  :ensure nil
  :after consult
  :config
  (require 'zlisp-consult))

;;;; Which Key:

(use-package which-key
  :ensure t
  :demand t
  :config
  (require 'which-key))

;;;; Embark:
;;;;; Main package:

(use-package embark
  :after which-key
  :demand t
  :commands (embark-act
             embark-dwim
             embark-bindings
             embark-act-noexit
             embark-switch-to-live-occur
             embark-occur-toggle-view
             embark-keymap-help)
  :custom
  (embark-prompter 'embark-keymap-prompter)
  :bind (("C-."   . embark-act)
         ("M-."   . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-completion-map
         ("C-;"   . embark-act-noexit)
         ("C-S-o" . embark-act)
         ("C-J"   . embark-switch-to-live-occur)
         ("M-q"   . embark-occur-toggle-view)
         :map completion-list-mode-map
         (";"     . embark-act)
         :map embark-file-map
         ("x"     . consult-file-externally)
         :map embark-general-map
         ("A"     . marginalia-cycle))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (progn
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))))

;;;;; ZLisp embark:

(use-package zlisp-embark
  :after embark
  :ensure nil
  :demand t
  :config
  (progn
    (require 'zlisp-embark)

    (define-key embark-file-map (kbd "D") #'zlisp/dired-here)
    (define-key embark-file-map (kbd "g") #'zlisp/consult-rg-here)

    (setq embark-indicators
          '(zlisp/embark-which-key-indicator
            embark-minimal-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

    (advice-add #'embark-completing-read-prompter :around
                #'zlisp/embark-hide-which-key-indicator)))

;;;;; Embark Consult:

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; Corfu:

(use-package corfu
  :hook (window-setup . global-corfu-mode)
  :bind (:map corfu-map
              ("C-j"      . corfu-next)
              ("C-k"      . corfu-previous)
              ("C-g"      . corfu-quit)
              ("M-l"      . corfu-show-location)
              ("M-SPC"    . corfu-insert-separator)
              ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("TAB"      . corfu-insert)
              ([tab]      . corfu-insert))
  :custom
  (corfu-auto                  t)          ; Enable auto-completion.
  (corfu-min-width             25)         ; Minimum completion window width.
  (corfu-max-width             100)        ; Maximum completion window width.
  (corfu-count                 10)         ; Number of candidates to show.
  (corfu-scroll-margin         5)          ; Use scroll margin.
  (corfu-cycle                 t)          ; Enable cycling.
  (corfu-separator             ?\s)        ;
  (corfu-quit-no-match         'separator) ;
  (corfu-quit-at-boundary      'separator) ;
  (corfu-preview-current       t)          ; Enable current candidate preview.
  (corfu-preselect             'prompt)    ; Preselect the prompt.
  (corfu-history-mode          1)          ;
  (corfu-popupinfo-delay       1)          ;
  :config
  (progn

    (defun corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point
                               (list (current-local-map)))
        (corfu-mode 1)))

    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
    (add-hook 'eshell-mode-hook (lambda ()
                                  (setq-local corfu-quit-no-match    t
                                              corfu-quit-at-boundary t
                                              corfu-auto             nil)
                                  (corfu-mode)))

    (require 'corfu-popupinfo)
    (corfu-popupinfo-mode 1)
    (global-corfu-mode)))

;;;; Emacs completion settings:

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'compolete)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p))

;;;; Cape:

(use-package cape
  :ensure t
  :after corfu
  :bind ("C-c p" . cape-prefix-map)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;;;; DAbbrev:

(use-package dabbrev
  :bind (("M-/"   . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;;;; Kind-Icon:

(use-package kind-icon
  :defer 1
  :custom
  (kind-icon-use-icons t)
  ;;(kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (svg-lib-icons-dir (concat *zmacs-cache-directory* "svg-lib/cache/"))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;; Yasnippet:

(defcustom zmacs-all-snippets-dir
  (concat zmacs-storage-directory "all-snippets/")
  "Directory for all snippet files."
  :group 'zmacs-emacs)

(use-package yasnippet
  :bind (:map yas-minor-mode-map
              ("C-'" . yas-expand))
  :preface
  (mkdir (concat zmacs-all-snippets-dir "zmacs/") t)
  (mkdir (concat zmacs-all-snippets-dir "yasnippets/") t)
  :custom
  (yas-snippet-dirs `(,(concat zmacs-all-snippets-dir "zmacs/")
                      ,(concat zmacs-all-snippets-dir "yasnippets/")))
  (yas--loaddir yas-snippets-dirs)
  (yas-installed-snippets-dir yas-snippets-dirs)
  (yas--default-user-snippets-dir yas-snippets-dirs)
  :config
  (progn
    (defun zlisp/yas-org-mode-hook ()
      ;; XXX Well, might be able to abuse this gem later :)
      (setq-local yas-buffer-local-condition '(not (org-in-src-block-p t))))

    (add-hook 'org-mode-hook #'zlisp/yas-org-mode-hook)

    (with-eval-after-load 'warnings
      (push '(yaznippet backquote-change) warning-suppress-types))

    (yas-global-mode 1)))

(use-package yasnippet-snippets
  :after (yasnippet)
  :custom
  (yasnippet-snippets-dir (concat zmacs-all-snippets-dir "yasnippets/")))

;;;; Eglot:

(use-package eglot
  :ensure nil
  :after yasnippet
  :config
  (with-eval-after-load 'flymake
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (put 'eglot-note    'flymake-overlay-control nil)
                (put 'eglot-warning 'flymake-overlay-control nil)
                (put 'eglot-error   'flymake-overlay-control nil)

                (setq eldoc-documentation-functions
                      (cons #'flymake-eldoc-function
                            (remove #'flymake-eldoc-function
                                    eldoc-documentation-functions)))

                (eglot-inlay-hints-mode -1))))

  (setopt eglot-ignored-server-capabilities '(:documentHighlightProvider
                                              :documentLinkProvider
                                              :inlayHintProvider)
          eglot-send-changes-idle-time      0.5
          eglot-events-buffer-size          0
          eglot-sync-connect                nil))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-m r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "M-m o") #'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "M-m h") #'eldoc)
  (define-key eglot-mode-map (kbd "M-m =") #'eglot-format)
  (define-key eglot-mode-map (kbd "M-m ?") #'xref-find-references)
  (define-key eglot-mode-map (kbd "M-.")   #'xref-find-definitions))

;;;; Avy

(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-:")     #'avy-goto-char)
  (global-set-key (kbd "C-'")     #'avy-goto-char-2)
  (global-set-key (kbd "M-g f")   #'avy-goto-line)
  (global-set-key (kbd "M-g e")   #'avy-goto-word-0)
  (global-set-key (kbd "M-g w")   #'avy-goto-word-1)
  (global-set-key (kbd "C-c C-j") #'avy-goto-word-1))

;;;; Provide.

(provide 'zmacs-completion)

;;; zmacs-completion.el ends here.
