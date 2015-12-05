;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Auto-completion packages.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2015 Paul Ward <pward@alertlogic.com>
;;;
;;; Author:     Paul Ward <pward@alertlogic.com>
;;; Maintainer: Paul Ward <pward@alertlogic.com>
;;; Created:    04 Dec 2015 14:43:38
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

(setq auto-completion-packages
      '(auto-complete
        ac-ispell
        company
        company-statistics
        helm-c-yasnippet
        hippie-exp
        yasnippet
        auto-yasnippet))

(unless (version< emacs-version "24.4")
  (push 'company-quickhelp auto-completion-packages))

(defun auto-completion:init-ac-ispell ()
  (use-package ac-ispell
    :defer t
    :init
    (progn
      (setq ac-ispell-requires 4)
      (eval-after-load 'auto-complete
        '(ac-ispell-setup)))))

(defun auto-complete:init-auto-complete ()
  (use-package auto-complete
    :defer t
    :init
    (setq ac-auto-start 0
          ac-delay 0.2
          ac-quick-help-delay 1.
          ac-use-fuzzy t
          ac-fuzzy-enable t
          ac-comphist-file (concat +bootstrap-cache-directory+
                                   "ac-comphist.dat")
          tab-always-indent 'complete
          ac-dwim t)
    :config
    (progn
      (require 'auto-complete-config)
      (setq-default ac-source '(ac-source-abbrev
                                ac-source-dictionary
                                ac-source-words-in-same-mode-buffers))
      (when (bootstrap-layer:package-used-p 'yasnippet)
        (push 'ac-source-yasnippet ac-sources))
      (add-to-list 'completion-styles 'initials t)
      (define-key ac-completing-map (kbd "C-j") 'ac-next)
      (define-key ac-completing-map (kbd "C-k") 'ac-previous)
      (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
      (bootstrap:diminish auto-complete-mode " ⓐ" " a"))))

(defun auto-completion:init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay 0.2
            company-minimum-prefix-length 2
            company-require-match nil
            company-dabbrev-ignore-case nil
            company-dabbrev-downcase nil
            company-frontends '(company-pseudo-tooltip-frontend))

      (defvar *company-fci-mode-on-p* nil)

      (defun company-turn-off-fci (&rest ignore)
        (when (boundp 'fci-mode)
          (setq *company-fci-mode-on-p* fci-mode)
          (when fci-mode
            (fci-mode -1))))

      (defun company-maybe-turn-on-fci (&rest ignore)
        (when *company-fci-mode-on-p*
          (fci-mode 1)))

      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
    :config
    (progn
      (bootstrap:diminish company-mode " ⓐ" " a")

      (defun bootstrap::company-complete-common-or-cycle-keyword ()
        (interactive)
        (company-complete-common-or-cycle -1))

      (bootstrap::auto-completion-set-RET-key-behavior 'company)
      (bootstrap::auto-completion-set-TAB-key-behavior 'company)

      (let ((map company-active-map))
        (define-key map (kbd "C-/") 'company-search-candidates)
        (define-key map (kbd "C-M-/") 'company-filter-candidates)
        (define-key map (kbd "C-d") 'company-show-doc-buffer)
        (define-key map (kbd "C-j") 'company-select-next)
        (define-key map (kbd "C-k") 'company-select-previous)
        (define-key map (kbd "C-l") 'company-complete-selection))

      ;; Nicer looking faces
      (custom-set-faces
       '(company-tooltip-common
         ((t (:inherit company-tooltip
              :weight bold
              :underline nil))))
       '(company-tooltip-common-selection
         ((t (:inherit company-tooltip-selection
              :weight bold
              :underline nil)))))

      (defun bootstrap::company-transformer-cancel (candidates)
        (unless (member company-prefix *company-mode-completion-cancel-keywords*)
          candidates))

      (setq company-transformers '(bootstrap::company-transformer-cancel
                                   company-sort-by-occurrence)))))

(defun auto-completion:init-company-statistics ()
  (use-package company-statistics
    :if *auto-completion-enable-sort-by-usage*
    :defer t
    :init
    (progn
      (setq company-statistics-file (concat +bootstrap-cache-directory+
                                            "company-statistics-cache.el"))
      (add-hook 'company-mode-hook 'company-statistics-mode))))

(defun auto-completion:init-company-quickhelp ()
  (use-package company-quickhelp
    :if (and *auto-completion-enable-help-tooltip*
             (display-graphic-p))
    :defer t
    :init (add-hook 'company-mode-hook 'company-quickhelp-mode)))

(defun auto-completion:init-helm-c-yasnippet ()
  (use-package helm-c-yasnippet
    :defer t
    :init
    (progn
      (defun bootstrap:helm-yas ()
        (interactive)
        (bootstrap:load-yasnippet)
        (require 'helm-c-yasnippet)
        (call-interactively 'helm-yas-complete))

      (setq helm-c-yas-space-match-any-greedy t))))

(defun auto-completion:init-hippie-exp ()
  ;; replace dabbrev-expand
  (global-set-key (kbd "M-/") 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol))

  (when (bootstrap-layer:package-used-p 'yasnippet)
    (push 'yas-hippie-try-expand hippie-expand-try-functions-list)))

(defun auto-completion:init-yasnippet ()
  (use-package yasnippet
    :commands (yas-global-mode yas-minor-mode)
    :init
    (progn
     ;; We don't want undefined variable errors
      (defvar yas-global-mode nil)

      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))

      ;; allow nested expansions
      (setq yas-triggers-in-field t)

      ;; this makes it easy to get out of a nested expansion
      (define-key yas-minor-mode-map
        (kbd "M-s-/") 'yas-next-field)

      ;; add key into candidate list
      (setq helm-yas-display-key-on-candidate t)

      (defun bootstrap:load-yasnippet ()
        (unless yas-global-mode
          (progn
            (yas-global-mode 1)
            (let ((private-yas-dir
                   (if *auto-completion-private-snippets-directory*
                       *auto-completion-private-snippets-directory*
                     (concat +bootstrap-private-directory+
                             "snippets/"))))
              (setq yas-snippet-dirs
                    (append (list private-yas-dir)
                            (when (boundp 'yas-snippet-dirs)
                              yas-snippet-dirs)))
              (yas-load-directory private-yas-dir t)
              (setq yas-wrap-around-region t))))
        (yas-minor-mode 1))

      (bootstrap:add-to-hooks 'bootstrap:load-yasnippet '(prog-mode-hook
                                                          markdown-mode-hook
                                                          org-mode-hook))
      (defun bootstrap:force-yasnippet-off ()
        (yas-minor-mode -1)
        (setq yas-dont-activate t))

      (bootstrap:add-to-hooks 'bootstrap:force-yasnippet-off
                              '(term-mode-hook
                                shell-mode-hook
                                eshell-mode-hook)))
    :config
    (progn
      (defvar smartparens-enabled-initially t)

      (add-hook 'yas-before-expand-snippet-hook
                (lambda ()
                  (setq smartparens-enabled-initially smartparens-mode)
                  (smartparens-mode -1)))

      (add-hook 'yas-after-exit-snippet-hook
                (lambda ()
                  (when smartparens-enabled-initially
                    (smartparens-mode 1))))

      (bootstrap:diminish yas-minor-mode " ⓨ" " y"))))

(defun auto-completion:init-auto-yasnippet ()
  (use-package auto-yasnippet
    :defer t
    :init
    (progn
      (setq aya-persist-snippets-dir (concat
                                      +bootstrap-private-directory+
                                      "snippets/"))

      (defun bootstrap:auto-yasnippet-expand ()
        (interactive)
        (call-interactively 'aya-expand)))))

;;; packages.el ends here
