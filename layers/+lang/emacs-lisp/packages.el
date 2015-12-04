
(setq emacs-lisp-packages
      '(company
        eldoc
        elisp-slime-nav
        (emacs-lisp :location built-in)
        flycheck
        ielm
        macrostep
        semantic
        smartparens
        srefactor))

(use-package ielm
  :config
  (progn
    (defun ielm-indent-line ()
      (interactive)
      (let ((current-point (point)))
        (save-restriction
          (narrow-to-region (search-backward-regexp "^ELISP>")
                            (goto-char current-point))
          (lisp-indent-line))))))
    
(defun emacs-lisp:post-init-company ()
  (bootstrap:add-company-hook ielm-mode)
  (push '(company-files company-capf) *company-backends-ielm-mode*))

(defun emacs-lisp:init-elisp-slime-nav ()
  (use-package elisp-slime-anv
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))))

(defun emacs-lisp:init-macrostep ()
  (use-package macrostep
    :defer t
    :mode ("\\*.el\\'" . emacs-lisp-mode)))

(defun emacs-lisp:post-init-flycheck ()
  (setq flycheck-emacs-lisp-load-path 'inherit))

(defun emacs-lisp:post-init-semantic ()
  (semantic/enable-semantic-mode 'emacs-lisp-mode)
  (eval-after-load 'semantic
    '(semantic-default-elisp-setup)))

(defun emacs-lisp:post-init-srefactor ()
  (add-hook 'emacs-lisp-mode-hook 'bootstrap:lazy-load-srefactor)
  (use-package srefactor-lisp
    :commands (srefactor-lisp-format-buffer
               srefactor-lisp-format-defun
               srefactor-lisp-format-sexp
               srefactor-lisp-one-line)))

(defun emacs-lisp:post-init-smartparens ()
  (defun bootstrap:eval-current-form-sp (&optional arg)
    (interactive "P")
    (require 'smartparens)
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp)))

  (defun bootstrap:eval-current-symbol-so ()
    (interactive)
    (require 'smartparens)
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp))))

(defun bootstrap:emacs-lisp-hook ()
  (font-lock-mode 1)
  (smartparens-mode 1)
  (company-mode 1)
  (eldoc-mode 1)
  (auto-fill-mode 1))

(defun bootstrap:ielm-hook ()
  (font-lock-mode 1)
  (smartparens-mode -1)
  (company-mode 1)
  (eldoc-mode 1)
  (auto-fill-mode 1))

(defun emacs-lisp:init-emacs-lisp ()
  (push 'company-capf *company-backends-emacs-lisp-mode*)
  (bootstrap:add-company-hook emacs-lisp-mode)
  (add-hook 'emacs-lisp-mode-hook 'bootstrap:emacs-lisp-hook)
  (add-hook 'lisp-interaction-mode-hook 'bootstrap:emacs-lisp-hook)
  (add-hook 'ielm-mode-hook 'bootstrap:ielm-hook))
