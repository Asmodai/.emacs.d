
(require 'cl)
(require 'cl-lib)
(require 'bootstrap-emacs-backports)
(require 'bootstrap-buffer)
(require 'bootstrap-predicates)
(require 'bootstrap-funs)
(require 'bootstrap-git)
(require 'bootstrap-auto-completion)
(require 'bootstrap-layers)
(require 'bootstrap-use-package-ext)

(defgroup bootstrap nil
  "Bootstrap customisations."
  :group 'starter-kit
  :prefix 'bootstrap-)

(defconst +bootstrap-loading-char+ ?█
  "Progress bar character.")

(defvar *bootstrap-loading-string* ""
  "Progress bar string.")

(defvar *bootstrap-loading-counter* 0
  "Progress bar counter.")

(defconst +bootstrap-loading-dots-chunk-count+ 3
  "Number of 'dots' per chunk.")

(defconst +bootstrap-loading-dots-count+ (window-total-size nil 'width)
  "Number of positions in order to fill a window.")

(defconst +bootstrap-loading-dots-chunk-size+
  (/ +bootstrap-loading-dots-count+
     +bootstrap-loading-dots-chunk-count+)
  "Number of dot chunks.")

(defvar *bootstrap-loading-dots-chunk-threshold* 0)

;; XXX move
(defvar *bootstrap-loading-progress-bar* t)

;; XXX move
(defvar *bootstrap-startup-lists* nil)

(defun bootstrap-init ()
  ;; We'd like UTF-8 if we can.
  (prefer-coding-system 'utf-8)

  ;; Disable the toolbar if it's enabled.
  (when (and (fboundp 'tool-bar-mode)
             (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))

  ;; Disable the scroll bar if it's enabled.
  (when (and (fboundp 'scroll-bar-mode)
             (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))

  ;; Disable tooltips in the echo area.
  (when (and (fboundp 'tooltip-mode)
             (not (eq tooltip-mode -1)))
    (tooltip-mode -1))

  ;; Ensure that menus are enabled unless we're on a terminal.
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode (if (null window-system)
                       -1
                     1)))

  ;; Create the startup buffer.
  (bootstrap-buffer:startup-screen)

  ;; Load packages
  (bootstrap:load-or-install-protected-package 'dash t)
  (bootstrap:load-or-install-protected-package 's t)
  (bootstrap:load-or-install-protected-package 'bind-key t)
  (bootstrap:load-or-install-protected-package 'use-package t)
  (setq use-package-verbose *bootstrap-verbose*)
  (bootstrap:load-or-install-protected-package 'package-build t)
  (setq quelpa-verbose *bootstrap-verbose*
        quelpa-dir (concat +bootstrap-cache-directory+ "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
        quelpa-update-melpa-p nil)
  (bootstrap:load-or-install-protected-package 'quelpa t)
  (setq use-package-inject-hooks t)
  (bootstrap:load-or-install-protected-package 'which-key t)
  (bootstrap-layer:init)
  (load-theme 'spacemacs-dark t)
  (bootstrap-buffer:startup-screen))


(provide 'bootstrap-core)
