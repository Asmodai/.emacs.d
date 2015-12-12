
(defconst +bootstrap-start-time+ (current-time)
  "Time that Bootstrap was started.")

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
(require 'bootstrap-theme-support)
(require 'bootstrap-font-support)
(require 'memory-usage)
(require 'user-config)

;; Load this very early.
(when (windows-p)
  (package-install 'use-package)
  (require 'use-package))

(defgroup bootstrap nil
  "Bootstrap customisations."
  :group 'starter-kit
  :prefix 'bootstrap-)



(defvar *bootstrap-use-ido* nil)
(defvar *bootstrap-helm-resize* nil)
(defvar *bootstrap-helm-no-header* nil)
(defvar *bootstrap-helm-position* 'bottom)
(defvar *bootstrap-auto-save-file-location* 'cache)
(defvar *bootstrap-which-key-delay* 0.4)
(defvar *bootstrap-which-key-position* 'bottom)
(defvar *bootstrap-loading-progress-bar* t)
(defvar *bootstrap-mode-line-unicode-symbols* t)
(defvar *bootstrap-persistent-server* nil)
(defvar *bootstrap-smartparens-strict-mode* nil)
(defvar *bootstrap-highlight-delimiters* 'all)
(defvar *bootstrap-delete-orphan-packages* t)
(defvar *bootstrap-search-tools* '("ack" "grep"))
(defvar *bootstrap-default-package-repository* 'melpa-stable)
(defvar *bootstrap-startup-lists '(recents projects bookmarks))
(defvar *bootstrap-smooth-scrolling* t)













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

(defvar *bootstrap-default-mode-line* mode-line-format)

(defun bootstrap-init ()
  ;; We'd like UTF-8 if we can.
  (prefer-coding-system 'utf-8)

  ;; Use only spaces and no tabs
  (setq-default indent-tabs-mode nil
                default-tab-width 2)

  ;; Create the startup buffer.
  (switch-to-buffer (get-buffer-create +bootstrap-buffer-name+))
  (bootstrap-buffer:set-mode-line "")

  ;; Silence `ad-handle-definition' about advised functions being redefined.
  (setq ad-redefinition-action 'accept)

  ;; Load the default theme
  (let ((default-theme (car *bootstrap-themes*)))
    (bootstrap:load-theme default-theme)
    (setq *bootstrap-layer-protected-packages*
          (append
           (delq nil (mapcar 'bootstrap::get-theme-package *bootstrap-themes*))
           *bootstrap-layer-protected-packages*))
    (setq-default *bootstrap-current-theme* default-theme))

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

  ;; Set the default font.
  (when (not (terminal-p))
    (if (find-font (font-spec :name (car *bootstrap-default-font*)))
        (bootstrap:set-default-font *bootstrap-default-font*)
      (bootstrap-buffer:warning "Cannot find font \"%s\"!"
                                (car *bootstrap-default-font*))))

  ;; Display the startup banner
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

  ;; Done.
  (bootstrap-mode))

(defun bootstrap:setup-startup-hook ()
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (when (fboundp 'bootstrap-layer:init)
       (bootstrap-layer:init))
     (let ((elapsed (float-time (time-subtract (current-time)
                                               +bootstrap-start-time+))))
       (bootstrap-buffer:append
        (format "\n[%s packages loaded in %.3fs]\n"
                (bootstrap-layer::configured-packages-count)
                elapsed)))
     (when *bootstrap-startup-lists*
       (bootstrap-buffer:insert-startupify-lists))
     (if *bootstrap-layer-error-count*
         (bootstrap-buffer:set-mode-line
          (format (concat "%s error(s) at startup! "
                          "Emacs might not be operating properly.")
                  *bootstrap-layer-error-count*))
       (bootstrap-buffer:set-mode-line *bootstrap-default-mode-line*))
     (when (bootstrap-layer:package-used-p 'powerline)
       (bootstrap::restore-powerline (current-buffer)))
     (force-mode-line-update))))

(bootstrap:setup-startup-hook)

(provide 'bootstrap-core)
