
;; Maximum mesage log size
(setq message-log-max 16384)

;; Record startup time
(defconst bootstrap-start-time (current-time))

;; Require packages.
(require 'subr-x nil 'noerror)
(require 'core-predicates)
;(require 'core-bytecode-compiler)
(require 'core-emacs-backports)
(require 'core-release-management)
(require 'core-auto-completion)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-emacs-buffer)
;;(require 'core-toggle)
(require 'core-use-package-ext)
;;(require 'core-keybindings)

;; Bootstrap group
(defgroup bootstrap nil
  "Bootstrap customisations."
  :group 'starter-kit
  :prefix 'bootstrap-)

(defvar bootstrap-loading-char ?█)
(defvar bootstrap-loading-string "")
(defvar bootstrap-loading-counter 0)
(defvar bootstrap-loading-dots-chunk-count 3)
(defvar bootstrap-loading-dots-count (window-total-size nil 'width))
(defvar bootstrap-loading-dots-chunk-size
  (/ bootstrap-loading-dots-count bootstrap-loading-dots-chunk-count))
(defvar bootstrap-loading-dots-chunk-threshold 0)

(defvar bootstrap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]          'widget-forward)
    (define-key map (kbd "C-i")    'widget-forward)
    (define-key map [backtab]      'widget-backward)
    (define-key map (kbd "RET")    'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Key map for bootstrap mode.")

(defvar bootstrap--default-mode-line mode-line-format
  "Backup of default mode line format.")

(define-derived-mode bootstrap-mode special-mode "Bootstrap"
  "Bootstrap major mode for the startup screen.

\\<bootstrap-mode-map>
"
  :group 'bootstrap
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defun bootstrap/init ()
  "Create the special buffer for `bootstrap-mode' and perform startup
initialisation."
  (prefer-coding-system 'utf-8)

  (bootstrap-loader/load-file)
  (bootstrap-loader|call-func bootstrap-loader/init
                              "Calling loader init...")
  (bootstrap-loader|call-func bootstrap-loader/user-init
                              "Calling loader user init...")

  (switch-to-buffer (get-buffer-create bootstrap-buffer-name))
  (bootstrap-buffer/set-mode-line "")
  (setq ad-redefinition-action 'accept)

  (let ((default-theme (car bootstrap-loader-themes)))
    (bootstrap/load-theme default-theme)
    (setq bootstrap-conf-layer--protected-packages
          (append
           (delq nil (mapcar 'bootstrap//get-theme-package
                             bootstrap-loader-themes))
           bootstrap-conf-layer--protected-packages))
    (setq-default bootstrap--cur-theme default-theme)
    (setq-default bootstrap--cycle-themes (cdr bootstrap-loader-themes)))

  (when (and (fboundp 'tool-bar-mode)
             (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'tool-tip-mode)
             (not (eq tool-tip-mode -1)))
    (tool-tip-mode -1))

  (if (find-font (font-spec :name (car bootstrap-loader-default-font)))
      (bootstrap/set-default-font bootstrap-loader-default-font)
    (bootstrap-buffer/warning "Cannot find font \"%s\"!"
                              (car bootstrap-loader-default-font)))

  (bootstrap-buffer/insert-banner-and-buttons)

  (bootstrap/load-or-install-protected-package 'dash t)
  (bootstrap/load-or-install-protected-package 's t)
  (bootstrap/load-or-install-protected-package 'bind-key t)
  (bootstrap/load-or-install-protected-package 'use-package t)

  (setq use-package-verbose bootstrap-loader-verbose-loading)
  (bootstrap/load-or-install-protected-package 'package-build t)

  (setq quelpa-verbose bootstrap-loader-verbose-loading
        quelpa-dir (concat bootstrap-cache-directory "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
        quelpa-update-melpa-p nil)
  (bootstrap/load-or-install-protected-package 'quelpa t)

  (setq use-package-inject-hooks t)
  (bootstrap/load-or-install-protected-package 'which-key t)

  (if bootstrap-loader-mode-line-unicode-symbols
      (setq-default bootstrap-version-check-lighter "[⇪]"))
  (bootstrap/set-new-version-lighter-mode-line-faces)
  (add-hook 'emacs-startup-hook 'bootstrap-buffer/goto-link-line)
  (bootstrap-mode))

(defun bootstrap/maybe-install-dotfile ()
  "Install the dotfile if it does not exist."
  (unless (file-exists-p bootstrap-loader-filepath)
    (bootstrap-buffer/set-mode-line "Loader installer")
    (bootstrap//redisplay)
    (when (bootstrap-loader/install)
      (bootstrap-loader/sync-configuration-layers '(16)))))

(defun bootstrap/display-and-copy-version ()
  "Echo the current Emacs version and copy it."
  (interactive)
  (let ((msg (format "Emacs v.%s (Bootstrap v.%s)"
                     emacs-version
                     bootstrap-version)))))

(defun display-startup-echo-area-message ()
  "Change the default welcome message of the minibuffer."
  (message "Emacs is ready!"))

(defun bootstrap/setup-startup-hook ()
  "Add post init processing."
  (add-hook
   'emacs-startup-hook
   (lambda ()
     (when (fboundp 'bootstrap-loader/user-config)
       (bootstrap-loader|call-func bootstrap-loader/user-config
                                   "Calling loader user config..."))
     (let ((elapsed (float-time
                     (time-subtract (current-time) bootstrap-start-time))))
       (bootstrap-buffer/append
        (format "\n[%s packages loaded in %.3fs]\n"
                (bootstrap-conf-layer/configured-packages-count)
                elapsed)))
     (when bootstrap-loader-startup-lists
       (bootstrap-buffer/insert-startupify-lists))
     (if bootstrap-conf-layer-error-count
         (bootstrap-buffer/set-mode-line
          (format (concat "%s error(s) at startup! "
                          "Emacs may not be able to operate properly.")
                  bootstrap-conf-layer-error-count))
       (bootstrap-buffer/set-mode-line bootstrap--default-mode-line))
     (force-mode-line-update)
     (bootstrap/check-for-new-version bootstrap-version-check-interval))))

(defun bootstrap/describe-system-info ()
  "Gathers info about your Emacs setup and copies it to the clipboard."
  (interactive)
  (let ((sysinfo (format
                  (concat "#### System Info\n"
                          "- OS: %s\n"
                          "- Emacs: %s\n"
                          "- Bootstrap: %s\n"
                          "- Layers:\n```elisp\n%s```\n")
                  system-type
                  emacs-version
                  bootstrap-version
                  (pp bootstrap-loader-configuration-layers))))
    (kill-new sysinfo)
    (message sysinfo)
    (message (concat "Information has been copied to the clipboard.\n"
                     "Check the *Messages* buffer if you need to review it."))))

(provide 'core-bootstrap)

