
;;; Some preliminary variables.
(setq-default debug-on-error t)         ; Debug on errors please.
(setq initial-buffer-choice nil         ; No thanks.
      inhibit-splash-screen t           ; I don't care about it.
      inhibit-startup-screen t          ; New version of above.
      inhibit-startup-message t)        ; Another alias for above.

;;; Copy this at your peril.
(setq inhibit-startup-echo-area-message "asmodai")

;; Set GC threshold.
(setq gc-cons-threshold 100000000)

;; Set this to T if you want verbose messages.
(defvar *bootstrap-verbose* t)

(defconst +bootstrap-emacs-min-version+ "24.3"
  "Minimum required version of Emacs.")

(defun bootstrap:emacs-version-ok ()
  (version<= +bootstrap-emacs-min-version+ emacs-version))

;; Load it all.
(when (bootstrap:emacs-version-ok)
  ;; Load in paths.
  (load-file (concat user-emacs-directory "core/bootstrap-paths.el"))

  ;; Require core packages.
  (require 'bootstrap-core)

  ;; Start the ball rolling.
  (bootstrap-init)

  (require 'server)
  (unless (server-running-p)
    (server-start)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
