
;; Set GC threshold.
(setq gc-cons-threshold 100000000)

;; Set Bootstrap version
(defconst bootstrap-version "0.1")

(defconst bootstrap-emacs-min-version "24.3"
  "Minimum required version of Emacs.")

(defun bootstrap/emacs-version-ok ()
  (version<= bootstrap-emacs-min-version emacs-version))

(when (bootstrap/emacs-version-ok)
  (load-file (concat user-emacs-directory "core/core-load-paths.el"))
  (require 'core-bootstrap)
  (require 'core-configuration-layer)
  (bootstrap/init)
  (bootstrap/maybe-install-dotfile)
  ;(configuration-layer/sync)
  (bootstrap/setup-startup-hook)
  (require 'server)
  (unless (server-running-p)
    (server-start)))
