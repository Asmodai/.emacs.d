;;; init.el --- Main initialisation  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    24 Oct 2024 04:23:07
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
;;;; Requirements:

(require 'cl-lib)
(require 'zlisp-platform)
(require 'zlisp-features)

;; Fix for macOS
(when (null user-home-directory)
  (setq user-home-directory (zlisp/get-home-directory)))

;;;; Early-loaded packages:
;;;;; Customise:

(use-package cus-edit
  :ensure nil
  :defer 1
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (when (not (file-exists-p custom-file))
    (write-file custom-file))
  (when (file-exists-p custom-file)
    (load custom-file)))

;;;;; Use Package:

(use-package use-package
  :custom
  (use-package-always-defer          nil)
  (use-package-verbose               t)
  (use-package-minimum-reported-time 0)
  (use-package-expand-minimally      nil)
  (use-package-always-ensure         *zmacs-should-ensure-packages*)
  (use-package-enable-imenu-support  t)
  (use-package-compute-statistics    t))

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;;;;; El Patch:

(use-package el-patch
  :ensure t
  :defer nil
  :hook (emacs-startup . el-patch-use-package-mode)
  :custom
  (el-patch-enable-use-package-integration t))

;;;;; GNU TLS:

(use-package gnutls
  :ensure nil
  :defer 1
  :custom
  (gnutls-verify-error t)
  (gnutls-min-prime-bits 3072))

;;;;; Auto-Compile:

(use-package auto-compile
  :ensure t
  :defer 1
  :custom
  (progn
    (auto-compile-display-buffer nil)
    (auto-compile-mode-line-counter nil)
    (auto-compile-use-mode-line nil)
    (auto-compile-update-autoloads t))
  :config
  (progn
    (require 'auto-compile)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;;;; Customisation:

(defcustom zmacs-use-persistent-scratch t
  "Should ZMACS use a persistent scratch buffer?"
  :type 'boolean
  :group 'zmacs-emacs
  :tag "Use persistent scratch buffer?")

(defcustom zmacs-preferred-shell "zsh"
  "Which shell should Emacs prefer?"
  :type 'string
  :group 'zmacs-emacs
  :tag "Preferred Unix shell")

(defcustom zmacs-storage-directory
  (expand-file-name (concat user-home-directory "/Dropbox/Emacs/"))
  "Which directory contains our Emacs user file storage."
  :type 'string
  :group 'zmacs-emacs
  :tag "ZMACS storage directory")

(defcustom zmacs-org-directory
  (expand-file-name (concat user-home-directory "/Dropbox/Emacs/org/"))
  "Which directory contains our Org files?"
  :type 'string
  :group 'zmacs-emacs
  :tag "ZMACS org directory")

(defcustom zmacs-projects-directory
  (expand-file-name (concat user-home-directory "/Projects/"))
  "Which directory contains our projects?"
  :type 'string
  :group 'zmacs-emacs
  :tag "ZMACS projects directory")

;; Project repo directory.
(setq magic-repository-directories
      (list (cons zmacs-projects-directory 1)))

;;;; Deal with `user.el'

(defvar zmacs-user-config
  (expand-file-name (concat user-emacs-directory "user.el"))
  "File containing user-specific settings.")

;; Ensure `user.el' exists.
(or (file-exists-p zmacs-user-config)
    (progn
      (with-temp-file zmacs-user-config
        (insert ";; user.el --- User-specific settings   -*- mode: emacs-lisp -*-

(setopt user-full-name         \"Your Name\"           ; Your full name.
        user-mail-address      \"you@example.com\"     ; Your e-mail address.
        user-url-address       \"http://example.com/\" ; Your website URL.
        user-work-mail-address \"you@example.com\"     ; Your work e-email.
        initial-major-mode     'fundamental-mode

        ;; Calendar settings.
        user-latitude  0.0                             ; Your latitude.
        user-longitude 0.0                             ; Your longitude.
        user-city      \"some city\"                   ; Your city.
        user-gcal-secret \"https://...\")              ; Your gcal secret ics.

;; user.el ends here.
"))))

;; Now load it.
(load-file zmacs-user-config)

;;;; Start the ball rolling:

;;;;; Stuff to load in early:
(require 'zlisp-base)
(require 'zlisp-timing)
(require 'zmacs-early)

;;;;; Library modules:
(require 'zlisp-list)
(require 'zlisp-colour)

;;;;; Main modules:
(require 'zmacs-scratch)
(require 'zmacs-base)
(require 'zmacs-server)
(require 'zmacs-frames)
(require 'zmacs-windows)
(require 'zmacs-buffers)
(require 'zmacs-fonts)
(require 'zmacs-faces)
(require 'zmacs-colours)
(require 'zmacs-completion)
(require 'zmacs-help)
(require 'zmacs-theme)
(require 'zmacs-palette)
(require 'zmacs-dashboard)
(require 'zmacs-modeline)
(require 'zmacs-navigation)
(require 'zmacs-dired)
(require 'zmacs-search)
(require 'zmacs-templates)

;;;;; Projects and version control:
(require 'zmacs-vc)
(require 'zmacs-projects)

;;;;; Shells:
(require 'zmacs-shell)
(require 'zmacs-eshell)

;;;;; Org:
(require 'zmacs-org-base)
(require 'zmacs-org-extensions)

;;;;; Calendar:
(require 'zmacs-calendar)

;;;;; Writing:
(require 'zmacs-writing)
(require 'zmacs-citation)
(require 'zmacs-notes)

;;;;; Programming:
(require 'zmacs-programming)
(require 'zmacs-prog-html)
(require 'zmacs-prog-yaml)
(require 'zmacs-prog-json)
(require 'zmacs-prog-debug)
(require 'zmacs-prog-lisp)
(require 'zmacs-prog-common-lisp)
(require 'zmacs-prog-c)
(require 'zmacs-prog-go)
(require 'zmacs-prog-js)
(require 'zmacs-prog-ruby)

;;;;; Functions:
(require 'zlisp-memory)
(require 'zlisp-files) ;; consult, dired
(require 'zlisp-window)
(require 'zlisp-frame)
(require 'zlisp-buffer)
(require 'zlisp-text) ;; org
(require 'zlisp-clipboard)
(require 'zlisp-ui) ;; org, markdown

;;;; Hooks:

;; Set up the `after init' hook.
(add-hook 'after-init-hook
          #'(lambda ()
              (interactive)
              (require 'server)
              (or (server-running-p)
                  (server-start))))

;; Set up after-startup hook
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message
               (format "Emacs ready in %0.2f seconds with %d collections."
                       (float-time
                        (time-subtract after-init-time before-init-time))
                       gcs-done))
              (setf diary-display-function #'diary-fancy-display)))

;;;; Garbage Collection:

(require 'zlisp-gc)

;;; init.el ends here.
