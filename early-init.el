;;; early-init.el --- Early initialisation  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    23 Oct 2024 16:20:19
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

;;;===================================================================
;;;{{{ Speed hacks:

;;; Load in our features support package:
(load (concat user-emacs-directory "lisp/zlisp/zlisp-features"))
(require 'zlisp-features)

;;; Set preposterous GC settings so GC won't invoke mid-way though initila
;;; startup.
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.5)

;;; Set this now so we don't have to worry about it later on.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold       (* 1000 1000 8)
                  gc-cons-percentage      0.1)))

;;; Do we have native compilation support?
(defvar *zmacs-has-native-compilation* (and (fboundp 'native-comp-available-p)
                                            (native-comp-available-p))
  "Is native compilation available in this Emacs?")

(defvar *zmacs-has-fast-json*
  (and (subrp (symbol-function 'json-serialize))
       (equal (json-serialize (json-parse-string "[123]")) "[123]"))
  "Does Emacs implement `json-serialize' in C?")

;;; Place `:fast-json' in features if it's available.
(if *zmacs-has-fast-json*
    (zlisp-add-feature :fast-json)
  (warn "This Emacs is using the older elisp JSON functions."))

;;; Place `:native-compilation' in feature if it's available.
(if *zmacs-has-native-compilation*
    (zlisp-add-feature :native-compilation)
  (message "Native compilation is *not* available."))

;;; Bug fix.
(if (not (boundp 'comp-enable-subr-trampolines))
    (setq comp-enable-subr-trampolines native-comp-enable-subr-trampolines))

;;; Improve LSP performance.
(setq read-process-output-max (* 1024 1024))

;;; Silence `nativecomp' warnings.
(setopt native-comp-async-report-warnings-errors nil)

;;; Native compiler settings.
(setopt native-comp-speed 2)
(setopt native-comp-deferred-compilation t)

;;; Ensure warnings are really suppressed.
(require 'warnings)
(setopt warning-suppress-types '((comp)))
(setopt debug-on-error nil)
(byte-compile-disable-warning 'obsolete)
 (setq byte-compile-warnings '(not-unused
                               free-vars
                               unresolved
                               noruntime
                               lexical
                               make-local
                               ;;obsolete
                               ))

;;; When-let errors
;;; https://github.com/alphapapa/frame-purpose.el/issues/3
(eval-and-compile
  (when (version< emacs-version "26")
    (zlisp-add-feature :no-native-when-let*)
    (zlisp-add-feature :no-native-if-let*)
    (with-no-warnings
      (defalias 'when-let* #'when-let)
      (function-put #'when-let* 'lisp-indent-function 1)
      (defalias 'if-let* #'if-let)
      (function-put #'if-let* 'lisp-indent-function 2))))

;;; Variable binding depth.
;;; This variable controls the number of lisp bindings that can exists at a time.
;;; We should make it fairly large for modern machines.
;;; NOTE: Obsolete on Emacs 29+
;;; https://www.reddit.com/r/emacs/comments/9jp9zt/anyone_know_what_variable_binding_depth_exceeds/
(when (version< emacs-version "29")
  (setq max-specpdl-size 13000))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Variables:

(defcustom zmacs-active-theme nil
  "Current active ZMACS theme."
  :type 'symbol
  :group 'zmacs)

(defvar *zmacs-cache-directory*
  (expand-file-name (concat user-emacs-directory "cache/"))
  "Directory for various cached things.")

(defvar *zmacs-lisp-directory*
  (expand-file-name (concat user-emacs-directory "lisp/"))
  "Directory for the core components of ZMACS.")

(defvar *zmacs-should-ensure-packages* t
  "Should we ensure packages with use-package?")

(or (file-directory-p *zmacs-cache-directory*)
    (make-directory *zmacs-cache-directory*))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Settings:

(setopt frame-inhibit-implied-resize t
        frame-title-format           "ZMACS"
        inhibit-startup-screen       t
        inhibit-startup-message      t
        inhibit-splash-screen        t
        initial-scratch-message      nil)

(setopt tool-bar-mode nil
        scroll-bar-mode nil)

;;; This is probably better than `inhibit-startup-echo-area-message'.
(defun display-startup-echo-area-message ()
  (message ""))

(setopt auto-save-list-file-prefix
        (concat *zmacs-cache-directory*
                "auto-save-list/.saves-"))

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Load path:

(setopt load-prefer-newer t)

;;; Set up base layers.
(push *zmacs-lisp-directory* load-path)

;;; Add layer subdirectories.
(dolist (file (directory-files-recursively *zmacs-lisp-directory* ""  t nil t))
  (if (and (not (or (equal file ".")
                    (equal file "..")))
           (file-directory-p file))
      (add-to-list 'load-path file)))

;;; Load these early.
(require 'zlisp-timing)
(require 'zlisp-platform)

;;;}}}
;;;===================================================================

;;;===================================================================
;;;{{{ Configure package system:

(require 'package)

(setopt package-archives '(("elpa"       . "https://elpa.gnu.org/packages/")
                           ("elpa-devel" . "https://elpa.gnu.org/devel/")
                           ("nongnu"     . "https://elpa.nongnu.org/nongnu/")
                           ("melpa"      . "https://melpa.org/packages/"))

        ;; Highest number gets priority (what is not mentioned gets priority 0)
        package-archive-priorities '(;; Prefer development packages
                                     ("elpa-devel" . 99)
                                     ("melpa" . 90)))

;; Make sure the elpa/ folder exists after setting it above.
(unless (file-exists-p package-user-dir)
  (mkdir package-user-dir t))

(setopt package-quickstart-file (expand-file-name "package-quickstart.el"
                                                  *zmacs-cache-directory*))

(package-initialize)
(when (version< emacs-version "29")
  (package-refresh-contents)
  (package-install 'use-package))

;;;}}}
;;;===================================================================

;;; early-init.el ends here.
