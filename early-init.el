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

;;;; Minimum requrements:

(defvar zmacs-min-emacs-version "29.0"
  "Miniumum version of Emacs that this .emacs.d/ supports.")

(if (not (version<= zmacs-min-emacs-version emacs-version))
    (error (concat "Your version of Emacs (%s) is too old. "
                   "ZMACS requires emacs version %s or above.")
           emacs-version
           zmacs-min-emacs-version))

;;;; Speed hacks
;;;;; Garbage collector:

;; Load in our features support package:
(load (concat user-emacs-directory "lisp/zlisp/zlisp-features"))
(require 'zlisp-features)

;; Initial startup GC settings:
;;
;; Danger: these settings will cause Emacs to hang during heavy workloads (due
;; to Mark and Sweep), so ensure these are set sanely after startup.
;;
;; See the addition to `emacs-startup-hook' below.
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.5)

;; Sane GC settings:
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Default GC value is 800,000 bytes -- 800 _kilobytes_.
            ;;
            ;; This seems a bit silly, and can result in GC being triggered
            ;; during something as simple as scrolling a buffer.
            ;;
            ;; Conversely, setting the GC to silly amounts will result in GCs
            ;; bringing Emacs to its knees at the most inopportune moments
            ;; possible -- because of Mark and Sweep.  I wish Emacs used
            ;; generational collection.
            ;;
            ;; Anyhow, here we set the GC cons threshold to 256MiB and the
            ;; percentage to 0.3%.  This *could* result in hangs where the
            ;; collector stops the world for a somewhat long period of time, but
            ;; if we ensure that collection always takes place when Emacs is not
            ;; being used (idle timer, focus switch), then this could well be
            ;; demoted to "non-issue" status.
            (setq gc-cons-threshold  268435456 ;; 800000
                  gc-cons-percentage 0.3)))

;;;;; Native compilation:

;; Do we have native compilation support?
(defvar *zmacs-has-native-compilation* (and (fboundp 'native-comp-available-p)
                                            (native-comp-available-p))
  "Is native compilation available in this Emacs?")

(defvar *zmacs-has-fast-json*
  (and (subrp (symbol-function 'json-serialize))
       (equal (json-serialize (json-parse-string "[123]")) "[123]"))
  "Does Emacs implement `json-serialize' in C?")

;; Place `:fast-json' in features if it's available.
(if *zmacs-has-fast-json*
    (zlisp/add-feature :fast-json)
  (warn "This Emacs is using the older elisp JSON functions."))

;; Place `:native-compilation' in feature if it's available.
(if *zmacs-has-native-compilation*
    (zlisp/add-feature :native-compilation)
  (message "Native compilation is *not* available."))

;; BUG: On the macOS release of Emacs 29.4, the symbol
;; `comp-enable-subr-trampolines' is not bound.
(when (and (boundp 'native-comp-enable-subr-trampolines)
           (boundp 'comp-enable-subr-trampolines))
  (setq comp-enable-subr-trampolines native-comp-enable-subr-trampolines)
  (zlisp/add-feature :native-comp-subr-trampolines))

;; Improve LSP performance.
(setq read-process-output-max (* 1024 1024))

;; Silence `nativecomp' warnings.
(setopt native-comp-async-report-warnings-errors nil)

;; Native compiler settings.
(setopt native-comp-speed 2)
(setopt native-comp-deferred-compilation t)

;;;;; Compiler warnings:

;; Ensure warnings are really suppressed.
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

;; When-let errors
;; https://github.com/alphapapa/frame-purpose.el/issues/3
(eval-and-compile
  (when (version< emacs-version "26")
    (zlisp/add-feature :no-native-when-let*)
    (zlisp/add-feature :no-native-if-let*)
    (with-no-warnings
      (defalias 'when-let* #'when-let)
      (function-put #'when-let* 'lisp-indent-function 1)
      (defalias 'if-let* #'if-let)
      (function-put #'if-let* 'lisp-indent-function 2))))

;; Variable binding depth.
;; This variable controls the number of lisp bindings that can exists at a time.
;; We should make it fairly large for modern machines.
;; NOTE: Obsolete on Emacs 29+
;; https://www.reddit.com/r/emacs/comments/9jp9zt/anyone_know_what_variable_binding_depth_exceeds/
(when (version< emacs-version "29")
  (setq max-specpdl-size 13000))

;;;; Variables:

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

;;;; Settings:

(setopt frame-inhibit-implied-resize t
        frame-title-format           "ZMACS"
        inhibit-startup-screen       t
        inhibit-startup-message      t
        inhibit-splash-screen        t
        initial-scratch-message      nil)

(when (not (eq window-system nil))
  (setopt tool-bar-mode nil
	  scroll-bar-mode nil))

;; This is probably better than `inhibit-startup-echo-area-message'.
(defun display-startup-echo-area-message ()
  (message ""))

(setopt auto-save-list-file-prefix
        (concat *zmacs-cache-directory*
                "auto-save-list/.saves-"))

;;;; Load path:

(setopt load-prefer-newer t)

;; Set up base layers.
(push *zmacs-lisp-directory* load-path)

;; XXX this is terribad.
;; Add layer subdirectories.
(let ((subdirs (cl-loop for ent in (directory-files *zmacs-lisp-directory* t)
                        for file = (file-name-nondirectory ent)
                        when (and (not (or (equal file ".")
                                            (equal file "..")
                                            (equal file ".git")))
                                   (file-directory-p ent))
                        collect ent)))
  (cl-loop for dir in subdirs
           do (progn
                (add-to-list 'load-path dir)
                (cl-loop for ent in (directory-files dir t)
                         for file = (file-name-nondirectory ent)
                         when (and (not (or (equal file ".")
                                            (equal file "..")
                                            (equal file ".git")))
                                   (file-directory-p ent))
                         do (add-to-list 'load-path ent)))))

;; Load these early.
(require 'zlisp-timing)
(require 'zlisp-platform)

;;;; Configure package system:

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

;;; early-init.el ends here.
