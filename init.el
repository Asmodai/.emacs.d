;;; -*- Mode: Emacs-Lisp; Mode: auto-fill -*-
;;;
;;; init.el --- Emacs initialisation file.
;;;
;;; Time-stamp: <Friday Mar 23, 2012 11:44:27 asmodai>
;;; Revision:   91
;;;
;;; Copyright (c) 2005-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Sun Jun 05 21:44:38 2005
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
;;; NOTICE:  This file is never bytecompiled.
;;;
;;; Emacsen this has been tested on:
;;;   GNU Emacs 23.2.3       Windows 7
;;;   GNU Emacs 23.3.1       Windows 7
;;;   GNU Emacs 23.3.1       GNU/Linux
;;;
;;;}}}

;;; ==================================================================
;;;{{{ Initial settings (removes startup messages et al):

;;;
;;; This will be the initial scratch buffer message.
(defvar *my-scratch-message*
  ";;; Oi yous, type yer stinkin crap here, or C-x C-f yersel ter visit a file.
;;; Have fun, ya basterd!

")

;;;
;;; Some preliminary variables.
(setq-default debug-on-error t)         ; Debug on errors please.
(setq initial-buffer-choice nil         ; No thanks.
      inhibit-splash-screen t           ; I don't care about it.
      inhibit-startup-screen t          ; New version of above.
      inhibit-startup-message t         ; Another alias for above.
      initial-scratch-message *my-scratch-message*)

;;;
;;; Copy this at your peril.
(setq inhibit-startup-echo-area-message "asmodai")

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Required elisp:

;;;
;;; this is going to be quite hairy.  The Emacs we are running with
;;; won't necessarily know what is correct re `load-path' in order to
;;; load in various required elisp files.  We are also equally as
;;; clueless about the Emacs we are currently running in.  Therefore,
;;; we need to define some variables and functions here in init.el in
;;; order to have enough functionality to deal with `load-path' and
;;; our custom elisp hacks.
;;;

;;; ------------------------------------------------------------------
;;;{{{ GNU Emacs 18 support:

;;;
;;; These are functions I deen necessary to have available in order to
;;; make Emacs 18 usable from my perspective.  I am a Zetalisp and
;;; Common Lisp hacker.  I expect various Zmacs-like entities.  Emacs
;;; 18 does not provide them.
;;;
;;; A note for myself.  This is Emacs, not Genera.  Do not attempt to
;;; implement lots of extraneous Lisp here.  Just implement the bare
;;; essentials.
;;;

;;;
;;; Our first job is to set up `emacs-major-version' and
;;; `emacs-minor-version' here so that our elisp will be aware that
;;; we are using Emacs 18.
(if (string-match "^18.*" emacs-version)
    (setq emacs-major-version (string-to-int (substring emacs-version
                                                        0
                                                        2))
          emacs-minor-version (string-to-int (substring emacs-version
                                                        3
                                                        5))))

;;;
;;; Define `when' as a macro if it is not fbound.
(if (not (fboundp 'when))
    (defmacro when (pred &rest body)
      (list 'cond (cons pred body))))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Predicates:

;;; ..................................................................
;;;{{{ Flavour predicates:

(defconst xemacs-p
  (featurep 'xemacs)
  "T if we are running in XEmacs.")

(defconst emacs-p
  (not xemacs-p)
  "T if we are running in GNU Emacs.")

;;;}}}
;;; ..................................................................

;;; ..................................................................
;;;{{{ Version predicates:

;;;{{{ GNU Emacs:

(defconst emacs=18-p
  (and (not xemacs-p)
       (= emacs-major-version 18))
  "T if we are running in GNU Emacs 18.")

(defconst emacs=19-p
  (and (not xemacs-p)
       (= emacs-major-version 19))
  "T if we are running in GNU Emacs 19.")

(defconst emacs=20-p
  (and (not xemacs-p)
       (= emacs-major-version 20))
  "T if we are running in GNU Emacs 20.")

(defconst emacs=21-p
  (and (not xemacs-p)
       (= emacs-major-version 21))
  "T if we are running in GNU Emacs 21.")

(defconst emacs=22-p
  (and (not xemacs-p)
       (= emacs-major-version 22))
  "T if we are running in GNU Emacs 22.")

(defconst emacs=23-p
  (and (not xemacs-p)
       (= emacs-major-version 23))
  "T if we are running in GNU Emacs 23.")

;;;
;;; The follwing are for major groupings of versions.  e.g. code that
;;; works in Emacs 21, 22, 23... etc.
;;;

(defconst emacs>=19-p
  (and (not xemacs-p)
       (>= emacs-major-version 19))
  "T if we are running in GNU Emacs 19 or higher.")

(defconst emacs>=20-p
  (and (not xemacs-p)
       (>= emacs-major-version 20))
  "T if we are running in GNU Emacs 20 or higher.")

(defconst emacs>=21-p
  (and (not xemacs-p)
       (>= emacs-major-version 21))
  "T if we are running in GNU Emacs 21 or higher.")

(defconst emacs>=22-p
  (and (not xemacs-p)
       (>= emacs-major-version 22))
  "T if we are running in GNU Emacs 22 or higher.")

(defconst emacs>=23-p
  (and (not xemacs-p)
       (>= emacs-major-version 23))
  "T if we are running in GNU Emacs 23 or higher.")

;;;}}}

;;;{{{ XEmacs:

(defconst xemacs=19-p
  (and xemacs-p
       (= emacs-major-version 19))
  "T if we are running in XEmacs 19.")

(defconst xemacs=20-p
  (and xemacs-p
       (= emacs-major-version 20))
  "T if we are running in XEmacs 20.")

(defconst xemacs=21-p
  (and xemacs-p
       (= emacs-major-version 21))
  "T if we are running in XEmacs 21.")

(defconst xemacs>=19-p
  (and xemacs-p
       (>= emacs-major-version 19))
  "T if we are running in XEmacs 19 or higher.")

(defconst xemacs>=20-p
  (and xemacs-p
       (>= emacs-major-version 20))
  "T if we are running in XEmacs 20 or higher.")

(defconst xemacs>=21-p
  (and xemacs-p
       (>= emacs-major-version 21))
  "T if we are running in XEmacs 21 or higher.")

;;;}}}

;;;}}}
;;; ..................................................................

;;; ..................................................................
;;;{{{ Operating System predicates:

(defconst windows-32-p
  (memq system-type '(ms-windows win386))
  "T if we are running on Windows 386 through Windows 9x/Me.")

(defconst windows-nt-p
  (eq system-type 'windows-nt)
  "T if we are running on Windows NT or higher.")

(defconst windows-p
  (or windows-32-p windows-nt-p)
  "T if we are running on some sort of Windows.")

(defconst ms-dos-p
  (memq system-type '(dos ms-dos))
  "T if we are running on some version of DOS.")

(defconst linux-p
  (or (eq system-type 'linux)
      (eq system-type 'gnu/linux))
  "T if we are running on GNU/Linux.")

(defconst next-mach-p
  (eq system-type 'next-mach)
  "T if we are running on a NeXT Mach system.")

(defconst mac-os-x-p
  (eq system-type 'darwin)
  "T if we are running on Mac OS X or Darwin.")

(defconst vms-p
  (or (eq system-type 'vms)
      (eq system-type 'vax-vms))
  "T if we are running on VMS.")

(defconst unix-p
  (or (memq system-type '(aix-v3        ; IBM AIX
                          berkeley-unix ; BSD
                          usg-unix-v    ; Unix System V
                          dgux          ; Data General Unix System V
                          gnu           ; GNU Hurd
                          hpux          ; HP-UX
                          irix          ; SGI Irix
                          netbsd        ; NetBSD
                          rtu           ; Some sort of SysV
                          sco           ; SCO SysV 3.2
                          SCO\ sysv5uw7 ; Gee, thanks SCO.
                          unisoft-unix  ; Ancient Unix for the Sun 1
                          xenix))       ; MS/SCO Xenix
      linux-p
      next-mach-p
      mac-os-x-p)
  "T if we are running on a UNIX system of some sort.")

(defconst cygwin-p
  (eq system-type 'cygwin)
  "T if we are running atop the Cygwin emulation layer.")

(defconst emx-p
  (eq system-type 'emx)
  "T if we are running atop the EMX emulation layer.")

;;;}}}
;;; ..................................................................

;;; ..................................................................
;;;{{{ Windowing system predicates:

(defconst x-windows-p
  (eq window-system 'x)
  "T if we are running inside the X Window System.")

(defconst presentation-manager-p
  (eq window-system 'pm)
  "T if we are running inside OS/2's Presentation Manager.")

(defconst nextstep-p
  (eq window-system 'ns)
  "T if we are running inside NeXTSTEP, GNUstep, or Cocoa.")

(defconst terminal-p
  (eq window-system nil)
  "T if we are running in a text-based terminal of some sort.")

;;;}}}
;;; ..................................................................

;;; ..................................................................
;;;{{{ Toolkit predicates:

(defconst motif-p
  (featurep 'motif)
  "T if this Emacs was built using the Motif UI toolkit.")

(defconst xt-p
  (featurep 'x-toolkit)
  "T if this Emacs was built using the X Toolkit.")

;;;}}}
;;; ..................................................................

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ User home directory resolution:

(defun home-path-for-system ()
  (cond
    ;;
    ;; On VMS, SYS$LOGIN should contain what we need.
    (vms-p
     (getenv "SYS$LOGIN"))
    ;;
    ;; For Unix, just assume HOME is set.
    (unix-p
     (getenv "HOME"))
    ;;
    ;; The environment variable that contains the user's home directory
    ;; on NT depends on which version is installed.  NT 3 and 4 use
    ;; HOMEDIR whereas 2000 and higher use USERPROFILE.
    (windows-nt-p
     (let ((dir (getenv "HOMEDIR")))
       (when (null dir)
         (setq dir (getenv "USERPROFILE")))
       dir))
    ;;
    ;; Assume that HOME is available.  If it is not, then we just throw
    ;; all caution to the wind and guess at C:\.
    (windows-32-p
     (let ((dir (getenv "HOME")))
       (when (null dir)
         (setq dir "C:\\"))
       dir))
    ;;
    ;; If EMX is installed then there's usually a HOME defined in
    ;; CONFIG.SYS.  If there is no HOME defined, then just guess at
    ;; C:\.
    (emx-p
     (let ((dir (getenv "HOME")))
       (when (null dir)
         (setq dir "C:\\"))
       dir))))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Configure `load-path':

(defvar default-load-paths
  '((t            . ".emacs.d/init/")   ; Location of main init files.
    (t            . ".emacs.d/base/")   ; Base init files.
    (t            . ".emacs.d/custom/") ; Custom files.
    (t            . ".emacs.d/lisp/")   ; General elisp packages.
    ;;
    ;; Mode hacks
    (emacs>=19-p  . ".emacs.d/modes/")  ; Emacs 18 doesnt have any.
    (xemacs>=19-p . ".emacs.d/modes/")
    (xemacs>=19-p . ".emacs.d/xcompat/"); XEmacs compat code.
    ;;
    ;; Third-party packages
    (emacs>=20-p  . ".emacs.d/third-party/")
    (emacs>=21-p  . ".emacs.d/third-party/ruby/")
    (emacs>=23-p  . ".emacs.d/third-party/erlang/")
    (emacs>=21-p  . ".emacs.d/third-party/slime/")
    (emacs>=21-p  . ".emacs.d/third-party/slime/contrib/")
    (emacs>=22-p  . ".emacs.d/third-party/w3/lisp/")
    (emacs>=22-p  . ".emacs.d/third-party/w3/contrib/")
    (emacs>=23-p  . ".emacs.d/third-party/company/")
    ;;
    ;; Packages that have been tested with XEmacs
    (xemacs>=19-p . ".emacs.d/third-party/")
    ;; Not tested ruby-mode with XEmacs.
    (xemacs>=21-p . ".emacs.d/third-party/slime/")
    (xemacs>=21-p . ".emacs.d/third-party/slime/contrib/"))
  "Default locations for init.el to find other elisp code.")

;;;
;;; Parse `default-load-paths' here.  This is hairy as it gets to work
;;; with Emacs 18!
(mapcar (function
         (lambda (x)
          (let ((test (car x))
                (path (cdr x))
                (home (home-path-for-system)))
            (when (eval test)
              (setq load-path (append
                               (list
                                (file-name-as-directory
                                 (expand-file-name path home)))
                               load-path))))))
        default-load-paths)

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Configure `custom-load-paths':

(defvar custom-load-paths
  '(;;
    ;; Unix
    (:system unix-p ("/usr/local/share/emacs/site-lisp"))
    (:system unix-p ("/usr/share/emacs/site-lisp"))
    ;;
    ;; NeXTSTEP
    (:system next-mach-p ("/LocalLibrary/emacs/site-lisp/"))
    (:system next-mach-p ("/usr/local/gcl-2.2/elisp/"))
    ;;
    ;; Emacs 22 and higher
    (:version emacs>=22-p (:merge-home ".emacs.d/third-party/w3"))
    (:version emacs>=22-p (:merge-home ".emacs.d/third-party/w3/lisp/"))
    )
  "Custom load paths.")

;;;
;;; Parse `custom-load-paths' here.  This gets to work with Emacs 18!
(let ((do-merge (function
                 (lambda (data)
                  (cond
                    ;; Pathname requires merging with the uers home
                    ;; directory.
                    ((eq (car data) ':merge-home)
                     (expand-file-name (car (cdr data))
                                       (home-path-for-system)))
                    ;;
                    ;; Pathname requires two components be merged
                    ;; together.
                    ((eq (car data) ':merge)
                     (expand-file-name (car (cdr data))
                                       (car (cdr (cdr data)))))
                    ;;
                    ;; Pathname is either a single component or
                    ;; something that we do not handle here yet.
                    (t 
                     (car data)))))))
  (mapcar (function
           (lambda (x)
            (let* ((type (car x))       ; `:system', `:version' et al.
                   (test (car (cdr x))) ; `unix-p', `emacs-p' et al.
                   (paths
                    (cond
                      ;;
                      ;; Path is dependent on a particular machine.
                      ((eq type ':machine)
                       (if (eval test)
                           (apply do-merge
                                  (list (car (cdr (cdr x)))))))
                      ;;
                      ;; Path is dependent on a particular operating
                      ;; system.
                      ((eq type ':system)
                       (if (eval test)
                           (apply do-merge
                                  (list (car (cdr (cdr x)))))))
                      ;;
                      ;; Path is dependent on a particular version of
                      ;; Emacs.
                      ((eq type ':version)
                       (if (eval test)
                           (apply do-merge
                                  (list (car (cdr (cdr x)))))))
                      ;;
                      ;; Path has no dependencies.
                      (t
                       (apply do-merge (list type))))))
              ;;
              ;; Add the resulting path to `load-path'.
              (if (not (null paths))
                  (setq load-path
                        (append
                         (list
                          (file-name-as-directory paths))
                         load-path))))))
          custom-load-paths))

;;;
;;; The `cl' package is available from Emacs/XEmacs 19 onwards.
(when (or emacs>=19-p
          xemacs>=19-p)
  (require 'cl))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Configure `custom-file':

(setq custom-file
      (expand-file-name
       (cond
         (emacs=18-p     ".emacs.d/custom/custom-18.el")
         (emacs=19-p     ".emacs.d/custom/custom-19.el")
         (emacs=20-p     ".emacs.d/custom/custom-20.el")
         (emacs=21-p     ".emacs.d/custom/custom-21.el")
         (emacs=22-p     ".emacs.d/custom/custom-22.el")
         (emacs=23-p     ".emacs.d/custom/custom-23.el")
         (xemacs=19-p    ".emacs.d/custom/custom-x19.el")
         (xemacs=20-p    ".emacs.d/custom/custom-x20.el")
         (xemacs=21-p    ".emacs.d/custom/custom-x21.el"))
       (home-path-for-system)))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Missing functionality in GNU Emacs:

;;;
;;; If we are on GNU Emacs 20.x then we need to deal with the fact
;;; that `font-lock-unfontify-buffer' might be missing.  This symbol
;;; seems present in GNU Emacs 20.6 however, so assume that any
;;; version lesser than 20.6 will require the fix.
(when (and emacs-p
           (= emacs-major-version 20)
           (< emacs-minor-version 6)
           (not (fboundp 'font-lock-unfontify-buffer)))
  (defalias 'font-lock-unfontify-buffer 'ignore)
  (require 'font-lock))

;;;}}}
;;; ------------------------------------------------------------------

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Bytecode compiler hacks:

;;;
;;; The bytecode cache directory is version-specific.
(defvar +bc-cache-directory+
  (expand-file-name
   (concat ".emacs.d/cache/"
           (if xemacs-p
               "x")
           (number-to-string emacs-major-version)
           "/")
   (home-path-for-system))
  "The path of the directory containing cached bytecode for this Emacs
version.")

;;;
;;; If the cache directory does not exist, then create it.
(when (not (file-exists-p +bc-cache-directory+))
  (make-directory +bc-cache-directory+))

;;;
;;; Load in our bytecode compiler hacks.
(load "bytecode")

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Startup:

;;;
;;; We're done with setting up Emacs Lisp, now's the time to actually
;;; load stuff.
;;;

;;; Load in our custom file
(load custom-file)

;;; We have several utility functions in the file `utils.el', so load
;;; those in.
(compile-load "utils")

;;;
;;; Load in the master init file.
(when (and (boundp 'emacs-p)
           (boundp 'xemacs-p))
  (load "emacs-init"))

;;;
;;; Load our custom key bindings.
(compile-load "keys")

;;;
;;; Load our theme last, so we can clobber anything that might be in
;;; the custom file.
(when (emacs-version>= 19 0)
  (load "theme"))

;;;
;;; Tell the user that we're done
(message "Emacs is ready, happy hacking!")

;;;}}}
;;; ==================================================================

;;; init.el ends here.
