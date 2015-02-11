;;; -*- Mode: Emacs-Lisp; Mode: auto-fill -*-
;;;
;;; init.el --- Emacs initialisation file.
;;;
;;; Time-stamp: <Wednesday Feb  4, 2015 13:34:07 asmodai>
;;; Revision:   129
;;;
;;; Copyright (c) 2005-2015 Paul Ward <asmodai@gmail.com>
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
;;;   GNU Emacs 22.1.1       Mac OS X 10.5.8
;;;   GNU Emacs 23.1.1       GNU/Linux
;;;   GNU Emacs 23.2.3       Windows 7
;;;   GNU Emacs 23.3.1       Windows 7
;;;   GNU Emacs 23.3.1       GNU/Linux
;;;   GNU Emacs 23.4.1       Mac OS X 10.5.8
;;;   GNU Emacs 24.1.1       Mac OS X 10.5.8
;;;   GNU Emacs 24.2.1       GNU/Linux
;;;   GNU Emacs 24.3.1       GNU/Linux
;;;
;;;}}}

;;;==================================================================
;;;{{{ Initial settings (removes startup messages et al):

;;; This will be the initial scratch buffer message.
(defvar *my-scratch-message*
  ";;; Visit a file or type here.
")

;;; Some preliminary variables.
(setq-default debug-on-error t)         ; Debug on errors please.
(setq initial-buffer-choice nil         ; No thanks.
      inhibit-splash-screen t           ; I don't care about it.
      inhibit-startup-screen t          ; New version of above.
      inhibit-startup-message t         ; Another alias for above.
      initial-scratch-message *my-scratch-message*)

;;; Copy this at your peril.
(setq inhibit-startup-echo-area-message "asmodai")

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Required elisp:

;;; this is going to be quite hairy.  The Emacs we are running with
;;; won't necessarily know what is correct re `load-path' in order to
;;; load in various required elisp files.  We are also equally as
;;; clueless about the Emacs we are currently running in.  Therefore,
;;; we need to define some variables and functions here in init.el in
;;; order to have enough functionality to deal with `load-path' and
;;; our custom elisp hacks.

;;;------------------------------------------------------------------
;;;{{{ GNU Emacs 18 support:

;;; These are functions I deem necessary to have available in order to
;;; make Emacs 18 usable from my perspective.  I am a Zetalisp and
;;; Common Lisp hacker.  I expect various Zmacs-like entities.  Emacs
;;; 18 does not provide them.
;;;
;;; A note for myself.  This is Emacs, not Genera.  Do not attempt to
;;; implement lots of extraneous Lisp here.  Just implement the bare
;;; essentials.

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

;;; Define `when' as a macro if it is not fbound.
(if (not (fboundp 'when))
    (defmacro when (pred &rest body)
      (list 'cond (cons pred body))))

;;; Define `push' as a macro if it is not fbound.
(if (not (fboundp 'push))
    (defmacro push (value lst)
      (list 'setq (list 'cons value lst))))

;;; Define `cdadr' as a macro if it is not fbound.
(if (not (fboundp 'caddr))
    (defsubst cdadr (thing)
      (cdr (car (cdr thing)))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Operating system predicates:

;;; These need to be loaded early, thus do not get bytecode-compiled.

(defsubst windows-32-p ()
  "T if we are running on Windows 386 through Windows 9x/Me."
  (memq system-type '(ms-windows win386)))

(defsubst windows-nt-p ()
  "T if we are running on Windows NT or higher."
  (eq system-type 'windows-nt))

(defsubst windows-p ()
  "T if we are running on some sort of Windows."
  (or (windows-32-p)
      (windows-nt-p)))

(defsubst ms-dos-p ()
  "T if we are running on some version of DOS."
  (memq system-type '(dos ms-dos)))

(defsubst linux-p ()
  "T if we are running on GNU/Linux."
  (or (eq system-type 'linux)
      (eq system-type 'gnu/linux)))

(defsubst next-mach-p ()
    "T if we are running on a NeXT Mach system."
  (eq system-type 'next-mach))

(defsubst mac-os-x-p ()
  "T if we are running on Mac OS X or Darwin."
  (eq system-type 'darwin))

(defsubst vms-p ()
  "T if we are running on VMS."
  (or (eq system-type 'vms)
      (eq system-type 'vax-vms)))

(defsubst unix-p ()
  "T if we are running on a UNIX system of some sort."
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
      (linux-p)
      (next-mach-p)
      (mac-os-x-p)))

(defsubst cygwin-p ()
  "T if we are running atop the Cygwin emulation layer."
  (eq system-type 'cygwin))

(defsubst emx-p ()
  "T if we are running atop the EMX emulation layer on IBM OS/2."
  (eq system-type 'emx))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Home path resolution:

(defun home-path-for-system ()
  "Returns the path representing a users' home directory."
  (cond
    ;; On VMS, SYS$LOGIN should contain what we need.
    ((vms-p)
     (getenv "SYS$LOGIN"))
    ;; For Unix, just assume HOME is set.
    ((unix-p)
     (getenv "HOME"))
    ;; The environment variable that contains the user's home directory
    ;; on NT depends on which version is installed.  NT 3 and 4 use
    ;; HOMEDIR whereas 2000 and higher use USERPROFILE.
    ((windows-nt-p)
     (let ((dir (getenv "HOMEDIR")))
       (when (null dir)
         (setq dir (getenv "USERPROFILE")))
       dir))
    ;; Assume that HOME is available.  If it is not, then we just throw
    ;; all caution to the wind and guess at C:\.
    ((windows-32-p)
     (let ((dir (getenv "HOME")))
       (when (null dir)
         (setq dir "C:\\"))
       dir))
    ;; If EMX is installed then there's usually a HOME defined in
    ;; CONFIG.SYS.  If there is no HOME defined, then just guess at
    ;; C:\.
    ((emx-p)
     (let ((dir (getenv "HOME")))
       (when (null dir)
         (setq dir "C:\\"))
       dir))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Base load path:

(defvar base-load-paths
  '(".emacs.d/init/"
    ".emacs.d/base/"
    ".emacs.d/custom/"
    ".emacs.d/lisp/")
  "Default base system load paths.")

;;; Parse `base-load-paths' here, in a way that works on Emacs 18.
(mapcar (function
         (lambda (path)
          (let ((home (home-path-for-system)))
            (setq load-path (append
                             (list
                              (file-name-as-directory
                               (expand-file-name path home)))
                             load-path)))))
        base-load-paths)

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Bytecode compiler:

;;; The bytecode cache directory is version-specific.
(defvar +bc-cache-directory+
  (expand-file-name
   (concat ".emacs.d/cache/"
           (if (featurep 'xemacs)
               "x")
           (number-to-string emacs-major-version)
           "/")
   (home-path-for-system))
  "The path of the directory containing cached bytecode for this Emacs
version.")

;;; If the cache directory does not exist, then create it.
(when (not (file-exists-p +bc-cache-directory+))
  (make-directory +bc-cache-directory+ t))

;;; Load in our bytecode compiler hacks.
(load "bytecode")

;;; Now eat our own dog food.
(compile-load "bytecode")

;;; Now compile and load most of the base system.
(compile-load "preds")
(compile-load "funs")
(compile-load "macros")
(compile-load "fixes")

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Configure `load-path':

(defvar default-load-paths
  '(;; Mode hacks
    ((emacs>=19-p)  . ".emacs.d/modes/")  ; Emacs 18 doesnt have any.
    ((xemacs>=19-p) . ".emacs.d/modes/")
    ((xemacs>=19-p) . ".emacs.d/xcompat/"); XEmacs compat code.

    ;; Third-party packages
    ((emacs>=20-p)  . ".emacs.d/third-party/")
    ((emacs>=21-p)  . ".emacs.d/third-party/ruby/")
    ((emacs>=23-p)  . ".emacs.d/third-party/erlang/")
    ((emacs>=21-p)  . ".emacs.d/third-party/slime/")
    ((emacs>=21-p)  . ".emacs.d/third-party/slime/contrib/")
    ((emacs>=23-p)  . ".emacs.d/third-party/company/")

    ;; CEDET (now part of GNU Emacs 24)
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/")
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/cogre/")
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/common/")
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/ede/")
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/eieio/")
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/semantic/")
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/speedbar/")
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/srecode/")
    ((emacs=23-p)   . ".emacs.d/third-party/cedet-1.1/contrib/")

    ;; ECB
    ((emacs>=23-p)  . ".emacs.d/third-party/ecb/")

    ;; Packages that have been tested with XEmacs
    ((xemacs>=19-p) . ".emacs.d/third-party/")
    ;; Not tested ruby-mode with XEmacs.
    ((xemacs>=21-p) . ".emacs.d/third-party/slime/")
    ((xemacs>=21-p) . ".emacs.d/third-party/slime/contrib/"))
  "Default locations for init.el to find other elisp code.")

;;; Parse `default-load-paths' here.
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
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Configure `custom-load-paths':

(defvar custom-load-paths
  '(;; Unix
    (:system (unix-p) ("/usr/local/share/emacs/site-lisp"))
    (:system (unix-p) ("/usr/share/emacs/site-lisp"))

    ;; NeXTSTEP
    (:system (next-mach-p) ("/LocalLibrary/emacs/site-lisp/"))
    (:system (next-mach-p) ("/usr/local/gcl-2.2/elisp/")))
  "Custom load paths.")

;;; Parse `custom-load-paths' here.  This gets to work with Emacs 18!
(let ((do-merge (function
                 (lambda (data)
                  (cond
                    ;; Pathname requires merging with the uers home
                    ;; directory.
                    ((eq (car data) ':merge-home)
                     (expand-file-name (car (cdr data))
                                       (home-path-for-system)))
                    ;; Pathname requires two components be merged
                    ;; together.
                    ((eq (car data) ':merge)
                     (expand-file-name (car (cdr data))
                                       (car (cdr (cdr data)))))
                    ;; Pathname is either a single component or
                    ;; something that we do not handle here yet.
                    (t 
                     (car data)))))))
  (mapcar (function
           (lambda (x)
            (let* ((type (car x))       ; `:system', `:version' et al.
                   (test (car (cdr x))) ; `(unix-p)', `(emacs-p)' et al.
                   (paths
                    (cond
                      ;; Path is dependent on a particular machine.
                      ((eq type ':machine)
                       (if (eval test)
                           (apply do-merge
                                  (list (car (cdr (cdr x)))))))
                      ;; Path is dependent on a particular operating
                      ;; system.
                      ((eq type ':system)
                       (if (eval test)
                           (apply do-merge
                                  (list (car (cdr (cdr x)))))))
                      ;; Path is dependent on a particular version of
                      ;; Emacs.
                      ((eq type ':version)
                       (if (eval test)
                           (apply do-merge
                                  (list (car (cdr (cdr x)))))))
                      ;; Path has no dependencies.
                      (t
                       (apply do-merge (list type))))))
              ;; Add the resulting path to `load-path'.
              (if (not (null paths))
                  (setq load-path
                        (append
                         (list
                          (file-name-as-directory paths))
                         load-path))))))
          custom-load-paths))

;;; The `cl' package is available from Emacs/XEmacs 19 onwards.
(when (or (emacs>=19-p)
          (xemacs>=19-p))
  (require 'cl))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Configure `custom-file':

(defvar +custom-directory+
  (expand-file-name ".emacs.d/custom/" (home-path-for-system))
  "Path containg Emacs customisation files.")

;;; Set the custom file.
(setq custom-file
  (expand-file-name
   (concat +custom-directory+
           "custom-"
           (if (xemacs-p)
               "x")
           (number-to-string emacs-major-version)
           ".el")
   (home-path-for-system)))

;;; If the custom directory does not exist, then create it.
(when (not (file-exists-p +custom-directory+))
  (make-directory +custom-directory+ t))

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Startup:

;;; We're done with setting up Emacs Lisp, now's the time to actually
;;; load stuff.
(load "boot")

;;; Load in our custom file
(load custom-file)

;;; Load our custom key bindings.
(compile-load "bindings")

;;; Load in our custom variables.
(compile-load "settings")

;;; Load our theme last, so we can clobber anything that might be in
;;; the custom file.
(when (emacs-version>= 19 0)
  (compile-load "theme"))

;;; One or two packages require this to be non-void.
(setf stack-trace-on-error nil)

;;; Tell the user that we're done
(message "Emacs is ready, happy hacking!")

;;;}}}
;;;==================================================================

;;; init.el ends here.
