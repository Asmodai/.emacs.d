;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; emacs-init.el --- Version-specific init code
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 01:54:10 asmodai>
;;; Revision:   37
;;;
;;; Copyright (c) 2012  <asmodai@gmail.com>
;;;
;;; Author:      <asmodai@gmail.com>
;;; Maintainer:  <asmodai@gmail.com>
;;; Created:    24 Jan 2012 01:42:13
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
;;;}}}

;;; ==================================================================
;;;{{{ GNU Emacs:

;;; ------------------------------------------------------------------
;;;{{{ GNU Emacs 18:

(when emacs=18-p
  ;;;
  ;;; LEDIT mode for Franz Lisp.
  (load "ledit"))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ GNU Emacs 19:

;;;
;;; First, packages that are *specific* to GNU Emacs 19.
(when emacs=19-p
  (compile-load "19-extensions")
  (compile-load "19-template")
  (compile-load "19-folding")
  (initialize-template-binding)
  
  ;;
  ;; GCL support
  (when (or running-on-dauntless-p
            running-on-nova-p
            running-on-galaxy-p)
    (compile-load "gcl")
    (compile-load "dbl")
    (compile-load "lisp-complete"))
  
  (require 'paren)
  
  ;;
  ;; This is irritating as we use `time-stamp' with most other
  ;; versions of Emacs.  Just, we know it as `timestamp', and we can
  ;; modify the formats.  With 19 we can't, so we treat it separately.
  (require 'time-stamp)
  (set 'time-stamp-active t))

;;;
;;; Now stuff that will work with Emacs >= 19.
(when emacs>=19-p
  (compile-load "revision-hook"))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ GNU Emacs 20:

(when emacs=20-p
  ;;
  ;; GNU Emacs 20 doesn't have `font-lock-comment-delimiter-face', so
  ;; let's define it.
  (defvar font-lock-comment-delimiter-face 'font-lock-comment-face))

(when emacs>=20-p
  (compile-load "cparen")
  (cparen-activate)
   
  (compile-load "template")
  (template-initialize)
   
  (compile-load "licenses")
   
  (compile-load "folding")
  (folding-install)
  (folding-install-hooks)
  
  ;;
  ;; Configure comint
  (when windows-p
    (require 'comint)
    
    ;;
    ;; Tell comint that our shell likes to echo.
    (defun comint-false-echo ()
      (setq comint-process-echoes t))
    
    ;;
    ;; Add it to the comint mode
    (add-hook 'comint-mode-hook 'comint-false-echo)

    ;;
    ;; If the above doesn't work, change the cmd.exe arguments.
    (setq explicit-cmd.exe-args '("/q")
          explicit-cmdproxy.exe-args '("/q")))
   
  ;;
  ;; Emacs >= 20 has the right time-stamp options.
  (require 'time-stamp)
  (set 'time-stamp-active t)
  (set 'time-stamp-format "%:a %3b %2d, %4y %02H:%02M:%02S %u"))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ GNU Emacs 21:

(when emacs>=21-p
  (compile-load "highlight-parentheses")
  (compile-load "newcomment")
  
  ;;
  ;; For some reason, `hfy-etags-cmd-alist' isn't available when
  ;; htmlfontify is compiled during `compile-load', so we have to
  ;; load it in before compiling.
  ;;
  (compile-load "hfy-cmap")
  (load "htmlfontify")                  ; This slows things down
  (compile-load "htmlfontify")          ; having to load it twice.
  
  (when (not terminal-p)
    (require 'mwheel))
  
  (compile-load "mtorus")
  (mtorus-init)
  (mtorus-install-suggested-bindings)
  
  (compile-load "htmlize")
  
  (when terminal-p
    (compile-load "xt-mouse"))
  
  (compile-load "csharp-mode"))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ GNU Emacs 22:

(when emacs>=22-p
  (compile-load "paredit")
  
  (when unix-p
    (require 'tramp))
  
  (require 'image-mode)
  (require 'w3)
  (require 'erc))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ GNU Emacs 23:

(when emacs=23-p
  (compile-load "erlang-start")
  (compile-load "ruby-mode")
  (compile-load "rdoc-mode")
  (compile-load "inf-ruby")
  (compile-load "ruby-electric")
  (compile-load "ruby-style")
  
  (compile-load "company")
  (compile-load "company-abbrev")
  (compile-load "company-css")
  (compile-load "company-elisp")
  (compile-load "company-keywords")
  (compile-load "company-template")
  (compile-load "slime-company")
  (compile-load "inf-ruby-company")
  
  (autoload 'company-mode "company" nil t))

;;;}}}
;;; ------------------------------------------------------------------

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ XEmacs:

;;; ------------------------------------------------------------------
;;;{{{ XEmacs 19:

;;;
;;; I don't have a machine with XEmacs 19 right now.
;;;

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ XEmacs 20:

(when xemacs>=20-p
  (require 'font-lock)
  
  (unless (fboundp 'font-lock-add-keywords)
    (defalias 'font-lock-add-keywords 'ignore))
  
  (unless (fboundp 'define-major-mode)
    (defalias 'define-major-mode 'ignore))
  
  (unless (fboundp 'make-annotation)
    (defalias 'make-annotation 'ignore))
  
  (when (and (or x-windows-p
                 windows-p)
             (featurep 'toolbar))
    (compile-load "x-toolbar"))
  
  (compile-load "template")
  (template-initialize)
  
  (compile-load "revision-hook")
  (compile-load "licenses")
  
  (when x-windows-p
    (require 'x-mouse))
  
  (when terminal-p
    (compile-load "xt-mouse"))
  
  (compile-load "csharp-mode")
   
  (compile-load "timestamp")            ; This is in xcompat/
  (set 'time-stamp-active t)
  (set 'time-stamp-format "%:a %3b %2d, %4y %02H:%02M:%02S %u"))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ XEmacs 21:

(when xemacs>=21-p
  (when (emacs-version>= 21 4 10)
    (compile-load "newcomment")
    (require 'tramp)
    (compile-load "htmlfontify")
    
    (compile-load "mtorus")
    (mtorus-init)
    (mtorus-install-suggested-bindings)
    
    (compile-load "htmlize")
    (require 'image-mode))
  
  (when (emacs-version>= 21 4 20)
    (compile-load "folding")
    (folding-install)
    
    (require 'w3)
    (require 'erc)))

;;;}}}
;;; ------------------------------------------------------------------

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Common hooks and settings:

;;; ------------------------------------------------------------------
;;;{{{ Site major-mode hacks:

;;;
;;; These need to be loaded in last so they can utilize packages that
;;; are loaded in during the version-specific conditionals.
;;;

(when (or emacs>=19-p
          xemacs>=19-p)
  (compile-load "site-lisp-mode")
  (compile-load "site-c-mode")
  (compile-load "site-html-mode")
  (compile-load "site-perl-mode")
  (compile-load "site-ruby-mode")
  (compile-load "site-erlang-mode"))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Tabs are evil:

;;;
;;; Version for emacs = 18
(when emacs=18-p
  (push (function (lambda ()
          (if (not indent-tabs-mode)
              (untabify (point-min) (point-max)))))
        write-file-hooks))

;;;
;;; Version for emacs > 18
(when (or emacs>=19-p
          xemacs>=19-p)
  ;;
  ;; Force `indent-tabs-mode' to NIL.  Any mode that requires tabs will
  ;; set this to T anyhow.
  (setq-default indent-tabs-mode nil)

  ;;
  ;; This write hook will untabify the entire document unless
  ;; `indent-tabs-mode' is non-NIL.
  (add-hook 'write-file-hooks
            (lambda ()
              (when (not indent-tabs-mode)
                (untabify (point-min) (point-max))))))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Emacs 20 specific hooks:

(when emacs=20-p
  (when (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1)
    (setq font-lock-auto-fontify t))
  
  ;;
  ;; This is required for Emacs 20 on OS/2.
  (set-background-color "black"))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Hooks:

(when (or emacs>=19-p
          xemacs>=19-p)
  ;;
  ;; Auto-update the time stamp header.
  (when (featurep 'time-stamp)
    (add-hook 'write-file-hooks 'time-stamp))

  ;;
  ;; Auto-update the file header.
  (add-hook 'write-file-hooks 'auto-update-file-header)
  
  ;;
  ;; Enable folding mode on open if possible.
  (when (featurep 'folding)
    (add-hook 'find-file-hooks (lambda ()
                                 (folding-mode t)))))

;;;}}}
;;; ------------------------------------------------------------------

;;;}}}
;;; ==================================================================

;;; emacs-init.el ends here
