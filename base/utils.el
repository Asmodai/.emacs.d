;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; utils.el --- Various utilities
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 17:49:40 asmodai>
;;; Revision:   14
;;;
;;; Copyright (c) 2005-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    15 Feb 2011 12:26:50
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
;;;
;;;
;;;}}}

;;; ==================================================================
;;;{{{ Machine predicates:

;;;
;;; Physical hosts:
;;;   lisp-machine          Mac OS X
;;;   magellan              GNU/Linux
;;;   farragut              FreeBSD
;;;   lynx                  HP-UX
;;;   yorktown              Windows 7
;;;   nova                  OPENSTEP 4.2
;;;
;;; VM hosts:
;;;   dauntless             NeXTSTEP 3.3
;;;   galaxy                NeXTSTEP 4.0
;;;

(defun %running-on (host)
  "Returns T if the strin given in HOST matches the string contained
in `system-name'."
  (eq (string-match (concat "^" host) (system-name)) 0))

(defconst running-on-lisp-machine-p
  (or (%running-on "lisp-machine")
      (%running-on "lispm"))
  "T if Emacs is running on `lisp-machine'.")

(defconst running-on-magellan-p (%running-on "magellan")
  "T if Emacs is running on `magellan'.")

(defconst running-on-farragut-p (%running-on "farragut")
  "T if Emacs is running on `farragut'.")

(defconst running-on-lynx-p (%running-on "lynx")
  "T if Emacs is running on `lynx'.")

(defconst running-on-yorktown-p (%running-on "yorktown")
  "T if Emacs is running on `yorktown'.")

(defconst running-on-nova-p (%running-on "nova")
  "T if Emacs is running on `nova'.")

(defconst running-on-dauntless-p (%running-on "dauntless")
  "T if Emacs is running on `dauntless'.")

(defconst running-on-galaxy-p (%running-on "galaxy")
  "T if Emacs is running on `galaxy'.")

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ General utilities:

;;;
;;; Port of `emacs-program-name' for emacsen other than XEmacs.
(when (or (not xemacs-p)
          xemacs=19-p
          xemacs=20-p)
  (defconst emacs-program-name
    (if xemacs-p
        "xemacs"
        "emacs")
    "The name of the Emacs program.")

  (defconst emacs-program-version emacs-version
    "The version of the Emacs program.")

  (defconst xemacs-codename
    (if xemacs-p
        "Lucid"                         ; I know it's no longer Lucid.
        "GNU")
    "The codename for this Emacs program.")

  (defun construct-emacs-version-name ()
    "Construct an Emacs version string from the name and version."
    (concat emacs-program-name
            "-"
            emacs-program-version)))

;;;
;;; Port of `emacs-version>=' for emacsen other than XEmacs.
(if (or (not xemacs-p)
        xemacs=19-p
        xemacs=20-p)
    (defun emacs-version>= (major &optional minor patch)
      "Returns T if the Emacs version number is greater or equal to
the given MAJOR, MINOR and PATCH version numbers.

Only the MAJOR version number is required, the MINOR and PATCH
arguments are optional.

Only non-NIL arguments are used in the test, and PATCH is ignored but
present for compatability reasons."
      (cond
       ((> emacs-major-version major))  ; T if major is >
       ((< emacs-major-version major)   ; NIL if major is <
        nil)
       ((null minor))                   ; T if minor is NIL
       ((> emacs-minor-version minor))  ; T if minor is >
       ((< emacs-minor-version minor)   ; NIL if minor is <
        nil))))

;;;
;;; Port of `push' for Emacs 18.
(if emacs=18-p
    (defmacro push (value lst)
      (list 'setq (list 'cons value lst))))

;;;
;;; Enable `erase-buffer'.
(put 'erase-buffer 'disabled nil)

;;;
;;; Put mouse selection in the kill buffer.
(when (or windows-p
          x-windows-p
          nextstep-p
          presentation-manager-p)
  (defun mouse-track-drag-copy-to-kill (event count)
    "Copy the dragged region to the kill ring."
    (let ((region (default-mouse-track-return-dragged-selection
                   event)))
      (when region
        (copy-region-as-kill (car region) (cdr region)))
      nil))
  (add-hook 'mouse-track-drag-up-hook 'mouse-track-drag-copy-to-kill))

;;;
;;; Avoid deactivation of a region when the buffer end or beginning is
;;; reached.
(when (or emacs>=19-p
          (emacs-version>= 21 4 10))
  (defadvice line-move (around catch-buffer-border-error activate)
    "Catch errors `beginning-of-buffer' and `end-of-buffer' to avoid
deactivation of the region."
    (condition-case ()
         ad-do-it
       ((beginning-of-buffer end-of-buffer)))))

;;;
;;; Set a custom frame title.
(when (or emacs>=20-p
          xemacs>=19-p)
  (setq frame-title-format
        (concat (construct-emacs-version-name)
                (when (featurep 'mule)
                  " (Mule) ")
                "["
                xemacs-codename
                "] %b")))

;;;
;;; Resize the minibuffer when needed. (XEmacs only)
(when (and xemacs-p
           (emacs-version>= 21 4 10))
  (autoload 'resize-minibuffer-mode "rsz-minibuf" nil t)
  (resize-minibuffer-mode)
  (setq resize-minibuffer-window-exactly nil
        minibuffer-max-depth nil))

;;;
;;; Don't invert colours for passwords.
(when (or emacs>=20-p
          xemacs>=19-p)
  (setq passwd-invert-frame-when-keyboard-grabbed nil))

;;;
;;; Change modeline indicators.
(when emacs>=21-p
  (setq pending-delete-modeline-string " PD"
        filladept-mode-line-string " Fa")
  (add-minor-mode 'abbrev-mode " Ab"))

;;;
;;; ibuffer hacks.
(when (and emacs>=21-p
           xemacs>=20-p)
  (setq ibuffer-expert t
        ibuffer-default-sorting-mode 'major-mode
        ibuffer-fontification-level t
        ibuffer-saved-filter-groups
        '(("my-ibuffer-groups"
           ("Dired" (mode . dired-mode))
           ("Documentation" (or (mode . help-mode)
                                (mode . hyper-apropos-help-mode)
                                (mode . hyper-apropos-mode)
                                (mode . info-mode)
                                (mode . manual-mode)))
           ("Fundamental" (mode . fundamental-mode))
           ("Lisp" (or (mode . emacs-lisp-mode)
                       (mode . lisp-mode)
                       (mode . common-lisp-mode)
                       (mode . scheme-mode)
                       (mode . inferior-lisp-mode)
                       (mode . slime-repl-mode)))
           ("Programming" (or (mode . c-mode)
                              (mode . cc-mode)
                              (mode . c++-mode)
                              (mode . csharp-mode)
                              (mode . cperl-mode)
                              (mode . perl-mode)
                              (mode . ruby-mode)
                              (mode . java-mode)
                              (mode . python-mode)
                              (mode . shell-script-mode)
                              (mode . objc-mode)
                              (mode . asm-mode)
                              (mode . makefile-mode)))
           ("Markup" (or (mode . html-mode)
                         (mode . sgml-mode)
                         (mode . xml-mode)
                         (mode . css-mode)
                         (mode . tex-mode)
                         (mode . latex-mode)
                         (mode . texinfo-mode))))))
  
  (add-hook 'ibuffer-mode-hooks
            '(lambda ()
              (ibuffer-switch-to-saved-filter-groups
               "my-ibuffer-groups"
               (ibuffer-add-to-tmp-hide "\\*scratch\\*")))))

;;;
;;; Line and column numbers.
(if (and xemacs-p
         (emacs-version>= 21 5 6))
    (column-number-mode 1)
    (line-number-mode 1))

;;;
;;; Abbreviation dictionary.
(when xemacs-p
  (setq dictionary-abbrev-list
        `(("^/lisp" . "~/Projects/Lisp"))))

;;;
;;; Latin unity. (requires XEmacs and Mule)
(when (and (featurep 'mule)
           xemacs-p)
  (latin-unity-install)
  (add-to-list 'latin-unity-preapproved-coding-system-list
               'iso-8859-1))

;;;
;;; Workaround for an encoding bug in XEmacs 21.5.
(when (and (featurep 'mule)
           xemacs-p
           (emacs-version>= 21 5))
  (set-coding-priority-list '(iso-8-1
                              iso-8-2
                              iso-7
                              no-conversion
                              iso-8-designate
                              iso-lock-shift
                              shift-jis
                              big5
                              ucs-4
                              utf-8)))

;;;
;;; Tell abbrev to shut up.
(when emacs>=19-p
  (quietly-read-abbrev-file))

;;;
;;; Buffer tabs. (XEmacs only)
(when xemacs>=21-p
  (customize-set-variable 'gutter-buffer-tabs-visible-p t)
  (customize-set-variable 'gutters-buffers-tab-visible-p t))

;;;
;;; Blinking cursor.
(when emacs>=21-p
  (blink-cursor-mode 1))

;;;
;;; Language settings for mule.
(when (featurep 'mule)
  (set-language-environment "English"))

;;;
;;; Various `missing' features.
(when emacs>=20-p
  (eval-and-compile
    ;;
    ;; `line-beginning-position'. (used by newcomment.el)
    (unless (fboundp 'line-beginning-position)
      (defun line-beginning-position ()
        "Return the point of the beginning of the current line."
        (save-excursion
          (beginning-of-line)
          (point))))
    ;;
    ;; `line-end-position'. (used by newcomment.el)
    (unless (fboundp 'line-end-position)
      (defun line-end-position ()
        "Return the point of the end of the current line."
        (save-excursion
          (end-of-line)
          (point))))))

;;;
;;; Windowing system hacks
(when (and (not terminal-p)
           emacs>=20-p)
  (require 'msb))

;;;}}}
;;; ==================================================================

;;; utils.el ends here
