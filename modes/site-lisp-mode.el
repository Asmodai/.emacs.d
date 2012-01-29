;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-lisp-mode.el --- Lisp mode hacks.
;;;
;;; Time-stamp: <Sunday Jan 29, 2012 17:32:53 asmodai>
;;; Revision:   24
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Mon Jun 06 02:48:09 2005
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

;;;
;;; There are two inferior Lisp systems that could be used here:
;;; Franz' ELI/XELI and Slime.  You can choose which one to use by
;;; setting the following variable to one of `allegro', `slime', or
;;; `nil' if there is no special inferior Lisp (or you are using an
;;; older Lisp that does not support either.)
;;;
(defvar *interactive-lisp-mode*
  (cond (running-on-lisp-machine-p 'slime)
        (running-on-magellan-p 'slime)
        (running-on-farragut-p 'slime)
        (running-on-yorktown-p 'slime)
        (t nil))
  "Current Interactive Lisp mode.

Can be one of:

`slime'     - Connect to a Lisp using SLIME.
`allegro'   - Connect to Allegro Common Lisp using ELI.
`nil'       - Unsupported or old Lisp.")

;;; ==================================================================
;;;{{{ Utilities and predicates:

(defconst allegro-p
  (eq *interactive-lisp-mode* 'allegro)
  "T if we are using Franz Allegro Common Lisps' ELI.")

(defconst slime-p
  (eq *interactive-lisp-mode* 'slime)
  "T if we are using SLIME.")

(defconst interactive-lisp-p
  (or allegro-p slime-p)
  "T if we are using a Lisp interactive mode.")

;;;
;;; Sanity.  Slime will not load on older Emacsen.  ELI might if one
;;; has an old-enough copy of Allegro Common Lisp.
(when (and (or emacs=18-p
               emacs=19-p
               emacs=20-p)
           slime-p)
  (setq *interactive-lisp-mode* nil
        slime-p nil))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Franz Allegro Common Lisp:

(when allegro-p
  ;;
  ;; GNU Emacs gets 'eli', and XEmacs gets 'xeli'.
  (if xemacs-p
      (compile-load "xeli/fi-site-init")
      (compile-load "efi/fi-site-init"))

  ;;
  ;; Path to our Lisp image and the host to connect to.
  (setq fi:common-lisp-image-name
        (cond (windows-nt-p
               "c:/acl82express/allegro-express.exe")
              (linux-p
               "~/Projects/Lisp/implementations/acl81_express/alisp"))
        fi:common-lisp-host "localhost")

  ;;
  ;; Function to start up Allegro.
  (defun start-allegro-cl ()
    "Starts up Allegro Common Lisp using the image name defined in
`fi:common-lisp-image-name'."
    (interactive)
    (fi:common-lisp fi:common-lisp-buffer-name
                    fi:common-lisp-directory
                    fi:common-lisp-image-name
                    fi:common-lisp-image-arguments
                    fi:common-lisp-host
                    fi:common-lisp-image-file))

  ;;
  ;; Remove `fi:emacs-lisp-mode' because I do not like it.
  (let ((acc nil))
    (mapcar (function (lambda (x)
                        (let ((mode (cdr x)))
                          (if (not (eq mode 'fi:emacs-lisp-mode))
                              (push x acc)))))
            auto-mode-alist)
    (setf auto-mode-alist (nreverse acc)))

  ;;
  ;; Custom version of `fi:exit-lisp' that doesn't cause the remote
  ;; Lisp to exit.
  (defun fi:exit-lisp ()
    "Exits the local Lisp listener only."
    (interactive)
    (message "Exiting this session...")
    (let ((cl-buffer (get-buffer fi:common-lisp-buffer-name)))
      (when (and cl-buffer)
        (let* ((screen (selected-frame)))
          (if (and (string= (symbol-name (get major-mode 'screen-name))
                            (frame-name screen))
                   (one-window-p)
                   nil)
              (delete-frame screen)
              (bury-buffer)))))
    (message "Exiting this session... done."))

  ;;
  ;; Function to start ELI.
  (defun lisp-localhost (&rest args)
    (interactive)
    (fi:start-interactive-via-file "localhost"
                                   "*common-lisp-localhost*"
                                   "~/.eli-startup")))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ SLIME:

(when slime-p
  ;;
  ;; Set the inferior Lisp program.
  (setq inferior-lisp-program
        (cond (unix-p "sbcl --no-inform --no-linedit")
              (windows-nt-p "c:/ccl/wx86cl.exe")
              (t "lisp")))

  ;;
  ;; Load in SLIME.
  (compile-load "slime")
  (require 'slime)

  ;;
  ;; `slime-autodoc' does not work on XEmacs.
  (when emacs-p
    (compile-load "slime-fancy")
    (require 'slime-fancy))

  ;;
  ;; Configure which SLIME packages we want to load.
  (let ((slime-packages
         (if emacs-p
             '(slime-fancy slime-asdf slime-repl slime-autodoc)
             '(slime slime-asdf slime-repl))))
    (slime-setup slime-packages)))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Neat comments which should really be else-where:

(defun my-comment-region (start end)
  "Comment the region defined by `start' and `end' with a \"correct\"
number of comment characters depending on the major mode of the
buffer.  Has been known to get things horribly wrong."
  (interactive "*")
  (let* ((tmode (downcase mode-name))
         (chars (cond ((string= tmode "lisp") 3)
                      ((string= tmode "lisp interaction") 3)
                      ((string= tmode "scheme") 3)
                      ((string= tmode "emacs-lisp") 3)
                      (t 1))))
    (comment-region start end chars)))

(defun my-insert-comment (name &optional chars)
  "Insert a comment at the current point.  The comment will look
similar to:

<comment> <chars>
<comment> {{{ <name>

<comment> }}}
<comment> <chars>

To see an example of the output, look at site-lisp-mode.el."
  (interactive "*")
  (let ((start (point)))
    (insert (concat chars
                    "{{{ " name ":\n\n"
                    "}}}\n"
                    chars "\n"))
    (let ((end (point))
          (comment-padding ""))
      (my-comment-region start end))))

(defun make-group-lisp-comment (&rest args)
  "Insert a comment delimited by the = character."
  (interactive "*")
  (my-insert-comment
   "Group"
   "==================================================================\n"))

(defun make-major-lisp-comment (&rest args)
  "Insert a comment delimited by the - character."
  (interactive "*")
  (my-insert-comment
   "Major"
   "------------------------------------------------------------------\n"))

(defun make-minor-lisp-comment (&rest args)
  "Insert a comment delimited by the . character."
  (interactive "*")
  (my-insert-comment
   "Minor"
   "..................................................................\n"))

(defun make-plain-lisp-comment (&rest args)
  "Insert a comment with no delimitor character."
  (interactive "*")
  (my-insert-comment "Comment"))

;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Key bindings:

;;;
;;; Bindings for an Emacs that doesn't use a Symbolics keyboard
(unless (featurep 'symbolics)
  ;;
  ;; Bindings for Franz' ELI mode.
  (when allegro-p
    (global-set-key [(control f9)] 'start-allegro-cl)
    (global-set-key [(control f10)] 'lisp-localhost))
  
  ;;
  ;; Bindings for SLIME.
  (when slime-p
    (global-set-key [(control f9)] 'slime)
    (global-set-key [(control f10)] 'slime-connect)
    (global-set-key (kbd "<backtab>") 'slime-complete-symbol)
    (global-set-key [(meta r)] 'slime-reindent-region))
  
  ;;
  ;; Bindings for our custom comments.
  (when emacs>=19-p
    (global-set-key [(f6)] 'make-group-lisp-comment)
    (global-set-key [(f7)] 'make-major-lisp-comment)
    (global-set-key [(f8)] 'make-minor-lisp-comment)
    (global-set-key [(f9)] 'make-plain-lisp-comment)))

;;;
;;; Bindings for an Emacs that uses a Symbolics keyboard.
(when (featurep 'symbolics)
  ;;
  ;; Bindings for Franz' ELI mode.
  (when allegro-p
    (global-set-key [(control f9)] 'start-allegro-cl)
    (define-select-key "l" 'lisp-localhost))
  
  ;;
  ;; Bindings for SLIME.
  (when slime-p
    (define-select-key "s" 'slime)
    (define-select-key "l" 'slime-connect)
    (global-set-key (kbd "<backtab>") 'slime-complete-symbol)
    (global-set-key (vector +symbolics-complete-key+)
                    'slime-complete-symbol)
    (global-set-key [(meta r)] 'slime-reindent-retion))
  
  ;;
  ;; Bindings for custom comments.
  (when emacs>=19-p
    (define-function-key "1" 'make-group-lisp-comment)
    (define-function-key "2" 'make-major-lisp-comment)
    (define-function-key "3" 'make-minor-lisp-comment)
    (define-function-key "4" 'make-plain-lisp-comment)))
                  
;;;}}}
;;; ==================================================================

;;; ==================================================================
;;;{{{ Other mode hacks:

;;; ------------------------------------------------------------------
;;;{{{ Common Lisp indentation:

(when emacs>=19-p
  ;;
  ;; Load in indentation hacks for Common Lisp.
  (compile-load "cl-indent-patches")
 
  ;;
  ;; Patch the indenting function for XEmacs.
  (if xemacs-p
      (setq lisp-indent-function
            (function common-lisp-indent-function))))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ ParEdit mode:

;;;
;;; Configure the ParEdit sexpr editor
(when (featurep 'paredit)
  (when slime-p
    (add-hook 'slime-repl-mode-hook (lambda ()
                                      (paredit-mode +1)))

    (defun override-slime-repl-bindings ()
      (define-key slime-repl-mode-map
          (read-kbd-macro paredit-backward-delete-key) nil))
    (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings))

  (defvar electrify-return-match "[\]}\)\"]")

  (defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match'
then open and indent an empty line between the cursor and the
text.  Move the cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
        (if (looking-at electrify-return-match)
            (save-excursion
              (newline-and-indent)))
        (newline arg)
        (indent-according-to-mode)))

  (global-set-key (kbd "RET") 'electrify-return-if-match))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Parenthesis highlighting:

;;;
;;; Configure parenthesis highlighting.
(when (featurep 'highlight-parentheses)
  ;;
  ;; We want `highlight-parentheses' to work with autopair.
  (defun hi-parens-autopair ()
    (highlight-parentheses-mode t)
    (setq autopair-handle-action-fns
          (list 'autopair-default-handle-action
                '(lambda (action pair pos-before)
                  (hl-paren-color-update))))))

;;;}}}
;;; ------------------------------------------------------------------

;;; ------------------------------------------------------------------
;;;{{{ Other mode hooks:

;;;
;;; Hooks for auto-fill, font-lock, and SLIME.
(when emacs>=19-p
  ;;
  ;; I know these could be represented better, but I'd rather they
  ;; have fewer calls.
  ;;
  
  ;;
  ;; For interactive lisp, e.g. *scratch* buffer.
  (defun my-interactive-lisp-mode-hooks ()
    (when (or emacs=20-p
              emacs=21-p)
      (turn-on-font-lock)
      (font-lock-mode t))
    (when (featurep 'paredit)
      (paredit-mode +1))
    (when (featurep 'company)
      (company-mode t))
    (when (featurep 'highlight-parentheses)
      (hi-parens-autopair))
    (eldoc-mode t)
    (show-paren-mode t)
    (auto-fill-mode t))
  
  ;;
  ;; For non-SLIME inferior lisp modes.
  (defun my-inferior-lisp-mode-hooks ()
    (when (or emacs=20-p
              emacs=21-p)
      (turn-on-font-lock)
      (font-lock-mode t))
    (when (featurep 'paredit)
      (paredit-mode +1))
    (when (featurep 'company)
      (company-mode t))
    (when (featurep 'highlight-parentheses)
      (hi-parens-autopair))
    (show-paren-mode t))
  
  ;;
  ;; For SLIME lisp modes.
  (defun my-slime-lisp-mode-hooks ()
    (when (or emacs=20-p
              emacs=21-p)
      (turn-in-font-lock)
      (font-lock-mode 1))
    (when slime-p
      (slime-mode t))
    (when (featurep 'paredit)
      (paredit-mode +1))
    (when (featurep 'company)
      (company-mode t))
    (when (and slime-p
               (featurep 'company))
      (require 'slime-company))
    (when (featurep 'highlight-parentheses)
      (hi-parens-autopair))
    (show-paren-mode t)
    (auto-fill-mode t)
    (show-paren-mode t))
  
  ;;
  ;; For non-SLIME lisp modes.
  (defun my-lisp-mode-hooks ()
    (when (or emacs=20-p
              emacs=21-p)
      (turn-on-font-lock)
      (font-lock-mode 1))
    (when (featurep 'paredit)
      (paredit-mode +1))
    (when (featurep 'company)
      (company-mode t))
    (when (featurep 'highlight-parentheses)
      (hi-parens-autopair))
    (when (eq major-mode 'emacs-lisp-mode)
      (eldoc-mode t))
    (show-paren-mode t)
    (auto-fill-mode t)
    (show-paren-mode t))

  ;;
  ;; Hooks for modes derived from emacs-lisp-mode
  (add-hook 'lisp-interaction-mode-hook 'my-interactive-lisp-mode-hooks)
  (add-hook 'inferior-lisp-mode-hook 'my-inferior-lisp-mode-hooks)
  (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hooks)
  (add-hook 'lisp-mode-hook 'my-slime-lisp-mode-hooks)
  (add-hook 'scheme-mode-hook 'my-lisp-mode-hooks))

;;;}}}
;;; ------------------------------------------------------------------

;;;}}}
;;; ==================================================================

;;; site-lisp-mode.el ends here
