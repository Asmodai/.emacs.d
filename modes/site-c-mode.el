;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-c-mode.el --- C mode hacks.
;;;
;;; Time-stamp: <Tuesday Sep  4, 2012 18:31:35 asmodai>
;;; Revision:   61
;;;
;;; Copyright (c) 2011-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    16 Feb 2011 07:24:29
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

;;; Comment editing.
(when emacs>=21-p
  (require 'autopair)
  (require 'c-comment-edit)
  (if (featurep 'c-comment-edit)
      (setq c-comment-leader "  ")))

;;; C-family autoload support
(when (and emacs>=21-p
           (or running-on-yorktown-p
               running-on-lisp-machine-p)) 
  (autoload 'csharp-mode "csharp-mode"
    "Major mode for editing C# code." t)

  (setq auto-mode-alist
        (append '(("\\.cs$" . csharp-mode))
                auto-mode-alist)))

;;;
;;; ctags
(setq path-to-ctags (if windows-p
                        "C:/Program Files/Emacs-23.2/bin/ctags"
                        "/usr/bin/ctags"))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f %s/TAGS -e -R %s"
           path-to-ctags
           dir-name
           (directory-file-name dir-name))))

;;;
;;; The rest of this file is meant for Emacs >= 19.
;;;

(when emacs>=19-p
  ;; We quite like `cc-mode'.
  (require 'cc-mode)

  ;; Custom C indentation style.
  (defconst hackers-c-style
    '((c-tab-always-indent . t)
      (c-comment-only-line-offset . 0)
      (c-basic-offset . 2)
      (c-hanging-braces-alist . ((substatement-open after)
                                 (brace-list-open after)))
      (c-hanging-colons-alist . ((member-init-intro before)
                                 (inher-intro)
                                 (case-label after)
                                 (label after)
                                 (access-label after)))
      (c-cleanup-list . (scope-operator
                         empty-defun-braces
                         brace-else-brace
                         brace-elseif-brace
                         brace-catch-brace
                         defun-close-semi
                         list-close-comma
                         ))
      (c-offsets-alist . ((statement-block-intro . +)
                          (inline-open           . 0)
                          (arglist-close         . c-lineup-arglist)
                          (knr-argdecl-intro     . +)
                          (substatement-open     . 0)
                          (label                 . 0)
                          (statement-cont        . +)
                          (case-label            . +)))
      (c-echo-syntactic-information-p . t)
      (c-syntactic-indentation . t)
      (c-report-syntactic-errors . t))
    "Custom BSD-derived C syntax style.")
  
  ;; Custom C++ indentation style.
  (defconst hackers-c++-style
    '((c-tab-always-indent . t)
      (c-comment-only-line-offset . 0)
      (c-basic-offset . 2)
      (c-hanging-braces-alist . ((substatement-open after)
                                 (namespace-open after)
                                 (inline-open after)
                                 (class-open after)
                                 (module-open after)
                                 (brace-list-open after)))
      (c-hanging-colons-alist . ((member-init-intro before)
                                 (inher-intro)
                                 (case-label after)
                                 (label after)
                                 (access-label after)))
      (c-cleanup-list . (scope-operator
                         empty-defun-braces
                         brace-else-brace
                         brace-elseif-brace
                         brace-catch-brace
                         defun-close-semi
                         list-close-comma))
      (c-offsets-alist . ((statement-block-intro . +)
                          (inline-open           . 0)
                          (arglist-close         . c-lineup-arglist)
                          (knr-argdecl-intro     . +)
                          (substatement-open     . 0)
                          (label                 . 0)
                          (statement-cont        . +)
                          (case-label            . +)))
      (c-echo-syntactic-information-p . t)
      (c-syntactic-indentation . t)
      (c-report-syntactic-errors . t))
    "Custom BSD-derived C++ syntax style.")

  ;; Add the new styles to the available styles.
  (c-add-style "hackers-c" hackers-c-style)
  (c-add-style "hackers-c++" hackers-c++-style)

  ;; Set up the default styles
  (if (or running-on-yorktown-p
          running-on-lisp-machine-p
          running-on-magellan-p)
      (setq-default c-default-style
                    '((c-mode      . "hackers-c")
                      (c++-mode    . "hackers-c++")
                      (objc-mode   . "hackers-c")
                      (java-mode   . "hackers-c")
                      (cperl-mode  . "hackers-c")
                      (other       . "gnu")
                      (csharp-mode . "hackers-c")))
      (setq-default c-default-style
                    '((c-mode      . "hackers-c")
                      (c++-mode    . "hackers-c++")
                      (objc-mode   . "hackers-c")
                      (java-mode   . "hackers-c")
                      (other       . "gnu"))))
  
  ;; Set some defaults
  (add-to-list 'c-cleanup-list 'one-liner-defun)
  
  ;; Custom indentation commands.
  (defun my-custom-c-indent ()
    (unless (or (eq (caar c-syntactic-context) 'c)
                (eq (caar c-syntactic-context) 'brace-list-close)
                (eq (caar c-syntactic-context) 'topmost-intro))
      (save-excursion
        (let ((place (if (and (listp (cadr c-syntactic-context))
                              (not (null (cadr c-syntactic-context)))
                              (not (null (cadadr c-syntactic-context))))
                         (- (cadadr c-syntactic-context) 255)
                         (cadar c-syntactic-context))))
          (if (not (null place))
              (align place (point) nil))))))
  
  (add-hook 'c-special-indent-hook 'my-custom-c-indent)
  
  ;; Add some hooks
  (defun my-common-c-mode-hooks ()
    (when (or emacs=20-p emacs=21-p)
      (turn-on-font-lock)
      (font-lock-mode 1))
    (setq indent-tabs-mode nil
          c-max-one-liner-length 80)
    (if (eq major-mode 'c++-mode)
        (c-set-style "hackers-c++")
        (c-set-style "hackers-c"))
    (when (featurep 'company)
      (company-mode 1))
    (when (featurep 'highlight-parentheses)
      (hi-parens-autopair))
    (show-paren-mode 1)
    (eldoc-mode 1)
    (c-toggle-auto-state 1)
    (c-toggle-syntactic-indentation 1)
    (c-toggle-electric-state 1)
    (c-toggle-hungry-state -1)
    (c-toggle-auto-hungry-state -1)
    (c-toggle-auto-newline -1)
    (ecb-minor-mode 1)
    (subword-mode 1)
    (autopair-mode 1)
    (auto-fill-mode 1))

  (add-hook 'c-mode-common-hook 'my-common-c-mode-hooks)

  ;; CEDET/Semantic hooks
  (when (featurep 'semantic)
    (defun my-c-mode-cedet-hook ()
      (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
      (local-set-key [(control tab)] 'semantic-ia-complete-symbol)
      (local-set-key "\C-c." 'senator-complete-symbol))
    
    (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)))

;;;
;;; Doxygen stuff.
(when emacs>=22-p
  (require 'doc-mode)
  (add-hook 'c-mode-common-hook 'doc-mode))

;;; site-c-mode.el ends here
