;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; boot.el --- Emacs bootstrap.
;;;
;;; Time-stamp: <Wednesday Feb  4, 2015 13:34:41 asmodai>
;;; Revision:   2
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Feb 2015 12:28:43
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

;;;==================================================================
;;;{{{ Emacs 18:

(when (emacs=18-p)
  (load "ledit")
  (push (function (lambda ()
		    (if (not indent-tabs-mode)
			(untabify (point-min) (point-max)))))
	write-file-hooks))

;;;}}}
;;;==================================================================

(unless (emacs=18-p)
  
  (define-bootstrap
    version (emacs-p)
    
    when (emacs=19-p)
      compile-load ("19-extensions"
                    "19-template"
                    "19-folding")
      and require ('paren
                   'time-stamp)
      and do
        (set 'time-stamp-active t)
        (initialize-template-binding)
    
      ;; GCL support.
      and when (or (running-on-nova-p)
                   (running-on-galaxy-p)
                   (running-on-dauntless-p))
        compile-load ("gcl"
                      "dbl"
                      "lisp-complete")
      end                               ; when (or ...)
    end                                 ; when (emacs=19-p)
    
    when (emacs>=19-p)
      compile-load ("revision-hook")
    end
    
    when (emacs=20-p) do 
      (defvar font-lock-comment-delimiter-face 'font-lock-comment-face)
    end
    
    when (emacs>=20-p)
      compile-load ("cparen"
                    "template"
                    "licenses"
                    "folding")
      and require ('time-stamp)
      and do
        (cparen-activate)
        (template-initialize)
        (folding-install)
        (folding-install-hooks)
        (set 'time-stamp-active t)
        (set 'time-stamp-format "%:a %3b %2d, %4y %02H:%02M:%02S %u")

      ;; comint support.
      and when (windows-p)
        require ('comint)
        and do
          (defun comint-false-echo ()
            "Tell comint that our shell likes to echo."
            (if (eq major-mode 'inferior-emacs-lisp-mode)
                (setq comint-process-echoes nil)
                (setq comint-process-echoes t)))
          (add-hook 'comint-mode-hook 'comint-false-echo)
          (setq explicit-cmd.exe-args '("/q")
                explicit-cmdproxy.exe-args '("/q"))
      end				; and when (windows-p)
    end					; when (emacs>=20-p)

    when (emacs>=21-p)
      compile-load ("highlight-parentheses"
		    "newcomment"
		    "hfy-cmap"
		    "mtorus"
		    "htmlize"
		    "csharp-mode")
      and do
        (load "htmlfontify")	     ; Requires eval before a compile.
	(compile-load "htmlfontify")
	(mtorus-init)
	(mtorus-install-suggested-bindings)         
      and if (terminal-p)
        compile-load ("xt-mouse")
      else
        require ('mwheel)
      end
    end					; when (emacs>=21-p)
          
    when (emacs>=22-p)
      compile-load ("paredit"
		    "perl-reformat")
      and require ('tramp
		   'image-mode
		   'erc)
      ;; TODO: Symbolics keyboard support.
    end					; when (emacs>=22-p)
      
    when (emacs>=23-p)
      require ('cedet
	       'semantic
	       'srecode)
      and compile-load ("electric-shift-lock-mode"
			"erlang-start"
			"ruby-mode"
			"rdoc-mode"
			"inf-ruby"
			"ruby-electric"
			"ruby-style"
			"ppindent"
			"company"
			"company-abbrev"
			"company-css"
			"company-elisp"
			"company-semantic"
			"company-keywords"
			"company-template"
			"slime-company"
			"inf-ruby-company"
			"treetop-mode"
			"cmake-mode"
			"andersl-cmake-font-lock")
      and do
        (setq semantic-load-turn-useful-things-on t)
	(global-ede-mode -1)
	(global-semantic-decoration-mode 2)
	(global-semantic-highlight-edits-mode 1)
	(global-semantic-highlight-func-mode 1)
	(global-semantic-show-unmatched-syntax-mode 1)
	(global-semantic-stickyfunc-mode 1)
	(setq cedet-graphviz-dot-command
	      (cond ((mac-os-x-p)
		     "/usr/local/bin/dot")
		    ((and (unix-p)
			  (not (mac-os-x-p)))
		     "/usr/bin/dot")))
	(setq cedet-graphviz-neato-command
	      (cond ((mac-os-x-p)
		     "/usr/local/bin/neato")
		    ((and (unix-p)
			  (not (mac-os-x-p)))
		     "/usr/bin/neato")))
	(setq cedet-cscope-command
	      (cond ((unix-p)
		     "/usr/bin/cscope")))
	(unless (boundp 'x-max-tooltip-size)
	  (setq x-max-tooltip-size '(80 . 40)))
	(autoload 'company-mode "company" nil t)
    end					; when (emacs>=23-p)
      
    when (xemacs>=20-p)
      require ('font-lock)
      and compile-load ("template"
			"revision-hook"
			"licenses"
			"csharp-mode"
			"timestamp")
      and unless (fboundp 'font-lock-add-keywords) do
        (defalias 'font-lock-add-keywords 'ignore)
      end
      and unless (fboundp 'define-major-mode)
        (defalias 'define-major-mode 'ignore)
      end
      and unless (fboundp 'make-annotation)
        (defalias 'make-annotation 'ignore)
      end
      and when (and (or (x-windows-p)
			(windows-p))
		    (featurep 'toolbar))
        compile-load ("x-toolbar")
      end
      and when (x-windows-p)
        require ('x-mouse)
      end
      and when (terminal-p)
        compile-load ("xt-mouse")
      end
      and do
        (set 'time-stamp-active t)
        (set 'time-stamp-format "%:a %3b %2d, %4y %02H:%02M:%02S %u")
    end					; when (xemacs>=20-p)

    when (xemacs>=21-p)
      when (emacs-version>= 21 4 10)
        compile-load ("newcomment"
		      "htmlfontify"
		      "mtorus"
		      "htmlize")
	and require ('tramp
		     'image-mode)
	and do
	  (mtorus-init)
	  (mtorus-install-suggested-bindings)
      end
      and when (emacs-version>= 21 4 20)
        compile-load "folding"
	and require ('erc)
	and do
	  (folding-install)
      end
    end 				; when (xemacs>=21-p)

    compile-load ("perl-reformat"
		  "site-lisp-mode"
		  "site-c-mode"
		  "site-html-mode"
		  "site-perl-mode"
		  "site-lua-mode"
		  "site-erlang-mode"
		  "site-page-break-mode"
		  "site-ibuffer")
    
    when (or (emacs>=19-p) (xemacs>=19-p)) do
      (setq-default indent-tabs-mode nil)
      (add-hook (if (emacs=24-p)
		    'before-save-hook
		    'write-file-hooks)
		(lambda ()
		  (when (not indent-tabs-mode)
		    (untabify (point-min) (point-max)))))
    end

    when (emacs=20-p)
      when (fboundp 'global-font-lock-mode) do
        (global-font-lock-mode 1)
	(setq font-lock-auto-fontify t)
      end
      and do
        (set-background-color "black")
    end

    when (or (emacs>=19-p) (xemacs>=19-p)) do
      (when (featurep 'time-stamp)
	(add-hook (if (emacs=24-p)
		      'before-save-hook
  		      'write-file-hooks)
		  'time-stamp))
      (add-hook (if (emacs=24-p)
		    'before-save-hook
		    'write-file-hooks)
		'auto-update-file-header)
      (when (featurep 'folding)
	(add-hook 'find-file-hooks (lambda ()
				     (folding-mode t))))
    end))
  
 
;;; boot.el ends here
