;;; zmacs-base.el --- Base Emacs packages and settings  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 13:26:28
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

(require 'cl-lib)

;;;; Early stuff that's done before anything else:

;; Let's add some Zetalisp all up in this here house!
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set frame and icon titles.
(setq-default frame-title-format '("ZMACS")
              icon-title-format  '("ZMACS"))

;;;; Files package:

(use-package files
  :ensure nil
  :defer 1
  :custom
  (require-final-newline t)
  (large-file-warning-threshold 100000000)
  (confirm-kill-processes nil)
  (find-file-visit-truename t))

;;;; Emacs package:

(use-package emacs
  :ensure nil
  :custom
  (sentence-end-double-space nil)
  (tab-width                 8)
  (fill-column               80)
  (tab-always-indent         'complete)
  (indent-tabs-mode          nil)
  (make-pointer-invisible    t)
  (visible-bell              t)
  (blink-cursor-mode         0)
  :config
  (setq-default tab-width                  8
                fill-column                80
                indent-tabs-mode           nil
                tab-always-indent          'complete
                completion-cycle-threshold 3
                visible-bell               t)

  ;; Short answers.
  (if (boundp 'use-short-answers)
      (setq use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p)))

;;;; Subword package:

(use-package subword
  :ensure nil
  :hook (after-init . global-subword-mode))

;;;; Simple package:

(use-package simple
  :ensure nil
  :custom
  (line-move-visual t)
  (global-mark-ring-max 8)
  (mark-ring-max 8))

;;;; Display line numbers:

(use-package display-line-numbers
  :ensure nil
  :defer nil
  :commands display-line-numbers-mode
  :hook ((text-mode . display-line-numbers-mode)
         (prog-mode . display-line-numbers-mode))
  :config
  (setq-default display-line-numbers-type t
                display-line-numbers-width-start t))

;;;; Mule:

(use-package mule-cmds
  :ensure nil
  :defer t
  :config
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

;;;; Advice:

(use-package advice
  :ensure nil
  :defer 1
  :custom
  (ad-redefinition-action 'accept))

;;;; Page break lines:

(use-package page-break-lines
  :defer t
  :commands (global-page-break-lines-mode)
  :init
  (global-page-break-lines-mode))

;;;; Selections:

;; Allow deletion.
(delete-selection-mode)

;;;; Diminish:

(use-package diminish
  :ensure t
  :demand t
  :commands (diminish))

(defvar *zmacs-diminished-minor-modes* nil
  "List of diminished minor modes.")

(defun zmacs--prepare-diminish ()
  "Prepare to diminish minor modes."
  (let ((unicodep t))
    (dolist (mm *zmacs-diminished-minor-modes*)
      (let ((mode (car mm)))
        (when (and (boundp mode)
                   (symbol-value mode))
          (let* ((unicode (cadr mm))
                 (ascii (caddr mm))
                 (dim (if unicodep
                          unicode
                        (if ascii ascii unicode))))
            (diminish mode dim)))))))

(defun zmacs-diminish-hook (_)
  "Display diminished lighter in vanilla Emacs mode-line."
  (let ((unicodep t))
    (cl-loop for (mode uni nouni) in *zmacs-diminished-minor-modes*
             do (diminish mode (if unicodep uni nouni)))))

(defun zmacs--terminal-fix-mode-line-indicator-overlap (str)
  "Add a space between the two characters in STR.

This fixes an overlapping issue, that occurs when ZMACS is started in a
 terminal, and a modes mode line name is diminished to:
- A unicode character followed by a non unicode character, ex: \" Ⓔh\"
- Or to two unicode characters, ex: \" Ⓔⓗ\""
  (let ((first-char (substring str 1 2)) ; first char after the space
        second-char)
    (if (equal (char-charset (string-to-char first-char)) 'unicode)
        (progn
          (setq second-char (substring str 2 3)) ; second char after the space
          (concat first-char " " second-char))
      str)))

(defmacro zmacs-diminish (mode &optional unicode ascii)
  "Diminish MODE name in the mode line to either UNICODE or ASCII."
  (when (and unicode
             (not (display-graphic-p))
             (= (length unicode) 3))
    (setq unicode (zmacs--terminal-fix-mode-line-indicator-overlap unicode)))
  `(let ((cell (assq ',mode *zmacs-diminished-minor-modes*)))
     (if cell
         (setcdr cell '(,unicode ,ascii))
       (push '(,mode ,unicode ,ascii) *zmacs-diminished-minor-modes*))))

;; Set up diminish stuff.
(add-hook 'after-load-functions #'zmacs-diminish-hook)

;;;; Whitespace Butler:

(use-package ws-butler
  :ensure t
  :commands (ws-butler-mode
             ws-butler-global-mode)
  :hook (prog-mode . ws-butler-mode)
  :config
  (zmacs-diminish ws-butler-mode        " ⌫" " WB")
  (zmacs-diminish ws-butler-global-mode " ⌫" " WB"))

(global-set-key (kbd "S-s-SPC") #'cycle-spacing)

;;;; Save history:

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq-default savehist-file (concat *zmacs-cache-directory*
                                      "savehist"))

  (when (not (file-exists-p savehist-file))
    (write-file savehist-file))

  (setq savehist-save-minibuffer-history t
        history-length                   100)

  (put 'minibuffer-history 'history-length 50)
  (put 'kill-ring          'history-length 25)

  (savehist-mode 1))

;;;; Desktop:

(use-package desktop
  :ensure nil
  :commands (desktop-save-mode)
  :config
  (setq desktop-dirname             (concat *zmacs-cache-directory*
                                            "desktops")
        desktop-base-file-name      "emacs.desktop"
        desktop-base-lock-name      "lock"
        desktop-path                (list desktop-dirname)
        desktop-save                'ask-if-new
        desktop-files-not-to-save   (concat "^$" ".*magit$")
        desktop-restore-eager       4
        desktop-load-locked-desktop t)

  (when (not (file-exists-p desktop-dirname))
    (make-directory desktop-dirname t))

  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$")))

(defun zmacs-my-desktop ()
  "Load the desktop and enable autosaving."
  (interactive)
  (let ((desktop-load-locked-desktop "ask"))
    (desktop-read)
    (desktop-save-mode 1)))

(defun zmacs-save-dsektop-save-buffers-kill-emacs ()
  "Save buffers and current desktop every time we quit emacs."
  (interactive)
  (desktop-save-in-desktop-dir)
  (save-buffers-kill-emacs))

;;;; Timestamps:

(use-package time-stamp
  :ensure nil
  :commands (time-stamp zmacs-time-stamp)
  :hook (before-save . time-stamp)
  :custom
  (time-stamp-active     t)
  (time-stamp-line-limit 10)
  (time-stamp-format     "Last modified on %Y-%02m%02d-%02H:%02M:%02S"))

(use-package zlisp-time
  :ensure nil
  :demand t)

;;;; Long lines:

(use-package so-long
  :commands (global-so-long-mode
             so-long-mode)
  :hook (after-init . global-so-long-mode))

;;;; Read Only:

(setq view-read-only t)

;;;; Expand Region:

(use-package expand-region
  :defer 1)

;;;; Safe Variables:

(use-package files
  :ensure nil
  :defer t
  :config
  (setq safe-local-variable-values
        '((eval require 'org-roam-dev)
          (eval when
                (fboundp 'rainbow-mode)
                (rainbow-mode 1))
          (org-download-heading-lvl)
          (magit-todos-branch-list nil))))

;;;; Restart Emacs

(use-package restart-emacs
  :when (version< emacs-version "29")
  :commands restart-emacs)

;;;; Undo:

(use-package emacs
  :ensure nil
  :defer 1
  :config
  ;; Don't group undo steps. Why?
  ;; .. without this it groups actions into a fixed number of steps which
  ;; feels unpredictable.
  (fset 'undo-auto-amalgamate 'ignore)
  ;; Increase undo limits. Why?
  ;; .. ability to go far back in history can be useful, modern systems have
  ;; sufficient memory.
  ;; Limit of 64mb.
  (setq undo-limit 6710886400)
  ;; Strong limit of 1.5x (96mb)
  (setq undo-strong-limit 100663296)
  ;; Outer limit of 10x (960mb).
  ;; Note that the default is x100), but this seems too high.
  (setq undo-outer-limit 1006632960))

;;;; Undo Tree:

;; (use-package undo-tree
;;   :defer t
;;   :commands (undo-tree-visualizer-timestamps
;;              undo-tree-visualizer-diff
;;              undo-tree-enable-undo-in-region
;;              undo-tree-history-directory-alist)
;;   :custom
;;   (undo-tree-visualizer-timestamps t)
;;   (undo-tree-visualizer-diff       t)
;;   (undo-tree-enable-undo-in-region t)
;;   :config
;;   (require 'undo-tree)
;;   (setq undo-tree-history-directory-alist
;;         `(("." . ,(let ((dir (expand-file-name "undo-tree-history"
;;                                                *zmacs-cache-directory*)))
;;                     (if (file-exists-p dir)
;;                         (unless (file-accessible-directory-p dir)
;;                           (warn "Cannot access directory `%s'.
;; Perhaps you don't have required permissions, or it's not a directory.
;; See variable `undo-tree-history-directory-alist'." dir))
;;                       (make-directory dir))
;;                     dir))))

;;   (zmacs-diminish undo-tree-mode " ⎌" " U")
;;   (global-undo-tree-mode))

(use-package undo-fu
  :ensure t
  :demand t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key   (kbd "C-z") #'undo-fu-only-undo)
  (global-set-key   (kbd "C-r") #'undo-fu-only-redo))

(use-package undo-fu-session
  :commands (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))
(undo-fu-session-global-mode)

(use-package vundo
  :ensure t
  :demand t
  :config
  (global-unset-key (kbd "C-x u"))
  (global-set-key   (kbd "C-x u") #'vundo))

(use-package vundo-diff
  :ensure nil
  :after vundo
  :demand t)

;;;; Multisession:

(use-package multisession
  :defer t
  :config
  (setq multisession-directory (concat *zmacs-cache-directory*
                                       "multisession/")))

;;;; Editing binary files:

(use-package hexl
  :defer t)

;;;; Discover links:

(use-package link-hint
  :ensure t
  :defer t
  :commands (link-hint-open-link
             link-hint-copy-link
             link-hint-open-link-at-point)
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link)
  ("C-c l f" . link-hint-open-link-at-point))

;;;; Password generator:

(use-package password-generator
  :defer t)

;;;; PCRE to Emacs Lisp regexp.:

;; Leader is `C-c /'
(use-package pcre2el
  :defer t
  :config
  (require 'pcre2el))

;;;; String inflection:

(use-package string-inflection
  :defer t
  :commands (string-inflection-all-cycle)
  :config
  (require 'string-inflection)
  (global-set-key (kbd "C-c s i") #'string-inflection-all-cycle))

;;;; Editing strings at current point:

(use-package string-edit-at-point
  :defer t
  :commands (string-edit-at-point)
  :config
  (global-set-key (kbd "C-c s e") #'string-edit-at-point))

;;;; Multiline:

(use-package multi-line
  :defer t
  :commands (multi-line)
  :config
  (require 'multi-line)
  (global-set-key (kbd "C-c d") #'multi-line))

;;;; UUID generator:

(use-package uuidgen
  :ensure t
  :defer nil
  :autoload (uuidgen-1 uuidgen-4)
  :config (require 'uuidgen))

;;;; Highlight "TODO":

(use-package hl-todo
  :defer t
  :commands (global-hl-todo-mode
             hl-todo-mode)
  :config (global-hl-todo-mode 1))

;;;; Fill column indicator:

;; Display an indicator on the current fill column.
(use-package display-fill-column-indicator
  :defer t
  :commands (global-display-fill-column-indicator-mode
             display-fill-column-indicator-mode)
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (text-mode . display-fill-column-indicator-mode))
  :config
  (zmacs-diminish global-display-fill-column-indicator-mode " ⮠" "dF")
  (zmacs-diminish display-fill-column-indicator-mode        " ⮠" "dF")
  (add-to-list 'minor-mode-alist '(display-fill-column-indicator-mode)))

;;;; Popup:

(use-package popup
  :defer t)

;;;; Popwin:

;; TODO: I'm sure something more intelligent can be done here!
(use-package popwin
  :defer t
  :config
  (popwin-mode 1)
  (setq popwin:special-display-config nil)
  ;; buffers that we manage
  (push '("*quickrun*"
          :dedicated t
          :position bottom
          :stick t
          :noselect t
          :height 0.3)
        popwin:special-display-config)
  (push '("*Help*"
          :dedicated t
          :position bottom
          :stick t
          :noselect t
          :height 0.4)
        popwin:special-display-config)
  (push '("*Process List*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil
          :height 0.4)
        popwin:special-display-config)
  (push '(compilation-mode
          :dedicated nil
          :position bottom
          :stick t
          :noselect t
          :height 0.4)
        popwin:special-display-config)
  (push '(dap-server-log-mode
          :dedicated nil
          :position bottom
          :stick t
          :noselect t
          :height 0.4)
        popwin:special-display-config)
  (push '("*Shell Command Output*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil)
        popwin:special-display-config)
  (push '("*Async Shell Command*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil)
        popwin:special-display-config)
  (push '("*undo-tree*"
          :dedicated t
          :position right
          :stick t
          :noselect nil
          :width 60)
        popwin:special-display-config)
  (push '("*undo-tree Diff*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil
          :height 0.3)
        popwin:special-display-config)
  (push '("*ert*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil)
        popwin:special-display-config)
  (push '("*grep*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil)
        popwin:special-display-config)
  (push '("*nosetests*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil)
        popwin:special-display-config)
  (push '("^\*WoMan.+\*$"
          :regexp t
          :position bottom)
        popwin:special-display-config)
  (push '("*Template Message*"
          :dedicated t
          :position bottom
          :stick t
          :noselect nil)
        popwin:special-display-config)
  (push '("*disaster-assembly*"
          :dedicated t
          :position bottom
          :stack t
          :noselect nil)
        popwin:special-display-config)
  (push '("*Google Translate*"
          :dedicated t
          :position bottom
          :stick t
          :noselect t
          :height 0.4)
        popwin:special-display-config))

;;;; Posframe:

(use-package posframe
  :defer t
  :config
  (require 'posframe))

;;;; Fill column enforcement:

;; XXX
(use-package column-enforce-mode
 :defer t
 :commands (column-enforce-mode)
 :hook (prog-mode . column-enforce-mode))

;;;; Indentation guide:

;; This is traditional word-processing stuff, not source code indentation!
(use-package indent-guide
  :defer t
  :commands (indent-guide-mode
             indent-guide-delay)
  :custom (indent-guide-delay 0.3))

;;;; Default settings:

;; Defaults
(setq-default frame-resize-pixelwise t
              indent-tabs-mode       nil ; Tabs are evil!
              tab-width              8   ; Default tab width.
              fill-column            80  ; Fuck off, Python.

              ;; GNUTLS settings.
              gnutls-verify-error   t
              gnutls-min-prime-bits 3072

              ;; Don't expire passphrases.
              password-cache-expiry nil

              ;; Other things.
              mouse-yank-at-point                 t
              save-interprogram-paste-before-kill t
              apropos-do-all                      t
              require-final-newline               t

              ;; ediff settings.
              ediff-window-setup-function #'ediff-setup-windows-plain

              ;; Tramp settings
              tramp-default-method  "ssh"
              tramp-copy-size-limit nil

              ;; How to handle symlinks in VC modes.
              vc-follow-symlinks t

              ;; Annoyances.
              ring-bell-function 'ignore

              ;; How to handle hyperlinks.
              browse-url-browser-function #'eww-browse-url)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;;; Hooks:

;; Enable undo
(add-hook 'after-change-major-mode-hook 'buffer-enable-undo)

;; Set up whitespace cleanup.
(add-hook 'before-save-hook #'whitespace-cleanup)

;;;; Dired hacks:

;; XXX
(add-hook 'dired-load-hook (lambda ()
                             (load "dired-x")))

;;;; Mouse:

(or (display-graphic-p)
    (progn
      (xterm-mouse-mode 1)))

;;;; Scrolling:

(setq scroll-conservatively most-positive-fixnum)
(when (>= emacs-major-version 29)
  (pixel-scroll-precision-mode 1))

;;;; Nuke some crappy annoyances:

(global-unset-key (kbd "C-z"))          ; `suspend-frame'.
(global-unset-key (kbd "C-x C-z"))      ; Also `suspend-frame'.

;;;; Global modes:

(global-eldoc-mode 1)
;;(electric-pair-mode 1)
(column-number-mode 1)
(visual-line-mode 0)

(blink-cursor-mode -1)                  ; No cursor blinking.
(scroll-bar-mode 1)                     ; Want scroll bars.
;;(fringe-mode 1)                         ; Want fringes.
(menu-bar-mode -1)                      ; No menu bar.
(tool-bar-mode -1)                      ; No toolbar.
(electric-indent-mode)                  ; Electric indent mode.

(add-hook 'prog-mode-hook (lambda ()
                            (hl-line-mode 1)))

;;;; Enable/disable Emacs Lisp functions:

;; Enable some neat functions.
(put 'erase-buffer              'disabled nil)
(put 'downcase-region           'disabled nil)
(put 'upcase-region             'disabled nil)
(put 'narrow-to-region          'disabled nil)
(put 'dired-find-alternate-file 'disabled 'nil)

;; Never going to use these.
(put 'ns-print-buffer 'disabled t)
(put 'suspend-frame   'disabled t)

;;;; Crux:

(use-package crux
  :defer t
  :commands (crux-move-beginning-of-line
             crux-smart-open-line
             crux-smart-kill-line
             crux-other-window-or-switch-buffer
             crux-duplicate-current-line-or-region
             crux-kill-whole-line
             crux-open-with
             crux-smart-open-line-above
             crux-visit-term-buffer
             crux-duplicate-and-comment-current-line-or-region)
  :config
  (global-set-key (kbd "C-a")   #'crux-move-beginning-of-line)
  (global-set-key (kbd "C-x ;") #'comment-line)
  (global-set-key (kbd "C-o")   #'crux-smart-open-line)
  (global-set-key (kbd "C-k")   #'crux-smart-kill-line)

  (global-set-key (kbd "C-x C-o") #'crux-other-window-or-switch-buffer)
  (global-set-key (kbd "C-c C-o") #'crux-other-window-or-switch-buffer)
  (global-set-key (kbd "C-c C-l") #'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c C--") #'crux-kill-whole-line)

  (global-set-key (kbd "C-c o")   #'crux-open-with)
  (global-set-key (kbd "C-S-RET") #'crux-smart-open-line-above)
  (global-set-key (kbd "S-RET")   #'crux-smart-open-line)
  (global-set-key (kbd "C-c t")   #'crux-visit-term-buffer)

  (global-set-key (kbd "C-c ;") #'crux-duplicate-and-comment-current-line-or-region))

;;;; IAlign:

(use-package ialign
  :ensure t
  :commands (ialign)
  :config
  (global-set-key (kbd "C-x l") #'ialign))

;;;; Siege mode:

(use-package siege-mode
  :ensure t
  :vc (:fetcher github
       :repo tslilc/siege-mode))

;;;; NeoTree

(use-package neotree
  :ensure t
  :custom
  (neo-theme (if (display-graphic-p)
                 'icons
               'arrow)))

;;;; Diminishes:

(zmacs-diminish eldoc-mode        " ⓔ" " e")
(zmacs-diminish global-eldoc-mode " ⓔ" " e")

;;;; Provide package:

(provide 'zmacs-base)

;;; zmacs-base.el ends here.
