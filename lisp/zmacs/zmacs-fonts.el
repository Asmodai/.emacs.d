;;; zmacs-fonts.el --- Font-related stuff  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 15:11:00
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

;;;; Font checking:

(defun zmacs-font-available-p (font-name)
  "Check if FONT-NAME is available."
  (not (null (member font-name (font-family-list)))))

;;;; Symbols and Emoji:

(use-package fontset
  :ensure nil
  :custom
  (use-default-font-for-symbols t)
  :config
  (cond ((zmacs-font-available-p "Symbola")
         (set-fontset-font t 'symbol "Symbola" nil))
        ((zmacs-font-available-p "Apple Symbols")
         (set-fontset-font t 'symbol "Apple Symbols" nil))
        ((zmacs-font-available-p "Symbol")
         (set-fontset-font t 'symbol "Symbol" nil))
        ((zmacs-font-available-p "Seqoe UI Symbol")
         (set-fontset-font t 'symbol "Segoe UI Symbol" nil)))

  (when (and (>= emacs-major-version 28)
             (zmacs-font-available-p "Apple Color Emoji"))
    (set-fontset-font t 'emoji
                      '("Apple Color Emoji" . "iso10646-1")
                      nil 'prepend))

  (defface fallback
    ;; All the below are full Unicode.
    `((t :family ,(cond ((zmacs-font-available-p "Source Code Pro for Powerline")
                         "Source Code Pro for Powerline")
                        ((zmacs-font-available-p "Sauce Code Powerline")
                         "Sauce Code Powerline")
                        ((zmacs-font-available-p "Source Code Pro")
                         "Source Code Pro")
                        ((zmacs-font-available-p "Fira Code")
                         "Fira Code"))
         :inherit fringe))
    "Fallback face.")

  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback)))

(setq-default line-spacing 0.1)

;;;; Font lock:

(use-package font-lock
  :ensure nil
  :defer 1
  :custom
  (font-lock-maximum-decoration t)
  (font-lock-maximum-size nil))

(setopt text-scale-mode-step 1.08)
(bind-key "S-=" #'text-scale-increase)
(bind-key "S--" #'text-scale-decrease)
(bind-key "S-@" #'text-scale-adjust)

;;;; Icons:
;;;;; All The Icons:

(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)
  :init
  (add-hook 'after-setting-font-hook #'zmacs-font--check-icon)
  :custom
  (all-the-icons-scale-factor 1)
  :config
  (add-hook 'after-setting-font-hook #'zmacs-font--init-all-the-icons-fonts))

(defun zmacs-font--check-icon ()
  (cond ((and (zmacs-font-available-p "Weather Icons")
              (zmacs-font-available-p "github-octicons")
              (zmacs-font-available-p "FontAwesome")
              (zmacs-font-available-p "all-the-icons")
              (zmacs-font-available-p "file-icons")
              (zmacs-font-available-p "Material Icons"))
         (message "Icon fonts already installed!"))
        ((and (not (member unicode-fonts (font-family-list)))
              (not (windows-p)))
         (message "Installing necessary fonts...")
         (all-the-icons-install-fonts 'yes))
        (t
         (message "Please install fonts!"))))

(defun zmacs-font--init-all-the-icons-fonts ()
  (when (fboundp 'set-fontset-font)
    (dolist (font (list "Weather Icons"
                        "github-octicons"
                        "FontAwesome"
                        "all-the-icons"
                        "file-icons"
                        "Material Icons"))
      (set-fontset-font t 'unicode font nil 'prepend))))

;;;;; Nerd Icons:

(use-package nerd-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :defer t
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :if (display-graphic-p)
  :commands (all-the-icons-completion-marginalia-setup
             all-the-icons-completion-mode)
  :hook (emacs-startup . all-the-icons-completion-mode)
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;;;; Provide package:

(provide 'zmacs-fonts)

;;; zmacs-fonts.el ends here.
