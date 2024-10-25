;;; zmacs-dired.el --- Dired  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:12:43
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
(require 'zlisp-platform)

(use-package dired
  :ensure nil
  :commands (dired
             dired-jump
             dired-jump-other-window)
  :bind (:map dired-mode-map
         ("1" . dired-find-alternate-file)
         ("2" . zmacs-dired-updirectory))
  :config
  (defun zmacs-dired-updirectory ()
    (interactive)
    (find-alternate-file ".."))

  (when (zlisp-macos-p)
    (setq dired-use-ls-dired nil)
    (when (executable-find "gls")
      (setq insert-directory-program "gls")))

  (when (or (and (zlisp-macos-p)
                 (executable-find "gls"))
            (and (not (zlisp-macos-p))
                 (executable-find "ls")))
    (setq ls-lisp-use-insert-directory-program t)
    (setq dired-listing-switches "-laFh1v --group-directories-first"))

  (setq dired-ls-F-marks-symlinks t
        dired-clean-confirm-killing-deleted-buffers nil
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-deletion-confirmer 'y-or-n-p
        dired-dwim-target t
        wdired-allow-to-change-permissions t
        dired-guess-shell-alist-user '(("\.pdf$" . default)))

  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(use-package dired-narrow
  :bind* (:map dired-mode-map
               ("/" . dired-narrow)))

(use-package dired-quick-sort
  :bind* (:map dired-mode-map
               ("s" . hydra-dired-quick-sort/body)))

(use-package diredfl
  :hook (diredom.de . diredfl-global-mode))

(use-package peep-dired
  :after dired
  :commands (peep-dired)
  :bind* (:map dired-mode-map
          ("P" . peep-dired)
          :map peep-dired-mode-map
          ("j"   . peep-dired-next-file)
          ("k"   . peep-dired-prev-file)
          ("RET" . zmacs-peep-dired-open)
          ("TAB" . zmacs-other-window))
  :config
  (defun zmacs-peep-dired-open ()
    "Open files from peep-dired and clean up."
    (interactive)
    (peep-dired-kill-buffers-without-window)
    (dired-find-file)
    (delete-other-windows))

  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "gif"))
  (setq peep-dired-cleanup-eagerly nil)
  (setq peep-dired-enable-on-directories t)
  (setq peep-dired-cleanup-on-disable t))

(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
              ("s-c" . dired-ranger-copy)
              ("s-m" . dired-ranger-move)
              ("s-v" . dired-ranger-paste)))

;; Allow for cycling from bottom to top of dired buffer and vice versa
(add-hook 'dired-mode-hook
            (defun zmacs-dired-wrap ()
              "Cycle from bottom to top of buffer"
              (make-local-variable 'post-command-hook)
              (add-hook 'post-command-hook
                        (defun zmacs--dired-wrap-1 ()
                          ""
                          (if (= 1 (save-excursion
                                     (forward-line)))
                              (goto-line 3))
                          (if (= -1 (save-excursion
                                      (forward-line -1)))
                              (goto-line (count-lines
                                          (point-min)
                                          (point-max))))))))

(provide 'zmacs-dired)

;;; zmacs-dired.el ends here.
