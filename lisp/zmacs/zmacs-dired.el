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

;;;; Dired:

(use-package dired
  :ensure nil
  :demand t
  :commands (dired
             dired-jump
             dired-jump-other-window)
  :bind (:map dired-mode-map
         ("1" . dired-find-alternate-file))
  :config
  (progn

    (when (zlisp/macos-p)
      (setq dired-use-ls-dired nil)
      (when (executable-find "gls")
        (setq insert-directory-program "gls")))

    (when (or (and (zlisp/macos-p)
                   (executable-find "gls"))
              (and (not (zlisp/macos-p))
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

    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

;;;; Narrow:

(use-package dired-narrow
  :demand t
  :after dired
  :bind* (:map dired-mode-map
          ("/" . dired-narrow)))

;;;; Quick sort:

(use-package dired-quick-sort
  :demand t
  :after dired
  :bind* (:map dired-mode-map
          ("s" . hydra-dired-quick-sort/body)))

;;;; Dired font lock:

(use-package diredfl
  :demand t
  :after dired
  :hook (diredom.de . diredfl-global-mode))

;;;; Peep:

(use-package peep-dired
  :after dired
  :defer nil
  :demand t
  :commands (peep-dired)
  :bind* (:map dired-mode-map
          ("P" . peep-dired)
          :map peep-dired-mode-map
          ("j"   . peep-dired-next-file)
          ("k"   . peep-dired-prev-file))
  :config
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "gif"))
  (setq peep-dired-cleanup-eagerly nil)
  (setq peep-dired-enable-on-directories t)
  (setq peep-dired-cleanup-on-disable t))

;;;; Ranger:

(use-package dired-ranger
  :after dired
  :demand t
  :bind (:map dired-mode-map
         ("s-c" . dired-ranger-copy)
         ("s-m" . dired-ranger-move)
         ("s-v" . dired-ranger-paste)))

;;;; ZLisp dired:

(use-package zlisp-dired
  :ensure nil
  :after dired
  :demand t
  :bind (:map dired-mode-map
         ("2" . zlisp/dired-updirectory)
         :map peep-dired-mode-map
          ("RET" . zlisp/peep-dired-open)
          ("TAB" . zlisp/other-window))
  :config
  (require 'zlisp-window)
  (require 'zlisp-dired)
  (add-hook 'dired-mode-hook 'zlisp/dired-wrap))

;;;; Provide package:

(provide 'zmacs-dired)

;;; zmacs-dired.el ends here.
