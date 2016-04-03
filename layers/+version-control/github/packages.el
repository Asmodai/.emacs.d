;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- GitHub packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    05 Dec 2015 15:59:33
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;;{{{ License:
;;;
;;; This program is free software: you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation,
;;; either version 3 of the License, or (at your option) any
;;; later version.
;;;
;;; This program is distributed in the hope that it will be
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

(setq github-packages '(gist
                        git-link
                        github-browse-file
                        magit-gh-pulls))

(defun github:init-gist ()
  (use-package gist
    :defer t))

(defun gisthub:init-github-browse-file ()
  (use-package github-browse-file
    :defer t))

(defun github:init-git-link ()
  (use-package gitlink
    :defer t
    :init
    (progn
      (defun bootstrap:git-link-copy-url-only ()
        (interactive)
        (let (git-link-open-in-browser)
          (call-interactively 'git-link)))

      (defun bootstrap:git-link-commit-copy-url-only ()
        (interactive)
        (let (git-link-open-in-browser)
          (call-interactively 'git-link-commit)))

      (setq git-link-open-in-browser t))))

(defun github:init-magit-gh-pulls ()
  (bootstrap:use-package-add-hook magit
    :pre-config
    (progn
      (use-package magit-gh-pulls
        :init
        (progn
          (defun bootstrap:load-gh-pulls-mode ()
            (interactive)
            (magit-gh-pulls-mode)
            (magit-gh-pulls-reload))

          (defun bootstrap:fetch-gh-pulls-mode ()
            (interactive)
            (magit-gh-pulls-mode)
            (magit-gh-pulls-fetch-commits)))
        :config
        (bootstrap:diminish magit-gh-pulls-mode "GitHub-PR")))))

;;; packages.el ends here.
