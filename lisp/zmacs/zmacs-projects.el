;;; zmacs-projects.el --- Project packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:16:18
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

(eval-when-compile
  (require 'cl-lib))

;;;===================================================================
;;;{{{ Project:

(use-package project
  :ensure nil
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :bind (:map project-prefix-map
              ("P" . project-switch-project)
              ("t" . zmacs-goto-projects)
              ("R" . project-remember-projects-under))
  :custom
  (project-list-file (concat *zmacs-cache-directory* "/projects/"))
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-magit-dir "Magit status")))
  (project-vs-extra-root-markers '(".dir-locals.el"
                                   ".projetc.el"
                                   "package.json"
                                   "autogen.sh"))
  :config
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep))

  (project-forget-zombie-projects))

(defun zmacs--project-name ()
  "Return the name of the project without the path"
  (file-name-nondirectory (directory-file-name (if (vc-root-dir)
                                                   (vc-root-dir)
                                                 "-"))))

(defun project-magit-dir ()
  "Run magit in the current project's root."
  (interactive)
  (magit-status))

(define-key (current-global-map) (kbd "C-x p G") #'project-magit-dir)

(with-eval-after-load 'project
  (defun project-switch-project-open-dir (dir)
    "Switch to another project."
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively #'project-find-file))))

(use-package bookmark
  :ensure nil
  :defer 2
  :config
  (setq bookmark-default-file (concat *zmacs-cache-directory* "bookmarks")))

(defun zmacs-git-new-project ()
  "Initialise a new git repo."
  (interactive)
  (let ((project-dir (expand-file-name
                      (read-directory-name "New project root: "))))
    (magit-init project-dir)
    (setq default-directory project-dir)
    (let ((pr (project--find-in-directory default-directory)))
      (project-remember-project pr))))

;;;}}}
;;;===================================================================

(provide 'zmacs-projects)

;;; zmacs-projects.el ends here.
