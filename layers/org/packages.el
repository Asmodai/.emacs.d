;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- org-mode stuff.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 22:15:24
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

(setq org-packages
      '(company
        company-emoji
        emoji-cheat-sheet-plus
        gnuplot
        (org :location built-in)
        (org-plus-contrib :step pre)
        org-bullets
        (org-mime :location built-in)
        org-present
        org-repo-todo
        toc-org
        (ox-gfm :location local)))

(when (bootstrap-layer:layer-used-p 'auto-completion)
  (defun org:post-init-company ()
    (bootstrap:add-company-hook org-mode)
    (push 'company-capf company-backends-org-mode))

  (defun org:post-init-company-emoji ()
    (push 'company-emoji company-backends-org-mode)))

(defun org:post-init-emoji-cheat-sheet-plus ()
  (add-hook 'org-mode-hook 'bootstrap:display-emoji-cheat-sheet-hook))

(defun org:init-org-bullets ()
  (use-package org-bullets
    :defer t
    :init (add-hook 'org-mode-hook 'org-bullets-mode)))

(defun org:init-org-mime ()
  (use-package org-mime
    :defer t
    :commands (org-mime-htmlize org-mime-org-buffer-htmlize)))

(defun org:init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (defun bootstrap::org-present-start ()
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only))

      (defun bootstrap::org-present-ent ()
        (org-present-small)
        (org-remove-inline-images)
        (org-present-show-cursor)
        (org-present-read-write))

      (add-hook 'org-present-mode-hook 'bootstrap::org-present-start)
      (add-hook 'org-present-mode-quit-hook 'bootstrap::org-present-end))))

(defun org:init-org-repo-todo ()
  (use-package org-repo-todo
    :defer t))

(defun org:init-toc-org ()
  (use-package toc-org
    :defer t
    :init
    (progn
      (setq toc-org-max-depth 10)
      (add-hook 'org-mode-hook 'toc-org-enable))))

(defun org:init-ox-gfm ()
  (use-package ox-gfm
    :if *org-enable-github-support*
    :defer t
    :init
    (progn
      (eval-after-load 'org '(require 'ox-gfm))
      (autoload 'org-gfm-export-as-markdown "ox-gfm" "\
 Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org GFM Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

      (autoload 'org-gfm-convert-region-to-md "ox-gfm" "\
Assume the current region has org-mode syntax, and convert it
to Github Flavored Markdown.  This can be used in any buffer.
For example, you can write an itemized list in org-mode syntax in
a Markdown buffer and use this command to convert it.

\(fn)" t nil))))

;;; packages.el ends here
