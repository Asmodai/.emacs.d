;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- cscope packages.
;;;
;;; Copyright (c) 2015-2016 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 23:38:52
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

(setq cscope-packages '(helm-cscope
                        xcscope))

(setq cscope-excluded-packages '())

(defun cscope:init-xcscope ()
  (use-package xcscope
    :commands (cscope-index-files cscope:run-pyscope)
    :init
    (progn
      (setq cscope-option-do-not-update-database t
            cscope-display-cscope-buffer nil)

      (defun cscope::safe-project-root ()
        (and (fboundp 'projectile-project-root)
             (projectile-project-p)
             (projectile-project-root)))

      (defun cscope:run-pycscope (directory)
        (interactive
         (list (file-name-as-directory
                (read-directory-name
                 "Run PyCscope in directory: "
                 (cscope::safe-project-root)))))
        (let ((default-directory directory))
          (shell-command
           (format "pyscope -R -f '%s'"
                   (expand-file-name "cscope.out" directory))))))))

(defun cscope:init-helm-cscope ()
  (use-package helm-cscope
    :defer t))

;;; packages.el ends here
