;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-lua-mode.el --- Site Lua hooks.
;;;
;;; Time-stamp: <Sunday Apr 28, 2013 19:05:17 asmodai>
;;; Revision:   1
;;;
;;; Copyright (c) 2013 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    28 Apr 2013 18:58:25
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

(eval-when-compile
  (require 'cl))

;;;
;;; Load Lua mode.
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;;
;;; Add extensions.
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;;; site-lua-mode.el ends here
