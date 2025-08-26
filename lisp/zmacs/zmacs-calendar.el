;;; zmacs-calendar.el --- Calendar.  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    26 Aug 2025 14:47:06
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
(require 'icalendar)

;;;; Calendar framework:

(use-package calfw
  :ensure t
  :defer nil)

(use-package calfw-cal
  :after calfw
  :ensure t
  :defer nil)

(use-package calfw-ical
  :after calfw
  :ensure t
  :defer nil)

(use-package calfw-org
  :after calfw
  :ensure t
  :defer nil)

(with-eval-after-load 'calfw-org
  (require 'calfw)
  (require 'calfw-cal)

  (defun zmacs-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:org-create-source "Green")
      (cfw:cal-create-source "Orange")
      (cfw:ical-create-source "gcal" user-gcal-secret "IndianRed")))))

;;;; Provide package:

(provide 'zmacs-calendar)

;;; zmacs-calendar.el ends here.
