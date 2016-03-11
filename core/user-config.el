;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; user-config.el --- User configuration.
;;;
;;; Time-stamp: <>
;;; Revision:   0
;;;
;;; Copyright (c) 2015 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Dec 2015 17:55:28
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

;; Set various user details.
(setq user-url-address          (if (running-on-mbr15_pward-p)
                                    "http://www.alertlogic.com/"
                                  "http://lisphacker.uk/")
      user-full-name            "Paul Ward"
      user-mail-address         (if (running-on-mbr15_pward-p)
                                    "pward@alertlogic.com"
                                  "asmodai@gmail.com")
      url-personal-mail-address "asmodai@gmail.com")

(provide 'user-config)

;;; user-config.el ends here
