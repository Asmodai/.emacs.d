;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-perl-mode.el --- Perl mode hacks.
;;;
;;; Time-stamp: <Tuesday Jan 24, 2012 15:47:25 asmodai>
;;; Revision:   8
;;;
;;; Copyright (c) 2005-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Mon Jun 06 01:23:29 2005
;;; Keywords:   
;;; URL:        not distributed yet
;;;
;;; {{{ License:
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
;;; }}}
;;; {{{ Commentary:
;;;
;;; }}}

(when emacs>=20-p
  ;;
  ;; Require `cperl-mode'.
  (require 'cperl-mode)

  ;;
  ;; Alias `perl-mode'
  (defalias 'perl-mode 'cperl-mode)
  
  ;;
  ;; Just to force the issue
  (mapc (lambda (pair)
          (if (eq (cdr pair) 'perl-mode)
              (setcdr pair 'cperl-mode)))
        (append auto-mode-alist interpreter-mode-alist))
  
  ;;
  ;; Kill whitespace indications
  (setq cperl-invalid-face (if (and emacs-p
                                    (emacs-version>= 21 2 2))
                               nil
                               (quote off)))
  
  ;;
  ;; Set some options here.
  (setq cperl-font-lock t
        cperl-electric-parens t
        cperl-electric-linefeed t
        cperl-electric-keywords t
        cperl-clobber-lisp-bindings nil
        cperl-lazy-help-time 2
        cperl-auto-newline nil))

;;; site-perl-mode.el ends here

