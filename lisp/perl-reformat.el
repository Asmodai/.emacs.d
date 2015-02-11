;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; perl-reformat.el --- Perl reformatting.
;;;
;;; Time-stamp: <Monday Jul 21, 2014 11:31:48 asmodai>
;;; Revision:   8
;;;
;;; Copyright (c) 2014 Paul Ward <asmodai@gmail.com>
;;; Copyright (C) 2008, Johan Lindstrom
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    21 Jul 2014 07:56:52
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
;;; Reformat parameter lists easily from single line to multiple
;;; lines and back again. E.g. between this:
;;;
;;;     API::Link->new({rel => 'meta:version', href => $uri->as_string}),
;;;
;;; and this:
;;;
;;;     API::Link->new({
;;;         rel => 'meta:version',
;;;         href => $uri->as_string
;;;     }),
;;;
;;; The parameters are specified by the enclosing element, with either
;;; () or {} braces.
;;;
;;; The multiline format can also be aligned properly within the
;;; enclosing braced element, to end up like this:
;;;
;;;     API::Link->new({
;;;         rel  => 'meta:version',
;;;         href => $uri->as_string,
;;;         type => 'application/xml',
;;;     }),
;;;
;;;
;;;
;;; The user interface for this package consists of the following commands:
;;;
;;;     perl-reformat-mark-enclosing-block
;;;     perl-reformat-align-enclosing-block
;;;     perl-reformat-parameter-list-to-single-line
;;;     perl-reformat-parameter-list-to-multi-line
;;;     perl-reformat-parameter-list-toggle-multiple-single
;;;
;;;
;;;
;;;
;;; and this:
;;;
;;;     API::Link->new({
;;;         rel => 'meta:version',
;;;         href => $uri->as_string
;;;     }),
;;;
;;; The parameters are specified by the enclosing element, with either
;;; () or {} braces.
;;;
;;; The multiline format can also be aligned properly within the
;;; enclosing braced element, to end up like this:
;;;
;;;     API::Link->new({
;;;         rel  => 'meta:version',
;;;         href => $uri->as_string,
;;;         type => 'application/xml',
;;;     }),
;;;
;;;
;;;
;;; The user interface for this package consists of the following commands:
;;;
;;;     perl-reformat-mark-enclosing-block
;;;     perl-reformat-align-enclosing-block
;;;     perl-reformat-parameter-list-to-single-line
;;;     perl-reformat-parameter-list-to-multi-line
;;;     perl-reformat-parameter-list-toggle-multiple-single
;;;
;;;
;;;
;;; Setup:
;;;
;;; (require 'perl-reformat)
;;; (global-set-key (kbd "C-S-u") 'perl-reformat-mark-enclosing-block)
;;; (global-set-key (kbd "\C-o m a") 'perl-reformat-align-enclosing-block)
;;; (global-set-key (kbd "\C-o m p") 'perl-reformat-paramlist-toggle-multi-single)
;;;
;;; or whatever keys you'r like to use. If you use PerlySense
;;;
;;;   http://search.cpan.org/dist/Devel-PerlySense/lib/Devel/PerlySense.pm
;;;
;;; then the above key bindings are suitable, otherwise you probably
;;; want to assign other C-c prefixed keys.
;;;
;;;}}}

(eval-when-compile
  (require 'cl)
  (require 'cperl-mode)
  (require 'thingatpt)
  (require 'newcomment))

(defun ensure-mark ()
  (and (not mark-active)
       (set-mark-command nil)))

(defun perl-reformat-paramlist-toggle-multi-single ()
  "Reformat a list of parameters enclosed in () or {} into a
nicely formatted single lines, or multiple lines depending on
what it currently is."
  (interactive)

  (if (save-excursion
        (perl-reformat-mark-enclosing-block)
        (= (perl-reformat-line-of-pos (point)) (perl-reformat-line-of-pos (mark))))
      (perl-reformat-parameter-list-to-multi-line nil)
      (perl-reformat-parameter-list-to-single-line)))

(defun perl-reformat-parameter-list-to-single-line ()
  "Reformat a list of parameters enclosed in () or {} into a
nicely formatted single line. Trailing comments are stacked on
top."
  (interactive)

  (perl-reformat-mark-enclosing-block)

  (save-excursion
    (goto-char (+ 1 (point)))

    ;; After opening brace. Bring up the first line
    (perl-reformat-collapse-all-whitespace)

    (while (search-forward-regexp "," (mark) t)
      (if (not (or (perl-reformat-in-comment-p) (in-string-p)))
          (progn
            (if (looking-back " " 2)
                (perl-reformat-collapse-all-whitespace))
            (perl-reformat-collapse-whitespace))))

    ;; Remove possibly trailing ,
    (goto-char (- (mark) 1))
    (if (looking-back ", " 3)
        (delete-backward-char 2))
    (perl-reformat-collapse-all-whitespace))

  (forward-char 1)
  (save-excursion
    (perl-reformat-mark-enclosing-block)
    (goto-char (+ 1 (point)))

    (while (search-forward-regexp "[ ]*=>[ ]*" (mark) t)
      (let ((match-beg (match-beginning 0))
            (match-end (match-end 0)))
        (if (not (or (perl-reformat-in-comment-p)
                     (in-string-p)))
            (progn
              (delete-region match-beg match-end)
              (goto-char match-beg)
              (insert " => ")))))))

(defun perl-reformat-parameter-list-to-multi-line (arg)
  "Reformat a list of parameters enclosed in () or {} into nicely
formatted multiple lines. Line breaks after each parameter."
  (interactive "P")

  (save-excursion
    (perl-reformat-mark-enclosing-block)
    (goto-char (+ 1 (point)))

    ;; After opening brace. Move down the first line
    (perl-reformat-collapse-whitespace)
    (newline-and-indent)

    ;; Note: mark will move as we insert new lines, indentation
    ;; etc. That's good.
    (while (search-forward-regexp "," (mark) t)
      (if (not (or (perl-reformat-in-comment-p)
                   (in-string-p)))
          (progn
            (if (looking-back "\ " 5)
                (progn   (backward-char 1)
                         (perl-reformat-collapse-all-whitespace)))

            (indent-according-to-mode)

            (if (save-excursion
                  (and (search-forward-regexp "[,#]" (point-at-eol) t)
                       (looking-back "#" 2)))
                (progn
                  (next-line)
                  (beginning-of-line))
                (progn
                  (perl-reformat-collapse-whitespace)
                  (newline-and-indent))))))

    ;; Last line
    (goto-char (- (mark) 1))
    (perl-reformat-collapse-whitespace)
    (newline-and-indent)

    ;; Possibly add trailing ,
    (previous-line-nomark)
    (cperl-to-comment-or-eol)
    (if (not (search-backward-regexp ",\\ *" (point-at-bol) t))
        (progn
          (if (search-backward-regexp "[^ ]" (point-at-bol) t)
              (progn
                (forward-char 1)
                (insert ","))))))

  ;; Possibly align stuff in the block
  (if arg
      (perl-reformat-align-enclosing-block)))

(defun perl-reformat-in-comment-p ()
  "Return a true value if point is in a comment, else nil"
  (save-excursion
    (comment-beginning)))

(defun perl-reformat-collapse-whitespace ()
   "Reduce all whitespace surrounding point to a single space."
   ;; @@ This seems to be quite buggy at the moment
   (interactive)
   (kill-region (progn (re-search-backward "[^ \t\r\n]")
                       (forward-char)
                       (point))
                (progn (re-search-forward "[^ \t\r\n]")
                       (backward-char)
                       (point)))
   (insert-char ?\  1))

(defun perl-reformat-collapse-all-whitespace ()
  "Like perl-reformat-collapse-whitespace, but remove all whitespace"
  (interactive)
  (perl-reformat-collapse-whitespace)
  (if (looking-back " ")
      (delete-char -1))
  (forward-char 1))

(defun perl-reformat-mark-enclosing-block ()
  "Select the enclosing block, () or {}"
  (interactive)

  (while (in-string-p)
    (backward-char 1))
  (backward-up-list -1) ;;Move to end of enclosing braces
  (push-mark)
  (ensure-mark)
  (forward-list -1)) ;; Move to beginning of enclosing braces

(defun perl-reformat-align-enclosing-block ()
  "Align the enclosing block"
  (interactive)
  (save-excursion
    (perl-reformat-mark-enclosing-block)
    (narrow-to-region
     (point)
     (mark))
    (forward-line)
    (align-current)
    (widen)))

(defun perl-reformat-line-of-pos (pos)
  "Return the vertical position of pos"
  (+ (count-lines 1 pos)
     (if (= (current-column) 0)
         1
         0)))

(provide 'perl-reformat)

;;; perl-reformat.el ends here
