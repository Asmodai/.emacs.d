;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; site-perl-mode.el --- Perl mode hacks.
;;;
;;; Time-stamp: <Monday Jul 21, 2014 11:22:35 asmodai>
;;; Revision:   22
;;;
;;; Copyright (c) 2005-2012 Paul Ward <asmodai@gmail.com>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    Mon Jun 06 01:23:29 2005
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

(when (emacs>=20-p)
  ;;
  ;; Require some modules
  (require 'cperl-mode)
  (require 'perl-reformat)
  (require 'ffap)
  (require 'etags)

  (defun ffap-perl-module (file)
    (let ((real-file (shell-command-to-string (concat "perldoc -ml " file))))
      (unless (string-match "No module found for " real-file)
        (substring real-file 0 -1))))
  
  (eval-after-load 'ffap
    '(add-to-list 'ffap-alist '(cperl-mode . ffap-perl-module)))
  
  (defadvice ffap-string-at-point (before ffap-perl-module-fix activate)
    (when (and (ad-get-arg 0)
               (eq major-mode 'cperl-mode))
      (ad-set-arg 0 major-mode)))
  
  ;;
  ;; FFAP
  (setq ffap-perl-inc-dirs
        (apply 'append
               (mapcar (function ffap-all-subdirs)
                       (split-string
                        (shell-command-to-string
                         "perl -e 'pop @INC; print join(q/ /,@INC);'")))))
  
  (defun my-cperl-ffap-locate (name)
    (let* ((r (replace-regexp-in-string ":"
                                        "/"
                                        (file-name-sans-extension name)))
           (e (replace-regexp-in-string "//" "/" r))
           (x (ffap-locate-file e '(".pm" ".pl" ".xs") ffap-perl-inc-dirs)))
      x))
  
  ;;
  ;; Alias `perl-mode'
  (defalias 'perl-mode 'cperl-mode)
  
  ;;
  ;; ElDoc hack
  (defun my-cperl-eldoc-doc-function ()
    "Return a meaningful docstring for `eldoc-mode'."
    (car (let ((cperl-message-on-help-error nil))
           (cperl-get-help))))
  
  ;;
  ;; Add the ElDoc hack.
  (add-hook 'cperl-mode-hook
            (lambda ()
              (eldoc-mode t)
              (local-set-key (kbd "C-h f") 'cperl-perldoc)
              (make-local-variable 'Man-switches)
              (setq Man-switches nil)
              (set (make-local-variable 'eldoc-documentation-function)
                   'my-cperl-eldoc-doc-function)))
  
  (setq cperl-tags-file-name "perl-tags")
  
  (defun cperl-write-tags (&optional file erase recurse dir inbuffer noxs topdir)
    ;; If INBUFFER, do not select buffer, and do not save
    ;; If ERASE is `ignore', do not erase, and do not try to delete old info.
    (require 'etags)
    (if file nil
        (setq file (if dir default-directory (buffer-file-name)))
        (if (and (not dir) (buffer-modified-p)) (error "Save buffer first!")))
    (or topdir
        (setq topdir default-directory))
    (let ((tags-file-name cperl-tags-file-name)
          (case-fold-search (and (featurep 'xemacs) (eq system-type 'emx)))
          xs rel tm)
      (save-excursion
        (cond (inbuffer nil)            ; Already there
              ((file-exists-p tags-file-name)
               (if (featurep 'xemacs)
                   (visit-tags-table-buffer)
                   (visit-tags-table-buffer tags-file-name)))
              (t (set-buffer (find-file-noselect tags-file-name))))
        (cond
          (dir
           (cond ((eq erase 'ignore))
                 (erase
                  (erase-buffer)
                  (setq erase 'ignore)))
           (let ((files
                  (condition-case err
                       (directory-files file t
                                        (if recurse nil cperl-scan-files-regexp)
                                        t)
                     (error
                      (if cperl-unreadable-ok nil
                          (if (y-or-n-p
                               (format "Directory %s unreadable.  Continue? " file))
                              (setq cperl-unreadable-ok t
                                    tm nil)     ; Return empty list
                              (error "Aborting: unreadable directory %s" file)))))))
             (mapc (function
                    (lambda (file)
                     (cond
                       ((string-match cperl-noscan-files-regexp file)
                        nil)
                       ((not (file-directory-p file))
                        (if (string-match cperl-scan-files-regexp file)
                            (cperl-write-tags file erase recurse nil t noxs topdir)))
                       ((not recurse) nil)
                       (t (cperl-write-tags file erase recurse t t noxs topdir)))))
                   files)))
          (t
           (setq xs (string-match "\\.xs$" file))
           (if (not (and xs noxs))
               (progn
                 (cond ((eq erase 'ignore) (goto-char (point-max)))
                       (erase (erase-buffer))
                       (t
                        (goto-char 1)
                        (setq rel file)
                        ;; On case-preserving filesystems (EMX on OS/2) case might be encoded in properties
                        (set-text-properties 0 (length rel) nil rel)
                        (and (equal topdir (substring rel 0 (length topdir)))
                             (setq rel (substring file (length topdir))))
                        (if (search-forward (concat "\f\n" rel ",") nil t)
                            (progn
                              (search-backward "\f\n")
                              (delete-region (point)
                                             (save-excursion
                                               (forward-char 1)
                                               (if (search-forward "\f\n"
                                                                   nil 'toend)
                                                   (- (point) 2)
                                                   (point-max)))))
                            (goto-char (point-max)))))
                 (insert (cperl-find-tags file xs topdir))))))
        (if inbuffer nil                        ; Delegate to the caller
            (save-buffer 0)                     ; No backup
            (if (fboundp 'initialize-new-tags-table) ; Do we need something special in XEmacs?
                (initialize-new-tags-table))))))
  
  ;;
  ;; Just to force the issue
  (mapc (lambda (pair)
          (if (eq (cdr pair) 'perl-mode)
              (setcdr pair 'cperl-mode)))
        (append auto-mode-alist interpreter-mode-alist))
  
  ;;
  ;; Set up perl reformatting
  (global-set-key (kbd "C-S-u") 'perl-reformat-mark-enclosing-block)
  (global-set-key (kbd "C-S-a") 'perl-reformat-align-enclosing-block)
  (global-set-key (kbd "C-S-m") 'perl-reformat-paramlist-toggle-multi-single)
  
  ;;
  ;; Perl indentation
  (setq cperl-indent-level 2
        cperl-close-paren-offset -2
        cperl-continued-statement-offset 2
        cperl-indent-parens-as-block t
        cperl-tab-always-indent t)
  
  ;;
  ;; Set some options here.
  (setq cperl-hairy t                   ; Hairy mode.
        cperl-electric-parens nil       ; No electric parentheses.
        cperl-clobber-lisp-bindings nil ; Don't clobber Lisp bindings.
        cperl-lazy-help-time 2          ; Lazy help time.
        cperl-auto-newline nil))        ; No auto newlines.

;;; site-perl-mode.el ends here
