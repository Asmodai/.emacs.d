;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; packages.el --- Perl packages.
;;;
;;; Copyright (c) 2016 Paul Ward <asmodai@gmail.com>
;;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    11 Mar 2016 23:53:41
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

(setq perl-packages '(indent-guide
                      (cperl-mode   :location built-in)
                      (inf-perl     :location local)
                      (jpl-reformat :location local)))

(defun perl:init-inf-perl ()
  (use-package inf-perl
    :defer t
    :init (require 'inf-perl)))

(defun perl:init-jpl-reformat ()
  (use-package jpl-reformat
    :defer t
    :init
    (progn

      (defun ensure-mark ()
        "A deprecated function that is needed by jpl-reformat"
        (and (not mark-active)
             (set-mark-command nil)))

      (require 'jpl-reformat)

      (global-set-key (kbd "C-S-u") 'jpl-reformat-mark-enclosing-block)
      (global-set-key (kbd "C-S-a") 'jpl-reformat-align-enclosing-block)

      (global-set-key (kbd "C-S-m")
                      'jpl-reformat-parameter-list-toggle-multiple-single))))

(defun perl:init-cperl-mode ()
  (use-package cperl-mode
    :defer t
    :init
    (progn
      (require 'cperl-mode)
      (require 'ffap)
      (require 'etags)
      
      (defalias 'perl-mode 'cperl-mode)

      (defun ffap-perl-module (file)
        "`find-file-at-point' hack for Perl modules."
        (let ((real-file (shell-command-to-string
                          (concat "perldoc -ml " file))))
          (unless (string-match "No module found for " real-file)
            (substring real-file 0 -1))))

      (eval-after-load 'ffap
        '(add-to-list 'ffap-alist '(perl-mode . ffap-perl-module)))

      (defadvice ffap-string-at-point (before ffap-perl-module-fix activate)
        (when (and (ad-get-arg 0)
                   (eq major-mode 'perl-mode))
          (ad-set-arg 0 major-mode)))

      (setq ffap-perl-inc-dirs
            (apply 'append
                   (mapcar 'ffap-all-subdirs
                           (split-string
                            (shell-command-to-string
                             "perl -e 'pop @INC; print join(q/ /, @INC);'")))))

      (defun my-perl-ffap-locate (name)
        (let* ((r (replace-regexp-in-string ":"
                                            "/"
                                            (file-name-sans-extension name)))
               (e (replace-regexp-in-string "//" "/" r))
               (x (ffap-locate-file e '(".pm" ".pl" ".xs") ffap-perl-inc-dirs)))
          x))

      (defun my-perl-eldoc-doc-function ()
        "Return a meaningful docstring for `eldoc-mode'."
        (car (let ((cperl-message-on-help-error nil))
               (cperl-get-help))))

      (add-hook 'cperl-mode-hook
                (lambda ()
                  (eldoc-mode t)
                  (local-set-key (kbd "C-h f") 'cperl-perldoc)
                  (make-local-variable 'Man-switches)
                  (setq Man-switches nil)
                  (set (make-local-variable 'eldoc-documentation-function)
                       'my-perl-eldoc-doc-function)))

      (setq cperl-tags-file-name "perl-tags")

      (defun cperl-write-tags (&optional file erase recurse dir inbuffer noxs
                                         topdir)
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
                                          (if recurse
                                              nil
                                            cperl-scan-files-regexp)
                                          t)
                       (error
                        (if cperl-unreadable-ok
                            nil
                          (if (y-or-n-p
                               (format "Directory %s unreadable.  Continue? "
                                       file))
                              (setq cperl-unreadable-ok t
                                    tm nil)     ; Return empty list
                            (error "Aborting: unreadable directory %s"
                                   file)))))))
                (mapc (function
                       (lambda (file)
                         (cond
                          ((string-match cperl-noscan-files-regexp file)
                           nil)
                          ((not (file-directory-p file))
                           (if (string-match cperl-scan-files-regexp file)
                               (cperl-write-tags file erase recurse
                                                 nil t noxs topdir)))
                          ((not recurse) nil)
                          (t (cperl-write-tags file erase recurse
                                               t t noxs topdir)))))
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
                           ;; On case-preserving filesystems (EMX on OS/2) case
                           ;; might be encoded in properties
                           (set-text-properties 0 (length rel) nil rel)
                           (and (equal topdir (substring rel 0 (length topdir)))
                                (setq rel (substring file (length topdir))))
                           (if (search-forward (concat "\f\n" rel ",") nil t)
                               (progn
                                 (search-backward "\f\n")
                                 (delete-region (point)
                                                (save-excursion
                                                  (forward-char 1)
                                                  (if (search-forward
                                                       "\f\n" nil 'toend)
                                                      (- (point) 2)
                                                    (point-max)))))
                             (goto-char (point-max)))))
                    (insert (cperl-find-tags file xs topdir))))))
            (if inbuffer nil                        ; Delegate to the caller
              (save-buffer 0)                       ; No backup
              (if (fboundp 'initialize-new-tags-table)
                  (initialize-new-tags-table))))))

      (defun perltidy-region ()
        "Run `perltidy' on the current region."
        (interactive)
        (save-excursion
          (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

      (defun perltidy-defun ()
        "Run `perltidy' on the current definition."
        (interactive)
        (save-excursion
          (mark-defun)
          (perldity-region)))

      (mapc (lambda (pair)
              (if (eq (cdr pair) 'perl-mode)
                  (setcdr pair 'cperl-mode)))
            (append auto-mode-alist interpreter-mode-alist))

      (setq cperl-indent-level                          4
            cperl-close-paren-offset                   -4
            cperl-continued-statement-offset            4
            cpelr-indent-parens-as-block                t
            cperl-tab-always-indent                     t
            cperl-hairy                                 t
            cperl-highlight-variables-indiscriminately  t
            cperl-electric-parens                       nil
            cperl-clobber-lisp-bindings                 nil
            cperl-lazy-help-time                        2
            cperl-auto-newline                          nil))))

(defun perl:post-init-indent-guide ()
  (bootstrap:add-to-hooks 'indent-guide-mode '(cperl-mode-hook)))

(defun perl:post-init-redspace-mode ()
  (bootstrap:add-to-hooks 'redspace-mode ('cperl-mode-hook)))

;;; packages.el ends here.
