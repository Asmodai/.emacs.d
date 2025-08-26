;;; zmacs-dashboard.el --- The dashboard  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 15:47:10
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
(require 'zlisp-versioning)

;;;; Dependencies:
;;;;; Dashboard package.

(use-package dashboard
  :ensure t
  :demand t
  :commands (dashboard-setup-startup-hook)
  :config
  (dashboard-setup-startup-hook))

;;;;; Icons (in graphics mode):

(when (display-graphic-p)
  (require 'all-the-icons)
  (require 'nerd-icons))

;;;; Random quotes:

(defvar *zmacs--random-quotes*
  '((:lines ("Programs must be written for people to read, and only incidentally"
             "for machines to execute.")
     :author "Abelson & Sussman")
    (:lines ("Lisp is the greatest single programming language ever designed.")
     :author "Alan Kay")
    (:lines ("Lisp is a programmable programming language.")
     :author "John Foderaro")
    (:lines ("Lisp isn't a language, it's a building material.")
     :author "Alan Kay")
    (:lines ("Lisp is a programmer amplifier.")
     :author "Martin Rodgers")
    (:lines ("I object to doing things that computers can do.")
     :author "Olin Shivers")
    (:lines ("Common Lisp is politics, not art.")
     :author "Scott Fahlman")
    (:lines ("Lisp doesn't look any deader than usual to me.")
     :author "David Thornley")
    (:lines ("The only way to learn a new programming language is by writing"
             "programs in it.")
     :author "Kernighan & Ritchie")
    (:lines ("I suppose I should learn Lisp, but it seems so foreign.")
     :author "Paul Graham")
    (:lines ("Syntactic sugar causes cancer of the semicolon.")
     :author "Alan Perlis")
    (:lines ("If you want to know why Lisp doesn't win around you, find a mirror.")
     :author "Erik Naggum")
    (:lines ("Lisp is still #1 for key algorithmic techniques such as recursion"
             "and condescension.")
     :author "Verity Stob")
    (:lines ("If you give someone Fortran, he has Fortran."
             "If you give someone Lisp, he has any language he pleases.")
     :author "Guy Steele")
    (:lines ("A Lisp programmer knows the value of everything, but the cost of nothing.")
     :author "Alan Perlis")
    (:lines ("Perl is an abomination as a language.  But let's not go there.")
     :author "L. Peter Deutsch")
    (:lines ("By policy, Lisp has never really catered to mere mortals..."
             "And, of course, mere mortals have never really forgiven Lisp for not"
             "catering to them.")
     :author "Larry Wall")
    (:lines ("Those who do not know Lisp are doomed to reinvent it.")
     :author "Erik Naggum")
    (:lines ("Some may say Ruby is a bad rip-off of Lisp or Smalltalk,"
             "and I admit that. But it is nicer to ordinary people.")
     :author "Matz")
    (:lines ("Press <Hyper>-<Super>-<Shift>-<Control>-<Meta>-<Symbol>-<Abort> to"
             "engage Super Turbo Go Go Lisp Party Mode!")
     :author "Me, that's who!"))
  "Some random quotes to display on the dashboard.")

(defun zmacs--random-quote ()
  "Pick a random quote from `*zmacs--random-quotes*'."
  (let ((idx (% (abs (random))
                (length *zmacs--random-quotes*))))
    (nth idx *zmacs--random-quotes*)))

(defsubst zmacs--emacs-version ()
  "Produce a string describing this Emacs."
  (format "GNU Emacs %s -- ZMACS %s"
          emacs-version
          (or (zlisp/git-get-current-tag)
              (zlisp/git-get-current-branch-rev)
              "<local>")))

;;;; Variables:

(defvar *zmacs--dashboard-buffer-name* "*dashboard*"
  "The name of our dashboard buffer.")

(defvar *zmacs-ascii-banner-directory*
  (expand-file-name (concat *zmacs-lisp-directory* "banners/"))
  "Location of ASCII banners.")

(defvar *zmacs--ascii-banner-files* nil
  "A list of available banner files.")

;;;; Banner files:

(defun zmacs--get-banners ()
  "Get banners from the banner directory, if set."
  (when (or (null *zmacs-ascii-banner-directory*)
            (string-empty-p *zmacs-ascii-banner-directory*))
    (cl-return-from zmacs--get-banners nil))
  (cl-loop for file in (directory-files *zmacs-ascii-banner-directory*)
           collect (car (split-string file "\\.")) into names
           finally (return
                    (sort (mapcar #'string-to-number
                                  (cl-remove-if #'string-empty-p names))
                          #'<))))

;;;; Modeline hacks:

(defun zmacs--dashboard-hide-modeline ()
  "Hides the modeline in the dashboard buffer."
  (let* ((buffer (get-buffer *zmacs--dashboard-buffer-name*)))
    (with-current-buffer buffer
      (setq-local hl-line-mode -1)           ; Disable the line highlight.
      (setq-local mode-line-format nil)      ; Zap the modeline format.
      (setq-local header-line-format nil)))) ; Zap the header line.

;; Add the modeline hack to the dashboard mode hook.
(add-hook 'dashboard-mode-hook #'zmacs--dashboard-hide-modeline)

;;;; Icon hacks:

(defmacro zmacs--with-icon (icon text info-text &rest fn)
  "Return a list with ICON, TEXT, and INFO-TEXT.

If non-NIL, FN will be included.
If not on a graphical display, ICON is excluded."
  `(list ,(if (display-graphic-p)
              `,icon
            nil)
         ,text
         ,info-text
         ,@fn))

;;;; Buffer switching:

(defun zmacs-dashboard ()
  "Load dashboard and switch to the buffer."
  (interactive)
  (let ((buffer *zmacs--dashboard-buffer-name*))
    (when (not (get-buffer buffer))
      (dashboard-insert-startupify-lists))
    (switch-to-buffer buffer))
  (delete-other-windows))

(defun zmacs-goto-dashboard ()
  "Go to the dashboard."
  (interactive)
  (switch-to-buffer *zmacs--dashboard-buffer-name*))

;;;; Monkey patches.

;; Create a custom face for the init info section.
(defface dashboard-init-info-face
  '((t (:inherit shadow)))
  "Face used for init info."
  :group 'dashboard)

;; Monkey Patch(tm) `dashboard-insert-init-info' to use its own face for the
;; inserted object.
(defun dashboard-insert-init-info ()
  "Insert init info."
  (let ((init-info (if (functionp dashboard-init-info)
                       (funcall dashboard-init-info)
                     dashboard-init-info)))
    (dashboard-insert-center
     (propertize init-info
                 'face 'dashboard-init-info-face))))

;; Remove the silly leading ">" in terminal mode.
(setf dashboard-footer-icon
      (if (dashboard-display-icons-p)
          (pcase dashboard-icon-type
            ('all-the-icons
             (all-the-icons-fileicon "emacs"
                                     :height 1.1
                                     :v-adjust -0.05
                                     :face 'dashboard-footer-icon-face))
            ('nerd-icons
             (nerd-icons-sucicon "nf-custom-emacs"
                                 :height 1.1
                                 :v-adjust -0.05
                                 :face 'dashboard-footer-icon-face)))
        (propertize "" 'face 'dashboard-footer-icon-face)))

(defun zmacs--dashboard-format-quote-block (quote)
  (when (not (listp quote))
    (cl-return-from zmacs--dashboard-format-quote quote))
  (let ((the-quote  (cadr (member :lines  quote)))
        (the-author (cadr (member :author quote))))
    (concat "“" (mapconcat #'identity the-quote "\n") "”"
            "\n    -- " the-author)))

(defun dashboard-insert-footer ()
  "Insert footer of dashboard."
  (when-let ((footer (dashboard-random-footer))
             (footer-icon (dashboard-footer-icon)))
    (dashboard-insert-center
     (if (string-empty-p footer-icon)
         footer-icon
       (concat footer-icon ""))
     (propertize (if (listp footer)
                     ;;(mapconcat 'identity footer "\n")
                     (zmacs--dashboard-format-quote-block footer)
                   footer)
                 'face 'dashboard-footer-face)
     "\n")))

;;;; Settings:
;;;;; General settings:

(setq dashboard-startup-banner    (zmacs--get-banners)
      dashboard-center-content    t
      dashboard-show-shortcuts    t
      dashboard-set-foooter       t
      dashboard-banners-directory *zmacs-ascii-banner-directory*
      dashboard-banner-logo-title (zmacs--emacs-version)
      dashboard-footer-messages   *zmacs--random-quotes*
      dashboard-show-shortcuts    nil
      dashboard-navigation-cycle  nil)

;;;;; Settings that require a graphical system:

(when (display-graphic-p)
  (setq dashboard-set-heading-icons nil
        dashboard-set-file-icons    nil
        dashboard-icon-type         'all-the-icons))

;;;;; What to show on the dashboard:

(setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-newline
                                  dashboard-insert-newline
                                  dashboard-insert-footer))

;;;;; Stuff we're not bothering with:

(setq dashbraod-item-shortcuts '()
      dashboard-items          '())

;;;; Navigation buttons:

(setq dashboard-navigator-buttons
      (list
       (list (zmacs--with-icon (all-the-icons-fileicon "elisp"
                                                       :height 1.0
                                                       :v-adjust 0.0)
                               "Emacs Lisp"
                               "Start IELM."
                               (lambda (&rest _)
                                 (ielm)))
             (zmacs--with-icon (all-the-icons-fileicon "lisp"
                                                       :height 1.0
                                                       :v-adjust 0.0)
                               "Common Lisp"
                               "Start SLIME for localhost:4006."
                               (lambda (&rest _)
                                 (cond ((fboundp 'slime-connect)
                                        (slime-connect "localhost" 4006))
                                       ((fboundp 'sly-connect)
                                        (sly-connect "localhost" 4006))
                                       (t
                                        (message "SLIME is not installed!")))))
                          (zmacs--with-icon (all-the-icons-faicon "book"
                                                     :height 1.0
                                                     :v-adjust 0.0)
                               "Notes"
                               "Open notes in Dired."
                               (lambda (&rest _)
                                 (zmacs-notebook))))
       (list (zmacs--with-icon (all-the-icons-faicon "calendar"
                                                     :height 1.0
                                                     :v-adjust 0.0)
                               "Calendar"
                               "View calendar"
                               (lambda (&rest _)
                                 (zmacs-open-calendar)))
             (zmacs--with-icon (all-the-icons-faicon "tasks"
                                                     :height 1.0
                                 :v-adjust 0.0)
                               "Agenda"
                               "View daily agenda"
                               (lambda (&rest _)
                                 (org-agenda-list))))
       (list (zmacs--with-icon (all-the-icons-faicon "terminal"
                                                     :height 1.0
                                                     :v-adjust 0.0)
                               "Emacs Shell"
                               "Start EShell"
                               (lambda (&rest _)
                                 (eshell)))
             (zmacs--with-icon (all-the-icons-faicon "folder"
                                                     :height 1.0
                                                     :v-adjust 0.0)
                               "Dired"
                               "Start Dired"
                               (lambda (&rest _)
                                 (dired user-home-directory)))
             (zmacs--with-icon (all-the-icons-material "help_outline"
                                                       :height 1.1
                                                       :v-adjust 0.1)
                               "Journal"
                               "Open journal in org-mode."
                               (lambda (&rest _)
                                 (zmacs-goto-journal))))))

;;;; Load it up right now:

(zmacs-dashboard)

;;;; Provide package:

(provide 'zmacs-dashboard)

;;; zmacs-dashboard.el ends here.
