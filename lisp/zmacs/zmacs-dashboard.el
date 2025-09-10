;;; zmacs-dashboard.el --- The dashboard  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
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
     :author "Me, that's who!")
    (:lines ("Any sufficiently complicated C or Fortran program contains an ad hoc,"
             "informally-specified, bug-ridden, slow implementation of half of Common Lisp.")
     :author "Philip Greenspun")
    (:lines ("A language that doesn't affect the way you think about programming"
             "is not worth knowing.")
     :author "Alan Perlis")
    (:lines ("It is better to have 100 functions operate on one data structure"
             "than 10 functions on 10 data structures.")
     :author "Alan Perlis")
    (:lines ("Lisp is worth learning for the profound enlightenment experience"
             "you will have when you finally get it; that experience will make you"
             "a better programmer for the rest of your days.")
     :author "Eric S. Raymond")
    (:lines ("Python is not Lisp, but it's getting there.")
     :author "Peter Norvig")
    (:lines ("There are only two kinds of languages: the ones people complain about"
             "and the ones nobody uses.")
     :author "Bjarne Stroustrup")
    (:lines ("LISP is a language for manipulating symbols.")
     :author "John McCarthy")
    (:lines ("In Lisp, you don't just write your program; you write the language"
             "in which your program is written.")
     :author "Paul Graham")
    (:lines ("Once you understand Lisp, you will write Lisp programs"
             "in any language.")
     :author "Usenet folklore")
    (:lines ("C is a portable assembly; Lisp is a portable language lab.")
     :author "Anonymous")
    (:lines ("If you think the parentheses are the problem,"
             "I have some bad news about your algorithm.")
     :author "Anonymous")
    (:lines ("Parentheses are just hugs for your code.")
     :author "Me")
    (:lines ("Macros don't add features; they remove limitations.")
     :author "Anonymous")
    (:lines ("Homoiconicity means your code can butter its own toast.")
     :author "Me")
    (:lines ("When in doubt, add a level of indirection;"
             "when in Lisp, add a macro.")
     :author "Anonymous")
    (:lines ("Common Lisp: where the standard library comes with a city map"
             "and a municipal government.")
     :author "Me")
    (:lines ("Programming languages should be designed not by piling feature upon feature,"
             "but by removing the weaknesses and restrictions that make additional features"
             "appear necessary.")
     :author "R5RS Authors")
    (:lines ("He who refuses to do arithmetic is doomed to talk nonsense.")
     :author "John McCarthy")
    (:lines ("Worse is better.")
     :author "Richard P. Gabriel")
    (:lines ("Macros are for introducing new linguistic abstractions—"
             "not for saving keystrokes.")
     :author "Kent Pitman (paraphrase)")
    (:lines ("A language should grow by enabling its users to grow it.")
     :author "Guy L. Steele Jr. (paraphrase)")
    (:lines ("We don't really understand something unless we can program it.")
     :author "Gerald Jay Sussman")
    (:lines ("Simple is not the same as easy.")
     :author "Rich Hickey")
    (:lines ("In dynamic languages, many ‘design patterns’ are just features of the language.")
     :author "Peter Norvig (paraphrase)")
    (:lines ("Lisp has no syntax—only conventions.")
     :author "Lisp folklore")
    (:lines ("To understand continuations, you must be willing to lose your stack.")
     :author "Scheme folklore")
    (:lines ("Homoiconicity: when your data structures moonlight as your syntax.")
     :author "Usenet wisdom")
    (:lines ("A good macro system is to syntax what higher-order functions are to control.")
     :author "Anonymous")
    (:lines ("Compilers and interpreters are the same idea—"
             "one runs now, the other runs ahead.")
     :author "Sussman & Steele (paraphrase)")
    (:lines ("Premature optimization is the root of all evil.")
     :author "Donald Knuth")
    (:lines ("Beware of bugs in the above code; I have only proved it correct,"
             "not tried it.")
     :author "Donald Knuth")
    (:lines ("Program testing can be used to show the presence of bugs, but never"
             "to show their absence!")
     :author "Edsger W. Dijkstra")
    (:lines ("Computer science is no more about computers than astronomy is"
             "about telescopes.")
     :author "Edsger W. Dijkstra")
    (:lines ("Simplicity is prerequisite for reliability.")
     :author "Edsger W. Dijkstra")
    (:lines ("The use of COBOL cripples the mind; its teaching should therefore"
             "be regarded as a criminal offense.")
     :author "Edsger W. Dijkstra")
    (:lines ("Algorithms + Data Structures = Programs.")
     :author "Niklaus Wirth")
    (:lines ("Wirth's law: software is getting slower more rapidly than hardware"
             "becomes faster.")
     :author "Niklaus Wirth")
    (:lines ("There are two ways of constructing a software design: one way is to"
             "make it so simple that there are obviously no deficiencies; and the"
             "other way is to make it so complicated that there are no obvious"
             "deficiencies.")
     :author "C. A. R. Hoare")
    (:lines ("I call it my billion-dollar mistake: the invention of the null"
             "reference.")
     :author "C. A. R. Hoare")
    (:lines ("Adding manpower to a late software project makes it later.")
     :author "Frederick P. Brooks Jr.")
    (:lines ("Plan to throw one away; you will, anyhow.")
     :author "Frederick P. Brooks Jr.")
    (:lines ("There are only two hard things in Computer Science: cache"
             "invalidation and naming things.")
     :author "Phil Karlton")
    (:lines ("The purpose of computing is insight, not numbers.")
     :author "Richard Hamming")
    (:lines ("All problems in computer science can be solved by another level of"
             "indirection… except for the problem of too many indirections.")
     :author "David Wheeler (with corollary)")
    (:lines ("The nice thing about standards is that there are so many"
             "to choose from.")
     :author "Andrew S. Tanenbaum")
    (:lines ("The first 90 percent of the code accounts for the first 90 percent"
             "of the development time. The remaining 10 percent accounts for the"
             "other 90 percent.")
     :author "Tom Cargill")
    (:lines ("Hofstadter's Law: It always takes longer than you expect, even when"
             "you take into account Hofstadter's Law.")
     :author "Douglas Hofstadter")
    (:lines ("Measuring programming progress by lines of code is like measuring"
             "aircraft building progress by weight.")
     :author "Bill Gates")
    (:lines ("When in doubt, use brute force.")
     :author "Ken Thompson")
    (:lines ("Talk is cheap. Show me the code.")
     :author "Linus Torvalds")
    (:lines ("Bad programmers worry about the code. Good programmers worry about"
             "data structures and their relationships.")
     :author "Linus Torvalds")
    (:lines ("Given enough eyeballs, all bugs are shallow.")
     :author "Eric S. Raymond")
    (:lines ("Everyone knows that debugging is twice as hard as writing a program"
             "in the first place; so if you're as clever as you can be when you"
             "write it, how will you ever debug it?")
     :author "Brian W. Kernighan")
    (:lines ("Controlling complexity is the essence of computer programming.")
     :author "Brian W. Kernighan")
    (:lines ("The cheapest, fastest, and most reliable components are those that"
             "aren't there.")
     :author "Gordon Bell"))
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

(defun zmacs-insert-random-quote ()
  "Insert a random quote into the current buffer."
  (interactive)
  (let* ((quote (zmacs--random-quote))
         (text  (zmacs--dashboard-format-quote-block quote)))
    (insert text)))

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
