;;; zmacs-org-base.el --- Base Org packages  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Paul Ward <paul@lisphacker.uk>
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    25 Oct 2024 16:26:46
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

;;;; Emoji cheat sheet:

(use-package emoji-cheat-sheet-plus
  :defer t)

;;;; HTMLize:

(use-package htmlize
  :defer t
  :commands (htmlize-buffer))

;;;; Org Babel:

(use-package ob
  :ensure nil
  :defer t
  :init
   (define-advice org-babel-execute-src-block (:before (&rest _) load-lang)
    (org-babel-do-load-languages 'org-babel-load-languages
                                 org-babel-load-languages)
    (advice-remove 'org-babel-excecute-src-block
                   'org-babel-execute-src-block@load-lang)))

;;;; Main package:

(use-package org
  :ensure t
  :defer t
  :commands (org-mode
             orgtbl-mode)
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-emphasis-regexp-components
        '("-—[:space:]('\"{["
          "\] - [:space:].,:!?;'\")}\\["
          "[:space:]"
          "."
          1))
  (defvar *zmacs-org-directory*
    (expand-file-name (concat zmacs-storage-directory "org/"))
    "Location of Org files.")
  :custom
  (org-directory                                *zmacs-org-directory*)
  ;; UI
  (org-auto-align-tags                          nil)
  (org-catch-invisible-edits                    'smart)
  (org-cycle-separator-lines                    0)
  (org-ellipsis                                 "…")
  (org-fontify-whole-heading-line               t)
  (org-fontify-quote-and-verse-blocks           t)
  (org-hide-emphasis-markers                    t)
  (org-hide-leading-stars                       nil)
  (org-image-actual-width                       500)
  (org-pretty-entities                          t)
  (org-pretty-entities-include-sub-superscripts t)
  (org-read-date-prefer-future                  'time)
  (org-startup-folded                           nil)
  (org-startup-with-inline-images               t)
  (org-tags-column                              60)
  ;;
  ;; Footnotes
  (org-footnote-section                         nil)
  (org-footnote-auto-adjust                     t)
  ;;
  ;; Indentation
  ;;(org-adapt-indentation                        t)
  (org-startup-indented                         t)
  (org-src-preserve-indentation                 t)
  ;;
  ;; Insertion/yanking
  (org-insert-heading-respect-content           t)
  (org-M-RET-may-split-line                     '((default . t)))
  (org-yank-adjusted-subtrees                   t)
  (org-yank-folded-subtrees                     t)
  ;;
  ;; Lists
  (org-list-allow-alphabetical                  t)
  (org-list-demote-modify-bullet                '(("+" . "-")
                                                  ("-" . "+")
                                                  ("*" . "+")))
  (org-list-indent-offset                       2)
  ;;
  ;; Logging
  (org-log-done                                 'time)
  (org-log-into-drawer                          t)
  (org-log-state-notes-inert-after-drawers      nil)
  (org-log-redeadline                           nil)
  (org-log-reschedule                           nil)
  ;;
  ;; Movement.
  (org-return-follows-link                      t)
  (org-special-ctrl-a/e                         t)
  ;;
  ;; Searching
  (org-imenu-depth                              8)
  (imenu-auto-rescan                            t)
  ;;
  ;; Source blocks
  (org-src-fontify-natively                     t)
  (org-src-window-setup                         'reorganize-frame)
  (org-src-tab-acts-natively                    t)
  (org-confirm-babel-evaluate                   t)
  ;;
  ;; Todo.
  (org-use-fast-todo-selection                  'expert)
  (org-enforce-todo-dependencies                t)
  (org-enforce-todo-checkbox-dependencies       t)
  ;;
  ;; Files.
  (org-clock-persist-file                       (concat *zmacs-cache-directory*
                                                        ".org-clock-save.el"))
  (org-id-locations-file                        (concat *zmacs-cache-directory*
                                                        ".org-id-locations"))
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (display-fill-column-indicator-mode -1)
                             (org-num-mode 1)
                             (org-indent-mode 1)
                             (org-table-header-line-mode 1)
                             (visual-line-mode 1))))

;;;; ZLisp org:

(use-package zlisp-org
  :after org
  :ensure nil
  :defer t
  :config
  (require 'zlisp-org))

;;;; Org Contrib:

(use-package org-contrib
  :ensure t
  :after org
  :config
  ;; ignore export of headlines marked with :ignore: tag
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;;;; Org Export:

(use-package ox
  :ensure nil
  :after org
  :custom
  (org-latex-hyperref-template     nil)
  (org-table-export-default-format "orgtbl-to-csv")
  (org-export-with-smart-quotes    t)
  (org-export-with-broken-links    t)
  (org-export-async-debug          t)
  (org-html-postamble              nil)
  (org-export-async-init-file      nil)
  (org-export-backends             '(ascii
                                     beamer
                                     html
                                     icalendar
                                     latex
                                     odt
                                     pandoc
                                     hugo
                                     md))
  (org-odt-preferred-output-format "docx"))

;;;; Org Agenda:

(use-package org-agenda
  :ensure nil
  :commands (org-agenda)
  :custom
  ;; Agenda logging
  (org-agenda-start-with-log-mode t)

  ;; Agenda styling
  (org-auto-align-tags nil) ;; Don't align tags
  (org-agenda-tags-column 0) ;; Put tags next to heading
  (org-agenda-breadcrumbs-separator "  ")
  (org-agenda-block-separator " ") ;; No default block seperator
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "–––––––––––––– Now")

  ;; Display properties
  (org-agenda-tags-column org-tags-column)
  (org-agenda-show-inherited-tags nil)
  (org-agenda-window-setup 'only-window)
  (org-agenda-restore-windows-after-quit t)

  ;; from stack overflow https://stackoverflow.com/a/22900459/6277148
  ;; note that the formatting is nicer that just using '%b'
  (org-agenda-prefix-format
   '((agenda . " %-18c%?-10t ")
     (timeline . "  % s")
     (todo . " ")
     (tags . " ")
     (search . " %i %-12:c")))

  ;; Scheduling
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-todo-ignore-scheduled 'future)
  (org-agenda-todo-ignore-deadlines 'far)
  (org-agenda-sorting-strategy
   '((agenda time-up) (todo time-up) (tags time-up) (search time-up)))
  (calendar-week-start-day 1) ;; Start week on Monday

  ;; Agenda Custom Commands
  ;; Configure custom agenda views
  ;; https://orgmode.org/manual/Storing-searches.html#Storing-searches
  ;; https://systemcrafters.cc/emacs-from-scratch/organize-your-life-with-org-mode/

  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((agenda "" ((org-agenda-span 'day)))
       (tags-todo "DEADLINE=\"<today>\""
                  ((org-agenda-overriding-header "Due Today!")))
       (tags-todo "+DEADLINE<\"<+5d>\"+DEADLINE>\"<today>\""
                  ((org-agenda-overriding-header "Due Soon")))
       (todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))
       (tags-todo "email" ((org-agenda-overriding-header "Email")))
       ))

     ("n" "Next Tasks"
      ((todo "NEXT"
             ((org-agenda-overriding-header "Next Tasks")))))

     ("W" "Work Tasks" tags-todo "+work")

     ;; Low-effort next actions
     ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
      ((org-agenda-overriding-header "Low Effort Tasks")
       (org-agenda-max-todos 20)
       (org-agenda-files org-agenda-files)))

     ("w" "Workflow Status"
      ((todo "WAIT"
             ((org-agenda-overriding-header "Waiting on External")
              (org-agenda-files org-agenda-files)))
       (todo "REVIEW"
             ((org-agenda-overriding-header "In Review")
              (org-agenda-files org-agenda-files)))
       (todo "PLAN"
             ((org-agenda-overriding-header "In Planning")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "BACKLOG"
             ((org-agenda-overriding-header "Project Backlog")
              (org-agenda-todo-list-sublevels nil)
              (org-agenda-files org-agenda-files)))
       (todo "READY"
             ((org-agenda-overriding-header "Ready for Work")
              (org-agenda-files org-agenda-files)))
       (todo "ACTIVE"
             ((org-agenda-overriding-header "Active Projects")
              (org-agenda-files org-agenda-files)))
       (todo "COMPLETED"
             ((org-agenda-overriding-header "Completed Projects")
              (org-agenda-files org-agenda-files)))
       (todo "CANCELED"
             ((org-agenda-overriding-header "Cancelled Projects")
              (org-agenda-files org-agenda-files))))))))

;;;;; Agenda Jump to Dashboard

(defun zmacs-jump-to-org-dashboard ()
  (interactive)
  (require 'org)
  (org-agenda nil "d"))

;;;;; Agenda Refresh

;; automatically refresh the agenda after adding a task
(defun zmacs-org-agenda-refresh ()
  (interactive)
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo)
      (message "[org agenda] refreshed!"))))
(add-hook 'org-capture-after-finalize-hook 'zmacs-org-agenda-refresh)

;;;;; Hydra for Agenda

;; Hydra for org agenda (graciously offered by Spacemacs)
(with-eval-after-load 'org-agenda
  (defhydra zmacs-hydra-org-agenda (:color pink :hint none)
    "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
    ;; Entry
    ("hA" org-agenda-archive-default)
    ("hk" org-agenda-kill)
    ("hp" org-agenda-priority)
    ("hr" org-agenda-refile)
    ("h:" org-agenda-set-tags)
    ("ht" org-agenda-todo)
    ;; Visit entry
    ("o"   link-hint-open-link :exit t)
    ("<tab>" org-agenda-goto :exit t)
    ("TAB" org-agenda-goto :exit t)
    ("SPC" org-agenda-show-and-scroll-up)
    ("RET" org-agenda-switch-to :exit t)
    ;; Date
    ("dt" org-agenda-date-prompt)
    ("dd" org-agenda-deadline)
    ("+" org-agenda-do-date-later)
    ("-" org-agenda-do-date-earlier)
    ("ds" org-agenda-schedule)
    ;; View
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vt" org-agenda-fortnight-view)
    ("vm" org-agenda-month-view)
    ("vy" org-agenda-year-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)
    ;; Toggle mode
    ("ta" org-agenda-archives-mode)
    ("tA" (org-agenda-archives-mode 'files))
    ("tr" org-agenda-clockreport-mode)
    ("tf" org-agenda-follow-mode)
    ("tl" org-agenda-log-mode)
    ("td" org-agenda-toggle-diary)
    ;; Filter
    ("fc" org-agenda-filter-by-category)
    ("fx" org-agenda-filter-by-regexp)
    ("ft" org-agenda-filter-by-tag)
    ("fr" org-agenda-filter-by-tag-refine)
    ("fh" org-agenda-filter-by-top-headline)
    ("fd" org-agenda-filter-remove-all)
    ;; Clock
    ("cq" org-agenda-clock-cancel)
    ("cj" org-agenda-clock-goto :exit t)
    ("ci" org-agenda-clock-in :exit t)
    ("co" org-agenda-clock-out)
    ;; Other
    ("q" nil :exit t)
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("gr" org-agenda-redo)))

;;;; Org Id:

(use-package org-id
  :ensure nil
  :after org
  :custom
  (org-id-locations-file (concat *zmacs-cache-directory* ".org-id-locations"))
  (org-id-method 'ts) ;; use timestamp for id
  (org-id-link-to-org-use-id 'create-if-interactive))

;;;; Org modules:

(with-eval-after-load 'org
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-tempo t)
  (add-to-list 'org-modules 'org-protocol t)

  (when (zlisp/macos-p)
    (add-to-list 'org-modules 'org-mac-link t)))

;;;; Inline tasks:

(use-package org-inlinetask
  :ensure nil
  :commands org-inlinetask-insert-task)

;;;; Tempo:

(use-package org-tempo
  :ensure nil
  :config
  (require 'org-tempo))

;;;; Org Archive:

(setq org-archive-location (concat *zmacs-org-directory*
                                   "/org-archive/archived.org::datetree/"))

;; Also tell org how to archive all the done tasks (DONE or CANCELED) in a
;; file.
;; From https://changelog.complete.org/archives/9877-emacs-3-more-on-org-mode
;; Based on a stack overflow answer.
;; See https://stackoverflow.com/a/27043756
(defun zmacs-org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELED" 'file))

;;;; Org Refile:

;; With the below settings, you can trigger Refile with C-c C-w in any Org file
;; and get a completing read of all headings up to three levels deep in all
;; files in org-agenda-files. You can also refile to the top header in a
;; document and create new parents.
(use-package org-refile
  :ensure nil
  :after org
  :custom
  (org-refile-targets '((nil :maxlevel . 9)
                        (org-agenda-files :maxlevel . 8)))
  (org-refile-use-cache t)  ;; use cache for org refile
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm))

(customize-set-variable 'org-file-apps
                        '(("\\.docx\\'" . default)
                          ("\\.mm\\'" . default)
                          ("\\.x?html?\\'" . default)
                          ("\\.pdf\\'" . emacs)
                          (auto-mode . emacs)))

;;;; Org Functions:

(defun org-emphasize-dwim (&optional char)
  (interactive)
  (unless (region-active-p)
    (zmacs-maybe-mark-word))
  (org-emphasize char))

(defun org-emphasize-with-verbatim-dwim ()
  (interactive)
  (org-emphasize-dwim ?=))

(defun org-emphasize-with-code-dwim ()
  (interactive)
  (org-emphasize-dwim ?~))

(defun zmacs--cursor-outside-of-any-word ()
  (not (bounds-of-thing-at-point 'word)))

(defun zmacs--cursor-at-beginning-of-a-word ()
  (eq (point) (car (bounds-of-thing-at-point 'word))))

(defun zmacs-maybe-mark-word ()
  "Mark the current word. If cursor is outside of a word bounds, mark the empty position."
  (interactive)
  (unless (or (zmacs--cursor-outside-of-any-word) (zmacs--cursor-at-beginning-of-a-word))
    (backward-word))
  (unless (zmacs--cursor-outside-of-any-word)
    (mark-word)))

(defun zmacs-org-advance ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-forward-heading-same-level 1))
  (org-narrow-to-subtree))

(defun zmacs-org-retreat ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-backward-heading-same-level 1))
  (org-narrow-to-subtree))


(defun zmacs-clone-buffer-and-narrow ()
  "Clone buffer and narrow outline tree"
  (interactive)
  (let ((buf (clone-indirect-buffer-other-window nil nil)))
    (with-current-buffer buf
      (cond ((derived-mode-p 'org-mode)
             (org-narrow-to-element))
            ((derived-mode-p 'markdown-mode)
             (markdown-narrow-to-subtree))))
    (switch-to-buffer-other-window buf)))

(defun zmacs-goto-org-files ()
  "goto org-files directory"
  (interactive)
  (let ((default-directory *zmacs-org-directory*))
    (call-interactively 'find-file)))

(defun zmacs-goto-todo.org ()
  "goto org-todo"
  (interactive)
  (find-file (concat *zmacs-org-directory* "todo.org")))

(defun zmacs-goto-someday.org ()
  "goto org-someday"
  (interactive)
  (find-file (concat *zmacs-org-directory* "someday.org")))

(defun zmacs-goto-reading.org ()
  "goto reading list"
  (interactive)
  (find-file (concat *zmacs-org-directory* "reading.org")))

(defun zmacs-goto-writing.org ()
  "goto writing list"
  (interactive)
  (find-file (concat *zmacs-org-directory* "writing.org")))

(defun zmacs-org-export-headlines-to-docx ()
  "Export all subtrees that are *not* tagged with :noexport: to
    separate docx files.

    Subtrees that do not have the :EXPORT_FILE_NAME: property set
    are exported to a filename derived from the headline text."
  (interactive)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-pandoc-export-to-docx nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level)))
  (shell-command-to-string "open ~/Dropbox/Work/Comments/Referee-Reports/ref-report.docx"))

(defun zmacs-org-export-headlines-to-pdf ()
  "Export all subtrees that are *not* tagged with :noexport: to
    separate pdf files.

    Subtrees that do not have the :EXPORT_FILE_NAME: property set
    are exported to a filename derived from the headline text."
  (interactive)
  ;; (require 'ox-pandoc)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-latex-export-to-pdf nil t nil nil '(:latex-class "org-notes"))
           ;; (org-pandoc-export-to-latex-pdf nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level))))

(defun zmacs-org-map-entries (org-file in-tags func)
  (let ((tags (if (stringp in-tags)
                  (list in-tags)
                in-tags)))

    (with-temp-buffer
      (org-mode)
      (insert-file-contents org-file-main)

      ;; Execute func at each heading that matches tags.
      (while (< (point) (point-max))

        ;; If find a heading...
        (and (search-forward-regexp "^\* " nil "end")

             ;; ...that matches the given tags...
             (seq-reduce
              (lambda(a b) (and a b))
              (mapcar
               (lambda (tag)
                 (beginning-of-line)
                 (search-forward-regexp
                  (concat ":" tag ":") (line-end-position) "end"))
               tags)
              t)

             ;; ... then execute given function with cursor at beginning of
             ;; heading.
             (progn
               (beginning-of-line)
               (save-excursion
                 (funcall func))
               (end-of-line)))))))

(defun zmacs-demote-everything (number beg end)
  "Add a NUMBER of * to all headlines between BEG and END.
    Interactively, NUMBER is the prefix argument and BEG and END are
    the region boundaries."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (narrow-to-region beg end)
        (goto-char (point-min))
        (let ((string (make-string number ?*)))
          (while (search-forward-regexp "^\\*" nil t)
            (insert string)))))))

(defun org-toggle-properties ()
  "Toggle visibility of properties in current header if it exists."
  (save-excursion
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))
    (when (org-header-property-p)
      (let* ((a (re-search-forward "\n\\:" nil t)))
        (if (outline-invisible-p (point))
            (outline-show-entry)
          (org-cycle-hide-drawers 'all))))))

(defun zmacs-org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
    A double return on an empty element deletes it. Use a prefix arg
    to get regular RET. "
  ;; See https://gist.github.com/alphapapa/61c1015f7d1f0d446bc7fd652b7ec4fe and
  ;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if ignore
      (org-return)
    (cond ((eq 'link (car (org-element-context)))
           ;; Open links like usual
           (org-open-at-point-global))
          ((and (fboundp 'org-inlinetask-in-task-p) (org-inlinetask-in-task-p))
           ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
           ;; Johansson!
           (org-return))
          ((org-at-item-checkbox-p)
           ;; Add checkboxes
           (org-insert-todo-heading nil))
          ((and (org-in-item-p) (not (bolp)))
           ;; Lists end with two blank lines, so we need to make sure we are also not
           ;; at the beginning of a line to avoid a loop where a new entry gets
           ;; created with only one blank line.
           (if (org-element-property :contents-begin (org-element-context))
               (org-insert-heading)
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))
             (org-return)))
          ((org-at-heading-p)
           (if (s-present? (org-element-property :title (org-element-context)))
               (progn
                 (org-end-of-meta-data)
                 (org-insert-heading))
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))))
          ((org-at-table-p)
           (if (--any? (string-empty-p it)
                       (nth (- (org-table-current-dline) 1) (org-table-to-lisp)))
               (org-return)
             ;; Empty row
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))
             (org-return)))
          (t
           (org-return)))))

(fset 'zmacs-org-checkbox-from-list
      [?a ?  ?\[ ?  ?\] escape ?\M-x return])

(defun org-update-link-syntax (&optional no-query)
  "Update syntax for links in current buffer.
Query before replacing a link, unless optional argument NO-QUERY
is non-nil."
  (interactive "P")
  (org-with-point-at 1
    (let ((case-fold-search t))
      (while (re-search-forward "\\[\\[[^]]*?%\\(?:2[05]\\|5[BD]\\)" nil t)
        (let ((object (save-match-data (org-element-context))))
          (when (and (eq 'link (org-element-type object))
                     (= (match-beginning 0)
                        (org-element-property :begin object)))
            (goto-char (org-element-property :end object))
            (let* ((uri-start (+ 2 (match-beginning 0)))
                   (uri-end (save-excursion
                              (goto-char uri-start)
                              (re-search-forward "\\][][]" nil t)
                              (match-beginning 0)))
                   (uri (buffer-substring-no-properties uri-start uri-end)))
              (when (or no-query
                        (y-or-n-p
                         (format "Possibly obsolete URI syntax: %S.  Fix? "
                                 uri)))
                (setf (buffer-substring uri-start uri-end)
                      (org-link-escape (org-link-decode uri)))))))))))

(defun org-table-wrap-to-width (width)
  "Wrap current column to WIDTH."
  (interactive (list (read-number "Enter column width: ")))
  (org-table-check-inside-data-field)
  (org-table-align)

  (let (cline (ccol (org-table-current-column)) new-row-count (more t))
    (org-table-goto-line 1)
    (org-table-goto-column ccol)

    (while more
      (setq cline (org-table-current-line))

      ;; Cut current field
      (org-table-copy-region (point) (point) 'cut)

      ;; Justify for width
      (setq org-table-clip
            (mapcar 'list (org-wrap (caar org-table-clip) width nil)))

      ;; Add new lines and fill
      (setq new-row-count (1- (length org-table-clip)))
      (if (> new-row-count 0)
          (org-table-insert-n-row-below new-row-count))
      (org-table-goto-line cline)
      (org-table-goto-column ccol)
      (org-table-paste-rectangle)
      (org-table-goto-line (+ cline new-row-count))

      ;; Move to next line
      (setq more (org-table-goto-line (+ cline new-row-count 1)))
      (org-table-goto-column ccol))

    (org-table-goto-line 1)
    (org-table-goto-column ccol)))

(defun org-table-insert-n-row-below (n)
  "Insert N new lines below the current."
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
         (new (org-table-clean-line line)))
    ;; Fix the first field if necessary
    (if (string-match "^[ \t]*| *[#$] *|" line)
        (setq new (replace-match (match-string 0 line) t t new)))
    (beginning-of-line 2)
    (setq new
          (apply 'concat (make-list n (concat new "\n"))))
    (let (org-table-may-need-update) (insert-before-markers new))  ;;; remove?
    (beginning-of-line 0)
    (re-search-forward "| ?" (point-at-eol) t)
    (and (or org-table-may-need-update org-table-overlay-coordinates) ;;; remove?
         (org-table-align))
    (org-table-fix-formulas "@" nil (1- (org-table-current-dline)) n)))

(fset 'export-last-subtree
      "\C-u\C-c\C-e")

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<f5>") 'export-last-subtree)))

(defun zmacs-org-select-tags-completing-read ()
  "Select tags to add to headline."
  (interactive)
  (let* ((current (org-get-tags (point)))
         (selected (completing-read-multiple "Select org tag(s): " (org-get-buffer-tags))))
    (alet (-uniq (append (-difference current selected)
                         (-difference selected current)))
      (org-set-tags it))))

(defun zmacs-org-link-copy-at-point ()
  (interactive)
  (save-excursion
    (let* ((ol-regex "\\[\\[.*?:.*?\\]\\(\\[.*?\\]\\)?\\]")
           (beg (re-search-backward "\\[\\["))
           (end (re-search-forward ol-regex))
           (link-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (kill-new link-string)
      (message "Org link %s is copied." link-string))))

(defun zmacs-org-replace-link-by-link-description ()
  "Replace an org link by its description or, if empty, its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

(defun zmacs-copy-and-uncheck (start end)
  "copy a region of regularly repeating checkbox items forward from
one week to the next, unchecking them at the same time"
  (interactive "r")
  (kill-new (replace-regexp-in-string (rx "[X]") "[ ]" (buffer-substring start end)))
  (setq deactivate-mark t))

(defun zmacs-org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'agenda))

(defun zmacs-org-tree-to-new-file ()
  (interactive)
  "Move an org subtree to a new file"
  (org-copy-subtree nil t)
  (find-file-other-window
   (read-file-name "Move subtree to file:" ))
  (org-paste-subtree))

(defun org-block-wrap ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(
                      ("a" . "ascii")
                      ("c" . "comment")
                      ("C" . "center")
                      ("e" . "example")
                      ("E" . "src emacs-lisp")
                      ("h" . "html")
                      ("l" . "laTeX")
                      ("n" . "notes")
                      ("q" . "quote")
                      ("s" . "src")
                      ("v" . "verse")
                      ))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+end_" choice "\n")
                (goto-char start)
                (insert "#+begin_" choice "\n")))
             (t
              (insert "#+begin_" choice "\n")
              (save-excursion (insert "#+end_" choice))))))))))

(defun zmacs-org-export-to-buffer-html-as-body (&optional async subtreep visible-only body-only ext-plist)
  "Export org buffer body to html"
  (interactive)
  (org-export-to-buffer 'html "*Org HTML Export*"
    async body-only ext-plist (lambda () (html-mode)))
  (zmacs-copy-whole-buffer-to-clipboard)
  (delete-windows-on "*Org HTML Export*")
  (message "HTML copied!"))

(defun zmacs-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-todo-log-states) ; turn off logging.
    (org-todo (if (= n-not-done 0)
                  "DONE"
                "TODO"))))

(defun zmacs--align-tags-here (to-col)
  "Align tags on the current headline to TO-COL.
Since TO-COL is derived from `org-tags-column', a negative value is
interpreted as alignment flush-right, a positive value as flush-left,
and 0 means insert a single space in between the headline and the tags."
  ;; source: https://list.orgmode.org/20200916225553.hrtxitzt46dzln7i@ionian.linksys.moosehall/
  (save-excursion
    (when (org-match-line org-tag-line-re)
      (let* ((tags-start (match-beginning 1))
             (tags-end (match-end 1))
             (tags-pixel-width
              (car (window-text-pixel-size (selected-window)
                                           tags-start tags-end)))
             (blank-start (progn
                            (goto-char tags-start)
                            (skip-chars-backward " \t")
                            (point)))
             ;; use this to avoid a 0-width space before tags on long lines:
             (blank-start-col (progn
                                (goto-char blank-start)
                                (current-column)))
             ;; this is to makes it work with org-indent-mode:
             (lpref (if (org-fold-folded-p) 0
                      (length (get-text-property (point) 'line-prefix)))))
        ;; If there is more than one space between the headline and
        ;; tags, delete the extra spaces.  Might be better to make the
        ;; delete region one space smaller rather than inserting a new
        ;; space?
        (when (> tags-start (1+  blank-start))
          (delete-region blank-start tags-start)
          (goto-char blank-start)
          (insert " "))
        (if (or (= to-col 0) (< (abs to-col) (1- blank-start-col)))
            ;; Just leave one normal space width
            (remove-text-properties blank-start (1+  blank-start)
                                    '(my-display nil))
          (message "In here: %s" lpref)
          (let ((align-expr
                 (if (> to-col 0)
                     ;; Left-align positive values
                     (+ to-col lpref)
                   ;; Right-align negative values by subtracting the
                   ;; width of the tags.  Conveniently, the pixel
                   ;; specification allows us to mix units,
                   ;; subtracting a pixel width from a column number.
                   `(-  ,(- lpref to-col) (,tags-pixel-width)))))
            (put-text-property blank-start (1+  blank-start)
                               'my-display
                               `(space . (:align-to ,align-expr)))))))))

(defun zmacs-fix-tag-alignment ()
  (setq org-tags-column 60) ;; adjust this
  (advice-add 'org--align-tags-here :override #'zmacs--align-tags-here)
  ;; this is needed to make it work with https://github.com/minad/org-modern:
  (add-to-list 'char-property-alias-alist '(display my-display))
  ;; this is needed to align tags upon opening an org file:
  (org-align-tags t))

(defun zmacs-org-mode-hook-fixes ()
  (electric-pair-local-mode -1))

(add-hook 'org-mode-hook #'zmacs-org-mode-hook-fixes)
(add-hook 'org-mode-hook #'zmacs-fix-tag-alignment)
(add-hook 'org-after-todo-statistics-hook #'zmacs-summary-todo)

;;;; Provide package:

(provide 'zmacs-org-base)

;;; zmacs-org-base.el ends here.
