;;; zmacs-line.el --- The ZMACS Modeline  -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
;; Copyright (c) 2024-2025 Paul Ward <paul@lisphacker.uk>
;; Copyright (c)           Colin McLear
;;
;; Author:     Paul Ward <paul@lisphacker.uk>
;; Maintainer: Paul Ward <paul@lisphacker.uk>
;; Created:    21 Oct 2024 01:05:05
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

(require 'face-remap)
(require 'cl-lib)

(when (display-graphic-p)
  (require 'all-the-icons))

;;;; Group

(defgroup zmacs-line nil
  "zmacs-line group"
  :group 'zmacs-emacs
  :link '(url-link :tag "Homepage" "https://github.com/Lambda-Emacs/zmacs-line")
  :tag "ZMACS modeline")

;;;; Custom Variable Settings

(defcustom zmacs-line-window-width-limit 0.25
  "The limit of the window width.
If `window-width' is smaller than the limit, some information won't be
displayed. It can be an integer or a float number. `nil' means no limit."
  :type '(choice integer
                 float
                 (const :tag "Disable" nil))
  :group 'zmacs-line
  :tag "Window width limit")

(defcustom zmacs-line-position 'bottom
  "Default modeline position (top or bottom)"
  :type '(choice
          (const :tag "Nil" nil)
          (const :tag "Top"    top)
          (const :tag "Bottom" bottom))
  :group 'zmacs-line
  :tag "Modeline position")

(defcustom zmacs-line-prefix t
  "Include a prefix icon to indicate buffer status in the status-line."
  :type 'boolean
  :group 'zmacs-line
  :tag "Use a prefix icon?")

(defcustom zmacs-line-prefix-padding t
  "Include prefix padding."
  :type 'boolean
  :group 'zmacs-line
  :tag "Use padding with the prefix?")

(defcustom zmacs-line-user-mode nil
  "User supplied mode to be evaluated for modeline."
  :type '(choice (const nil) function)
  :group 'zmacs-line
  :tag "User-supplied mode")

(defcustom zmacs-line-abbrev nil
  "If t then show abbreviated mode symbol in modeline.
Default is nil. To change the values of the major-mode symbols
see the value of `zmacs-line-abbrev-alist'"
  :group 'zmacs-line
  :type 'boolean
  :tag "Show abbrev symbol?")

(defcustom zmacs-line-git-diff-mode-line t
  "If t then show diff lines in modeline."
  :group 'zmacs-line
  :type 'boolean
  :tag "Show diff lines?")

(defcustom zmacs-line-vc-symbol ""
  "Symbol to use in buffers visiting files under version control"
  :group 'zmacs-line
  :type 'string
  :tag "Version control symbol")

;; Visual Bell
(defcustom zmacs-line-visual-bell t
  "If t then use `zmacs-line-visual-bell'."
  :group 'zmacs-line
  :type 'boolean
  :tag "Use visual bell?")

;; Invert status faces
;; This make zmacs-line look more like nano-modeline
(defcustom zmacs-line-status-invert nil
  "If t then invert the colors to get a box effect for the corner of the status line."
  :group 'zmacs-line
  :type 'boolean
  :tag "Invert colours for box?")

;; Mode line symbols
(defcustom zmacs-line-gui-ro-symbol " ⨂"  ;;  ⬤◯⨂
  "Modeline gui read-only symbol."
  :group 'zmacs-line
  :type 'string
  :tag "Read-only buffer symbol")

(defcustom zmacs-line-gui-mod-symbol " ⬤" ;;  ⨀⬤
  "Modeline gui modified symbol."
  :group 'zmacs-line
  :type 'string
  :tag "Modified buffer symbol")

(defcustom zmacs-line-gui-rw-symbol " ◯" ; λ ◉ ◎ ⬤◯
  "Modeline gui read-write symbol."
  :group 'zmacs-line
  :type 'string
  :tag "Read/write buffer symbol")

(defcustom zmacs-line-tty-ro-symbol " λ "
  "Modeline tty read-only symbol."
  :group 'zmacs-line
  :type 'string
  :tag "Read/only buffer symbol for TTY mode")

(defcustom zmacs-line-tty-mod-symbol " λ "
  "Modeline tty read-only symbol."
  :group 'zmacs-line
  :type 'string
  :tag "Modified buffer symbol for TTY mode")

(defcustom zmacs-line-tty-rw-symbol " λ "
  "Modeline tty read-write symbol."
  :group 'zmacs-line
  :type 'string
  :tag "Read/write buffer symbol for TTY mode")

(defcustom zmacs-line-truncate-value 30
  "Value of modeline truncate-length function."
  :group 'zmacs-line
  :type 'integer)

(defcustom zmacs-line-hspace " "
  "Space adjustment for right end of modeline."
  :type 'string
  :group 'zmacs-line
  :tag "Horizontal space")

(defcustom zmacs-line-space-top +.35
  "Space adjustment for top of status-line.
Positive is upwards"
  :type 'float
  :group 'zmacs-line
  :tag "Top space adjustment")

(defcustom zmacs-line-space-bottom -.5
  "Space adjustment for bottom of status-line.
Negative is downwards."
  :type 'float
  :group 'zmacs-line
  :tag "Bottom space adjustment")

(defcustom zmacs-line-symbol-position .067
  "Space adjustment for symbol in status-line.
Negative is downwards."
  :type 'float
  :group 'zmacs-line
  :tag "Buffer symbol adjustment")

(defcustom zmacs-line-syntax t
  "Show flycheck/flymake report in status-line."
  :type 'boolean
  :group 'zmacs-line
  :tag "Show flycheck/flymake report?")

(defcustom zmacs-line-which-func nil
  "Show `which-function-mode' display in status-line."
  :type 'boolean
  :group 'zmacs-line
  :tag "Show `which-function-mode'?")

(defcustom zmacs-line-flycheck-label "Issues: "
  "Show with flycheck/flymake issues count."
  :type 'string
  :group 'zmacs-line
  :tag "Flycheck label")

(defcustom zmacs-line-icon-time nil
  "When set to non-nil show the time as an icon clock.
Time info is only shown `display-time-mode' is non-nil"
  :type 'boolean
  :group 'zmacs-line
  :tag "Show time as icon clock?")

(defcustom zmacs-line-time-day-and-date-format "  %H:%M %Y-%m-%e "
  "`format-time-string'."
  :type 'string
  :group 'zmacs-line
  :tag "Time and date format")

(defcustom zmacs-line-time-format "  %H:%M "
  "`format-time-string'."
  :type 'string
  :group 'zmacs-line
  :tag "Time format")

(defcustom zmacs-line-time-icon-format " %s"
  "`format-time-string'."
  :type 'string
  :group 'zmacs-line
  :tag "Time icon format")

(defcustom zmacs-line-display-group-start "("
  "Modeline display group start indicator."
  :group 'zmacs-line
  :type 'string
  :tag "Display group start symbol")

(defcustom zmacs-line-display-group-end ")"
  "Modeline display group end indicator."
  :group 'zmacs-line
  :type 'string
  :tag "Display group end symbol")

(defcustom zmacs-line-mode-formats
  '(;; with :mode-p first
    (imenu-list-mode        :mode-p zmacs-line-imenu-list-mode-p
                            :format zmacs-line-imenu-list-mode)
    (org-capture-mode       :mode-p zmacs-line-org-capture-mode-p
                            :format zmacs-line-org-capture-mode
                            :on-activate zmacs-line-org-capture-activate
                            :on-deactivate zmacs-line-org-capture-deactivate)
    (prog-mode              :mode-p zmacs-line-prog-mode-p
                            :format zmacs-line-prog-mode
                            :on-activate zmacs-line-prog-activate
                            :on-deactivate zmacs-line-prog-deactivate)
    (mu4e-dashboard-mode    :mode-p zmacs-line-mu4e-dashboard-mode-p
                            :format zmacs-line-mu4e-dashboard-mode)
    (messages-mode          :mode-p zmacs-line-messages-mode-p
                            :format zmacs-line-messages-mode)
    (message-mode           :mode-p zmacs-line-message-mode-p
                            :format zmacs-line-message-mode)
    (term-mode              :mode-p zmacs-line-term-mode-p
                            :format zmacs-line-term-mode)
    (vterm-mode             :mode-p zmacs-line-vterm-mode-p
                            :format zmacs-line-term-mode)
    (eshell-mode            :mode-p zmacs-line-eshell-mode-p
                            :format zmacs-line-eshell-mode)
    (buffer-menu-mode       :mode-p zmacs-line-buffer-menu-mode-p
                            :format zmacs-line-buffer-menu-mode
                            :on-activate zmacs-line-buffer-menu-activate
                            :on-deactivate zmacs-line-buffer-menu-deactivate)
    (calendar-mode          :mode-p zmacs-line-calendar-mode-p
                            :format zmacs-line-calendar-mode
                            :on-activate zmacs-line-calendar-activate
                            :on-deactivate zmacs-line-calendar-deactivate)
    (completion-list-mode   :mode-p zmacs-line-completion-list-mode-p
                            :format zmacs-line-completion-list-mode)
    (deft-mode              :mode-p zmacs-line-deft-mode-p
      :format zmacs-line-deft-mode)
    (doc-view-mode          :mode-p zmacs-line-doc-view-mode-p
                            :format zmacs-line-doc-view-mode)
    (elfeed-search-mode     :mode-p zmacs-line-elfeed-search-mode-p
                            :format zmacs-line-elfeed-search-mode
                            :on-activate zmacs-line-elfeed-search-activate
                            :on-deactivate zmacs-line-elfeed-search-deactivate)
    (elfeed-show-mode       :mode-p zmacs-line-elfeed-show-mode-p
                            :format zmacs-line-elfeed-show-mode)
    (elpher-mode            :mode-p zmacs-line-elpher-mode-p
                            :format zmacs-line-elpher-mode
                            :on-activate zmacs-line-elpher-activate)
    (help-mode              :mode-p zmacs-line-help-mode-p
                            :format zmacs-line-help-mode)
    (helpful-mode           :mode-p zmacs-line-helpful-mode-p
                            :format zmacs-line-help-mode)
    (info-mode              :mode-p zmacs-line-info-mode-p
                            :format zmacs-line-info-mode
                            :on-activate zmacs-line-info-activate
                            :on-deactivate zmacs-line-info-deactivate)
    (magit-mode             :mode-p zmacs-line-magit-mode-p
                            :format zmacs-line-magit-mode)
    (mu4e-compose-mode      :mode-p zmacs-line-mu4e-compose-mode-p
                            :format zmacs-line-mu4e-compose-mode)
    (mu4e-headers-mode      :mode-p zmacs-line-mu4e-headers-mode-p
                            :format zmacs-line-mu4e-headers-mode)
    (mu4e-loading-mode      :mode-p zmacs-line-mu4e-loading-mode-p
                            :format zmacs-line-mu4e-loading-mode)
    (mu4e-main-mode         :mode-p zmacs-line-mu4e-main-mode-p
                            :format zmacs-line-mu4e-main-mode)
    (mu4e-view-mode         :mode-p zmacs-line-mu4e-view-mode-p
                            :format zmacs-line-mu4e-view-mode)
    (org-agenda-mode        :mode-p zmacs-line-org-agenda-mode-p
                            :format zmacs-line-org-agenda-mode)

    (org-clock-mode         :mode-p zmacs-line-org-clock-mode-p
                            :format zmacs-line-org-clock-mode
                            :on-activate zmacs-line-org-clock-activate
                            :on-deactivate zmacs-line-org-clock-deactivate)
    (pdf-view-mode          :mode-p zmacs-line-pdf-view-mode-p
                            :format zmacs-line-pdf-view-mode)
    (fundamental-mode       :mode-p zmacs-line-fundamental-mode-p
                            :format zmacs-line-fundamental-mode)
    (text-mode              :mode-p zmacs-line-text-mode-p
                            :format zmacs-line-text-mode)

    ;; hooks only go last
    (ein-notebook-mode      :on-activate zmacs-line-ein-notebook-activate
                            :on-deactivate zmacs-line-ein-notebook-deactivate)
    (esh-mode               :on-activate zmacs-line-esh-activate
                            :on-deactivate zmacs-line-esh-deactivate)
    (ispell-mode            :on-activate zmacs-line-ispell-activate
                            :on-deactivate zmacs-line-ispell-deactivate)
    (mu4e-mode              :on-activate zmacs-line-mu4e-activate
                            :on-deactivate zmacs-line-mu4e-deactivate))
  "Modes to be evalued for modeline.
KEY mode name, for reference only. Easier to do lookups and/or replacements.
:MODE-P the function to check if :FORMAT needs to be used, first one wins.
:ON-ACTIVATE and :ON-DEACTIVATE do hook magic on enabling/disabling the mode.
"
  :type '(alist :key-type symbol
                :value-type (plist :key-type (choice (const :mode-p)
                                                     (const :format)
                                                     (const :on-activate)
                                                     (const :on-deactivate))
                                   :value-type function))
  :group 'zmacs-line
  :tag "Modeline formats")

(defcustom zmacs-line-mode-format-activate-hook nil
  "Add hooks on activation of the mode.
This is for those modes that define their own status-line."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'zmacs-line
  :tag "Format activation hook")

(defcustom zmacs-line-mode-format-deactivate-hook nil
  "Remove hooks on de-activation of the mode.
This is for those modes that define their own status-line."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'zmacs-line
  :tag "Format deactivation hook")

(defcustom zmacs-line-default-mode-format 'zmacs-line-default-mode
  "Default mode to evaluate.
This is if no match could be found in `zmacs-lines-mode-formats'"
  :type 'function
  :group 'zmacs-line
  :tag "Default mode format")

;;;; Faces
;;;;; Line Faces

(defface zmacs-line-active
  '((t (:inherit (mode-line))))
  "Modeline face for active modeline."
  :group 'zmacs-line-active)

(defface zmacs-line-inactive
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive line."
  :group 'zmacs-line-inactive)

(defface zmacs-line-hspace-active
  '((t (:invisible t :family "Monospace" :inherit (mode-line))))
  "Face for vertical spacer in active line.")

(defface zmacs-line-hspace-inactive
  '((t (:invisible t :family "Monospace" :inherit (mode-line-inactive))))
  "Face for vertical spacer in inactive line.")

(defface zmacs-line-active-name
  '((t (:inherit (mode-line))))
  "Modeline face for active name element."
  :group 'zmacs-line-active)

(defface zmacs-line-inactive-name
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive name element."
  :group 'zmacs-line-inactive)

(defface zmacs-line-active-primary
  '((t (:weight light :inherit (mode-line))))
  "Modeline face for active primary element."
  :group 'zmacs-line-active)

(defface zmacs-line-inactive-primary
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive primary element."
  :group 'zmacs-line-inactive)

(defface zmacs-line-active-secondary
  '((t (:inherit mode-line)))
  "Modeline face for active secondary element."
  :group 'zmacs-line-active)

(defface zmacs-line-inactive-secondary
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive secondary element."
  :group 'zmacs-line-inactive)

(defface zmacs-line-active-tertiary
  '((t (:inherit mode-line)))
  "Modeline face for active tertiary element."
  :group 'zmacs-line-active)

(defface zmacs-line-inactive-tertiary
  '((t (:inherit (mode-line-inactive))))
  "Modeline face for inactive tertiary element."
  :group 'zmacs-line-inactive)


;;;;; Status Bar Faces

;; zmacs-line uses a colored symbol to indicate the status of the buffer

(defface zmacs-line-active-status-RO
  '((t (:inherit (mode-line) :foreground "yellow" (when zmacs-line-status-invert :inverse-video t))))
  "Modeline face for active READ-ONLY element."
  :group 'zmacs-line-active)

(defface zmacs-line-inactive-status-RO
  '((t (:inherit (mode-line-inactive) :foreground "light gray" (when zmacs-line-status-invert :inverse-video t))))
  "Modeline face for inactive READ-ONLY element."
  :group 'zmacs-line-inactive)

(defface zmacs-line-active-status-RW
  '((t (:inherit (mode-line) :foreground "green" (when zmacs-line-status-invert :inverse-video t))))
  "Modeline face for active READ-WRITE element."
  :group 'zmacs-line-active)

(defface zmacs-line-inactive-status-RW
  '((t (:inherit (mode-line-inactive) :foreground "light gray"  (when zmacs-line-status-invert :inverse-video t))))
  "Modeline face for inactive READ-WRITE element."
  :group 'zmacs-line-inactive)

(defface zmacs-line-active-status-MD
  '((t (:inherit (mode-line) :foreground "red" (when zmacs-line-status-invert :inverse-video t))))
  "Modeline face for active MODIFIED element."
  :group 'zmacs-line-active)

(defface zmacs-line-inactive-status-MD
  '((t (:inherit (mode-line-inactive) :foreground "light gray"  (when zmacs-line-status-invert :inverse-video t))))
  "Modeline face for inactive MODIFIED element."
  :group 'zmacs-line-inactive)


;;;;; Bell Faces

(defface zmacs-line-visual-bell '((t (:background "red3")))
  "Face to use for the mode-line when `zmacs-line-visual-bell-config' is used."
  :group 'zmacs-line
  :tag "Visual bell face")

;;;; Setup Functions

;;;;; Visual bell for mode line

;; See https://github.com/hlissner/emacs-doom-themes for the basic idea

(defun zmacs-line-visual-bell-fn ()
  "Blink the status-line red briefly. Set `ring-bell-function' to this to use it."
  (let ((zmacs-line--bell-cookie (if (eq zmacs-line-position 'bottom)
                                      (face-remap-add-relative 'mode-line 'zmacs-line-visual-bell)
                                    (face-remap-add-relative 'header-line 'zmacs-line-visual-bell)))
        (force-mode-line-update t))
    (run-with-timer 0.15 nil
                    (lambda (cookie buf)
                      (with-current-buffer buf
                        (face-remap-remove-relative cookie)
                        (force-mode-line-update t)))
                    zmacs-line--bell-cookie
                    (current-buffer))))

(defun zmacs-line-visual-bell-config ()
  "Enable flashing the status-line on error."
  (setq ring-bell-function #'zmacs-line-visual-bell-fn
        visible-bell t))

;;;;; Abbreviate Major-Mode
;; Source: https://www.masteringemacs.org/article/hiding-replacing-modeline-strings

(defcustom zmacs-line-abbrev-alist
  `((dired-mode . "Dir")
    (emacs-lisp-mode . "λ")
    (fundamental-mode . "F")
    (helpful-mode . "?")
    (help-mode . "?")
    (lisp-interaction-mode . "λΙ")
    (markdown-mode . "MD")
    (magit-mode . "MG")
    (nxhtml-mode . "NX")
    (prog-mode . "PR")
    (python-mode . "PY")
    (text-mode . "TX"))
  "Alist for `zmacs-line--abbrev'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *as substitute for* the original."
  :type '(alist
          :key-type (symbol :tag "Major mode")
          :value-type (string :tag "Abbreviation"))
  :group 'zmacs-line)

(defun zmacs-line--abbrev ()
  (cl-loop for abbrev in zmacs-line-abbrev-alist
           do (let* ((mode (car abbrev))
                     (mode-str (cdr abbrev))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))

;; Set abbrev (default is nil)
(when zmacs-line-abbrev
  (add-hook 'after-change-major-mode-hook #'zmacs-line--abbrev))

;;;;; Mode Name
(defun zmacs-line-user-mode-p ()
  "Should the user supplied mode be called for modeline?"
  zmacs-line-user-mode)

(defun zmacs-line-mode-name ()
  "Return current major mode name."
  (format-mode-line mode-name))

;;;;; String Truncate
(defun zmacs-line-truncate (str size &optional ellipsis)
  "If STR is longer than SIZE, truncate it and add ELLIPSIS."

  (let ((ellipsis (or ellipsis "…")))
    (if (> (length str) size)
        (format "%s%s" (substring str 0 (- size (length ellipsis))) ellipsis)
      str)))

;;;;; Branch display
;; -------------------------------------------------------------------
(defun zmacs-line-project-name ()
  "Return name of project without path."
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

(defun zmacs-line-vc-project-branch ()
  "Show project and branch name for file.
Otherwise show '-'."
  (let ((backend (vc-backend buffer-file-name)))
    (concat
     (if buffer-file-name
         (if vc-mode
             (let ((project-name (zmacs-line-project-name)))
               ;; Project name
               (unless (string= "-" project-name)
                 (concat
                  ;; Divider
                  (propertize " •" 'face `(:inherit fringe))
                  (format " %s" project-name))))))

     ;; Show branch
     (if vc-mode
         (concat " "
                 zmacs-line-vc-symbol
                 " "
                 (substring-no-properties vc-mode ;    
                                          (+ (if (eq backend 'Hg)
                                                 2
                                               3)
                                             2)))
       nil))))

;;;;; Dir display
;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
(defun zmacs-line-shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;;;;; Git diff in modeline
;; https://cocktailmake.github.io/posts/emacs-modeline-enhancement-for-git-diff/
(when zmacs-line-git-diff-mode-line
  (defadvice vc-git-mode-line-string (after plus-minus (file) compile activate)
    "Show the information of git diff in status-line"
    (setq ad-return-value
          (concat ad-return-value
                  (let ((plus-minus (vc-git--run-command-string
                                     file "diff" "--numstat" "--")))
                    (if (and plus-minus
                             (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" plus-minus))
                        (concat
                         " "
                         (format "+%s" (match-string 1 plus-minus))
                         (format "-%s" (match-string 2 plus-minus)))
                      ""))))))

;;;;; Git Parse Repo Status
;; See https://kitchingroup.cheme.cmu.edu/blog/2014/09/19/A-git-status-Emacs-modeline/
(defun zmacs-line-git-parse-status ()
  "Display the status of the repo."
  (interactive)
  (let ((U 0)   ; untracked files
        (M 0)   ; modified files
        (O 0)   ; other files
        (U-files "")
        (M-files "")
        (O-files ""))
    (dolist (line (split-string
                   (shell-command-to-string "git status --porcelain")
                   "\n"))
      (cond

       ;; ignore empty line at end
       ((string= "" line) nil)

       ((string-match "^\\?\\?" line)
        (setq U (+ 1 U))
        (setq U-files (concat U-files "\n" line)))

       ((string-match "^ M" line)
        (setq M (+ 1 M))
        (setq M-files (concat M-files "\n" line))
        )))

    ;; construct propertized string
    (concat
     (propertize
      (format "M:%d" M)
      'face (if (> M 0)
                'error
              'success)
      'help-echo M-files)
     (propertize "|" 'face 'magit-dimmed)
     (propertize
      (format "?:%d" U)
      'face (if (> U 0)
                'warning
              'success)
      'help-echo U-files))))

;;;;; Flycheck/Flymake Segment
(defvar-local zmacs-line--flycheck-text nil)
(defun zmacs-line--update-flycheck-segment (&optional status)
  "Update `zmacs-line--flycheck-text' against the reported flycheck STATUS."
  (setq zmacs-line--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat zmacs-line-flycheck-label
                                                 (number-to-string sum)
                                                 " ")
                                         'face (if .error
                                                   'error
                                                 'warning))))
                       (propertize "Good " 'face 'success)))
          ('running (propertize "Checking " 'face 'info))
          ('errored (propertize "Error " 'face 'error))
          ('interrupted (propertize "Paused " 'face 'fringe))
          ('no-checker ""))))

(defun zmacs-line-check-syntax ()
  "Display syntax-checking information from flymake/flycheck in the mode-line (if available)."
  (if (and (>= emacs-major-version 28)
           (boundp 'flymake-mode)
           flymake-mode)
      (concat (format-mode-line flymake-mode-line-format) " ")
    zmacs-line--flycheck-text))

(defun zmacs-line-show-func ()
  "Display `which-function-mode' output in mode-line."
  (if (and (boundp 'which-function-mode)
       (default-value 'which-function-mode))
      (concat (format-mode-line which-func-format) " ")
    ""))

;;;;; Display-time-mode
(defun zmacs-line-install-clockface-fonts ()
  "Install ClockFace fonts on the local system.

Thanks to the Doom Emacs project, for the basis of this
cross-platform font dowload/install code."
  (interactive)
  (let ((on-mac     (eq system-type 'darwin))
        (on-linux   (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
        (on-windows (memq system-type '(cygwin windows-nt ms-dos)))
        (name "ClockFace")
        (url-format "https://ocodo.github.io/ClockFace-font/%s")
        (fonts-list '("ClockFace-Regular.ttf"
                      "ClockFaceRect-Regular.ttf"
                      "ClockFaceSolid-Regular.ttf"
                      "ClockFaceRectSolid-Regular.ttf")))
    (unless (yes-or-no-p
             (format
              "Download%sthe ClockFace fonts, continue?"
              (if on-windows
                  " "
                " and install ")))
      (user-error "Aborted Download of ClockFace fonts"))
    (let* ((font-dest
            (cond (on-linux
                   (expand-file-name
                    "fonts/" (or (getenv "XDG_DATA_HOME")
                                 "~/.local/share")))
                  (on-mac
                   (expand-file-name "~/Library/Fonts/"))))
           (known-dest-p (stringp font-dest))
           (font-dest (or font-dest (read-directory-name "Font installation directory: " "~/"))))
      (unless (file-directory-p font-dest)
        (mkdir font-dest t))
      (dolist (font fonts-list)
        (url-copy-file (format url-format font)
                       (expand-file-name font font-dest)
                       t))
      (when known-dest-p
        (message "Font downloaded, updating font cache... Using <fc-cache -f -v> ")
        (shell-command-to-string "fc-cache -f -v"))
      (if on-windows
          (when (y-or-n-p "The %S font was downloaded, Windows users must install manually.\n\nOpen windows explorer?")
            (call-process "explorer.exe" nil nil nil font-dest))
        (message "Successfully %s %S fonts to %S!"
                 (if known-dest-p
                     "installed"
                   "downloaded")
                 name font-dest)))))

(defun zmacs-line-clockface-select-font ()
  "Select clockface icon font."
  (interactive)
  (let ((font (completing-read
               "Select clockface icon font: "
               '("ClockFace"
                 "ClockFaceSolid"
                 "ClockFaceRect"
                 "ClockFaceRectSolid"))))
    (zmacs-line-clockface-update-fontset font)))

(defun zmacs-line-clockface-update-fontset (&optional font)
  "Use ClockFace font for unicode #xF0000..F008F.
Optionally use another clockface font."
  (if (fboundp 'set-fontset-font)
      (set-fontset-font
       "fontset-default"
       (cons (decode-char 'ucs #xF0000)
             (decode-char 'ucs #xF008F))
       (or font "ClockFace"))
    (or font "ClockFace")))

;; Usage example for testing
;; - exal each one after font installation to test.
;; (uses the complete font name now)
;;
;; [x] (zmacs-line-clockface-update-fontset "ClockFace")
;; [x] (zmacs-line-clockface-update-fontset "ClockFaceRect")
;; [x] (zmacs-line-clockface-update-fontset "ClockFaceRectSolid")
;; [x] (zmacs-line-clockface-update-fontset "ClockFaceSolid")

;; Need to add some note about Doom Emacs font-set modification for the user:
;;
;; E.g.
;;
;; Doom Emacs will reset fontset-default when fonts are resized
;; (e.g. after `doom/increase-font-size' or `doom/decrease-font-size')
;;
;; So it's necessary to use `zmacs-line-clockface-update-fontset' after such events have
;; completed.
;;
;; (I haven't found a working solution, i.e. using the Doom hook `after-setting-font-hook' doesn't work.)

(defun zmacs-line-clockface-icons-unicode (hours minutes)
  "Return ClockFace icon unicode for HOURS and MINUTES."
  (let* ((minute (- minutes (% minutes 5)))
         (offset (round (+ (* (% hours 12) 12) (* 12 (/ minute 60.0))))))
       (+ offset #xF0000)))

(defun zmacs-line-time ()
  "Display the time when `display-time-mode' is non-nil."
  (when display-time-mode
    (let* ((time-unicode
            (cl-destructuring-bind (_ minute hour &rest n) (decode-time)
              (zmacs-line-clockface-icons-unicode hour minute))))
      (concat
        (unless zmacs-line-icon-time
          (if display-time-day-and-date
              (propertize (format-time-string zmacs-line-time-day-and-date-format))
            (propertize (format-time-string zmacs-line-time-format ) 'face `(:height 0.9))))
        (propertize
          (format zmacs-line-time-icon-format (char-to-string time-unicode)
           'display '(raise 0)))))))

;;;;; Status
(defun zmacs-line-status ()
  "Return buffer status, one of 'read-only, 'modified or 'read-write."

  (let ((read-only  (when (not (or (derived-mode-p 'vterm-mode)
                                   (derived-mode-p 'term-mode)
                                   (derived-mode-p 'Info-mode)
                                   (derived-mode-p 'help-mode)
                                   (derived-mode-p 'helpful-mode)
                                   (derived-mode-p 'elfeed-search)
                                   (derived-mode-p 'elfeed-show)))
                      buffer-read-only))
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  'modified)
          (read-only 'read-only)
          (t         'read-write))))

;;;;; Compose Status-Line
(defun zmacs-line-compose (status name primary tertiary secondary)
  "Compose a string with provided information.
Each section is first defined, along with a measure of the width of the status-line.
STATUS, NAME, PRIMARY, and SECONDARY are always displayed. TERTIARY is displayed only in some modes."
  (let* ((window (get-buffer-window (current-buffer)))

         (name-max-width (max 12
                              (- (window-body-width)
                                 (round (* 0.8 (length primary)))
                                 (length tertiary)
                                 (length secondary))))

         (name (if (and (stringp name) (> (length name) name-max-width))
                   (format "…%s" (substring name (- (length name) name-max-width -1)))
                 name))

         (status (or status (zmacs-line-status)))

         (active (eq window zmacs-line--selected-window))

         (prefix (cond ((eq zmacs-line-prefix nil) "")
                       (t
                        (cond ((derived-mode-p 'term-mode) " >_")
                              ((derived-mode-p 'vterm-mode) " >_")
                              ((derived-mode-p 'eshell-mode) " λ:")
                              ((derived-mode-p 'Info-mode) " ℹ")
                              ((derived-mode-p 'help-mode) " ?")
                              ((derived-mode-p 'helpful-mode) " ?")
                              ((eq status 'read-only)
                               (if (display-graphic-p) zmacs-line-gui-ro-symbol
                                 zmacs-line-tty-ro-symbol))
                              ((eq status 'read-write) (if (display-graphic-p) zmacs-line-gui-rw-symbol
                                                         zmacs-line-tty-rw-symbol))
                              ((eq status 'modified)   (if (display-graphic-p) zmacs-line-gui-mod-symbol
                                                         zmacs-line-tty-mod-symbol))
                              ((window-dedicated-p) (if (display-graphic-p) " ––" " --"))
                              ;; otherwise just use rw symbol
                              (t (if (display-graphic-p) zmacs-line-gui-rw-symbol
                                   zmacs-line-tty-rw-symbol))))))

         (face-modeline (if active
                            'zmacs-line-active
                          'zmacs-line-inactive))
         (face-prefix (if (not prefix) face-modeline
                        (if active
                            (cond ((eq status 'read-only)  'zmacs-line-active-status-RO)
                                  ((eq status 'read-write) 'zmacs-line-active-status-RW)
                                  ((eq status 'modified)   'zmacs-line-active-status-MD)
                                  ((or (derived-mode-p 'term-mode)
                                       (derived-mode-p 'vterm-mode)
                                       (derived-mode-p 'eshell-mode))
                                   'zmacs-line-active-status-MD)
                                  ((or (derived-mode-p 'Info-mode)
                                       (derived-mode-p 'help-mode)
                                       (derived-mode-p 'helpful-mode))
                                   'zmacs-line-active-status-RO)
                                  (t                       'zmacs-line-active))
                          (cond ((eq status 'read-only)  'zmacs-line-inactive-status-RO)
                                ((eq status 'read-write) 'zmacs-line-inactive-status-RW)
                                ((eq status 'modified)   'zmacs-line-inactive-status-MD)
                                ((or (derived-mode-p 'term-mode)
                                     (derived-mode-p 'vterm-mode)
                                     (derived-mode-p 'eshell-mode)
                                     (derived-mode-p 'Info-mode)
                                     (derived-mode-p 'help-mode)
                                     (derived-mode-p 'helpful-mode))
                                 'zmacs-line-inactive-status-RW)
                                (t                       'zmacs-line-inactive)))))
         (face-name (if active
                        'zmacs-line-active-name
                      'zmacs-line-inactive-name))
         (face-primary (if active
                           'zmacs-line-active-primary
                         'zmacs-line-inactive-primary))
         (face-secondary (if active
                             'zmacs-line-active-secondary
                           'zmacs-line-inactive-secondary))
         (face-tertiary (if active
                            'zmacs-line-active-tertiary
                          'zmacs-line-inactive-tertiary))
         (left
          ;; special face for special mode prefixes
          (concat
           (propertize prefix 'face face-prefix 'display `(raise ,zmacs-line-symbol-position))
           ;; this matters for inverse-video!
           (propertize " " 'face face-prefix  'display `(raise ,zmacs-line-space-top))

           (when zmacs-line-prefix-padding
             (propertize " " 'face face-modeline))

           (propertize name 'face face-name)

           (propertize " "  'face (if active 'zmacs-line-active
                                    'zmacs-line-inactive)
                       'display `(raise ,zmacs-line-space-bottom))

           (propertize primary 'face face-primary)))

         (right (concat tertiary (propertize secondary 'face face-secondary) (propertize  zmacs-line-hspace 'face face-modeline)))

         (right-len (length (format-mode-line right))))
    (concat
     left
     (propertize " " 'face face-modeline 'display `(space :align-to (- right ,right-len)))
     right)))

;;;; Mode Functions
;;;; Default display
(defun zmacs-line-default-mode ()
  "Compose the default status line."
  (let ((buffer-name (format-mode-line (if buffer-file-name
                                           (file-name-nondirectory (buffer-file-name))
                                         "%b")))
        (mode-name   (zmacs-line-mode-name))
        (branch      (zmacs-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (zmacs-line-compose (zmacs-line-status)
                         (zmacs-line-truncate buffer-name zmacs-line-truncate-value)
                         (concat zmacs-line-display-group-start
                                 mode-name
                                 (when branch
                                   branch)
                                 zmacs-line-display-group-end)
                         ""
                         ;; Narrowed buffer
                         (concat (if (buffer-narrowed-p)
                                     (concat
                                      (propertize "⇥ "  'face `(:inherit zmacs-line-inactive-secondary))
                                      position " ")
                                   position)
                                 (zmacs-line-time)))))

;;;;; Prog Mode
;; ---------------------------------------------------------------------
(defun zmacs-line-prog-mode-p ()
  (derived-mode-p 'prog-mode))

(defun zmacs-line-prog-mode ()
  (let ((buffer-name (format-mode-line (if buffer-file-name (file-name-nondirectory (buffer-file-name)) "%b")))
        (mode-name   (zmacs-line-mode-name))
        (branch      (zmacs-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (zmacs-line-compose (zmacs-line-status)
                         (zmacs-line-truncate buffer-name zmacs-line-truncate-value)
                         (concat zmacs-line-display-group-start mode-name
                                 (when branch branch)
                                 zmacs-line-display-group-end)

                         (concat
                          (if zmacs-line-which-func
                              (zmacs-line-show-func) "")
                          (if zmacs-line-syntax
                              (zmacs-line-check-syntax) ""))

                         (concat
                          ;; Narrowed buffer
                          (when (buffer-narrowed-p)
                            (propertize "⇥ "  'face `(:inherit zmacs-line-inactive-secondary)))
                          (if zmacs-line-syntax
                              (if (or (boundp 'flycheck-mode)
                                      (boundp 'flymake-mode))
                                  ;; (concat position zmacs-line-hspace)
                                  position)
                            position)

                          (zmacs-line-time)))))

(defun zmacs-line-prog-activate ()
  "Setup flycheck hooks."
  (add-hook 'flycheck-status-changed-functions #'zmacs-line--update-flycheck-segment)
  (add-hook 'flycheck-mode-hook #'zmacs-line--update-flycheck-segment)
  (when zmacs-line-git-diff-mode-line
    (add-hook 'after-save-hook #'vc-refresh-state)))

(defun zmacs-line-prog-deactivate ()
  "Remove flycheck hooks."
  (remove-hook 'flycheck-status-changed-functions #'zmacs-line--update-flycheck-segment)
  (remove-hook 'flycheck-mode-hook #'zmacs-line--update-flycheck-segment)
  (when zmacs-line-git-diff-mode-line
    (remove-hook 'after-save-hook #'vc-refresh-state)))

;;;;; Fundamental Mode

(defun zmacs-line-fundamental-mode-p ()
  (derived-mode-p 'fundamental-mode))

(defun zmacs-line-fundamental-mode ()
  (zmacs-line-default-mode))

;;;;; Text Mode

(defun zmacs-line-text-mode-p ()
  (derived-mode-p 'text-mode))

(defun zmacs-line-text-mode ()
  (zmacs-line-default-mode))


;;;;; Help (& Helpful) Mode
(defun zmacs-line-help-mode-p ()
  (derived-mode-p 'help-mode))

(defun zmacs-line-helpful-mode-p ()
  (derived-mode-p 'helpful-mode))

(defun zmacs-line-help-mode ()
  (zmacs-line-compose "HELP"
                       (format-mode-line "%b")
                       ""
                       ""
                       (format-mode-line "%l:%c:%o")))


;;;;; Info Display
;; ---------------------------------------------------------------------
(defun zmacs-line-info-breadcrumbs ()
  (let ((nodes (Info-toc-nodes Info-current-file))
        (cnode Info-current-node)
        (node Info-current-node)
        (crumbs ())
        (depth Info-breadcrumbs-depth)
        line)
    (save-excursion
      (while  (> depth 0)
        (setq node (nth 1 (assoc node nodes)))
        (if node (push node crumbs))
        (setq depth (1- depth)))
      (setq crumbs (cons "Top" (if (member (pop crumbs) '(nil "Top"))
                                   crumbs (cons nil crumbs))))
      (forward-line 1)
      (dolist (node crumbs)
        (let ((text
               (if (not (equal node "Top")) node
                 (format "%s"
                         (if (stringp Info-current-file)
                             (file-name-sans-extension
                              (file-name-nondirectory Info-current-file))
                           Info-current-file)))))
          (setq line (concat line (if (null line) "" " > ")
                             (if (null node) "..." text)))))
      (if (and cnode (not (equal cnode "Top")))
          (setq line (concat line (if (null line) "" " > ") cnode)))
      line)))

(defun zmacs-line-info-mode-p ()
  (derived-mode-p 'Info-mode))

(defun zmacs-line-info-mode ()
  (zmacs-line-compose "INFO"
                       ""
                       (concat zmacs-line-display-group-start
                               (zmacs-line-info-breadcrumbs)
                               zmacs-line-display-group-end)
                       ""
                       ""
                       ))

(defun zmacs-line-info-activate ()
  (if (eq zmacs-line-position 'top)
      (setq Info-use-header-line nil)))

(defun zmacs-line-info-deactivate ()
  (custom-reevaluate-setting 'Info-use-header-line))

;;;; Term & Vterm
;; ---------------------------------------------------------------------
;; term
(defun zmacs-line-term-mode-p ()
  (derived-mode-p 'term-mode))

;; vterm
(defun zmacs-line-vterm-mode-p ()
  (derived-mode-p 'vterm-mode))

(defun zmacs-line-term-mode ()
  (zmacs-line-compose " >_ "
                       "Terminal"
                       (concat zmacs-line-display-group-start
                               (file-name-nondirectory shell-file-name)
                               zmacs-line-display-group-end)
                       nil
                       (concat (zmacs-line-shorten-directory default-directory 32)
                               (zmacs-line-time))))


;; ---------------------------------------------------------------------

(defun zmacs-line-get-ssh-host (_str)
  (let ((split-defdir (split-string default-directory)))
    (if (equal (length split-defdir) 1)
        (car (split-string (shell-command-to-string "hostname") "\n"))
      (cadr split-defdir))))

(defun zmacs-line-ssh-mode ()
  (zmacs-line-compose " >_ "
                       "Terminal"
                       (concat zmacs-line-display-group-start
                               (zmacs-line-get-ssh-host default-directory)
                               zmacs-line-display-group-end)
                       nil
                       (concat (zmacs-line-shorten-directory (car (last (split-string default-directory ":"))) 32)
                               (zmacs-line-time))))

;;;; Eshell
;; ---------------------------------------------------------------------
(defun zmacs-line-eshell-mode-p ()
  (derived-mode-p 'eshell-mode))

(defun zmacs-line-eshell-mode ()
  (zmacs-line-compose " >_ "
                       "Eshell"
                       (concat zmacs-line-display-group-start
                               (buffer-name)
                               zmacs-line-display-group-end)
                       ""
                       (concat (zmacs-line-shorten-directory default-directory 32)
                               (zmacs-line-time))))

(defun zmacs-line-esh-activate ()
  (with-eval-after-load 'esh-mode
    (setq eshell-status-in-mode-line nil)))

(defun zmacs-line-esh-deactivate ()
  (custom-reevaluate-setting 'eshell-status-in-mode-line))

;;;; Messages Buffer Mode
;; ---------------------------------------------------------------------
(defun zmacs-line-messages-mode-p ()
  (derived-mode-p 'messages-buffer-mode))

(defun zmacs-line-messages-mode ()
  (zmacs-line-compose (zmacs-line-status)
                       "*Messages*"
                       ""
                       ""
                       (concat "" (zmacs-line-time))))

;;;; Message Mode
;; ---------------------------------------------------------------------
(defun zmacs-line-message-mode-p ()
  (derived-mode-p 'message-mode))

(defun zmacs-line-message-mode ()
  (zmacs-line-compose (zmacs-line-status)
                       "Message" "(Draft)" nil (zmacs-line-time)))

;;;; Docview Mode
;;---------------------------------------------------------------------
(defun zmacs-line-doc-view-mode-p ()
  (derived-mode-p 'doc-view-mode))

(defun zmacs-line-doc-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (zmacs-line-mode-name))
        (branch      (zmacs-line-vc-project-branch))
        (page-number (concat
                          (number-to-string (doc-view-current-page)) "/"
                          (or (ignore-errors
                                    (number-to-string (doc-view-last-page-number)))
                              "???"))))
    (zmacs-line-compose
     (zmacs-line-status)
     buffer-name
     (concat zmacs-line-display-group-start mode-name
             branch
             zmacs-line-display-group-end)
     nil
     (concat page-number
             (zmacs-line-time)))))

;;;; PDF View Mode
;; ---------------------------------------------------------------------
(defun zmacs-line-pdf-view-mode-p ()
  (derived-mode-p 'pdf-view-mode))

(with-eval-after-load 'pdf-tools
  (require 'pdf-view))

(defun zmacs-line-pdf-view-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (zmacs-line-mode-name))
        (branch      (zmacs-line-vc-project-branch))
        (page-number (concat
                      (number-to-string (eval `(pdf-view-current-page))) "/"
                      (or (ignore-errors
                            (number-to-string (pdf-cache-number-of-pages)))
                          "???"))))
    (zmacs-line-compose (zmacs-line-status)
                         buffer-name
                         (concat zmacs-line-display-group-start mode-name
                                 zmacs-line-display-group-end)
                         nil
                         (concat page-number " " (zmacs-line-time)))))

;;;; MenuMode

(defun zmacs-line-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun zmacs-line-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (zmacs-line-mode-name))
        (position    (format-mode-line "%l:%c:%o")))

    (zmacs-line-compose (zmacs-line-status)
                         buffer-name "" nil (concat position zmacs-line-hspace (zmacs-line-time)))))

;;;; Imenu-List
(defun zmacs-line-imenu-list-mode-p ()
  (derived-mode-p 'imenu-list-major-mode))

(defun zmacs-line-imenu-list-mode ()
  (let (
        ;; We take into account the case of narrowed buffers
        (buffer-name (buffer-name imenu-list--displayed-buffer))
        (branch      (zmacs-line-vc-project-branch))
        (position    (format-mode-line "%l:%c")))
    (zmacs-line-compose (zmacs-line-status)
                         buffer-name
                         "(imenu list)"
                         ""
                         "")))
;;;; Completion
;; ---------------------------------------------------------------------
(defun zmacs-line-completion-list-mode-p ()
  (derived-mode-p 'completion-list-mode))

(defun zmacs-line-completion-list-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (zmacs-line-mode-name))
        (position    (format-mode-line "%l:%c:%o")))

    (zmacs-line-compose (zmacs-line-status)
                         buffer-name "" nil (concat position zmacs-line-hspace))))

;;;; Deft Mode

(with-eval-after-load 'deft
  (defun zmacs-line--deft-print-header ()
    (force-mode-line-update)
    (widget-insert "\n")))

(defun zmacs-line-deft-mode-p ()
  (derived-mode-p 'deft-mode))

(defun zmacs-line-deft-mode ()
  (let ((prefix " DEFT ")
        (primary "Search:")
        (filter  (if deft-filter-regexp
                     (deft-whole-filter-regexp) "<filter>"))
        (matches (if deft-filter-regexp
                     (format "%d matches" (length deft-current-files))
                   (format "%d notes" (length deft-all-files)))))
    (zmacs-line-compose prefix primary filter nil matches)))

;;;; Calendar Mode
;; ---------------------------------------------------------------------
(defun zmacs-line-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun zmacs-line-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun zmacs-line-calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default)))))

(defun zmacs-line-calendar-activate ()
  (with-eval-after-load 'calendar
    (add-hook 'calendar-initial-window-hook
              #'zmacs-line-calendar-setup-header)))

(defun zmacs-line-calendar-deactivate ()
  (remove-hook 'calendar-initial-window-hook
               #'zmacs-line-calendar-setup-header))

;;;; Org Capture
;; ---------------------------------------------------------------------
(defun zmacs-line-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun zmacs-line-org-capture-mode ()
  (zmacs-line-compose (zmacs-line-status)
                       "Capture"
                       (concat zmacs-line-display-group-start
                               (org-capture-get :description)
                               zmacs-line-display-group-end)
                       nil
                       "Finish: C-c C-c, refile: C-c C-w, cancel: C-c C-k "))


(defun zmacs-line-org-capture-turn-off-header-line ()
  (setq-local header-line-format (default-value 'header-line-format))
  (message nil))

(defun zmacs-line-org-capture-activate ()
  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook
              #'zmacs-line-org-capture-turn-off-header-line)))

(defun zmacs-line-org-capture-deactivate ()
  (remove-hook 'org-capture-mode-hook
               #'zmacs-line-org-capture-turn-off-header-line))


;;;; Org Agenda
;; ---------------------------------------------------------------------
(defun zmacs-line-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun zmacs-line-org-agenda-mode ()
  (let ((zmacs-line-icon-time t))
    (zmacs-line-compose (zmacs-line-status)
                         "Agenda"
                         (concat zmacs-line-display-group-start (format "%S" org-agenda-current-span) zmacs-line-display-group-end)
                         ""
                         (concat (format-time-string "%A, %d %B %Y")
                                 (zmacs-line-time)
                                 (format-time-string " %H:%M")))))

;;;; Org Clock
;; ---------------------------------------------------------------------
(defun zmacs-line-org-clock-mode-p ()
  (and (boundp 'org-mode-line-string)
       (stringp org-mode-line-string)))

(defun zmacs-line-org-clock-mode ()
  (let ((buffer-name (format-mode-line "%b"))
        (mode-name   (zmacs-line-mode-name))
        (branch      (zmacs-line-vc-project-branch))
        (position    (format-mode-line "%l:%c:%o")))
    (zmacs-line-compose (zmacs-line-status)
                         buffer-name
                         (concat zmacs-line-display-group-start
                                 mode-name
                                 (when branch
                                   branch)
                                 zmacs-line-display-group-end)
                         ""
                         (concat
                          ;; Narrowed buffer
                          (when (buffer-narrowed-p)
                            (propertize "⇥ "  'face `(:inherit zmacs-line-inactive-secondary)))
                          org-mode-line-string
                          " "
                          nil
                          position
                          zmacs-line-hspace))))

(defun zmacs-line-org-clock-out ()
  (setq org-mode-line-string nil)
  (force-mode-line-update))

(defun zmacs-line-org-clock-activate ()
  (with-eval-after-load 'org-clock
    (add-hook 'org-clock-out-hook #'zmacs-line-org-clock-out)))

(defun zmacs-line-org-clock-deactivate ()
  (remove-hook 'org-clock-out-hook
               #'zmacs-line-org-clock-out))

;;;; Elfeed
;; ---------------------------------------------------------------------
(defun zmacs-line-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun zmacs-line-elfeed-search-mode ()
  (let* ((status  "NEWS")
         (no-database (zerop (elfeed-db-last-update)))
         (update      (> (elfeed-queue-count-total) 0))

         (name  (cond (no-database "No database")
                      (update      "Update:")
                      (t           "Search:")))
         (primary (cond  (no-database "")
                         (update
                          (let ((total (elfeed-queue-count-total))
                                (in-process (elfeed-queue-count-active)))
                            (format "%d jobs pending, %d active"
                                    (- total in-process) in-process)))
                         (t  (let* ((db-time (seconds-to-time (elfeed-db-last-update)))
                                    (unread))
                               (cond (elfeed-search-filter-active "")
                                     ((string-match-p "[^ ]" elfeed-search-filter)
                                      elfeed-search-filter)
                                     (""))))))
         (secondary (concat
                     (cond
                      ((zerop (elfeed-db-last-update)) "")
                      ((> (elfeed-queue-count-total) 0) "")
                      (t (elfeed-search--count-unread)))
                     (zmacs-line-time))))

    (zmacs-line-compose status name primary nil secondary)))

;; Elfeed uses header-line, we need to tell it to use our own format
(defun zmacs-line-elfeed-setup-header ()
  (setq header-line-format (default-value 'header-line-format)))

(defun zmacs-line-elfeed-search-activate ()
  (with-eval-after-load 'elfeed
    (if (eq zmacs-line-position 'top)
        (setq elfeed-search-header-function #'zmacs-line-elfeed-setup-header))))

(defun zmacs-line-elfeed-search-deactivate ()
  (if (boundp 'elfeed-search-header-function)
      (setq elfeed-search-header-function #'elfeed-search--header)))

;; ---------------------------------------------------------------------
(defun zmacs-line-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun zmacs-line-elfeed-show-mode ()
  (let* ((title (elfeed-entry-title elfeed-show-entry))
         (tags (elfeed-entry-tags elfeed-show-entry))
         (tags-str (mapconcat #'symbol-name tags ", "))
         (tag          (if tags
                           (concat zmacs-line-display-group-start
                                   tags-str
                                   zmacs-line-display-group-end)
                         " "))
         (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed (elfeed-entry-feed elfeed-show-entry))
         (entry-author (elfeed-meta elfeed-show-entry :author))
         (feed-title (if entry-author
                         (concat entry-author " (" (elfeed-feed-title feed) ")")
                       (elfeed-feed-title feed))))
    (zmacs-line-compose
     ""
     (zmacs-line-truncate title 65)
     tag
     ""
     (format-time-string "%Y-%m-%d %H:%M:%S" date))))


;;;; Mu4e

(defun zmacs-line-mu4e-last-query ()
  "Get the most recent mu4e query or nil if there is none."
  (if (fboundp 'mu4e-last-query)
      (mu4e-last-query)
    mu4e~headers-last-query))

(defun zmacs-line-mu4e-context ()
  "Return the current mu4e context as a non propertized string."
  (if (> (length (mu4e-context-name (mu4e-context-current))) 0)
      (concat
       zmacs-line-display-group-start
       (substring-no-properties (mu4e-context-name (mu4e-context-current)))
       zmacs-line-display-group-end)
    "(none)"))

(defun zmacs-line-mu4e-server-props ()
  "Encapsulates the call to the variable mu4e-/~server-props
depending on the version of mu4e."
  (if (version< mu4e-mu-version "1.6.0")
      mu4e~server-props
    mu4e--server-props))

(defun zmacs-line-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'zmacs-line)))

(defun zmacs-line-mu4e-deactivate ()
  (advice-remove #'mu4e~header-line-format #'zmacs-line))

;; ---------------------------------------------------------------------
(defun zmacs-line-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))

(defun zmacs-line-mu4e-dashboard-mode ()
  (zmacs-line-compose (zmacs-line-status)
                       (format "%d messages"
                               (plist-get (zmacs-line-mu4e-server-props) :doccount))
                       ""
                       ""
                       (zmacs-line-time)))

;; ---------------------------------------------------------------------
(defun zmacs-line-mu4e-loading-mode-p ()
  (derived-mode-p 'mu4e-loading-mode))

(defun zmacs-line-mu4e-loading-mode ()
  (zmacs-line-compose (zmacs-line-status)
                       (format-time-string "%A %d %B %Y, %H:%M ")
                       ""
                       "Loading..."
                       (zmacs-line-mu4e-context)))

;; ---------------------------------------------------------------------
(defun zmacs-line-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun zmacs-line-mu4e-main-mode ()
  (zmacs-line-compose (zmacs-line-status)
                       (format-time-string "%A %d %B %Y, %H:%M ")
                       ""
                       ""
                       (zmacs-line-mu4e-context)))

;; ---------------------------------------------------------------------
(defun zmacs-line-mu4e-compose-mode-p ()
  (derived-mode-p 'mu4e-compose-mode))

(defun zmacs-line-mu4e-compose-mode ()
  (zmacs-line-compose (zmacs-line-status)
                       (format-mode-line "%b")
                       ""
                       ""
                       (format "[%s] "
                               (zmacs-line-mu4e-quote
                                (mu4e-context-name (mu4e-context-current))))))

;; ---------------------------------------------------------------------
(defun zmacs-line-mu4e-quote (str)
  (if (version< mu4e-mu-version "1.8.0")
      (mu4e~quote-for-modeline str)
    (mu4e-quote-for-modeline str)))

(defun zmacs-line-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun zmacs-line-mu4e-headers-mode ()
  (let ((mu4e-modeline-max-width 80))
    (zmacs-line-compose
     (zmacs-line-status)
     "Search:"
     (or (zmacs-line-mu4e-quote
          (zmacs-line-mu4e-last-query)) "")
     ""
     (concat
      (format "[%s] "
              (zmacs-line-mu4e-quote
               (mu4e-context-name (mu4e-context-current))))
      (or (zmacs-line-time) "")))))

;; ---------------------------------------------------------------------
(defun zmacs-line-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun zmacs-line-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date    (mu4e-message-field msg :date)))
    (zmacs-line-compose (zmacs-line-status)
                         (or from "")
                         (concat zmacs-line-display-group-start
                                 (zmacs-line-truncate (or subject "") 50 "…")
                                 zmacs-line-display-group-end)
                         ""
                         (concat (or (format-time-string mu4e-headers-date-format date) "") " "))))

(defun zmacs-line-mu4e-activate ()
  (with-eval-after-load 'mu4e
    (advice-add 'mu4e~header-line-format :override #'zmacs-line)))

(defun zmacs-line-mu4e-deactivate ()
  (advice-remove #'mu4e~header-line-format #'zmacs-line))

;;;; Ein

(defun zmacs-line-ein-notebook-mode ()
  (let ((buffer-name (format-mode-line "%b")))
    (zmacs-line-compose (if (ein:notebook-modified-p) "MD" "RW")
                         buffer-name
                         ""
                         ""
                         (concat
                          (ein:header-line)
                          (zmacs-line-time)))))

;; since the EIN library itself is constantly re-rendering the notebook, and thus
;; re-setting the header-line-format, we cannot use the zmacs-line function to set
;; the header format in a notebook buffer. Fortunately, EIN exposes the
;; ein:header-line-format variable for just this purpose.

(defun zmacs-line-ein-notebook-activate ()
  (with-eval-after-load 'ein
    (if (eq zmacs-line-position 'top)
        (setq ein:header-line-format '((:eval (zmacs-line-ein-notebook-mode)))))))

(defun zmacs-line-ein-notebook-deactivate ()
  (if (boundp 'ein:header-line-format)
      (setq ein:header-line-format '(:eval (ein:header-line)))))


;;;; Buffer Menu Mode
;; ---------------------------------------------------------------------
(defun zmacs-line-buffer-menu-mode-p ()
  (derived-mode-p 'buffer-menu-mode))

(defun zmacs-line-buffer-menu-mode ()
  (let ((buffer-name "Buffer list")
        (mode-name   (zmacs-line-mode-name))
        (position    (format-mode-line "%l:%c")))

    (zmacs-line-compose nil
                         buffer-name
                         ""
                         nil
                         (concat
                          position
                          (zmacs-line-time)))))

;;(defun buffer-menu-mode-header-line ()
;;  (face-remap-add-relative
;;   'header-line `(:background ,(face-background 'nano-subtle))))
;;(add-hook 'Buffer-menu-mode-hook
;;          #'buffer-menu-mode-header-line)

(defun zmacs-line-buffer-menu-activate ()
  (if (eq zmacs-line-position 'top)
      (setq Buffer-menu-use-header-line nil)))

(defun zmacs-line-buffer-menu-deactivate ()
  (custom-reevaluate-setting 'Buffer-menu-use-header-line))

;;;; Elpher Mode
;; ---------------------------------------------------------------------
(defun zmacs-line-elpher-mode-p ()
  (derived-mode-p 'elpher-mode))

(defun zmacs-line-elpher-mode ()
  (let* ((display-string (elpher-page-display-string elpher-current-page))
         (sanitized-display-string (replace-regexp-in-string "%" "%%" display-string))
         (address (elpher-page-address elpher-current-page))
         (tls-string (if (and (not (elpher-address-about-p address))
                              (member (elpher-address-protocol address)
                                      '("gophers" "gemini")))
                         "(TLS encryption)"
                       "")))
    (zmacs-line-compose nil
                         sanitized-display-string
                         tls-string
                         nil
                         (zmacs-line-time))))

(defun zmacs-line-elpher-activate ()
  (with-eval-after-load 'elpher
    (setq elpher-use-header nil)))

;;;; Ispell Mode
;; ---------------------------------------------------------------------
(defun zmacs-line-enlarge-ispell-choices-buffer (buffer)
  (when (string= (buffer-name buffer) "*Choices*")
    (with-current-buffer buffer
      ;; (enlarge-window +2)
      (setq-local header-line-format nil)
      (setq-local mode-line-format nil))))

(defun zmacs-line-ispell-activate ()
  (with-eval-after-load 'ispell
    (advice-add #'ispell-display-buffer :after
                #'zmacs-line-enlarge-ispell-choices-buffer)))

(defun zmacs-line-ispell-deactivate ()
  (advice-remove #'ispell-display-buffer
                 #'zmacs-line-enlarge-ispell-choices-buffer))

;;;; Eldoc
;; ---------------------------------------------------------------------
;; `eldoc-minibuffer-message' changes `mode-line-format' but status-line when
;; `zmacs-line-position' is `top' fails to display info. Solution is to move
;; eldoc messages to the minibuffer/echo area.
(when (eq zmacs-line-position 'top)
  (setq eldoc-message-function #'message))

;;;; Magit
;; ---------------------------------------------------------------------
(defun zmacs-line-magit-mode-p ()
  (derived-mode-p 'magit-mode))

;; Add functions to parse repo every N seconds
(defvar zmacs-line-git-parse-last-update (float-time) "Last time we updated")
(defvar zmacs-line-git-parse-update-interval 15 "Minimum time between update in seconds")
(defvar zmacs-line-git-parse "" "Last value of the parse")

(defun zmacs-line-magit-mode ()
  (let* ((buffer-name (format-mode-line
                       (if buffer-file-name
                           (file-name-nondirectory (buffer-file-name))
                         "%b")))
         (mode-name   (zmacs-line-mode-name))
         (project     (file-name-nondirectory (directory-file-name (magit-toplevel))))
         (branch      (magit-get-current-branch))
         (status      (zmacs-line-git-parse-status)))
    (zmacs-line-compose (zmacs-line-status)
                         mode-name
                         (concat zmacs-line-display-group-start
                                 project
                                 zmacs-line-vc-symbol
                                 branch
                                 zmacs-line-display-group-end)
                         status
                         "")))


;;;; Setup zmacs-line
;; ---------------------------------------------------------------------
(defun zmacs-line-face-clear (face)
  "Clear FACE"
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    'unspecified))

;; ---------------------------------------------------------------------
(defvar zmacs-line--saved-mode-line-format nil)
(defvar zmacs-line--saved-header-line-format nil)
(defvar zmacs-line--selected-window nil)

(defun zmacs-line--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq zmacs-line--selected-window (selected-window)))

(defun zmacs-line ()
  "Build and set the modeline."
  (let* ((format
          '((:eval
             (funcall
              (or (catch 'found
                    (dolist (elt zmacs-line-mode-formats)
                      (let* ((config (cdr elt))
                             (mode-p (plist-get config :mode-p))
                             (format (plist-get config :format)))
                        (when mode-p
                          (when (funcall mode-p)
                            (throw 'found format))))))
                  zmacs-line-default-mode-format))))))
    (if (eq zmacs-line-position 'top)
        (progn
          (setq header-line-format format)
          (setq-default header-line-format format))
      (progn
        (setq mode-line-format format)
        (setq-default mode-line-format format)))))

(defun zmacs-line-update-windows ()
  "Hide the mode line depending on the presence of a window
below or a buffer local variable 'no-mode-line'."
  (dolist (window (window-list))
    (with-selected-window window
      (with-current-buffer (window-buffer window)
        (if (or (not (boundp 'no-mode-line)) (not no-mode-line))
            (setq mode-line-format
                  (cond ((one-window-p t) (list ""))
                        ((eq (window-in-direction 'below) (minibuffer-window)) (list ""))
                        ((not (window-in-direction 'below)) (list ""))
                        (t nil))))))))

(defun zmacs-line-mode--activate ()
  "Activate zmacs-line."

  ;; Save current mode-line and header-line
  (unless zmacs-line--saved-mode-line-format
    (setq zmacs-line--saved-mode-line-format mode-line-format)
    (setq zmacs-line--saved-header-line-format header-line-format))

  (dolist (elt zmacs-line-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-activate)))
      (when fn (funcall fn))))

  (run-hooks 'zmacs-line-mode-format-activate-hook)

  ;; Update selected window
  (zmacs-line--update-selected-window)
  ;; (setq zmacs-line--selected-window (selected-window))

  (setq         mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq         header-line-format nil)
  (setq-default header-line-format nil)

  (zmacs-line)

  ;; Use zmacs-line-visual-bell when var is set to t
  (when zmacs-line-visual-bell
    (zmacs-line-visual-bell-config))

  ;; This hooks is necessary to register selected window because when
  ;;  a modeline is evaluated, the corresponding window is always selected.
  (add-hook 'post-command-hook #'zmacs-line--update-selected-window)

  ;; This hooks hide the modeline for windows having a window below them
  ;; Disabled for the time being,
  ;;  -> see https://github.com/rougier/nano-modeline/issues/24
  ;; (add-hook 'window-configuration-change-hook #'zmacs-line-update-windows)

  (force-mode-line-update t))

;; Deactivate status-line
(defun zmacs-line-mode--deactivate ()
  "Deactivate zmacs-line and restore default mode-line."

  (dolist (elt zmacs-line-mode-formats)
    (let* ((config (cdr elt))
           (fn (plist-get config :on-deactivate)))
      (when fn (funcall fn))))

  (run-hooks 'zmacs-line-mode-format-deactivate-hook)

  (remove-hook 'post-command-hook
               #'zmacs-line--update-selected-window)
  (remove-hook 'window-configuration-change-hook
               #'zmacs-line-update-windows)

  ;; Deactivate zmacs-line-visual-bell
  (setq zmacs-line-visual-bell nil)

  (setq         mode-line-format zmacs-line--saved-mode-line-format)
  (setq-default mode-line-format zmacs-line--saved-mode-line-format)
  (setq         header-line-format zmacs-line--saved-header-line-format)
  (setq-default header-line-format zmacs-line--saved-header-line-format))

;;;; zmacs-line minor mode

;; Store the default mode-line format
(defvar zmacs-line--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode zmacs-line-mode
  "Toggle zmacs-line on or off."
  :group 'zmacs-line
  :global t
  :lighter nil

  (if zmacs-line-mode
      (zmacs-line-mode--activate)
    (zmacs-line-mode--deactivate))

  ;; Run any registered hooks
  (run-hooks 'zmacs-line-mode-hook))

(provide 'zmacs-line)

;;; zmacs-line.el ends here.
