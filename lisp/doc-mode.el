;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; doc-mode.el --- Doxygen mode.
;;;
;;; Time-stamp: <Tuesday Sep  4, 2012 17:23:22 asmodai>
;;; Revision:   3
;;;
;;; Copyright (c) 2012 Paul Ward <asmodai@gmail.com>
;;; Copyright (C) 2007, 2009 Nikolaj Schumacher
;;;
;;; Based on code written by Nikolaj Schumacher <bugs * nschum de>
;;;
;;; Author:     Paul Ward <asmodai@gmail.com>
;;; Maintainer: Paul Ward <asmodai@gmail.com>
;;; Created:    04 Sep 2012 14:45:52
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

(eval-when-compile
  (require 'cl))

(require 'semantic)
(require 'cc-mode)
(require 'newcomment)

(dolist (err `("^No tag found$"
               "^Semantic cannot parse buffer$"
               "^No template found$"
               "^doc-mode not enabled$"))
  (add-to-list 'debug-ignored-errors err))

;;;==================================================================
;;;{{{ Customisation group:

(defgroup doc-mode nil
  "Minor mode for editing in-code documentation for parsing with Doxygen."
  :group 'convenience
  :group 'tools)

(defcustom doc-mode-auto-check-p t
  "*Should the buffer documentation be checked after a Semantic reparse."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On"  t)))

(defcustom doc-mode-template-start "/**"
  "*The string to insert at the beginning of a comment."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-end " */"
  "*The string to insert at the end of a comment."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-continue " * "
  "*The string to insert at the beginning of each line in a comment."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-single-line-start "/** "
  "*The string to insert at the beginning of a single-line comment.
For using single-line comments, see `doc-mode-allow-single-line-comments'."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-single-line-end " */"
  "*The string to insert at the end of a single-line comment.
For using single-line comments, see `doc-mode-allow-single-line-comments'."
  :group 'doc-mode
  :type 'string)

(defcustom doc-mode-template-keyword-char "@"
  "*The character used to begin Doxygen keywords."
  :group 'doc-mode
  :type '(choice (const  :tag "@"     "@")
                 (const  :tag "\\"    "\\")
                 (string :tag "Other")))

(defcustom doc-mode-template-dwim-comment "!<"
  "*The string to use when documenting end-of-line comments."
  :group 'doc-mode
  :type '(choice (const  :tag "!<")
                 (string :tag "Other")))

(defcustom doc-mode-template-empty-line-after-summary nil
  "*Whether to put an empty line after the first one in the comment."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On"  t)))

(defcustom doc-mode-template-empty-line-before-keywords nil
  "*Whether to put an empty line before the keyword list in a
comment."
  :group 'doc-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On"  nil)))

(defcustom doc-mode-template-keywords
  '("brief" "deprecated" "tparam" "param" "returns" "return" "author"
    "exception" "throws" "version" "since" "overload" "see" "sa" "todo"
    "warning" "note" "details")
  "*Keywords that should be listed in this order.
All other keywords will be considered regular text."
  :group 'doc-mode
  :type '(repeat string))

(defcustom doc-mode-align-keyword-arguments t
  "*Whether to align the arguments to a keyword continued in the next line.
This may also be a number, describing how far to indent the argument list."
  :group 'doc-mode
  :type '(choice (const   :tag "Off"    nil)
                 (integer :tag "Indent" nil)
                 (const   :tag "On"     t)))

(defcustom doc-mode-fill-column 78
  "*The column at which to break text when formatting it.
If this is nil, `comment-fill-column' is used."
  :group 'doc-mode
  :type '(choice (const   :tag "Default"     nil)
                 (integer :tag "Fill Column")))

(defcustom doc-mode-keywords-from-tag-func 'doc-mode-keywords-from-tag
  "*Function used to generate keywords for a tag.
This must be a function that takes two arguments.  The first argument is the
Semantic tag for which to generate keywords, the second is a list of existing
keywords taken from the current doc comment.  It should return the new list of
keywords.  Each element in a keyword list can be either a string or a list with
a keyword, optional argument and optional description.  Additional entries with
undetermined content should be created with `doc-mode-new-keyword'."
  :group 'doc-mode
  :type 'function)

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Keywords:

(defconst doc-mode-font-lock-keywords
  (eval-when-compile
    `(;;
      ;; Single-argument keywords.
      (,(concat "[\\@]"
                (regexp-opt
                 '("addindex" "addtogroup" "anchor" "arg" "author"
                   "authors" "brief" "callgraph" "callergraph" "cite"
                   "code" "cond" "copybrief" "copydetails" "copydoc"
                   "copyright" "date" "defgroup" "deprecated"
                   "details" "dir" "dontinclude" "dot" "dotfile"
                   "else" "elseif" "endcode" "endcond" "enddot"
                   "endhtmlonly" "endif" "endinternal" "endlatexonly"
                   "endlink" "endmanonly" "endmsc" "endrtfonly"
                   "endverbatim" "endxmlonly" "example" "f$" "f[" "f]"
                   "f{" "f}" "file" "fn" "headerfile"
                   "hideinitializer" "htmlinclde" "htmlonly" "if"
                   "ifnot" "image" "include" "includelineno" "ingroup"
                   "internal" "invariant" "latexonly" "li" "line"
                   "link" "mainpage" "manonly" "memberof" "msc"
                   "mscfile" "name" "nosubgrouping" "note" "overload"
                   "p" "package" "page" "par" "paragraph" "post" "pre"
                   "private" "privatesection" "protected"
                   "protectedsection" "public" "publicsection" "ref"
                   "remark" "remarks" "result" "return" "returns"
                   "rtfonly" "sa" "section" "see" "short"
                   "showinitializer" "since" "skip" "skipline"
                   "snippet" "subpage" "subsection" "subsubsection"
                   "tableofcontents" "test" "until" "var" "verbatim"
                   "verbinclude" "version" "weakgroup" "xmlonly"
                   "xrefitem"
                   "$" "@" "\\" "&" "~" "<" ">" "#" "%") t)
                "\\>")
        (0 font-lock-keyword-face prepend))
      ;;
      ;; Ignore \n, it's too common.
      ("@n" (0 font-lock-keyword-face prepend))
      ;;
      ;; Type blocks
      (,(concat "\\([\\@]"
                (regexp-opt '("category" "class" "enum" "typdef" "def"
                              "struct" "exception" "throw" "throws"
                              "extends" "implements" "interface"
                              "property" "union") t)
                "\\)\\>\\(?:[ \t]+\\(\\sw+\\)\\)?")
        (1 font-lock-keyword-face prepend)
        (3 font-lock-type-face prepend))
      ;;
      ;; Param blocks
      (,(concat "\\([\\@]"
                (regexp-opt '("param" "param\[in\]" "param\[out\]"
                              "param\[in,out\]" "tparam" "related"
                              "relates" "relatedalso" "relatesalso") t)
                "\\)\\(?:[ \t]+\\(\\sw+\\)\\)?")
        (1 font-lock-keyword-face prepend)
        (3 font-lock-variable-name-face prepend))
      ;;
      ;; Return value block
      (,(concat "\\([@\\]retval\\)\\>\\(?:[ \t]+\\(\\sw+\\)\\)?")
        (1 font-lock-keyword-face prepend)
        (2 font-lock-function-name-face prepend))
      ;;
      ;; Warning face
      (,(concat "[\\@]"
                (regexp-opt '("attention" "warning" "todo" "bug") t)
                "\\>")
        (0 font-lock-warning-face prepend))
      ;;
      ;; Italics
      ("\\([\\@]em\\|[\\@][ae]\\)[ \t\n]+\\([^ \t\n]+\\)"
        (1 font-lock-keyword-face prepend)
        (2 'italic prepend))
      ;;
      ;; Underline
      ("\\([\\@][cp]\\)[ \t\n]+\\([^ \t\n]+\\)"
       (1 font-lock-keyword-face prepend)
       (2 'underline prepend))
      ;;
      ;; Bold
      ("\\([\\@]b\\)[ \t\n]+\\([^ \t\n]+\\)"
       (1 font-lock-keyword-face prepend)
       (2 'bold prepend)))))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Templates:

(defvar doc-mode-templates nil)

(make-variable-buffer-local 'doc-mode-templates)

(defun doc-mode-add-template (beg end)
  (let ((overlay (make-overlay beg (point))))
    (overlay-put overlay 'intangible t)
    (overlay-put overlay 'face 'highlight)
    (overlay-put overlay 'insert-in-front-hooks
                 '(doc-mode-replace-overlay))
    (overlay-put overlay 'modification-hooks
                 '(doc-mode-delete-overlay))
    (push overlay doc-mode-templates)))

(defvar doc-mode-temp nil)

(defun doc-mode-delete-overlay (ov after-p beg end &optional r)
  (unless after-p
    (mapc 'doc-mode-unfold-by-overlay
          (overlays-in (1- (overlay-start ov))
                       (1+ (overlay-end ov))))
    (delete-overlay ov)
    (setq doc-mode-templates (delq ov doc-mode-templates))))

(defun doc-mode-replace-overlay (ov after-p beg end &optional r)
  (unless after-p
    (let ((inhibit-modification-hooks nil))
      (delete-region (overlay-start ov)
                     (overlay-end ov)))))

;;;###autoload
(defun doc-mode-next-template (&optional pos limit)
  "Jump to the next unfinished documentation template in this buffer."
  (interactive)
  (unless pos
    (setq pos (point)))
  (unless limit
    (setq limit (point-max)))
  (let ((min-start limit)
        start)
    (dolist (ov doc-mode-templates)
      (setq start (overlay-start ov))
      (and (> start pos)
           (< start min-start)
           (setq min-start start)))
    (when (= min-start limit)
      (error "End of buffer"))
    (push-mark)
    (goto-char min-start)))

;;;###autoload
(defun doc-mode-previous-template (&optional pos limit)
  "Jump to the previous unfinished documentation template in this buffer."
  (interactive)
  (unless pos
    (setq pos (point)))
  (unless limit
    (setq limit (point-min)))
  (let ((max-start limit)
        start)
    (dolist (ov doc-mode-templates)
      (setq start (overlay-start ov))
      (and (< start pos)
           (> start max-start)
           (setq max-start start)))
    (when (= max-start limit)
      (error "Beginning of buffer"))
    (push-mark)
    (goto-char max-start)))

;;;###autoload
(defun doc-mode-first-template ()
  "Jump to the first unfinished documentation template in this buffer."
  (interactive)
  (condition-case err
      (doc-mode-next-template (point-min))
    (error (error "No template found"))))

;;;###autoload
(defun doc-mode-build-comment-start ()
  (format "%s%s%s"
          (remove-in-string comment-start " ")
          doc-mode-template-dwim-comment
          (if (string-match " " comment-start)
              " "
              "")))

;;;###autoload
(defun doc-mode-comment-indent (&optional continue)
  (interactive "*")
  (comment-normalize-vars)
  (let* ((empty (save-excursion (beginning-of-line)
                                (looking-at "[ \t]*$")))
         (starter (or (and continue comment-continue)
                      (and empty block-comment-start)
                      (doc-mode-build-comment-start)))
         (ender (or (and continue comment-contine "")
                    (and empty block-comment-end)
                    comment-end)))
    (unless starter
      (error "No comment syntax defined"))
    (beginning-of-line)
    (let* ((eolpos (line-end-position))
           (begpos (comment-search-forward eolpos t))
           cpos
           indent)
      (if begpos
          (setq cpos (point-marker))
          (save-excursion
            (indent-to comment-column)
            (setq begpos (point))
            (insert starter)
            (setq cpos (point-marker))
            (insert ender)))
      (goto-char begpos)
      (setq indent (save-excursion (funcall comment-indent-function)))
      (if (not indent)
          (indent-according-to-mode)
          (unless (save-excursion (skip-chars-backward " \t")
                                  (bolp))
            (setq indent
                  (min indent
                       (+ (current-column)
                          (- fill-column
                             (save-excursion
                               (end-of-line)
                               (current-column)))))))
          (if (= (current-column) indent)
              (goto-char begpos)
              (skip-chars-backwards " \t")
              (delete-region (point) begpos)
              (indent-to (if (bolp)
                             indent
                             (max indent (1+ (current-column)))))))
      (goto-char cpos)
      (set-marker cpos nil))))

;;;###autoload
(defun doc-mode-comment-dwim (arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and mark-active
           transient-mark-mode)
      (let ((beg (min (point) (mark)))
            (end (max (point) (mark))))
        (if (save-excursion
              (goto-char beg)
              (comment-forward (point-max))
              (<= end (point)))
            (uncomment-region beg end arg)
            (comment-region beg end arg)))
      (if (save-excursion
            (beginning-of-line)
            (not (looking-at "\\s-*$")))
          (if arg
              (comment-kill (and (integerp arg) arg))
              (doc-mode-comment-indent))
          (let ((add (if arg
                         (prefix-numeric-value arg)
                         (if (= (length comment-start) 1)
                             comment-add
                             0))))
            (indent-according-to-mode)
            (insert (comment-padright
                     (doc-mode-comment-indent)
                     add))
            (save-excursion
              (unless (string= "" comment-end)
                (insert (comment-padleft comment-end add)))
              (indent-according-to-mode))))))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Mode:

(defvar doc-mode-lighter " doc")

(defvar doc-mode-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";" 'doc-mode-comment-dwim)
    (define-key map "d" 'doc-mode-fix-tag-doc)
    (define-key map "c" 'doc-mode-check-tag-doc)
    (define-key map "r" 'doc-mode-remove-tag-doc)
    (define-key map "i" 'doc-mode-add-tag-doc)
    (define-key map "e" 'doc-mode-next-faulty-doc)
    (define-key map "n" 'doc-mode-next-template)
    (define-key map "\C-c" 'doc-mode-check-buffer)
    map))

(defvar doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-d" doc-mode-prefix-map)
    map)
  "Keymap used for `doc-mode'.")

;;;###autoload
(define-minor-mode doc-mode
  "Minor mode for editing in-code documentation."
  nil doc-mode-lighter doc-mode-map
  (if doc-mode
      (progn
        (font-lock-add-keywords nil doc-mode-font-lock-keywords)
        (when doc-mode-auto-check-p
          (add-hook 'semantic-after-auto-parse-hooks 'doc-mode-check-buffer
                    nil t)
          (add-hook 'semantic-after-idle-scheduler-reparse-hooks
                    'doc-mode-check-buffer nil t)))
    (dolist (ov doc-mode-templates)
      (delete-overlay ov))
    (kill-local-variable 'doc-mode-templates)
    (doc-mode-unfold-all)
    (font-lock-remove-keywords nil doc-mode-font-lock-keywords)
    (remove-hook 'semantic-after-auto-parse-hooks 'doc-mode-check-buffer t)
    (remove-hook 'semantic-after-idle-scheduler-reparse-hooks
                 'doc-mode-check-buffer t))
  (when font-lock-mode
    (font-lock-fontify-buffer)))

;;;}}}
;;;==================================================================

;;;==================================================================
;;;{{{ Tags:

(defun doc-mode-current-tag ()
  (when (semantic-parse-tree-unparseable-p)
    (error "Semantic can't parse buffer"))
  (when (or (semantic-parse-tree-needs-rebuild-p)
            (semantic-parse-tree-needs-update-p))
    (condition-case nil
        (semantic-fetch-tags)
      (error (error "Semantic can't parse buffer"))))
  (save-excursion
    (or (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (progn (beginning-of-line) (skip-chars-forward " \t\n") nil)
        (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (if (not (looking-at "/\\*\\*"))
            (semantic-current-tag-of-class 'type)
          (progn (search-forward "*/" nil t)
                 (skip-chars-forward " \t\n")
                 nil))
        (semantic-current-tag-of-class 'function)
        (semantic-current-tag-of-class 'variable)
        (semantic-current-tag-of-class 'type))))

(defun doc-mode-current-tag-or-bust ()
  (or (doc-mode-current-tag) (error "No tag found")))

;;;------------------------------------------------------------------
;;;{{{ Insertion:

(defun doc-mode-line-indent (keyword)
  "Determine left side offset when indenting LINE."
  (if (numberp doc-mode-align-keyword-arguments)
      doc-mode-align-keyword-arguments
      (+ 1 (length (car keyword))
         (if (equal (car keyword) "param")
             (1+ (length (cdr keyword)))
             0))))

(defun doc-mode-insert (text)
  "Insert TEXT if a string, or a template if 'prompt."
  (if (stringp text)
      (insert text)
      (let ((beg (point)))
        (insert (if (listp (cadr text))
                    (cadadr text)
                    (cadr text)))
        (when doc-mode
          (doc-mode-add-template beg (point))))))

(defun doc-mode-insert-markup (markup &optional argument description)
  (insert doc-mode-template-keyword-char markup)
  (when argument
    (insert " ")
    (doc-mode-insert argument))
  (when description
    (insert " ")
    (doc-mode-insert description)))

(defun doc-mode-insert-line (line indent &optional one-liner)
  (indent-to-column indent)
  (let ((beg (point)))
    (unless one-liner
      (insert doc-mode-template-continue))
    (if (and (consp line) (not (eq (car line) 'prompt)))
        (apply 'doc-mode-insert-markup line)
      (doc-mode-insert line))
    (delete-char (- (skip-chars-backward " \t")))
    (when (> (point) (+ beg 2))
      (save-excursion (fill-region beg (point) 'left t)))
    (unless one-liner
      (insert "\n"))))

(defun doc-mode-insert-keyword (keyword indent)
  (indent-to-column indent)
  (let ((fill-column (or doc-mode-fill-column comment-fill-column fill-column))
        (fill-prefix (when doc-mode-align-keyword-arguments
                       (concat (buffer-substring (point-at-bol) (point))
                               doc-mode-template-continue
                               (make-string (doc-mode-line-indent keyword)
                                            ? )))))
    (doc-mode-insert-line keyword indent)))

(defun doc-mode-insert-doc (keywords &optional pos)
  "Insert a documentation at POS.
LINES is a list of keywords."
  (save-excursion
    (if pos
        (goto-char pos)
      (setq pos (point)))
    (let ((indent (current-column)))

      (if (and (not (cdr keywords)) doc-mode-allow-single-line-comments)
          (progn (insert doc-mode-template-single-line-start)
                 (if (listp (cadr keywords))
                     (doc-mode-insert-line (car keywords) 0 t)
                     (doc-mode-insert (car keywords)))
                 (insert doc-mode-template-single-line-end "\n"))
        (insert doc-mode-template-start "\n")

        ;; first line
        (when (or (stringp (car keywords))
                  (eq 'prompt (caar keywords)))
          (doc-mode-insert-line (pop keywords) indent))

        (when (and doc-mode-template-empty-line-after-summary
                   (or (null doc-mode-template-empty-line-before-keywords)
                       (stringp (cadr keywords))))
          (doc-mode-insert-line "" indent))

        ;; paragraphs
        (if (cdr keywords)
            (while (stringp (car keywords))
              (doc-mode-insert-line (pop keywords) indent)
              (when (stringp (car keywords))
                (doc-mode-insert-line "" indent)))
          (while (stringp (car keywords))
            (doc-mode-insert-line (pop keywords) indent)))

        (when doc-mode-template-empty-line-before-keywords
          (doc-mode-insert-line "" indent))

        ;; keywords
        (while keywords
          (doc-mode-insert-keyword (pop keywords) indent))
        (indent-to-column indent)
        (insert doc-mode-template-end "\n"))

      ;; re-indent original line
      (if (< (current-column) indent)
          (indent-to-column indent)
        (move-to-column indent t))))

    (and doc-mode-jump-to-template doc-mode-templates
         (ignore-errors (doc-mode-next-template pos (point)))))

(defun doc-mode-remove-doc (point)
  "Remove the documentation before POINT."
  (let* ((bounds (doc-mode-find-doc-bounds point))
         (beg (plist-get bounds :beg))
         (end (plist-get bounds :end)))
    (when bounds
      (save-excursion
        (goto-char beg)
        (incf beg (skip-chars-backward " \t"))
        (goto-char end)
        (incf end (skip-chars-forward " \t"))
        (when (eolp) (incf end))
        (delete-region beg end)))))

;;;###autoload
(defun doc-mode-remove-tag-doc (tag)
  "Remove the documentation for TAG.
If called interactively, use the tag given by `doc-mode-current-tag'."
  (interactive (list (doc-mode-current-tag-or-bust)))
  (doc-mode-remove-doc (semantic-tag-start tag)))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Registering:

(defun doc-mode-find-doc-bounds (pos)
  "Find the documentation right before POS.
If there is anything but whitespace between the documentation and POS, nil is
returned.  Otherwise a cons of the doc's beginning and end is given."
  (let (end)
    (save-excursion
      (goto-char pos)
      (when (re-search-backward "[ \t]*\n[ \t]*\\=" nil t)
        (setq end (point))
        (cond
          ;; /// Doxygen comment */
          ((looking-back "[ \t]*//[/!]\\(.*\\)$")
           (forward-line -1)
           (while (looking-at "[ \t]*//[/!]\\(.*\\)$")
             (forward-line -1))
           (forward-line 1)
           (skip-chars-forward " \t")
           `(:beg ,(point) :end ,end :column ,(current-indentation)))
          ;; /** JavaDoc comment */
          ((looking-back "\\*/")
           (goto-char (match-beginning 0))
           ;; search for /*, not allowing any */ in between
           (when (and (re-search-backward "\\(/\\*\\)\\|\\*/" nil t)
                      (match-beginning 1)
                      (memq (char-after (1+ (match-beginning 1))) '(?! ?*)))
             `(:beg ,(point) :end ,end :column
                    ,(current-column)))))))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Formatting:

(defun doc-mode-new-keyword (keyword &optional argument string)
  (unless string
    (setq string "<doc>"))
  (if (equal keyword "param")
      (list keyword argument `(prompt ,string))
      (list keyword `(prompt ,string))))

(defun doc-mode-has-return-value-p (tag)
  "Test if TAG has a return value to format."
  (and (eq (semantic-tag-class tag) 'function)
       (not (equal (semantic-tag-type tag) "void"))
       (not (semantic-tag-get-attribute tag :constructor-flag))
       (or (not (equal (semantic-tag-type tag) "int"))
           ;; semantic bug, constructors sometimes appear to have int type
           (save-excursion (goto-char (semantic-tag-start tag))
                           (and (re-search-forward "\\(\\<int\\>\\)\\|{\\|;"
                                                   (semantic-tag-end tag) t)
                                (match-beginning 1))))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Extracting:

(defun doc-mode-extract-summary (beg end)
  (let ((bounds (doc-mode-find-summary beg end)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun doc-mode-find-summary (beg end)
  (save-excursion
    (goto-char beg)
    (if (or (re-search-forward "^[@\\]brief \\([^\t ][^\n]*\n\\)" end t)
            (re-search-forward "\\<\\(.*\\)\\(\\*+/\\|\n\\)" end t))
        (cons (match-beginning 1) (match-end 1))
        (cons beg beg))))

(defconst doc-mode-begin-regexp
  (eval-when-compile (concat "[ \t\n]*"
                             "\\("
                             "/\\*\\(\\*+\\|!\\)"
                             "\\|"
                             "//[!/]"
                             "\\)[ \t]*")))

(defun doc-mode-clean-doc (beg end)
  "Remove the comment delimiters between BEG and END."
  (save-excursion
    (goto-char beg)
    (when (looking-at doc-mode-begin-regexp)
      (setq beg (match-end 0)))
    (goto-char end)
    (when (looking-back "[ \t\n\r]*\\*+/" nil t)
      (setq end (match-beginning 0)))
    (let ((lines (split-string (buffer-substring-no-properties beg end)
                               "[ \t]*\n[ \t]*\\(\\*/?\\|//[!/]\\)?[ \t]*")))
      (while (equal (car lines) "")
        (pop lines))
      (mapconcat 'identity lines "\n"))))

(defun doc-mode-extract-keywords (beg end)
  "Extract documentation keywords between BEG and END.
Returns a alist of keywords, where each element is the list (keyword
argument value) or (keyword argument)."
  (let* ((paragraphs (doc-mode-clean-doc beg end))
         (doc "")
         (pos 0)
         match results)

    (when (string-match
           "[ \t\n]*\\(\\(.\\|\n\\)*?\\)\\([@\\]\\<\\(.\\|\n\\)*\\'\\)"
           paragraphs)
      (setq doc (match-string-no-properties 3 paragraphs)
            paragraphs (match-string-no-properties 1 paragraphs)))

    ;; first line summary
    (when (string-match "\\`[ \t\n]*\\(.+\\.\\)\\([ \n]+\\|\\'\\)" paragraphs)
      (push (match-string 1 paragraphs) results)
      (setq pos (match-end 0)))

    ;; other paragraphs
    (dolist (paragraph (split-string (substring paragraphs pos)
                                     "[ \t]*\n\\(\n+[ \t]*\\|$\\)" t))
      (push (replace-regexp-in-string "[\n\r]" " " paragraph) results))

    ;; keywords
    (dolist (keyword (cdr (split-string doc "[@\\]\\<")))
      (setq match (split-string keyword))
      (push (if (equal (car match) "param")
                (list (car match) (cadr match)
                      (mapconcat 'identity (cddr match) " "))
              (list (car match) (mapconcat 'identity (cdr match) " ")))
            results))
    (nreverse results)))

(defun doc-mode-extract-keywords-for-tag (tag)
  (let ((bounds (doc-mode-find-doc-bounds (semantic-tag-start tag))))
    (when bounds (doc-mode-extract-keywords (plist-get bounds :beg)
                                            (plist-get bounds :end)))))

(defun doc-mode-find-keyword (keyword keywords)
  (when keywords
    (if (and (consp (car keywords)) (string= (car (car keywords)) keyword))
        (cons (car keywords) (doc-mode-find-keyword keyword (cdr keywords)))
      (doc-mode-find-keyword keyword (cdr keywords)))))

(defun doc-mode-filter-keyword (keyword keywords)
  (when keywords
    (if (and (consp (car keywords)) (string= (car (car keywords)) keyword))
        (doc-mode-filter-keyword keyword (cdr keywords))
      (cons (car keywords) (doc-mode-filter-keyword keyword (cdr keywords))))))

(defun doc-mode-find-eligible-tags ()
  (when buffer-file-name
    (unless (or (semantic-parse-tree-unparseable-p)
                (semantic-parse-tree-needs-rebuild-p)
                (semantic-parse-tree-needs-update-p))
      (ignore-errors
        (let (tags)
          (semantic-brute-find-tag-by-function
           (lambda (tag)
             (when (semantic-tag-start tag)
               (case (semantic-tag-class tag)
                 ((function variable) (push tag tags))
                 (type (setq tags
                             (nconc (semantic-tag-type-members tag)
                                    tags))))))
           (semanticdb-file-stream buffer-file-name))
          tags)))))

;;;}}}
;;;------------------------------------------------------------------

;;;------------------------------------------------------------------
;;;{{{ Checking:

(defsubst doc-mode-position (element list)
  "Return the first position of ELEMENT in LIST.
Returns (length LIST) if no occurrence was found."
  (let ((pos 0))
    (while (and list (not (equal element (pop list))))
      (incf pos))
    pos))

(defun doc-mode-keyword< (a b tag)
  (if (equal (car a) "param")
      (let* ((args (mapcar 'semantic-tag-name
                          (semantic-tag-get-attribute tag :arguments)))
             (a-param (cadr a))
             (b-param (cadr b))
             (a-pos (doc-mode-position a-param args))
             (b-pos (doc-mode-position b-param args)))
        (if (= a-pos b-pos) 
             (string< a-param b-param)
          (< a-pos b-pos)))
    (string< (cadr a) (cadr b))))

(defun doc-mode-sort-keywords (keywords tag)
  (let ((lists (make-vector (1+ (length doc-mode-template-keywords)) nil))
        description)
    (dolist (k keywords)
      (if (or (stringp k)
              (and (eq (car k) 'prompt))
              (and (string= (car k) "brief")))
          (push k description)
        (push k (elt lists (doc-mode-position (car k)
                                              doc-mode-template-keywords)))))
    (let ((i (length lists)) result)
      (while (> i 0)
        (setq result (nconc (sort (elt lists (decf i))
                                  (lambda (a b) (doc-mode-keyword< a b tag)))
                            result)))
      (nconc (nreverse description) result))))

(defun doc-mode-update-parameters (old new)
  "Cleanse and sort NEW parameters according to OLD parameter list."
  (let (params car-new)
    (while (setq car-new (pop new))
      (push (or (dolist (p old) ;; search for match in old
                  (when (equal (cadr p) car-new)
                    (setq old (delete p old))
                    (return p)))
                ;; this parameter wasn't there before
                (if (or (null old) (member (cadr (car old)) new))
                    ;; insertion, new
                    (doc-mode-new-keyword "param" car-new)
                  ;; the old parameter at this pos isn't there anymore, rename
                  (list* "param" car-new (cddr (pop old)))))
            params))
    (nreverse params)))

(defun doc-mode-keywords-from-tag (tag keywords)
  "Create keywords for a Semantic TAG, taking descriptions from old KEYWORDS"
  (let ((old-params (doc-mode-find-keyword "param" keywords))
        (new-params (mapcar 'semantic-tag-name
                            (semantic-tag-get-attribute tag :arguments))))
    ;; fix return value
    (if (doc-mode-has-return-value-p tag)
        ;; add
        (unless (doc-mode-find-keyword "returns" keywords)
          (push (doc-mode-new-keyword "returns") keywords))
      ;; remove
      (setq keywords (doc-mode-filter-keyword "returns" keywords)))
    (unless (stringp (car keywords))
      (push (doc-mode-new-keyword "brief"
                                  ""
                                  (format "Description for %s."
                                          (semantic-tag-name tag)))
            keywords))
    (doc-mode-sort-keywords (nconc (doc-mode-update-parameters old-params
                                                               new-params)
                                   (doc-mode-filter-keyword "param" keywords))
                            tag)))

;;;###autoload
(defun doc-mode-fix-tag-doc (tag)
  (interactive (list (doc-mode-current-tag-or-bust)))
  (let ((keywords (funcall doc-mode-keywords-from-tag-func
                           tag (doc-mode-extract-keywords-for-tag tag))))
    (doc-mode-remove-tag-doc tag)
    (doc-mode-insert-doc keywords (semantic-tag-start tag))
    ;; update lighter
    (doc-mode-check-buffer)))

;;;###autoload
(defalias 'doc-mode-add-tag-doc 'doc-mode-fix-tag-doc)

(defun doc-mode-format-message (type parameters)
  (when parameters
    (concat (case type
              ('missing "Missing")
              ('invalid "Invalid"))
            " parameter" (when (cdr parameters) "s") ": "
            (mapconcat 'identity parameters ", "))))

;;;###autoload
(defun doc-mode-check-tag-doc (tag &optional print-message-p)
  (interactive (list (doc-mode-current-tag-or-bust) t))
  (let* ((actual (doc-mode-extract-keywords-for-tag tag))
         (expected (mapcar 'semantic-tag-name
                           (semantic-tag-get-attribute tag :arguments))))
    (if actual
        (let ((no-doc-p (not (stringp (car actual))))
              ;; we only report parameters
              (actual (mapcar 'cadr (doc-mode-find-keyword "param"
                                                           actual)))
              invalid)
          (dolist (keyword actual)
            (if (member keyword expected)
                (setq expected (delete keyword expected))
              (push keyword invalid)))
          (when print-message-p
            (message "%s" (concat (and no-doc-p "Missing documentation")
                                  (and no-doc-p expected "\n")
                                  (doc-mode-format-message 'missing expected)
                                  (and (or no-doc-p expected) invalid "\n")
                                  (doc-mode-format-message 'invalid invalid))))
          (or no-doc-p expected invalid))
      (when print-message-p
        (message "Missing comment"))
      t)))

;;;###autoload
(defun doc-mode-check-buffer ()
  (interactive)
  (kill-local-variable 'doc-mode-lighter)
  (dolist (tag (doc-mode-find-eligible-tags))
    (when (doc-mode-check-tag-doc tag)
      (set (make-local-variable 'doc-mode-lighter) " doc!")
      (return t))))

(defun doc-mode-first-faulty-tag-doc ()
  (dolist (tag (sort (doc-mode-find-eligible-tags)
                     (lambda (a b) (< (semantic-tag-start a)
                                      (semantic-tag-start b)))))
    (when (doc-mode-check-tag-doc tag)
      (return tag))))

;;;###autoload
(defun doc-mode-next-faulty-doc ()
  "Jump to the next faulty documentation and print error."
  (interactive)
  (let ((tag (or (doc-mode-first-faulty-tag-doc)
                 (error "End of buffer"))))
    (push-mark)
    (goto-char (semantic-tag-start tag))
    ;; check again with message
    (doc-mode-check-tag-doc tag t)))

;;;}}}
;;;------------------------------------------------------------------

;;;}}}
;;;==================================================================

(provide 'doc-mode)

;;; doc-mode.el ends here
