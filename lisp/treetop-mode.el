(eval-when-compile
  (require 'cl))

(defconst treetop-keyword-end-re
  (if (string-match "\\_>" "ruby")
      "\\_>"
      "\\>"))

(defconst treetop-block-beg-keywords
  '("class" "module" "def" "if" "unless" "case" "while" "until" "for" "begin" "do" "grammar" "rule"))

(defconst treetop-block-beg-re
  (regexp-opt treetop-block-beg-keywords)
  "Regexp to match the beginning of blocks.")

(defconst treetop-non-block-do-re
  (concat (regexp-opt '("while" "until" "for" "rescue") t) treetop-keyword-end-re)
  "Regexp to match keywords that nest without blocks.")

;;; adding grammar and rule for treetop funtimes...
(defconst treetop-indent-beg-re
  (concat "\\(\\s *" (regexp-opt '("class" "module" "def" "grammar" "rule") t) "\\)\\|"
          (regexp-opt '("if" "unless" "case" "while" "until" "for" "begin" "grammar" "rule")))
  "Regexp to match where the indentation gets deeper.")

(defconst treetop-modifier-beg-keywords
  '("if" "unless" "while" "until")
  "Modifiers that are the same as the beginning of blocks.")

(defconst treetop-modifier-beg-re
  (regexp-opt treetop-modifier-beg-keywords)
  "Regexp to match modifiers same as the beginning of blocks.")

(defconst treetop-modifier-re
  (regexp-opt (cons "rescue" treetop-modifier-beg-keywords))
  "Regexp to match modifiers.")

(defconst treetop-block-mid-keywords
  '("then" "else" "elsif" "when" "rescue" "ensure")
  "Keywords where the indentation gets shallower in middle of block statements.")

(defconst treetop-block-mid-re
  (regexp-opt treetop-block-mid-keywords)
  "Regexp to match where the indentation gets shallower in middle of block statements.")

(defconst treetop-block-op-keywords
  '("and" "or" "not")
  "Regexp to match boolean keywords.")

(defconst treetop-block-hanging-re
  (regexp-opt (append treetop-modifier-beg-keywords treetop-block-op-keywords))
  "Regexp to match hanging block modifiers.")

(defconst treetop-block-end-re "\\<end\\>")

(defconst treetop-here-doc-beg-re
  "\\(<\\)<\\(-\\)?\\(\\([a-zA-Z0-9_]+\\)\\|[\"]\\([^\"]+\\)[\"]\\|[']\\([^']+\\)[']\\)"
  "Regexp to match the beginning of a heredoc.")

(defconst treetop-here-doc-end-re
  "^\\([ \t]+\\)?\\(.*\\)\\(.\\)$"
  "Regexp to match the end of heredocs.

This will actually match any line with one or more characters.
It's useful in that it divides up the match string so that
`treetop-here-doc-beg-match' can search for the beginning of the heredoc.")

(defun treetop-here-doc-end-match ()
  "Return a regexp to find the end of a heredoc.

This should only be called after matching against `treetop-here-doc-beg-re'."
  (concat "^"
          (if (match-string 2) "[ \t]*" nil)
          (regexp-quote
           (or (match-string 4)
               (match-string 5)
               (match-string 6)))))

(defun treetop-here-doc-beg-match ()
  "Return a regexp to find the beginning of a heredoc.

This should only be called after matching against `treetop-here-doc-end-re'."
  (let ((contents (regexp-quote (concat (match-string 2) (match-string 3)))))
    (concat "<<"
            (let ((match (match-string 1)))
              (if (and match (> (length match) 0))
                  (concat "\\(?:-\\([\"']?\\)\\|\\([\"']\\)" (match-string 1) "\\)"
                          contents "\\b\\(\\1\\|\\2\\)")
                (concat "-?\\([\"']\\|\\)" contents "\\b\\1"))))))

(defconst treetop-delimiter
  (concat "[?$/%(){}#\"'`.:]\\|<<\\|\\[\\|\\]\\|\\<\\("
          treetop-block-beg-re
          "\\)\\>\\|" treetop-block-end-re
          "\\|^=begin\\|" treetop-here-doc-beg-re))

(defconst treetop-negative
  (concat "^[ \t]*\\(\\(" treetop-block-mid-re "\\)\\>\\|"
          treetop-block-end-re "\\|}\\|\\]\\)")
  "Regexp to match where the indentation gets shallower.")

(defconst treetop-operator-re "[-,.+*/%&|^~=<>:]"
  "Regexp to match operators.")

(defconst treetop-symbol-chars "a-zA-Z0-9_"
  "List of characters that symbol names may contain.")
(defconst treetop-symbol-re (concat "[" treetop-symbol-chars "]")
  "Regexp to match symbols.")

(defvar treetop-mode-abbrev-table nil
  "Abbrev table in use in treetop-mode buffers.")

(define-abbrev-table 'treetop-mode-abbrev-table ())

(defvar treetop-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "{" 'treetop-electric-brace)
    (define-key map "}" 'treetop-electric-brace)
    (define-key map (kbd "M-C-a") 'treetop-beginning-of-defun)
    (define-key map (kbd "M-C-e") 'treetop-end-of-defun)
    (define-key map (kbd "M-C-b") 'treetop-backward-sexp)
    (define-key map (kbd "M-C-f") 'treetop-forward-sexp)
    (define-key map (kbd "M-C-p") 'treetop-beginning-of-block)
    (define-key map (kbd "M-C-n") 'treetop-end-of-block)
    (define-key map (kbd "M-C-h") 'treetop-mark-defun)
    (define-key map (kbd "M-C-q") 'treetop-indent-exp)
    (define-key map (kbd "TAB")   'treetop-indent-line)
    (define-key map (kbd "C-M-h") 'backward-kill-word)
    (define-key map (kbd "C-j")   'reindent-then-newline-and-indent)
    (define-key map (kbd "C-m")   'newline)
    map)
  "Keymap used in treetop-mode.")

(defvar treetop-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\` "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?? "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table to use in treetop-mode.")

(defcustom treetop-indent-tabs-mode nil
  "Indentation can insert tabs in ruby mode if this is non-nil."
  :type 'boolean :group 'ruby)

(defcustom treetop-indent-level 2
  "Indentation of ruby statements."
  :type 'integer :group 'ruby)

(defcustom treetop-comment-column 32
  "Indentation column of comments."
  :type 'integer :group 'ruby)

(defcustom treetop-deep-arglist t
  "Deep indent lists in parenthesis when non-nil.
Also ignores spaces after parenthesis when 'space."
  :group 'ruby)

(defcustom treetop-deep-indent-paren '(?\( ?\[ ?\] t)
  "Deep indent lists in parenthesis when non-nil. t means continuous line.
Also ignores spaces after parenthesis when 'space."
  :group 'ruby)

(defcustom treetop-deep-indent-paren-style 'space
  "Default deep indent style."
  :options '(t nil space) :group 'ruby)

(defcustom treetop-encoding-map '((shift_jis . cp932) (shift-jis . cp932))
  "Alist to map encoding name from Emacs to Ruby."
  :group 'ruby)

(defcustom treetop-insert-encoding-magic-comment t
  "*Insert a magic emacs 'coding' comment upon save if this is non-nil."
  :type 'boolean :group 'ruby)

(defcustom treetop-use-encoding-map t
  "Use `treetop-encoding-map' to set encoding magic comment if this is non-nil."
  :type 'boolean :group 'ruby)

;; Safe file variables
(put 'treetop-indent-tabs-mode 'safe-local-variable 'booleanp)
(put 'treetop-indent-level 'safe-local-variable 'integerp)
(put 'treetop-comment-column 'safe-local-variable 'integerp)
(put 'treetop-deep-arglist 'safe-local-variable 'booleanp)

(defun treetop-imenu-create-index-in-block (prefix beg end)
  "Create an imenu index of methods inside a block."
  (let ((index-alist '()) (case-fold-search nil)
        name next pos decl sing)
    (goto-char beg)
    (while (re-search-forward "^\\s *\\(\\(class\\s +\\|\\(class\\s *<<\\s *\\)\\|module\\s +\\)\\([^\(<\n ]+\\)\\|\\(def\\|alias\\)\\s +\\([^\(\n ]+\\)\\)" end t)
      (setq sing (match-beginning 3))
      (setq decl (match-string 5))
      (setq next (match-end 0))
      (setq name (or (match-string 4) (match-string 6)))
      (setq pos (match-beginning 0))
      (cond
       ((string= "alias" decl)
        (if prefix (setq name (concat prefix name)))
        (push (cons name pos) index-alist))
       ((string= "def" decl) 
        (if prefix
            (setq name
                  (cond
                   ((string-match "^self\." name)
                    (concat (substring prefix 0 -1) (substring name 4)))
                  (t (concat prefix name)))))
        (push (cons name pos) index-alist)
        (treetop-accurate-end-of-block end))
       (t
        (if (string= "self" name)
            (if prefix (setq name (substring prefix 0 -1)))
          (if prefix (setq name (concat (substring prefix 0 -1) "::" name)))
          (push (cons name pos) index-alist))
        (treetop-accurate-end-of-block end)
        (setq beg (point))
        (setq index-alist
              (nconc (treetop-imenu-create-index-in-block
                      (concat name (if sing "." "#"))
                      next beg) index-alist))
        (goto-char beg))))
    index-alist))

(defun treetop-imenu-create-index ()
  "Create an imenu index of all methods in the buffer."
  (nreverse (treetop-imenu-create-index-in-block nil (point-min) nil)))

(defun treetop-accurate-end-of-block (&optional end)
  "TODO: document."
  (let (state
        (end (or end (point-max))))
    (while (and (setq state (apply 'treetop-parse-partial end state))
                (>= (nth 2 state) 0) (< (point) end)))))

(defun treetop-mode-variables ()
  "Set up initial buffer-local variables for treetop-mode."
  (set-syntax-table treetop-mode-syntax-table)
  (setq local-abbrev-table treetop-mode-abbrev-table)
  (setq indent-tabs-mode treetop-indent-tabs-mode)
  (set (make-local-variable 'indent-line-function) 'treetop-indent-line)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) treetop-comment-column)
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t))

(defun treetop-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "[^\0-\177]" nil t)
      (goto-char (point-min))
      (let ((coding-system
             (or coding-system-for-write
                 buffer-file-coding-system)))
        (if coding-system
            (setq coding-system
                  (or (coding-system-get coding-system 'mime-charset)
                      (coding-system-change-eol-conversion coding-system nil))))
        (setq coding-system
              (if coding-system
                  (symbol-name
                   (or (and treetop-use-encoding-map
                            (cdr (assq coding-system treetop-encoding-map)))
                       coding-system))
                "ascii-8bit"))
        (if (looking-at "^#![^\n]*ruby") (beginning-of-line 2))
        (cond ((looking-at "\\s *#.*-\*-\\s *\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)\\s *\\(;\\|-\*-\\)")
               (unless (string= (match-string 2) coding-system)
                 (goto-char (match-beginning 2))
                 (delete-region (point) (match-end 2))
                 (and (looking-at "-\*-")
                      (let ((n (skip-chars-backward " ")))
                        (cond ((= n 0) (insert "  ") (backward-char))
                              ((= n -1) (insert " "))
                              ((forward-char)))))
                 (insert coding-system)))
              ((looking-at "\\s *#.*coding\\s *[:=]"))
              (t (when treetop-insert-encoding-magic-comment
                   (insert "# -*- coding: " coding-system " -*-\n"))))))))

(defun treetop-current-indentation ()
  "Return the indentation level of current line."
  (save-excursion
    (beginning-of-line)
    (back-to-indentation)
    (current-column)))

(defun treetop-indent-line (&optional flag)
  "Correct the indentation of the current ruby line."
  (interactive)
  (treetop-indent-to (treetop-calculate-indent)))

(defun treetop-indent-to (column)
  "Indent the current line to COLUMN."
  (when column
    (let (shift top beg)
      (and (< column 0) (error "invalid nest"))
      (setq shift (current-column))
      (beginning-of-line)
      (setq beg (point))
      (back-to-indentation)
      (setq top (current-column))
      (skip-chars-backward " \t")
      (if (>= shift top) (setq shift (- shift top))
        (setq shift 0))
      (if (and (bolp)
               (= column top))
          (move-to-column (+ column shift))
        (move-to-column top)
        (delete-region beg (point))
        (beginning-of-line)
        (indent-to column)
        (move-to-column (+ column shift))))))

(defun treetop-special-char-p (&optional pos)
  "Return t if the character before POS is a special character.
If omitted, POS defaults to the current point.
Special characters are `?', `$', `:' when preceded by whitespace,
and `\\' when preceded by `?'."
  (setq pos (or pos (point)))
  (let ((c (char-before pos)) (b (and (< (point-min) pos)
                                      (char-before (1- pos)))))
    (cond ((or (eq c ??) (eq c ?$)))
          ((and (eq c ?:) (or (not b) (eq (char-syntax b) ? ))))
          ((eq c ?\\) (eq b ??)))))

(defun treetop-expr-beg (&optional option)
  "TODO: document."
  (save-excursion
    (store-match-data nil)
    (let ((space (skip-chars-backward " \t"))
          (start (point)))
      (cond
       ((bolp) t)
       ((progn
          (forward-char -1)
          (and (looking-at "\\?")
               (or (eq (char-syntax (char-before (point))) ?w)
                   (treetop-special-char-p))))
        nil)
       ((and (eq option 'heredoc) (< space 0)) t)
       ((or (looking-at treetop-operator-re)
            (looking-at "[\\[({,;]")
            (and (looking-at "[!?]")
                 (or (not (eq option 'modifier))
                     (bolp)
                     (save-excursion (forward-char -1) (looking-at "\\Sw$"))))
            (and (looking-at treetop-symbol-re)
                 (skip-chars-backward treetop-symbol-chars)
                 (cond
                  ((looking-at (regexp-opt
                                (append treetop-block-beg-keywords
                                        treetop-block-op-keywords
                                        treetop-block-mid-keywords)
                                'words))
                   (goto-char (match-end 0))
                   (not (looking-at "\\s_")))
                  ((eq option 'expr-qstr)
                   (looking-at "[a-zA-Z][a-zA-z0-9_]* +%[^ \t]"))
                  ((eq option 'expr-re)
                   (looking-at "[a-zA-Z][a-zA-z0-9_]* +/[^ \t]"))
                  (t nil)))))))))

(defun treetop-forward-string (term &optional end no-error expand)
  "TODO: document."
  (let ((n 1) (c (string-to-char term))
        (re (if expand
                (concat "[^\\]\\(\\\\\\\\\\)*\\([" term "]\\|\\(#{\\)\\)")
              (concat "[^\\]\\(\\\\\\\\\\)*[" term "]"))))
    (while (and (re-search-forward re end no-error)
                (if (match-beginning 3)
                    (treetop-forward-string "}{" end no-error nil)
                  (> (setq n (if (eq (char-before (point)) c)
                                     (1- n) (1+ n))) 0)))
      (forward-char -1))
    (cond ((zerop n))
          (no-error nil)
          ((error "unterminated string")))))

(defun treetop-deep-indent-paren-p (c)
    "TODO: document."
  (cond ((listp treetop-deep-indent-paren)
         (let ((deep (assoc c treetop-deep-indent-paren)))
           (cond (deep
                  (or (cdr deep) treetop-deep-indent-paren-style))
                 ((memq c treetop-deep-indent-paren)
                  treetop-deep-indent-paren-style))))
        ((eq c treetop-deep-indent-paren) treetop-deep-indent-paren-style)
        ((eq c ?\( ) treetop-deep-arglist)))

(defun treetop-parse-partial (&optional end in-string nest depth pcol indent)
    "TODO: document throughout function body."
  (or depth (setq depth 0))
  (or indent (setq indent 0))
  (when (re-search-forward treetop-delimiter end 'move)
    (let ((pnt (point)) w re expand)
      (goto-char (match-beginning 0))
      (cond
       ((and (memq (char-before) '(?@ ?$)) (looking-at "\\sw"))
        (goto-char pnt))
       ((looking-at "[\"`]")            ;skip string
        (cond
         ((and (not (eobp))
               (treetop-forward-string (buffer-substring (point) (1+ (point))) end t t))
          nil)
         (t
          (setq in-string (point))
          (goto-char end))))
       ((looking-at "'")
        (cond
         ((and (not (eobp))
               (re-search-forward "[^\\]\\(\\\\\\\\\\)*'" end t))
          nil)
         (t
          (setq in-string (point))
          (goto-char end))))
       ((looking-at "/=")
        (goto-char pnt))
       ((looking-at "/")
        (cond
         ((and (not (eobp)) (treetop-expr-beg 'expr-re))
          (if (treetop-forward-string "/" end t t)
              nil
            (setq in-string (point))
            (goto-char end)))
         (t
          (goto-char pnt))))
       ((looking-at "%")
        (cond
         ((and (not (eobp))
               (treetop-expr-beg 'expr-qstr)
               (not (looking-at "%="))
               (looking-at "%[QqrxWw]?\\([^a-zA-Z0-9 \t\n]\\)"))
          (goto-char (match-beginning 1))
          (setq expand (not (memq (char-before) '(?q ?w))))
          (setq w (match-string 1))
          (cond
           ((string= w "[") (setq re "]["))
           ((string= w "{") (setq re "}{"))
           ((string= w "(") (setq re ")("))
           ((string= w "<") (setq re "><"))
           ((and expand (string= w "\\"))
            (setq w (concat "\\" w))))
          (unless (cond (re (treetop-forward-string re end t expand))
                        (expand (treetop-forward-string w end t t))
                        (t (re-search-forward
                            (if (string= w "\\")
                                "\\\\[^\\]*\\\\"
                              (concat "[^\\]\\(\\\\\\\\\\)*" w))
                            end t)))
            (setq in-string (point))
            (goto-char end)))
         (t
          (goto-char pnt))))
       ((looking-at "\\?")              ;skip ?char
        (cond
         ((and (treetop-expr-beg)
               (looking-at "?\\(\\\\C-\\|\\\\M-\\)*\\\\?."))
          (goto-char (match-end 0)))
         (t
          (goto-char pnt))))
       ((looking-at "\\$")              ;skip $char
        (goto-char pnt)
        (forward-char 1))
       ((looking-at "#")                ;skip comment
        (forward-line 1)
        (goto-char (point))
        )
       ((looking-at "[\\[{(]")
        (let ((deep (treetop-deep-indent-paren-p (char-after))))
          (if (and deep (or (not (eq (char-after) ?\{)) (treetop-expr-beg)))
              (progn
                (and (eq deep 'space) (looking-at ".\\s +[^# \t\n]")
                     (setq pnt (1- (match-end 0))))
                (setq nest (cons (cons (char-after (point)) pnt) nest))
                (setq pcol (cons (cons pnt depth) pcol))
                (setq depth 0))
            (setq nest (cons (cons (char-after (point)) pnt) nest))
            (setq depth (1+ depth))))
        (goto-char pnt)
        )
       ((looking-at "[])}]")
        (if (treetop-deep-indent-paren-p (matching-paren (char-after)))
            (setq depth (cdr (car pcol)) pcol (cdr pcol))
          (setq depth (1- depth)))
        (setq nest (cdr nest))
        (goto-char pnt))
       ((looking-at treetop-block-end-re)
        (if (or (and (not (bolp))
                     (progn
                       (forward-char -1)
                       (setq w (char-after (point)))
                       (or (eq ?_ w)
                           (eq ?. w))))
                (progn
                  (goto-char pnt)
                  (setq w (char-after (point)))
                  (or (eq ?_ w)
                      (eq ?! w)
                      (eq ?? w))))
            nil
          (setq nest (cdr nest))
          (setq depth (1- depth)))
        (goto-char pnt))
       ((looking-at "def\\s +[^(\n;]*")
        (if (or (bolp)
                (progn
                  (forward-char -1)
                  (not (eq ?_ (char-after (point))))))
            (progn
              (setq nest (cons (cons nil pnt) nest))
              (setq depth (1+ depth))))
        (goto-char (match-end 0)))
       ((looking-at "rule\\s +[^(\n;]*")
        (if (or (bolp)
                (progn
                  (forward-char -1)
                  (not (eq ?_ (char-after (point))))))
            (progn
              (setq nest (cons (cons nil pnt) nest))
              (setq depth (1+ depth))))
        (goto-char (match-end 0)))
       ((looking-at (concat "\\<\\(" treetop-block-beg-re "\\)\\>"))
        (and
         (save-match-data
           (or (not (looking-at (concat "do" treetop-keyword-end-re)))
               (save-excursion
                 (back-to-indentation)
                 (not (looking-at treetop-non-block-do-re)))))
         (or (bolp)
             (progn
               (forward-char -1)
               (setq w (char-after (point)))
               (not (or (eq ?_ w)
                        (eq ?. w)))))
         (goto-char pnt)
         (setq w (char-after (point)))
         (not (eq ?_ w))
         (not (eq ?! w))
         (not (eq ?? w))
         (skip-chars-forward " \t")
         (goto-char (match-beginning 0))
         (or (not (looking-at treetop-modifier-re))
             (treetop-expr-beg 'modifier))
         (goto-char pnt)
         (setq nest (cons (cons nil pnt) nest))
         (setq depth (1+ depth)))
        (goto-char pnt))
       ((looking-at ":\\(['\"]\\)")
        (goto-char (match-beginning 1))
        (treetop-forward-string (buffer-substring (match-beginning 1) (match-end 1)) end))
       ((looking-at ":\\([-,.+*/%&|^~<>]=?\\|===?\\|<=>\\)")
        (goto-char (match-end 0)))
       ((looking-at ":\\([a-zA-Z_][a-zA-Z_0-9]*[!?=]?\\)?")
        (goto-char (match-end 0)))
       ((or (looking-at "\\.\\.\\.?")
            (looking-at "\\.[0-9]+")
            (looking-at "\\.[a-zA-Z_0-9]+")
            (looking-at "\\."))
        (goto-char (match-end 0)))
       ((looking-at "^=begin")
        (if (re-search-forward "^=end" end t)
            (forward-line 1)
          (setq in-string (match-end 0))
          (goto-char end)))
       ((looking-at "<<")
        (cond
         ((and (treetop-expr-beg 'heredoc)
               (looking-at "<<\\(-\\)?\\(\\([\"'`]\\)\\([^\n]+?\\)\\3\\|\\(?:\\sw\\|\\s_\\)+\\)"))
          (setq re (regexp-quote (or (match-string 4) (match-string 2))))
          (if (match-beginning 1) (setq re (concat "\\s *" re)))
          (let* ((id-end (goto-char (match-end 0)))
                 (line-end-position (save-excursion (end-of-line) (point)))
                 (state (list in-string nest depth pcol indent)))
            ;; parse the rest of the line
            (while (and (> line-end-position (point))
                        (setq state (apply 'treetop-parse-partial
                                           line-end-position state))))
            (setq in-string (car state)
                  nest (nth 1 state)
                  depth (nth 2 state)
                  pcol (nth 3 state)
                  indent (nth 4 state))
            ;; skip heredoc section
            (if (re-search-forward (concat "^" re "$") end 'move)
                (forward-line 1)
              (setq in-string id-end)
              (goto-char end))))
         (t
          (goto-char pnt))))
       ((looking-at "^__END__$")
        (goto-char pnt))
       ((and (looking-at treetop-here-doc-beg-re)
             (boundp 'treetop-indent-point))
        (if (re-search-forward (treetop-here-doc-end-match)
                               treetop-indent-point t)
            (forward-line 1)
          (setq in-string (match-end 0))
          (goto-char treetop-indent-point)))
       (t
        (error (format "bad string %s"
                       (buffer-substring (point) pnt)
                       ))))))
  (list in-string nest depth pcol))

(defun treetop-parse-region (start end)
  "TODO: document."
  (let (state)
    (save-excursion
      (if start
          (goto-char start)
        (treetop-beginning-of-indent))
      (save-restriction
        (narrow-to-region (point) end)
        (while (and (> end (point))
                    (setq state (apply 'treetop-parse-partial end state))))))
    (list (nth 0 state)                 ; in-string
          (car (nth 1 state))           ; nest
          (nth 2 state)                 ; depth
          (car (car (nth 3 state)))     ; pcol
          ;(car (nth 5 state))          ; indent
          )))

(defun treetop-indent-size (pos nest)
  "Returns the indentation level in spaces NEST levels deeper than POS."
  (+ pos (* (or nest 1) treetop-indent-level)))

(defun treetop-calculate-indent (&optional parse-start)
  "Returns the proper indentation level of the current line."
  ;; TODO: Document body
  (save-excursion
    (beginning-of-line)
    (let ((treetop-indent-point (point))
          (case-fold-search nil)
          state bol eol begin op-end
          (paren (progn (skip-syntax-forward " ")
                        (and (char-after) (matching-paren (char-after)))))
          (indent 0))
      (if parse-start
          (goto-char parse-start)
        (treetop-beginning-of-indent)
        (setq parse-start (point)))
      (back-to-indentation)
      (setq indent (current-column))
      (setq state (treetop-parse-region parse-start treetop-indent-point))
      (cond
       ((nth 0 state)                   ; within string
        (setq indent nil))              ;  do nothing
       ((car (nth 1 state))             ; in paren
        (goto-char (setq begin (cdr (nth 1 state))))
        (let ((deep (treetop-deep-indent-paren-p (car (nth 1 state)))))
          (if deep
              (cond ((and (eq deep t) (eq (car (nth 1 state)) paren))
                     (skip-syntax-backward " ")
                     (setq indent (1- (current-column))))
                    ((let ((s (treetop-parse-region (point) treetop-indent-point)))
                       (and (nth 2 s) (> (nth 2 s) 0)
                            (or (goto-char (cdr (nth 1 s))) t)))
                     (forward-word -1)
                     (setq indent (treetop-indent-size (current-column)
                                                    (nth 2 state))))
                    (t
                     (setq indent (current-column))
                     (cond ((eq deep 'space))
                           (paren (setq indent (1- indent)))
                           (t (setq indent (treetop-indent-size (1- indent) 1))))))
            (if (nth 3 state) (goto-char (nth 3 state))
              (goto-char parse-start) (back-to-indentation))
            (setq indent (treetop-indent-size (current-column) (nth 2 state))))
          (and (eq (car (nth 1 state)) paren)
               (treetop-deep-indent-paren-p (matching-paren paren))
               (search-backward (char-to-string paren))
               (setq indent (current-column)))))
       ((and (nth 2 state) (> (nth 2 state) 0)) ; in nest
        (if (null (cdr (nth 1 state)))
            (error "invalid nest"))
        (goto-char (cdr (nth 1 state)))
        (forward-word -1)               ; skip back a keyword
        (setq begin (point))
        (cond
         ((looking-at "do\\>[^_]")      ; iter block is a special case
          (if (nth 3 state) (goto-char (nth 3 state))
            (goto-char parse-start) (back-to-indentation))
          (setq indent (treetop-indent-size (current-column) (nth 2 state))))
         (t
          (setq indent (+ (current-column) treetop-indent-level)))))
       
       ((and (nth 2 state) (< (nth 2 state) 0)) ; in negative nest
        (setq indent (treetop-indent-size (current-column) (nth 2 state)))))
      (when indent
        (goto-char treetop-indent-point)
        (end-of-line)
        (setq eol (point))
        (beginning-of-line)
        (cond
         ((and (not (treetop-deep-indent-paren-p paren))
               (re-search-forward treetop-negative eol t))
          (and (not (eq ?_ (char-after (match-end 0))))
               (setq indent (- indent treetop-indent-level))))
         ((and
           (save-excursion
             (beginning-of-line)
             (not (bobp)))
           (or (treetop-deep-indent-paren-p t)
               (null (car (nth 1 state)))))
          ;; goto beginning of non-empty no-comment line
          (let (end done)
            (while (not done)
              (skip-chars-backward " \t\n")
              (setq end (point))
              (beginning-of-line)
              (if (re-search-forward "^\\s *#" end t)
                  (beginning-of-line)
                (setq done t))))
          (setq bol (point))
          (end-of-line)
          ;; skip the comment at the end
          (skip-chars-backward " \t")
          (let (end (pos (point)))
            (beginning-of-line)
            (while (and (re-search-forward "#" pos t)
                        (setq end (1- (point)))
                        (or (treetop-special-char-p end)
                            (and (setq state (treetop-parse-region parse-start end))
                                 (nth 0 state))))
              (setq end nil))
            (goto-char (or end pos))
            (skip-chars-backward " \t")
            (setq begin (if (and end (nth 0 state)) pos (cdr (nth 1 state))))
            (setq state (treetop-parse-region parse-start (point))))
          (or (bobp) (forward-char -1))
          (and
           (or (and (looking-at treetop-symbol-re)
                    (skip-chars-backward treetop-symbol-chars)
                    (looking-at (concat "\\<\\(" treetop-block-hanging-re "\\)\\>"))
                    (not (eq (point) (nth 3 state)))
                    (save-excursion
                      (goto-char (match-end 0))
                      (not (looking-at "[a-z_]"))))
               (and (looking-at treetop-operator-re)
                    (not (treetop-special-char-p))
                    ;; operator at the end of line
                    (let ((c (char-after (point))))
                      (and
;;                     (or (null begin)
;;                         (save-excursion
;;                           (goto-char begin)
;;                           (skip-chars-forward " \t")
;;                           (not (or (eolp) (looking-at "#")
;;                                    (and (eq (car (nth 1 state)) ?{)
;;                                         (looking-at "|"))))))
                       (or (not (eq ?/ c))
                           (null (nth 0 (treetop-parse-region (or begin parse-start) (point)))))
                       (or (not (eq ?| (char-after (point))))
                           (save-excursion
                             (or (eolp) (forward-char -1))
                             (cond
                              ((search-backward "|" nil t)
                               (skip-chars-backward " \t\n")
                               (and (not (eolp))
                                    (progn
                                      (forward-char -1)
                                      (not (looking-at "{")))
                                    (progn
                                      (forward-word -1)
                                      (not (looking-at "do\\>[^_]")))))
                              (t t))))
                       (not (eq ?, c))
                       (setq op-end t)))))
           (setq indent
                 (cond
                  ((and
                    (null op-end)
                    (not (looking-at (concat "\\<\\(" treetop-block-hanging-re "\\)\\>")))
                    (eq (treetop-deep-indent-paren-p t) 'space)
                    (not (bobp)))
                   (widen)
                   (goto-char (or begin parse-start))
                   (skip-syntax-forward " ")
                   (current-column))
                  ((car (nth 1 state)) indent)
                  (t
                   (+ indent treetop-indent-level))))))))
      (goto-char treetop-indent-point)
      (beginning-of-line)
      (skip-syntax-forward " ")
      (if (looking-at "\\.[^.]")
          (+ indent treetop-indent-level)
        indent))))

(defun treetop-electric-brace (arg)
  "Inserts a brace and re-indents the current line."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (treetop-indent-line t))

;; TODO: Why isn't one treetop-*-of-defun written in terms of the other?
(defun treetop-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current top-level defun.
With ARG, move backward multiple defuns. Negative ARG means
move forward."
  (interactive "p")
  (and (re-search-backward (concat "^\\(" treetop-block-beg-re "\\)\\b")
                           nil 'move (or arg 1))
       (beginning-of-line)))

(defun treetop-end-of-defun (&optional arg)
  "Move forward to the end of the current top-level defun.
With ARG, move forward multiple defuns. Negative ARG means
move backward."
  (interactive "p")
  (and (re-search-forward (concat "^\\(" treetop-block-end-re "\\)\\($\\|\\b[^_]\\)")
                          nil 'move (or arg 1))
       (beginning-of-line))
  (forward-line 1))

(defun treetop-beginning-of-indent ()
  "TODO: document"
  ;; I don't understand this function.
  ;; It seems like it should move to the line where indentation should deepen,
  ;; but treetop-indent-beg-re only accounts for whitespace before class, module and def,
  ;; so this will only match other block beginners at the beginning of the line.
  (and (re-search-backward (concat "^\\(" treetop-indent-beg-re "\\)\\b") nil 'move)
       (beginning-of-line)))

(defun treetop-move-to-block (n)
  "Moves to the beginning (N < 0) or the end (N > 0) of the current block
or blocks containing the current block."
  ;; TODO: Make this work for n > 1,
  ;; make it not loop for n = 0,
  ;; document body
  (let (start pos done down)
    (setq start (treetop-calculate-indent))
    (setq down (looking-at (if (< n 0) treetop-block-end-re
                             (concat "\\<\\(" treetop-block-beg-re "\\)\\>"))))
    (while (and (not done) (not (if (< n 0) (bobp) (eobp))))
      (forward-line n)
      (cond
       ((looking-at "^\\s *$"))
       ((looking-at "^\\s *#"))
       ((and (> n 0) (looking-at "^=begin\\>"))
        (re-search-forward "^=end\\>"))
       ((and (< n 0) (looking-at "^=end\\>"))
        (re-search-backward "^=begin\\>"))
       (t
        (setq pos (current-indentation))
        (cond
         ((< start pos)
          (setq down t))
         ((and down (= pos start))
          (setq done t))
         ((> start pos)
          (setq done t)))))
      (if done
          (save-excursion
            (back-to-indentation)
            (if (looking-at (concat "\\<\\(" treetop-block-mid-re "\\)\\>"))
                (setq done nil))))))
  (back-to-indentation))

(defun treetop-beginning-of-block (&optional arg)
  "Move backward to the beginning of the current block.
With ARG, move up multiple blocks."
  (interactive "p")
  (treetop-move-to-block (- (or arg 1))))

(defun treetop-end-of-block (&optional arg)
  "Move forward to the end of the current block.
With ARG, move out of multiple blocks."
  ;; Passing a value > 1 to treetop-move-to-block currently doesn't work.
  (interactive)
  (treetop-move-to-block (or arg 1)))

(defun treetop-forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
With ARG, do it many times. Negative ARG means move backward."
  ;; TODO: Document body
  (interactive "p")
  (if (and (numberp arg) (< arg 0))
      (treetop-backward-sexp (- arg))
    (let ((i (or arg 1)))
      (condition-case nil
          (while (> i 0)
            (skip-syntax-forward " ")
            (cond ((looking-at "\\?\\(\\\\[CM]-\\)*\\\\?\\S ")
                   (goto-char (match-end 0)))
                  ((progn
                     (skip-chars-forward ",.:;|&^~=!?\\+\\-\\*")
                     (looking-at "\\s("))
                   (goto-char (scan-sexps (point) 1)))
                  ((and (looking-at (concat "\\<\\(" treetop-block-beg-re "\\)\\>"))
                        (not (eq (char-before (point)) ?.))
                        (not (eq (char-before (point)) ?:)))
                   (treetop-end-of-block)
                   (forward-word 1))
                  ((looking-at "\\(\\$\\|@@?\\)?\\sw")
                   (while (progn
                            (while (progn (forward-word 1) (looking-at "_")))
                            (cond ((looking-at "::") (forward-char 2) t)
                                  ((> (skip-chars-forward ".") 0))
                                  ((looking-at "\\?\\|!\\(=[~=>]\\|[^~=]\\)")
                                   (forward-char 1) nil)))))
                  ((let (state expr)
                     (while
                         (progn
                           (setq expr (or expr (treetop-expr-beg)
                                          (looking-at "%\\sw?\\Sw\\|[\"'`/]")))
                           (nth 1 (setq state (apply 'treetop-parse-partial nil state))))
                       (setq expr t)
                       (skip-chars-forward "<"))
                     (not expr))))
            (setq i (1- i)))
        ((error) (forward-word 1)))
      i)))

(defun treetop-backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).
With ARG, do it many times. Negative ARG means move forward."
  ;; TODO: Document body
  (interactive "p")
  (if (and (numberp arg) (< arg 0))
      (treetop-forward-sexp (- arg))
    (let ((i (or arg 1)))
      (condition-case nil
          (while (> i 0)
            (skip-chars-backward " \t\n,.:;|&^~=!?\\+\\-\\*")
            (forward-char -1)
            (cond ((looking-at "\\s)")
                   (goto-char (scan-sexps (1+ (point)) -1))
                   (case (char-before)
                     (?% (forward-char -1))
                     ('(?q ?Q ?w ?W ?r ?x)
                      (if (eq (char-before (1- (point))) ?%) (forward-char -2))))
                   nil)
                  ((looking-at "\\s\"\\|\\\\\\S_")
                   (let ((c (char-to-string (char-before (match-end 0)))))
                     (while (and (search-backward c)
                                 (eq (logand (skip-chars-backward "\\") 1)
                                     1))))
                   nil)
                  ((looking-at "\\s.\\|\\s\\")
                   (if (treetop-special-char-p) (forward-char -1)))
                  ((looking-at "\\s(") nil)
                  (t
                   (forward-char 1)
                   (while (progn (forward-word -1)
                                 (case (char-before)
                                   (?_ t)
                                   (?. (forward-char -1) t)
                                   ((?$ ?@)
                                    (forward-char -1)
                                    (and (eq (char-before) (char-after)) (forward-char -1)))
                                   (?:
                                    (forward-char -1)
                                    (eq (char-before) :)))))
                   (if (looking-at treetop-block-end-re)
                       (treetop-beginning-of-block))
                   nil))
            (setq i (1- i)))
        ((error)))
      i)))

(defun treetop-mark-defun ()
  "Put mark at end of this Ruby function, point at beginning."
  (interactive)
  (push-mark (point))
  (treetop-end-of-defun)
  (push-mark (point) nil t)
  (treetop-beginning-of-defun)
  (re-search-backward "^\n" (- (point) 1) t))

(defun treetop-indent-exp (&optional shutup-p)
  "Indent each line in the balanced expression following the point.
If a prefix arg is given or SHUTUP-P is non-nil, no errors
are signalled if a balanced expression isn't found."
  (interactive "*P")
  (let ((here (point-marker)) start top column (nest t))
    (set-marker-insertion-type here t)
    (unwind-protect
        (progn
          (beginning-of-line)
          (setq start (point) top (current-indentation))
          (while (and (not (eobp))
                      (progn
                        (setq column (treetop-calculate-indent start))
                        (cond ((> column top)
                               (setq nest t))
                              ((and (= column top) nest)
                               (setq nest nil) t))))
            (treetop-indent-to column)
            (beginning-of-line 2)))
      (goto-char here)
      (set-marker here nil))))

(defun treetop-add-log-current-method ()
  "Returns the current method name as a string.
This string includes all namespaces.

For example:

  #exit
  String#gsub
  Net::HTTP#active?
  File::open.

See `add-log-current-defun-function'."
  ;; TODO: Document body
  ;; Why does this append a period to class methods?
  (condition-case nil
      (save-excursion
        (let (mname mlist (indent 0))
          ;; get current method (or class/module)
          (if (re-search-backward
               (concat "^[ \t]*\\(def\\|class\\|grammar\\|rule\\|module\\)[ \t]+"
                       "\\("
                       ;; \\. and :: for class method
                        "\\([A-Za-z_]" treetop-symbol-re "*\\|\\.\\|::" "\\)"
                        "+\\)")
               nil t)
              (progn
                (setq mname (match-string 2))
                (unless (or (string-equal "def" (match-string 1))
                            (string-equal "rule" (match-string 1))) 
                  (setq mlist (list mname) mname nil))
                (goto-char (match-beginning 1))
                (setq indent (current-column))
                (beginning-of-line)))
          ;; nest class/module
          (while (and (> indent 0)
                      (re-search-backward
                       (concat
                        "^[ \t]*\\(class\\|module\\)[ \t]+"
                        "\\([A-Z]" treetop-symbol-re "*\\)")
                       nil t))
            (goto-char (match-beginning 1))
            (if (< (current-column) indent)
                (progn
                  (setq mlist (cons (match-string 2) mlist))
                  (setq indent (current-column))
                  (beginning-of-line))))
          (when mname
            (let ((mn (split-string mname "\\.\\|::")))
              (if (cdr mn)
                  (progn
                    (cond
                     ((string-equal "" (car mn))
                      (setq mn (cdr mn) mlist nil))
                     ((string-equal "self" (car mn))
                      (setq mn (cdr mn)))
                     ((let ((ml (nreverse mlist)))
                        (while ml
                          (if (string-equal (car ml) (car mn))
                              (setq mlist (nreverse (cdr ml)) ml nil))
                          (or (setq ml (cdr ml)) (nreverse mlist))))))
                    (if mlist
                        (setcdr (last mlist) mn)
                      (setq mlist mn))
                    (setq mn (last mn 2))
                    (setq mname (concat "." (cadr mn)))
                    (setcdr mn nil))
                (setq mname (concat "#" mname)))))
          ;; generate string
          (if (consp mlist)
              (setq mlist (mapconcat (function identity) mlist "::")))
          (if mname
              (if mlist (concat mlist mname) mname)
            mlist)))))

(defconst treetop-font-lock-syntactic-keywords
  `(;; #{ }, #$hoge, #@foo are not comments
    ("\\(#\\)[{$@]" 1 (1 . nil))
    ;; the last $', $", $` in the respective string is not variable
    ;; the last ?', ?", ?` in the respective string is not ascii code
    ("\\(^\\|[\[ \t\n<+\(,=]\\)\\(['\"`]\\)\\(\\\\.\\|\\2\\|[^'\"`\n\\\\]\\)*?\\\\?[?$]\\(\\2\\)"
     (2 (7 . nil))
     (4 (7 . nil)))
    ;; $' $" $` .... are variables
    ;; ?' ?" ?` are ascii codes
    ("\\(^\\|[^\\\\]\\)\\(\\\\\\\\\\)*[?$]\\([#\"'`]\\)" 3 (1 . nil))
    ;; regexps
    ("\\(^\\|[=(,~?:;<>]\\|\\(^\\|\\s \\)\\(if\\|elsif\\|unless\\|while\\|until\\|when\\|and\\|or\\|&&\\|||\\)\\|g?sub!?\\|scan\\|split!?\\)\\s *\\(/\\)[^/\n\\\\]*\\(\\\\.[^/\n\\\\]*\\)*\\(/\\)"
     (4 (7 . ?/))
     (6 (7 . ?/)))
    ("^=en\\(d\\)\\_>" 1 "!")
    ("^\\(=\\)begin\\_>" 1 (treetop-comment-beg-syntax))
    ;; Currently, the following case is highlighted incorrectly:
    ;;
    ;;   <<FOO
    ;;   FOO
    ;;   <<BAR
    ;;   <<BAZ
    ;;   BAZ
    ;;   BAR
    ;;
    ;; This is because all here-doc beginnings are highlighted before any endings,
    ;; so although <<BAR is properly marked as a beginning, when we get to <<BAZ
    ;; it thinks <<BAR is part of a string so it's marked as well.
    ;;
    ;; This may be fixable by modifying treetop-in-here-doc-p to use
    ;; treetop-in-non-here-doc-string-p rather than syntax-ppss-context,
    ;; but I don't want to try that until we've got unit tests set up
    ;; to make sure I don't break anything else.
    (,(concat treetop-here-doc-beg-re ".*\\(\n\\)")
     ,(+ 1 (regexp-opt-depth treetop-here-doc-beg-re))
     (treetop-here-doc-beg-syntax))
    (,treetop-here-doc-end-re 3 (treetop-here-doc-end-syntax)))
  "Syntactic keywords for treetop-mode. See `font-lock-syntactic-keywords'.")

(defun treetop-comment-beg-syntax ()
  "Returns the syntax cell for a the first character of a =begin.
See the definition of `treetop-font-lock-syntactic-keywords'.

This returns a comment-delimiter cell as long as the =begin
isn't in a string or another comment."
  (when (not (nth 3 (syntax-ppss)))
    (string-to-syntax "!")))

(unless (functionp 'syntax-ppss)
  (defun syntax-ppss (&optional pos)
    (parse-partial-sexp (point-min) (or pos (point)))))

(defun treetop-in-ppss-context-p (context &optional ppss)
  (let ((ppss (or ppss (syntax-ppss (point)))))
    (if (cond
         ((eq context 'anything)
          (or (nth 3 ppss)
              (nth 4 ppss)))
         ((eq context 'string)
          (nth 3 ppss))
         ((eq context 'heredoc)
          (and (nth 3 ppss)
               ;; If it's generic string, it's a heredoc and we don't care
               ;; See `parse-partial-sexp'
               (not (numberp (nth 3 ppss)))))
         ((eq context 'non-heredoc)
          (and (treetop-in-ppss-context-p 'anything)
               (not (treetop-in-ppss-context-p 'heredoc))))
         ((eq context 'comment)
          (nth 4 ppss))
         (t
          (error (concat
                  "Internal error on `treetop-in-ppss-context-p': "
                  "context name `" (symbol-name context) "' is unknown"))))
        t)))

(defun treetop-in-here-doc-p ()
  "Returns whether or not the point is in a heredoc."
  (save-excursion
    (let ((old-point (point)) (case-fold-search nil))
      (beginning-of-line)
      (catch 'found-beg
        (while (re-search-backward treetop-here-doc-beg-re nil t)
          (if (not (or (treetop-in-ppss-context-p 'anything)
                       (treetop-here-doc-find-end old-point)))
              (throw 'found-beg t)))))))

(defun treetop-here-doc-find-end (&optional limit)
  "Expects the point to be on a line with one or more heredoc
openers. Returns the buffer position at which all heredocs on the
line are terminated, or nil if they aren't terminated before the
buffer position `limit' or the end of the buffer."
  (save-excursion
    (beginning-of-line)
    (catch 'done
      (let ((eol (save-excursion (end-of-line) (point)))
            (case-fold-search nil)
            ;; Fake match data such that (match-end 0) is at eol
            (end-match-data (progn (looking-at ".*$") (match-data)))
            beg-match-data end-re)
        (while (re-search-forward treetop-here-doc-beg-re eol t)
          (setq beg-match-data (match-data))
          (setq end-re (treetop-here-doc-end-match))

          (set-match-data end-match-data)
          (goto-char (match-end 0))
          (unless (re-search-forward end-re limit t) (throw 'done nil))
          (setq end-match-data (match-data))

          (set-match-data beg-match-data)
          (goto-char (match-end 0)))
        (set-match-data end-match-data)
        (goto-char (match-end 0))
        (point)))))

(defun treetop-here-doc-beg-syntax ()
  "Returns the syntax cell for a line that may begin a heredoc.
See the definition of `treetop-font-lock-syntactic-keywords'.

This sets the syntax cell for the newline ending the line
containing the heredoc beginning so that cases where multiple
heredocs are started on one line are handled correctly."
  (save-excursion
    (goto-char (match-beginning 0))
    (unless (or (treetop-in-ppss-context-p 'non-heredoc)
                (treetop-in-here-doc-p))
      (string-to-syntax "|"))))

(defun treetop-here-doc-end-syntax ()
  "Returns the syntax cell for a line that may end a heredoc.
See the definition of `treetop-font-lock-syntactic-keywords'."
  (let ((pss (syntax-ppss)) (case-fold-search nil))
    ;; If we aren't in a string, we definitely aren't ending a heredoc,
    ;; so we can just give up.
    ;; This means we aren't doing a full-document search
    ;; every time we enter a character.
    (when (treetop-in-ppss-context-p 'heredoc pss)
      (save-excursion
        (goto-char (nth 8 pss))  ; Go to the beginning of heredoc.
        (let ((eol (point)))
          (beginning-of-line)
          (if (and (re-search-forward (treetop-here-doc-beg-match) eol t) ; If there is a heredoc that matches this line...
                   (not (treetop-in-ppss-context-p 'anything)) ; And that's not inside a heredoc/string/comment...
                   (progn (goto-char (match-end 0)) ; And it's the last heredoc on its line...
                          (not (re-search-forward treetop-here-doc-beg-re eol t))))
              (string-to-syntax "|")))))))

(if (featurep 'xemacs)
    (put 'treetop-mode 'font-lock-defaults
         '((treetop-font-lock-keywords)
           nil nil nil
           beginning-of-line
           (font-lock-syntactic-keywords
            . treetop-font-lock-syntactic-keywords))))

(defvar treetop-font-lock-syntax-table
  (let ((tbl (copy-syntax-table treetop-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" tbl)
    tbl)
  "The syntax table to use for fontifying treetop-mode buffers.
See `font-lock-syntax-table'.")

(defconst treetop-font-lock-keywords
  (list
   ;; functions
   '("^\\s *def\\s +\\([^( \t\n]+\\)"
     1 font-lock-function-name-face)
   ;; keywords
   (cons (concat
          "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b\\(defined\\?\\|"
          (regexp-opt
           '("alias_method"
             "alias"
             "and"
             "begin"
             "break"
             "case"
             "catch"
             "class"
             "def"
             "do"
             "elsif"
             "else"
             "fail"
             "ensure"
             "for"
             "grammar"
             "end"
             "if"
             "in"
             "module_function"
             "module"
             "next"
             "not"
             "or"
             "public"
             "private"
             "protected"
             "raise"
             "redo"
             "rescue"
             "retry"
             "return"
             "rule"
             "then"
             "throw"
             "super"
             "unless"
             "undef"
             "until"
             "when"
             "while"
             "yield")
           t)
          "\\)"
          treetop-keyword-end-re)
         2)
   ;; here-doc beginnings
   (list treetop-here-doc-beg-re 0 'font-lock-string-face)
   ;; variables
   '("\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b\\(nil\\|self\\|true\\|false\\)\\>"
     2 font-lock-variable-name-face)
   ;; variables
   '("\\(\\$\\([^a-zA-Z0-9 \n]\\|[0-9]\\)\\)\\W"
     1 font-lock-variable-name-face)
   '("\\(\\$\\|@\\|@@\\)\\(\\w\\|_\\)+"
     0 font-lock-variable-name-face)
   ;; general delimited string
   '("\\(^\\|[[ \t\n<+(,=]\\)\\(%[xrqQwW]?\\([^<[{(a-zA-Z0-9 \n]\\)[^\n\\\\]*\\(\\\\.[^\n\\\\]*\\)*\\(\\3\\)\\)"
     (2 font-lock-string-face))
   ;; constants
   '("\\(^\\|[^_]\\)\\b\\([A-Z]+\\(\\w\\|_\\)*\\)"
     2 font-lock-type-face)
   ;; symbols
   '("\\(^\\|[^:]\\)\\(:\\([-+~]@?\\|[/%&|^`]\\|\\*\\*?\\|<\\(<\\|=>?\\)?\\|>[>=]?\\|===?\\|=~\\|\\[\\]=?\\|\\(\\w\\|_\\)+\\([!?=]\\|\\b_*\\)\\|#{[^}\n\\\\]*\\(\\\\.[^}\n\\\\]*\\)*}\\)\\)"
     2 font-lock-reference-face)
   ;; expression expansion
   '("#\\({[^}\n\\\\]*\\(\\\\.[^}\n\\\\]*\\)*}\\|\\(\\$\\|@\\|@@\\)\\(\\w\\|_\\)+\\)"
     0 font-lock-variable-name-face t)
   ;; warn lower camel case
                                        ;'("\\<[a-z]+[a-z0-9]*[A-Z][A-Za-z0-9]*\\([!?]?\\|\\>\\)"
                                        ;  0 font-lock-warning-face)
   )
  "Additional expressions to highlight in ruby mode.")

;;;###autoload
(defun treetop-mode ()
  "Major mode for editing Ruby scripts.
\\[treetop-indent-line] properly indents subexpressions of multi-line
class, module, def, if, while, for, do, and case statements, taking
nesting into account.

The variable treetop-indent-level controls the amount of indentation.
\\{treetop-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map treetop-mode-map)
  (setq mode-name "Treetop")
  (setq major-mode 'treetop-mode)
  (treetop-mode-variables)

  (set (make-local-variable 'imenu-create-index-function)
       'treetop-imenu-create-index)
  (set (make-local-variable 'add-log-current-defun-function)
       'treetop-add-log-current-method)

  (add-hook
   (cond ((boundp 'before-save-hook)
          (make-local-variable 'before-save-hook)
          'before-save-hook)
         ((boundp 'write-contents-functions) 'write-contents-functions)
         ((boundp 'write-contents-hooks) 'write-contents-hooks))
   'treetop-mode-set-encoding)

  (set (make-local-variable 'font-lock-defaults)
       '((treetop-font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords)
       treetop-font-lock-keywords)
  (set (make-local-variable 'font-lock-syntax-table)
       treetop-font-lock-syntax-table)
  (set (make-local-variable 'font-lock-syntactic-keywords)
       treetop-font-lock-syntactic-keywords)

  (if (fboundp 'run-mode-hooks)
      (run-mode-hooks 'treetop-mode-hook)
    (run-hooks 'treetop-mode-hook)))

;;; Invoke treetop-mode when appropriate

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.treetop$" . treetop-mode))

(provide 'treetop-mode)