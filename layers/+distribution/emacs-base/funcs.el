
(defun bootstrap:emacsbin-path ()
  (interactive)
  (concat exec-directory
          (if (windows-p)
              "bin/")
          "emacs"))

(defun bootstrap:emacs-start ()
  (interactive)
  (call-process (bootstrap:emacsbin-path) nil 0 nil)
  (message "Started `emacs' - it will be ready soon..."))

(defun bootstrap:emacs-debug-init ()
  (interactive)
  (call-process (bootstrap:emacsbin-path) nil 0 nil "--debug-init")
  (message "Started `emacs --debug-init' - it will be ready soon..."))

(defun bootstrap:emacs-reload ()
  (interactive)
  (load-file user-init-file)
  (message ".emacs reloaded successfully."))

(defun bootstrap:emacs-Q ()
  (interactive)
  (call-process (bootstrap:emacsbin-path) nil 0 nil "-Q")
  (message "Started `emacs -Q' - it will be ready soon..."))

(defun bootstrap:add-to-hooks (fun hooks)
  "Add a function to the given hooks."
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun bootstrap:add-to-hook (hook &rest funs)
  "Add all given functions to a hook."
  (dolist (fun funs)
    (add-hook hook fun)))

(defun bootstrap:add-all-to-hook (hook funs)
  "Add a list of functions to a hook"
  (bootstrap:add-to-hook hook funs))

(defun bootstrap:echo (msg &rest args)
  "Display MSG in echo area without logging it."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

(defun bootstrap:jump-in-buffer ()
  (interactive)
  (cond ((eq major-mode 'org-mode)
         (call-interactively 'helm-org-in-buffer-headings))
        (t
         (call-interactively 'helm-semantic-or-imenu))))

(defun bootstrap:split-and-new-line ()
  (interactive)
  (sp-split-sexp 1)
  (sp-newline))

(defun bootstrap:push-mark-and-goto-beginning-of-line ()
  (interactive)
  (push-mark (point))
  (beginning-of-line))

(defun bootstrap:push-mark-and-got-end-of-line ()
  (interactive)
  (push-mark (point))
  (end-of-line))

(defvar *bootstrap-indent-sensitive-modes*
  '(coffee-mode
    python-mode
    slim-mode
    haml-mode
    yaml-mode
    makefile-mode
    makefile-gmake-mode
    makefile-imake-mode
    makefile-bsdmake-mode)
  "Modes for which auto-indenting is suppressed.")

(defcustom bootstrap-yank-indent-threshold 1000
  "Threshold over which indentation does not automatically occur."
  :type 'number
  :group 'bootstrap)

(defun bootstrap:indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode *bootstrap-indent-sensitive-modes*)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-region (point-min) (point-max))
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(defun bootstrap:eval-current-form ()
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set\\|(let")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun bootstrap:eval-and-replace ()
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error
     (message "Invalid expression.")
     (insert (current-kill 0)))))

(defun bootstrap:useless-buffer-p (buffer)
  "Determines if a buffer is useful."
  (let ((buf-paren-major-mode (get (with-current-buffer buffer
                                     major-mode)
                                   'derived-mode-parent))
        (buf-name (buffer-name buffer)))
    ;; first find if useful buffer exists, if so returns nil and don't check for
    ;; useless buffers. If no useful buffer is found, check for useless buffers.
    (unless (cl-loop for regexp in *bootstrap-useful-buffers-regexp* do
                     (when (or (eq buf-paren-major-mode 'comint-mode)
                               (string-match regexp buf-name))
                       (return t)))
      (cl-loop for regexp in *bootstrap-useless-buffers-regexp* do
               (when (string-match regexp buf-name)
                 (return t))))))

;; from magnars modified by ffevotte for dedicated windows support
(defun bootstrap:rotate-windows (count)
  "Rotate your windows.
Dedicated windows are left untouched. Giving a negative prefix
argument takes the kindows rotate backwards."
  (interactive "p")
  (let* ((non-dedicated-windows (remove-if 'window-dedicated-p (window-list)))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))

                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun bootstrap:rotate-windows-backward (count)
  "Rotate your windows backward."
  (interactive "p")
  (bootstrap:rotate-windows (* -1 count)))

(defun bootstrap:next-useful-buffer ()
  "Switch to the next buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (next-buffer)
    (while (and (bootstrap:useless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (next-buffer))))

(defun bootstrap:previous-useful-buffer ()
  "Switch to the previous buffer and avoid special buffers."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (previous-buffer)
    (while (and (bootstrapuseless-buffer-p (current-buffer))
                (not (eq (current-buffer) start-buffer)))
      (previous-buffer))))

;; from magnars
(defun bootstrap:rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename
                  (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir))
                            (yes-or-no-p
                             (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'"
                        name
                        (file-name-nondirectory new-name))))))))

;; from magnars
(defun bootstrap:delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename
                  (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; from magnars
(defun bootstrap:sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:"
                                 buffer-file-name))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun bootstrap:kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (let (name (buffer-name))
    (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                               buffer-file-name))
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
      (message "Buffers deleted!"))))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun bootstrap:toggle-current-window-dedication ()
  "Toggle dedication state of a window."
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun bootstrap:show-and-copy-buffer-filename ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;; adapted from bozhidar
;; http://emacsredux.com/blog/2013/05/18/instant-access-to-init-dot-el/
(defun bootstrap:find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing user-init-file))

(defun bootstrap:new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun bootstrap:layout-triple-columns ()
  " Set the layout to triple columns. "
  (interactive)
  (golden-ratio-mode 0)
  (delete-other-windows)
  (dotimes (i 2) (split-window-right))
  (balance-windows))

(defun bootstrap:layout-double-columns ()
  " Set the layout to double columns. "
  (interactive)
  (golden-ratio-mode 1)
  (delete-other-windows)
  (split-window-right))

(defun bootstrap:home ()
  "Go to home Spacemacs buffer"
  (interactive)
  (switch-to-buffer "*bootstrap*"))

(defun bootstrap:insert-line-above-no-indent (count)
  (interactive "p")
  (let ((p (+ (point) count)))
    (save-excursion
       (if (eq (line-number-at-pos) 1)
          (move-beginning-of-line)
        (progn
          (previous-line)
          (move-end-of-line)))
      (while (> count 0)
        (insert "\n")
        (setq count (1- count))))
    (goto-char p)))

(defun bootstrap:insert-line-below-no-indent (count)
  "Insert a new line below with no identation."
  (interactive "p")
  (save-excursion
    (move-end-of-line)
    (while (> count 0)
      (insert "\n")
      (setq count (1- count)))))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun bootstrap:kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This
function, unlike the built-in `kill-matching-buffers` does so
WITHOUT ASKING. The optional second argument indicates whether to
kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defvar *bootstrap-really-kill-emacs* nil
  "prevent window manager close from closing instance.")

(defun bootstrap:persistent-server-running-p ()
  "Requires spacemacs-really-kill-emacs to be toggled and
dotspacemacs-persistent-server to be t"
  (and (fboundp 'server-running-p)
       (server-running-p)
       *bootstrap-persistent-server*))

(defadvice kill-emacs (around spacemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (and (not *bootstrap-really-kill-emacs*)
           (bootstrap:persistent-server-running-p))
      (bootstrap:frame-killer)
    ad-do-it))

(defadvice save-buffers-kill-emacs (around spacemacs-really-exit activate)
  "Only kill emacs if a prefix is set"
  (if (or *bootstrap-really-kill-emacs*
          (not *bootstrap-persistent-server*))
      ad-do-it
    (bootstrap:frame-killer)))

(defun bootstrap:save-buffers-kill-emacs ()
  "Save all changed buffers and exit Spacemacs"
  (interactive)
  (setq *bootstrap-really-kill-emacs* t)
  (save-buffers-kill-emacs))

(defun bootstrap:kill-emacs ()
  "Lose all changes and exit Spacemacs"
  (interactive)
  (setq *bootstrap-really-kill-emacs* t)
  (kill-emacs))

(defun bootstrap:prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (setq *bootstrap-really-kill-emacs* t)
  (save-some-buffers)
  (kill-emacs))

(defun bootstrap:frame-killer ()
  "Kill server buffer and hide the main Emacs window"
  (interactive)
  (server-kill-buffer)
  (condition-case nil
      (delete-frame nil 1)
      (error
       (make-frame-invisible nil 1))))

;; taken from Prelude: https://github.com/bbatsov/prelude
(defmacro bootstrap:advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defun bootstrap:safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun bootstrap:safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (if (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
      (erase-buffer)))

(defun bootstrap:alternate-buffer ()
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (if (alternate-buffer)
      (switch-to-buffer (car (alternate-buffer)))
    (switch-to-buffer (other-buffer (current-buffer) t))))

(defun bootstrap:highlight-TODO-words ()
  "Highlight keywords for  "
  (interactive)
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun current-line ()
  "Return the line at point as a string."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun bootstrap:next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (if (and (boundp 'flycheck-mode)
           (symbol-value flycheck-mode))
      (call-interactively 'flycheck-next-error)
    (call-interactively 'next-error)))

(defun bootstrap:previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (if (and (boundp 'flycheck-mode)
           (symbol-value flycheck-mode))
      (call-interactively 'flycheck-previous-error)
    (call-interactively 'previous-error)))

(defun bootstrap:switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun bootstrap:comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; http://stackoverflow.com/a/10216338/4869
(defun bootstrap:copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun bootstrap:copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun bootstrap:align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from
;; http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun bootstrap:align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro bootstrap:create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "spacemacs/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (bootstrap:align-repeat start end ,regexp ,justify-right after)))))

(bootstrap:create-align-repeat-x "comma" "," nil t)
(bootstrap:create-align-repeat-x "semicolon" ";" nil t)
(bootstrap:create-align-repeat-x "colon" ":" nil t)
(bootstrap:create-align-repeat-x "equal" "=")
(bootstrap:create-align-repeat-x "math-oper" "[+\\-*/]")
(bootstrap:create-align-repeat-x "ampersand" "&")
(bootstrap:create-align-repeat-x "bar" "|")
(bootstrap:create-align-repeat-x "left-paren" "(")
(bootstrap:create-align-repeat-x "right-paren" ")" t)

(defun bootstrap:write-file ()
  "Write the file if visiting a file.
   Otherwise ask for new filename."
  (interactive)
  (if (buffer-file-name)
      (call-interactively 'evil-write)
    (call-interactively 'write-file)))


(defun bootstrap:dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun bootstrap:unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun bootstrap:copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

(defun bootstrap::image-p (object)
  "Tests whether the given object is an image (a list whose
first element is the symbol `image')."
  (and (listp object)
       object
       (eq 'image (car object))))

(defun bootstrap:uniquify-lines ()
  "Remove duplicate adjacent lines in region or current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
            (end (if (region-active-p) (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun bootstrap:sort-lines ()
  "Sort lines in region or current buffer"
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defvar *bootstrap-linum-mdown-line* nil
  "Define persistent variable for linum selection")

(defun bootstrap:line-at-click ()
  "Determine the visual line at click"
  (save-excursion
    (let ((click-y (cddr (mouse-position)))
          (debug-on-error t)
          (line-move-visual t))
      (goto-char (window-start))
      (next-line (1- click-y))
      (1+ (line-number-at-pos))
      )))

(defun bootstrap:md-select-linum (event)
  "Set point as spacemacs-linum-mdown-line"
  (interactive "e")
  (mouse-select-window event)
  (goto-line (bootstrap:line-at-click))
  (set-mark (point))
  (setq *bootstrap-linum-mdown-line*
        (line-number-at-pos)))

(defun bootstrap:mu-select-linum ()
  "Select code block between point and spacemacs-linum-mdown-line"
  (interactive)
  (when *bootstrap-linum-mdown-line*
    (let (mu-line)
      (setq mu-line (bootstrap:line-at-click))
      (goto-line (max *bootstrap-linum-mdown-line* mu-line))
      (set-mark (line-end-position))
      (goto-line (min *bootstrap-linum-mdown-line* mu-line))
      (setq *bootstrap-linum-mdown-line* nil))))

(defun bootstrap:select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))

;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
      (lambda (buf str)

        (if (or (string-match "exited abnormally" str)
                (string-match "FAILED" (buffer-string)))

            ;; there were errors
            (message "There were errors. SPC-e-n to visit.")
          (unless (or (string-match "Grep finished" (buffer-string))
                      (string-match "Ag finished" (buffer-string))
                      (string-match "nosetests" (buffer-name)))

            ;; no errors
            (message "compilation ok.")))))

;; from http://www.emacswiki.org/emacs/WordCount
(defun bootstrap:count-words-analysis (start end)
  "Count how many times each word is used in the region.
 Punctuation is ignored."
  (interactive "r")
  (let (words)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\w+" end t)
        (let* ((word (intern (match-string 0)))
               (cell (assq word words)))
          (if cell
              (setcdr cell (1+ (cdr cell)))
            (setq words (cons (cons word 1) words))))))
    (when (interactive-p)
      (message "%S" words))
    words))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun bootstrap:yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) bootstrap-yank-indent-threshold)
      (indent-region beg end nil)))

(bootstrap:advise-commands
 "indent" (yank yank-pop) around
 "If current mode is not one of spacemacs-indent-sensitive-modes
 indent yanked text (with universal arg don't indent)."
 ad-do-it
 (if (and (not (equal '(4) (ad-get-arg 0)))
          (not (member major-mode *bootstrap-indent-sensitive-modes*))
          (or (derived-mode-p 'prog-mode)
              (member major-mode *bootstrap-indent-sensitive-modes*)))
     (let ((transient-mark-mode nil)
           (save-undo buffer-undo-list))
       (bootstrap:yank-advised-indent-function (region-beginning)
                                               (region-end)))))

(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun bootstrap:insert-date-string (&optional time-value)
  "Insert the current date/time at the cursor."
  (interactive)
  (let ((time-string (if (emacs=18-p)
                         (current-time-string)
                       (current-time-string time-value))))
    (insert time-string)
    time-string))

