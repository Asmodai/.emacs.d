
;; Auto-refresh
(global-auto-revert-mode 1)

;; Also auto-refresh dired, but be quiet.
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Regexp for useful and useless buffers for smarter buffer switching
(defvar bootstrap-useless-buffers-regexp '("*\.\+")
  "Regexp used to determine if a buffer is not useful.")

(defvar bootstrap-useful-buffers-regexp
  '("\\*\\(scratch\\|terminal\.\+\\|ansi-term\\|eshell\\)\\*")
  "Regexp used to define buffers that are useful despite matching
`bootstrap-useless-buffers-regexp'.")

;; Be quiet, Emacs!
(setq ring-bell-function 'ignore
      visible-bell nil)

;; Hack to fix a bug with tabulated-list.el
;; see: http://redd.it/2dgy52
(defun tabulated-list-revert (&rest ignored)
  "The `revert-buffer-function' for `tabulated-list-mode'.
It runs `tabulated-list-revert-hook', then calls `tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'tabulated-list-mode)
    (error "The current buffer is not in Tabulated List mode"))
  (run-hooks 'tabulated-list-revert-hook)
  ;; hack is here
  ;; (tabulated-list-print t)
  (tabulated-list-print))

;; Mouse functionality in terminals.
(xterm-mouse-mode 1)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

;; Use only spaces and no tabs
(setq-default indent-tabs-mode nil
              default-tab-width 2)

;; Use system trash for file deletion
;; should work on Windows and Linux distros
;; on OS X, see contrib/osx layer
(setq delete-by-moving-to-trash t)

;; auto fill breaks line beyond current-fill-column
(setq-default default-fill-column 80)
;;(bootstrap|diminish auto-fill-function " Ⓕ" " F")

;; persistent abbreviation file
(setq abbrev-file-name (concat +bootstrap-cache-directory+ "abbrev_defs"))

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

;; important for golden-ratio to better work
(setq window-combination-resize t)

;; fringes
(setq-default fringe-indicator-alist
              '((truncation . nil) (continuation . nil)))

;; Show column number in mode line
(setq column-number-mode t)

;; Default linum format string.
(defvar *bootstrap-linum-format-string* "%4d")

(defun bootstrap::linum-get-format-string ()
  "Dynmically compute the linum format string for `linum-format'."
  (let* ((width (length (number-to-string
                         (count-lines (point-min) (point-max)))))
         (format (concat "%" (number-to-string width) "d")))
    (setq *bootstrap-linum-format-string* format)))

;; Add a hook for the dynamic linum format.
(add-hook 'linum-before-numbering-hook 'bootstrap::linum-get-format-string)

(defun bootstrap::linum-format (line-number)
  "Generate the linum format string."
  (propertize (format *bootstrap-linum-format-string* line-number)
              'face 'linum))

;; Set linum format generator.
(setq linum-format 'bootstrap::linum-format)

(defun bootstrap::linum-hook ()
  "`linum-mode' hook for modes that want to load linum."
  (linum-mode))

;; Enable linum-mode for programming modes.
(add-hook 'prog-mode-hook 'bootstrap::linum-hook)

;; Enable linum-mode for all text-based modes.
(add-hook 'text-mode-hook 'bootstrap::linum-hook)

;; highlight current line
(global-hl-line-mode t)

;; no blink
(blink-cursor-mode 0)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face
                  minibuffer-prompt))

(defvar *bootstrap-global-mode-line-excludes* nil
  "List of elements to exclude from the global modeline string.
These should have their own segments in the modeline.")

;; save custom variables in ~/.spacemacs
(setq custom-file +bootstrap-custom-file+)

;; scratch buffer empty
(setq initial-scratch-message nil)

;; don't create backup~ files
(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; Auto-save file
(setq auto-save-default (not (null +bootstrap-auto-save-directory+)))
(setq auto-save-list-file-prefix (concat +bootstrap-auto-save-directory+))

;; always save TRAMP URLs to cache directory no matter what is the value
;; of `dotspacemacs-auto-save-file-location'
(let ((autosave-dir (concat +bootstrap-auto-save-directory+ "dist/")))
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,autosave-dir  t)))
  (unless (or (file-exists-p autosave-dir)
              (null +bootstrap-auto-save-directory+))
    (make-directory autosave-dir t)))

(case +bootstrap-auto-save-directory+
  (cache (let ((autosave-dir (concat +bootstrap-auto-save-directory+ "site/")))
           (add-to-list 'auto-save-file-name-transforms
                        `(".*" ,autosave-dir t) 'append)
           (unless (file-exists-p autosave-dir)
             (make-directory autosave-dir t))))
  (original (setq auto-save-visited-file-name t))
  (_ (setq auto-save-default nil
           auto-save-list-file-prefix nil)))

;; remove annoying ellipsis when printing sexp in message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

(setq url-configuration-directory (concat +bootstrap-cache-directory+ "url")
      eshell-directory-name (concat +bootstrap-cache-directory+ "eshell" )
      tramp-persistency-file-name (concat +bootstrap-cache-directory+ "tramp"))

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; remove prompt if the file is opened in other clients
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

;; Put mouse selection in the kill buffer
(when (or (windows-p)
          (x-windows-p)
          (nextstep-p)
          (presentation-manager-p))
  (defun mouse-track-drag-copy-to-kill (event count)
    "Copy the dragged region to the kill ring."
    (let ((region (default-mouse-track-return-dragged-selection
                    event)))
      (when region
        (copy-region-as-kill (car region) (cdr region)))
      nil))
  (add-hook 'mouse-track-drag-up-hook 'mouse-track-drag-copy-to-kill))

;; Avoid deactivation of a region when the buffer end or beginning is
;; reached.
(defadvice line-move (around catch-buffer-border-error activate)
  "Catch errors `beginning-of-buffer' and `end-of-buffer' to avoid
deactivation of the region."
  (condition-case ()
      ad-do-it
    ((beginning-of-buffer end-of-buffer))))

;; Window system hacks
(when (not (terminal-p))
  (require 'msb))

;; Various hacks
(setq zmacs-regions t
      complex-buffers-menu-p t
      case-fold-search t
      case-replace t
      mouse-yank-at-point t
      require-final-newline t)

;; Buffer boundaries and empty lines
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)
