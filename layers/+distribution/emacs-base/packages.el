
(setq emacs-base-packages
      '(ace-link
        ace-window
        adaptive-wrap
        aggressive-indent
        auto-dictionary
        auto-highlight-symbol
        avy
        buffer-move
        (centered-cursor :location local)
        clean-aindent-mode
        define-word
        desktop
        doc-view
        eval-sexp-fu
        expand-region
        fancy-battery
        flx-ido
        golden-ratio
        google-translate
        helm-ag
        helm-make
        helm-mode-manager
        helm-swoop
        helm-themes
        highlight-indentation
        highlight-numbers
        highlight-parentheses
        (hl-anything :excluded t)
        hungry-delete
        info+
        iedit
        indent-guide
        open-junk-file
        move-text
        neotree
        pcre2el
        powerline
        rainbow-delimiters
        recentf
        smartparens
        smooth-scrolling
        spray
        volatile-highlights
        window-numbering
        (zoom-frm :location local)
        bind-key
        bookmark
        diminish
        (electric-indent-mode :location built-in)
        ediff
        eldoc
        exec-path-from-shell
        fill-column-indicator
        helm
        helm-descbinds
        helm-projectile
        (ido :location built-in)
        ido-vertical-mode
        page-break-lines
        popup
        popwin
        (process-menu :location built-in)
        projectile
        quelpa
        savehist
        saveplace
        subword
        spacemacs-theme
        undo-tree
        (uniquify :location built-in)
        use-package
        which-key
        whitespace
        winner
        company
        folding
        htmlize
        (template :location local)))
        

(defvar *bootstrap-diminished-minor-modes* '())

;; Paradox from MELPA is not compatible with 24.3, so we use
;; a local paradox with 24.3
(if  (version< emacs-version "24.4")
    (push '(paradox :location local) emacs-base-packages)
  (push 'paradox emacs-base-packages))

(defun emacs-base:init-ace-link ()
  (use-package ace-link
    :commands bootstrap:ace-buffer-links
    :init
    (progn
      (define-key bootstrap-mode-map "o" 'bootstrap:ace-buffer-links)
      (eval-after-load "info"
        '(define-key Info-mode-map "o" 'ace-link-info))
      (eval-after-load "help-mode"
        '(define-key help-mode-map "o" 'ace-link-help))
      (eval-after-load "eww"
        '(progn
           (define-key eww-link-keymap "o" 'ace-link-eww)
           (define-key eww-mode-map "o" 'ace-link-eww))))
    :config
    (progn
      (defvar *bootstrap-link-pattern* "~?/.+\\|\s\\[")
      (defun bootstrap::collect-bootstrap-buffer-links ()
        (let ((end (window-end))
              points)
          (save-excursion
            (goto-char (window-start))
            (while (re-search-forward *bootstrap-link-pattern* end t)
              (push (+ (match-beginning 0) 1) points))
            (nreverse points))))
      (defun bootstrap:ace-buffer-links ()
        "Ace jump to links in `bootstrap' buffer."
        (interactive)
        (let ((res (avy--with-avy-keys bootstrap:ace-buffer-links
                    (avy--process
                        (bootstrap::collect-bootstrap-buffer-links)
                        #'avy--overlay-pre))))
            (when res
            (goto-char (1+ res))
            (widget-button-press (point))))))))

(defun emacs-base:init-ace-window ()
  (use-package ace-window
    :defer t))

(defun emacs-base:init-adaptive-wrap ()
  (use-package adaptive-wrap
    :config
    (progn
      (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))))

(defun emacs-base:init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :config
    (bootstrap:diminish aggressive-indent-mode " Ⓘ" " I")))

(defun emacs-base:init-auto-dictionary ()
  (use-package auto-dictionary
    :disabled t
    :defer t
    :init
    (progn
      (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1))))))

(defun emacs-base:init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :init
    (progn
      (setq ahs-case-fold-search nil
            ahs-default-range 'ahs-range-whole-buffer
            ahs-idle-timer 0
            ahs-idle-interval 0.25
            ahs-inhibit-face-list nil)
      
      (bootstrap:add-to-hooks 'auto-highlight-symbol-mode
                              '(prog-mode-hook markdown-mode-hook)))
    :config
    (progn
      (defvar-local bootstrap-last-ahs-highlight-p nil)
      (defvar-local bootstrap-ahs-searching-forward t)

      (defun bootstrap:goto-last-searched-ahs-symbol ()
        (interactive)
        (if bootstrap-last-ahs-highlight-p
            (progn
              (goto-char (nth 1 bootstrap-last-ahs-highlight-p))
              (bootstrap:ahs-highlight-now-wrapper))
          (message "No symbol has been searched for now.")))

      (defun bootstrap:ensure-ahs-enabled-locally ()
        "Ensures that AHS has been enabled for the local buffer."
        (unless
            (bound-and-true-p ahs-mode-line)
          (auto-highlight-symbol-mode)))

      (defun bootstrap:ahs-highlight-now-wrapper()
        "Safe wrapper for `ahs-highlight-now'."
        (eval '(progn
                 (bootstrap:ensure-ahs-enabled-locally)
                 (ahs-highlight-now))
              nil))

      (defun bootstrap:enter-ahs-forward ()
        "Go to the next occurrence of a symbol under the point with
`auto-highlight-symbol'."
        (interactive)
        (setq bootstrap-ahs-searching-forward t)
        (bootstrap:quick-ahs-forward))

      (defun bootstrap:enter-ahs-backward ()
        "Go to the previous occurrence of the symbol under the point with
`auto-highlight-symbol'."
        (interactive)
        (setq bootstrap-ahs-searching-forward nil)
        (bootstrap:quick-ahs-forward))

      (defun bootstrap:quick-ahs-forward ()
        "Go to the next occurrence of the symbol under the point with
`auto-highlight-symbol'."
        (interactive)
        (bootstrap::quick-ahs-move t))

      (defun bootstrap:quick-ahs-backward ()
        (interactive)
        (bootstrap::quick-ahs-move t))

      (defun bootstrap::quick-ahs-move (forward)
        "Move to an occurrence of the symbol under the point using
`auto-highlight-symbol'.

If FORWARD is non-NIL, then the motion will be forward; otherwise the motion
will be backward."
        (bootstrap:ahs-highlight-now-wrapper)
        (if (eq forward bootstrap-ahs-searching-forward)
            (ahs-forward)
          (ahs-backward)))

      (defun bootstrap:symbol-highlight ()
        "Highlight the symbol under the point with `auto-highlight-symbol'."
        (interactive)
        (bootstrao:ahs-highlight-now-wrapper)
        (setq bootstrap-last-ahs-highlight-p (ahs-highlight-p)))

      (defun bootstrap:symbol-highlight-reset-range ()
        "Resets the range for `auto-highlight-symbol'."
        (interactive)
        (ahs-change-range ahs-default-range))

      (bootstrap:hide-lighter auto-highlight-symbol-mode))))

(defun emacs-base:init-avy ()
  (use-package avy
    :defer t
    :init
    (setf avy-keys (number-sequence ?a ?z)
          avy-all-windows 'all-frames
          avy-background t)))

(defun emacs-base:init-buffer-move ()
  (use-package buffer-move
    :defer t))

(defun emacs-base:init-centered-cursor ()
  (use-package centered-cursor-mode
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :config
    (progn
      (setq ccm-recenter-at-end-of-file t
            ccm-ignored-commands '(mouse-drag-region
                                   mouse-set-point
                                   widget-button-click
                                   scroll-bar-toolkit-scroll))
      (bootstrap:diminish centered-cursor-mode " ⊝" " -"))))

(defun emacs-base:init-clean-aindent-mode ()
  (use-package clean-aindent-mode
    :defer t
    :init
    (add-hook 'prog-mode-hook 'clean-aindent-mode)))

(defun emacs-base:init-desktop ()
  (use-package desktop
    :defer t
    :config
    (progn
      (setq desktop-dirname +bootstrap-cache-directory+)
      (push +bootstrap-cache-directory+ desktop-path))))

(defun emacs-base:init-define-word ()
  (use-package define-word
    :defer t))

(defun emacs-base:init-dired+ ()
  (use-package dired+
    :defer t))

(defun emacs-base:init-doc-view ()
  (use-package doc-view
    :defer t
    :config
    (progn
      (defun bootstrap:doc-view-search-now-query ()
        "Initiate a new doc view query."
        (interactive)
        (doc-view-search 'newquery))

      (defun bootstrap:doc-view-search-new-query-backward ()
        "Initiate a new doc view query that searches backwards."
        (interactive)
        (doc-view0search 'newquery t))

      (defadvice doc-view-toggle-display
          (around bootstrap:doc-view-togle-display activate)
        (if (eq major-mode 'doc-view-mode)
            (progn
              ad-do-it
              (text-mode)
              (doc-view-minor-mode))
          ad-do-it)))))

(defun emacs-base:init-eval-sexp-fu ()
  ;; ignore obsolete function warning generated on startup
  (let ((byte-compile-not-obsolete-funcs
         (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
    (require 'eval-sexp-fu)))

(defun emacs-base:init-expand-region ()
  (use-package expand-region
    :defer t
    :config
    (progn
      (when (bootstrap:package-used-p 'helm-ag)
        (defadvice er/prepare-for-more-expansions-internal
            (around helm-ag/prepare-for-more-expansions-internal activate)
          ad-do-it
          (let ((new-msg (concat (car ad-return-value)
                                 ", / to search in project, "
                                 "f to search in files, "
                                 "b to search in opened buffers"))
                (new-bindings (cdr ad-return-value)))
            (cl-pushnew
             '("/"
               (lambda ()
                 (call-interactively
                  'bootstrap:helm-project-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("b"
               (lambda ()
                 (call-interactively
                  'bootstrap:helm-buffers-smart-do-search-region-or-symbol)))
             new-bindings)
            (setq ad-return-value (cons new-msg new-bindings)))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r"))))

(defun emacs-base:init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (progn
      (push 'fancy-battery-mode-line *bootstrap-global-mode-line-excludes*)

      (defun bootstrap:mode-line-battery-percentage ()
        "Return the lod percentage or an empty string."
        (let ((p (cdr (assq ?p fancy-battery-last-status))))
          (if (and fancy-battery-show-percentage
                   p
                   (not (string= "N/A" p)))
              (concat " " p "%%") "")))

      (defun bootstrap:mode-line-battery-time ()
        "Return the remaining time left on battery."
        (let ((time (cdr (assq ?t fancy-battery-last-status))))
          (cond ((string= "0:00" time)
                 "")
                ((string= "N/A" time)
                 "")
                ((string-empty-p time)
                 "")
                (t
                 (concat " (" time ")")))))

      (setq-default fancy-battery-show-percentage t)

      (when (or (running-on-voyager-p)
                (running-on-mbr15_pward-p))
        (fancy-battery-mode)))
    :config
    (progn

      (defun fancy-battery-default-mode-line ()
        "Assemble a mode line string for Fancy Battery Mode."
        (when fancy-battery-last-status
          (let* ((type (cdr (assq ?L fancy-battery-last-status)))
                 (percentage (bootstrap:mode-line-battery-percentage))
                 (time (bootstrap:mode-line-battery-time)))
            (cond
             ((string= "on-line" type) " No Battery")
             ((string-empty-p type) " No Battery")
             (t (concat (if (string= "AC" type) " AC" "") percentage time))))))

      (defun fancy-battery-powerline-face ()
        "Return a face appropriate for powerline"
        (let ((type (cdr (assq ?L fancy-battery-last-status))))
          (if (and type (string= "AC" type))
              'fancy-battery-charging
            (pcase (cdr (assq ?b fancy-battery-last-status))
              ("!"  'fancy-battery-critical)
              ("+"  'fancy-battery-charging)
              ("-"  'fancy-battery-discharging)
              (_ 'fancy-battery-discharging))))))))

(defun emacs-base:init-flx-ido ()
  (use-package flx-ido))

(defun emacs-base:init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :config
    (progn
      (setq golden-ratio-exclude-modes '("bs-mode"
                                         "calc-mode"
                                         "ediff-mode"
                                         "dired-mode"
                                         "gud-mode"
                                         "gdb-locals-mode"
                                         "gdb-registers-mode"
                                         "gdb-breakpoints-mode"
                                         "gdb-threads-mode"
                                         "gdb-frames-mode"
                                         "gdb-inferior-io-mode"
                                         "gud-mode"
                                         "gdb-inferior-io-mode"
                                         "gdb-disassembly-mode"
                                         "gdb-memory-mode"
                                         "restclient-mode"
                                         "speedbar-mode"))

      (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*[hH]elm.*")

      (setq golden-ratio-extra-commands
            (append golden-ratio-extra-commands
                    '(ace-window
                      ace-delete-window
                      ace-select-window
                      ace-swap-window
                      ace-maximize-window
                      avy-pop-mark
                      windmove-left
                      windmove-right
                      windmove-up
                      windmove-down
                      select-window-0
                      select-window-1
                      select-window-2
                      select-window-3
                      select-window-4
                      select-window-5
                      select-window-6
                      select-window-7
                      select-window-8
                      select-window-9
                      buf-move-left
                      buf-move-right
                      buf-move-up
                      buf-move-down
                      ess-eval-buffer-and-go
                      ess-eval-function-and-go
                      ess-eval-line-and-go)))

      (defun bootstrap:no-golden-ratio-for-buffers (bufname)
        (and (get-buffer bufname)
             (get-buffer-window bufname 'visible)))

      (defun bootstrap:no-golden-ratio-guide-key ()
        (or (bootstrap:no-golden-ratio-for-buffers " *guide-key*")
            (bootstrap:no-golden-ratio-for-buffers " *popwin-dummy*")))

      (add-to-list 'golden-ratio-inhibit-functions
                   'bootstrap:no-golden-ratio-guide-key)
      (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
      (add-to-list 'golden-ratio-exclude-buffer-names "*LV*")
      (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")

      (bootstrap:diminish golden-ratio-mode " ⓖ" " g"))))

(defun emacs-base:init-google-translate ()
  (use-package google-translate
    :commands (google-translate-query-translate
               google-translate-at-point
               google-translate-query-translate-reverse
               google-translate-at-point-reverse)
    :init
    (progn
      (defun bootstrap:set-google-translate-languages (source target)
        "Set source language for google translate.
For instance pass En as source for english."
        (interactive
         "sEnter source language (ie. En): \nsEnter target language (ie. En): "
         source target)
        (message
         (format "Set google translate source language to %s and target to %s"
                 source target))
        (setq google-translate-default-source-language source)
        (setq google-translate-default-target-language target)))
    :config
    (progn
      (require 'google-translate-default-ui)
      (setq google-translate-enable-ido-completion t)
      (setq google-translate-show-phonetic t)
      (setq google-translate-default-source-language "En")
      (setq google-translate-default-target-language "Es"))))

(defun emacs-base:init-helm-ag ()
  (use-package helm-ag
    :defer t
    :init
    (progn
      (defun bootstrap::helm-do-ag-region-or-symbol (func &optional dir)
        "Search with `ag' with a default input."
        (require 'helm-ag)
        (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                   ;; make thing-at-point choosing the active region first
                   ((symbol-function 'this-fn)
                    (symbol-function 'thing-at-point))
                   ((symbol-function 'thing-at-point)
                    (lambda (thing)
                      (let ((res (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (this-fn thing))))
                        (when res (rxt-quote-pcre res))))))
          (funcall func dir)))

      (defun bootstrap::helm-do-search-find-tool (base tools default-inputp)
        "Create a cond form given a TOOLS string list and evaluate it."
        (eval
         `(cond
           ,@(mapcar
              (lambda (x)
                `((executable-find ,x)
                  ',(let ((func
                           (intern
                            (format (if default-inputp
                                        "bootstrap:%s-%s-region-or-symbol"
                                      "bootstrap:%s-%s")
                                    base x))))
                      (if (fboundp func)
                          func
                        (intern (format "%s-%s"  base x))))))
                     tools)
           (t 'helm-do-grep))))

      ;; Search in current file ----------------------------------------------

      (defun bootstrap:helm-file-do-ag (&optional _)
        "Wrapper to execute `helm-ag-this-file.'"
        (interactive)
        (helm-ag-this-file))

      (defun bootstrap:helm-file-do-ag-region-or-symbol ()
        "Search in current file with `ag' using a default input."
        (interactive)
        (bootstrap:helm-do-ag-region-or-symbol 'bootstrap:helm-file-do-ag))

      (defun bootstrap:helm-file-smart-do-search (&optional default-inputp)
        "Search in current file using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (bootstrap::helm-do-search-find-tool "helm-file-do"
                                              *bootstrap-search-tools*
                                              default-inputp)))

      (defun bootstrap:helm-file-smart-do-search-region-or-symbol ()
        "Search in current file using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (bootstrap:helm-file-smart-do-search t))

      ;; Search in files -----------------------------------------------------

      (defun bootstrap:helm-files-do-ag (&optional dir)
        "Search in files with `ag' using a default input."
        (interactive)
        (helm-do-ag dir))

      (defun bootstrap:helm-files-do-ag-region-or-symbol ()
        "Search in files with `ag' using a default input."
        (interactive)
        (bootstrap::helm-do-ag-region-or-symbol 'bootstrap:helm-files-do-ag))

      (defun bootstrap:helm-files-do-ack (&optional dir)
        "Search in files with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun bootstrap:helm-files-do-ack-region-or-symbol ()
        "Search in files with `ack' using a default input."
        (interactive)
        (bootstrap::helm-do-ag-region-or-symbol 'bootstrap:helm-files-do-ack))

      (defun bootstrap:helm-files-do-pt (&optional dir)
        "Search in files with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun bootstrap:helm-files-do-pt-region-or-symbol ()
        "Search in files with `pt' using a default input."
        (interactive)
        (bootstrap::helm-do-ag-region-or-symbol 'bootstrap:helm-files-do-pt))

      (defun bootstrap:helm-files-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (bootstrap::helm-do-search-find-tool "helm-files-do"
                                              *bootstrap-search-tools*
                                              default-inputp)))

      (defun bootstrap:helm-files-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools'.
with default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (bootstrap:helm-files-smart-do-search t))

      ;; Search in buffers ---------------------------------------------------

      (defun bootstrap:helm-buffers-do-ag (&optional _)
        "Wrapper to execute `helm-ag-buffers.'"
        (interactive)
        (helm-do-ag-buffers))

      (defun bootstrap:helm-buffers-do-ag-region-or-symbol ()
        "Search in opened buffers with `ag' with a default input."
        (interactive)
        (bootstrap::helm-do-ag-region-or-symbol 'bootstrap:helm-buffers-do-ag))

      (defun bootstrap:helm-buffers-do-ack (&optional _)
        "Search in opened buffers with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun bootstrap:helm-buffers-do-ack-region-or-symbol ()
        "Search in opened buffers with `ack' with a default input."
        (interactive)
        (bootstrap::helm-do-ag-region-or-symbol 'bootstrap:helm-buffers-do-ack))

      (defun bootstrap:helm-buffers-do-pt (&optional _)
        "Search in opened buffers with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun bootstrap:helm-buffers-do-pt-region-or-symbol ()
        "Search in opened buffers with `pt' using a default input."
        (interactive)
        (bootstrap::helm-do-ag-region-or-symbol 'bootstrap:helm-buffers-do-pt))

      (defun bootstrap:helm-buffers-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (bootstrap::helm-do-search-find-tool "helm-buffers-do"
                                              *bootstrap-search-tools*
                                              default-inputp)))

      (defun bootstrap:helm-buffers-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools' with
default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (bootstrap:helm-buffers-smart-do-search t))

      ;; Search in project ---------------------------------------------------

      (defun bootstrap:helm-project-do-ag ()
        "Search in current project with `ag'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun bootstrap:helm-project-do-ag-region-or-symbol ()
        "Search in current project with `ag' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (bootstrap::helm-do-ag-region-or-symbol 'helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun bootstrap:helm-project-do-ack ()
        "Search in current project with `ack'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (bootstrap:helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun bootstrap:helm-project-do-ack-region-or-symbol ()
        "Search in current project with `ack' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (bootstrap::helm-do-ag-region-or-symbol
               'bootstrap:helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun bootstrap:helm-project-do-pt ()
        "Search in current project with `pt'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (bootstrap:helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun bootstrap:helm-project-do-pt-region-or-symbol ()
        "Search in current project with `pt' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (bootstrap::helm-do-ag-region-or-symbol 'bootstrap:helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun bootstrap:helm-project-smart-do-search (&optional default-inputp)
        "Search in current project using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (let ((projectile-require-project-root nil))
         (call-interactively
          (bootstrap::helm-do-search-find-tool "helm-project-do"
                                               *bootstrap-search-tools*
                                               default-inputp))))

      (defun bootstrap:helm-project-smart-do-search-region-or-symbol ()
        "Search in current project using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (bootstrap:helm-project-smart-do-search t)))))


(defun emacs-base:init-helm-make ()
  (use-package helm-make
    :defer t))

(defun emacs-base:init-helm-mode-manager ()
  (use-package helm-mode-manager
    :defer t))

(defun emacs-base:init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :init
    (progn
      (setq helm-swoop-split-with-multiple-windows t
            helm-swoop-split-direction 'split-window-vertically
            helm-swoop-speed-or-color t
            helm-swoop-split-window-function 'helm-default-display-buffer
            helm-swoop-pre-input-function (lambda () ""))

      (defun bootstrap:helm-swoop-region-or-symbol ()
        "Call `helm-swoop' with default input."
        (interactive)
        (let ((helm-swoop-pre-input-function
               (lambda ()
                 (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (let ((thing (thing-at-point 'symbol t)))
                     (if thing thing ""))))))
          (call-interactively 'helm-swoop))))))

(defun emacs-base:init-helm-themes ()
  (use-package helm-themes
    :defer t))

(defun emacs-base:init-highlight-indentation ()
  (use-package highlight-indentation
    :defer t
    :init
    (progn
      (bootstrap:diminish highlight-indentation-mode " ⓗⁱ" " hi")
      (bootstrap:diminish highlight-indentation-current-column-mode
                          " ⓗᶜ" " hc"))))

(defun emacs-base:init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'asm-mode-hook (lambda ()
                                 (highlight-numbers-mode -1))))))

(defun emacs-base:init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :init
    (progn
      (when (member *bootstrap-highlight-delimiters* '(all current))
        (add-hook 'prog-mode-hook 'highlight-parentheses-mode))
      (setq hl-paren-delay 0.2)
      (setq hl-paren-colors
            (cond ((display-graphic-p)
                   '("firebrick1" "DarkRed" "IndianRed"
                     "LightCoral" "Salmon" "DarkSalmon"))
                  ((and (terminal-p)
                        (256-colour-p))
                   '("color-124" "color-129" "color-163"
                     "color-127" "color-135" "color-161"))
                  (t
                   '("magenta" "cyan" "green"
                     "red" "blue" "white")))))
    :config
    (progn
      (bootstrap:diminish highlight-parentheses-mode " ⓗᵖ" " hp")
      (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold))))
    
(defun emacs-base:init-hl-anything ()
  (use-package hl-anything
    :init
    (progn
      (hl-highlight-mode)
      (setq-default hl-highlight-save-file
                    (concat +bootstrap-cache-directory+ ".hl-save")))))

(defun emacs-base:init-hungry-delete ()
  (use-package hungry-delete
    :defer t
    :config
    (progn
      (setq-default hungry-delete-chars-to-skip " \t\f\v")
      (define-key hungry-delete-mode-map (kbd "S-DEL") 'hungry-delete-backward)
      (define-key hungry-delete-mode-map (kbd "DEL") 'delete-backward-char))))

(defun emacs-base:init-iedit ()
  (use-package iedit
    :defer t
    :init
    (progn
      (setq iedit-current-symbol-default t
            iedit-only-at-symbol-boundaries t
            iedit-toggle-key-default nil))
    :config
    (progn
      (defun iedit-toggle-selection ()
        "Override default iedit function to be able to add arbitrary overlays.

It will toggle the overlay under point or create an overlay of one character."
        (interactive)
        (iedit-barf-if-buffering)
        (let ((ov (iedit-find-current-occurrence-overlay)))
          (if ov
              (iedit-restrict-region (overlay-start ov) (overlay-end ov) t)
            (save-excursion
              (push (iedit-make-occurrence-overlay (point) (1+ (point)))
                    iedit-occurrences-overlays))
            (setq iedit-mode
                  (propertize
                   (concat " Iedit:" (number-to-string
                                      (length iedit-occurrences-overlays)))
                   'face 'font-lock-warning-face))
            (force-mode-line-update)))))))

(defun emacs-base:init-indent-guide ()
  (use-package indent-guide
    :defer t
    :init
    (progn
      (setq indent-guide-delay 0.3)
      (bootstrap:diminish indent-guide-mode " ⓘ" " i"))))

(defun emacs-base:init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands open-junk-file
    :init
    (setq open-junk-file-directory
          (concat +bootstrap-cache-directory+ "junk/"))))

(defun emacs-base:init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (eval-after-load 'info
        '(require 'info+))
      (setq Info-fontify-angle-bracketed-flag nil))))

(defun emacs-base:init-move-text ()
  (use-package move-text
    :defer t))

(defun emacs-base:init-neotree ()
  (use-package neotree
    :defer t
    :commands neo-global--window-exists-p
    :init
    (progn
      (setq neo-window-width 32
            neo-create-file-auto-open t
            neo-banner-message nil
            neo-show-updir-line nil
            neo-mode-line-type 'neotree
            neo-smart-open t
            neo-dont-be-alone t
            neo-persist-show nil
            neo-show-hidden-files t
            neo-auto-indent-point t
            neo-modern-sidebar t
            neo-vc-integration '(face))

      (defun bootstrap:neotree-expand-or-open ()
        "Collapse a neotree node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (if (file-directory-p node)
                (progn
                  (neo-buffer--set-expand node t)
                  (neo-buffer--refresh t)
                  (when neo-auto-indent-point
                    (next-line)
                    (neo-point-auto-indent)))
              (call-interactively 'neotree-enter)))))

      (defun bootstrap:neotree-collapse ()
        "Collapse a neotree node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (when (file-directory-p node)
              (neo-buffer--set-expand node nil)
              (neo-buffer--refresh t))
            (when neo-auto-indent-point
              (neo-point-auto-indent)))))

      (defun bootstrap:neotree-collapse-or-up ()
        "Collapse an expanded directory node or go to the parent node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (if (file-directory-p node)
                (if (neo-buffer--expanded-node-p node)
                    (bootstrap:neotree-collapse)
                  (neotree-select-up-node))
              (neotree-select-up-node)))))

      (defun neotree-find-project-root ()
        (interactive)
        (if (neo-global--window-exists-p)
            (neotree-hide)
          (let ((origin-buffer-file-name (buffer-file-name)))
            (neotree-find (projectile-project-root))
            (neotree-find origin-buffer-file-name)))))))

(defun emacs-base:init-pcre2el ()
  (use-package pcre2el
    :defer t
    :commands rxt-fontify-regexp-at-point))

(defun emacs-base:init-paradox ()
  (use-package paradox
    :commands paradox-list-packages
    :init
    (progn
      (setq paradox-execute-asynchronously nil)
      
      (defun bootstrap:paradox-list-packages ()
        "Load depdendencies for auth and open the package list."
        (interactive)
        (require 'epa-file)
        (require 'auth-source)
        (when (and (not (boundp 'paradox-github-token))
                   (file-exists-p "~/.authinfo.gpg"))
          (let ((authinfo-result (car (auth-source-search
                                       :max 1
                                       :host "github.com"
                                       :port "paradox"
                                       :user "paradox"
                                       :require '(:secret)))))
            (let ((paradox-token (plist-get authinfo-result :secret)))
              (setq paradox-github-token (if (functionp paradox-token)
                                             (funcall paradox-token)
                                           paradox-token)))))
        (paradox-list-packages nil)))))

(defun emacs-base:init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (when (member *bootstrap-highlight-delimiters* '(any all))
        (bootstrap:add-to-hooks 'rainbow-delimiters-mode
                                '(prog-mode-hook))))))

(defun emacs-base:init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (bootstrap:add-to-hooks (if *bootstrap-smartparens-strict-mode*
                                  'smartparens-strict-mode
                                'smartparens-mode)
                              '(prog-mode-hook))

      ;; enable smartparens-mode in `eval-expression'
      (defun conditionally-enable-smartparens-mode ()
        "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
        (if (eq this-command 'eval-expression)
            (smartparens-mode)))

      (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)
      
      (setq sp-show-pair-delay 0.2
            ;; fix paren highlighting in normal mode
            sp-show-pair-from-inside t
            sp-cancel-autoskip-on-backward-movement nil))
    :config
    (progn
      (require 'smartparens-config)
      (show-smartparens-global-mode +1)

      (bootstrap:diminish smartparens-mode " ⓟ" " p")

      (defun bootstrap:smartparens-pair-newline (id action context)
        (save-excursion
          (newline)
          (indent-according-to-mode)))

      (defun bootstrap:smartparens-pair-newline-and-indent (id action context)
        (bootstrap:smartparens-pair-newline id action contetx)
        (indent-according-to-mode))

      ;; don't create a pair with single quote in minibuffer
      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

      (sp-pair "{" nil :post-handlers
               '(:add (bootstrap:smartparens-pair-newline-and-indent "RET")))
      (sp-pair "[" nil :post-handlers
               '(:add (bootstrap:smartparens-pair-newline-and-indent "RET"))))))

(defun emacs-base:init-smooth-scrolling ()
  (if *bootstrap-smooth-scrolling*
      (use-package smooth-scrolling
        :init
        (setq smooth-scroll-margin 5
              scroll-conservatively 101
              scroll-preserve-screen-position t
              auto-window-vscroll nil)
        :config
        (setq scroll-margin 5))

    ;; deactivate the defadvice's
    (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
    (ad-activate 'previous-line)
    (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
    (ad-activate 'next-line)
    (ad-disable-advice 'isearch-repeat 'after 'isearch-smooth-scroll)
    (ad-activate 'isearch-repeat)))

(defun emacs-base:init-spray ()
  (use-package spray
    :commands spray-mode
    :init
    :config
    (progn
      (define-key spray-mode-map (kbd "h") 'spray-backward-word)
      (define-key spray-mode-map (kbd "l") 'spray-forward-word)
      (define-key spray-mode-map (kbd "q") 'spray-quit))))

(defun emacs-base:init-window-numbering ()
  (use-package window-numbering
    :config
    (progn
      (when (bootstrap-layer:package-used-p 'powerline)
        (defun window-numbering-install-mode-line (&optional position)
          "Do nothing, the display is handled by the powerline."))
      (setq window-numbering-auto-assign-0-to-minibuffer nil)
      (window-numbering-mode 1))

    (defun bootstrap:window-number ()
      "Return the number of the window."
      (let* ((num (window-numbering-get-number))
             (str (if num (int-to-string num))))
        (cond
         ((not (bootstrap:symbol-value
                *bootstrap-mode-line-unicode-symbols*)) str)
         ((equal str "1")  "➊")
         ((equal str "2")  "➋")
         ((equal str "3")  "➌")
         ((equal str "4")  "➍")
         ((equal str "5")  "➎")
         ((equal str "6")  "❻")
         ((equal str "7")  "➐")
         ((equal str "8")  "➑")
         ((equal str "9")  "➒")
         ((equal str "0")  "➓"))))

    (defun bootstrap::window-numbering-assign (windows)
      "Custom number assignment for special buffers."
      (mapc (lambda (w)
              (when (and (boundp 'neo-global--window)
                         (eq w neo-global--window))
                (window-numbering-assign w 0)))
            windows))

    ;; Change window numbering keys from meta to super.
    (cl-loop for i from 0 to 9
             do (progn
                  (unbind-key (concat "M-" (number-to-string i))
                              window-numbering-keymap)
                  (global-set-key (kbd (concat "s-" (number-to-string i)))
                                  (intern (concat "select-window-"
                                                  (number-to-string i))))))

    (add-hook 'window-numbering-before-hook
              'bootstrap::window-numbering-assign)
    (add-hook 'neo-after-create-hook '(lambda (w)
                                        (window-numbering-update)))))

(defun emacs-base:init-volatile-highlights ()
  (use-package volatile-highlights
    :config
    (progn
      (volatile-highlights-mode t)
      (bootstrap:hide-lighter volatile-highlights-mode))))

(defun emacs-base:init-zoom-frm ()
  (use-package zoom-frm
    :commands (zoom-frm-unzoom
               zoom-frm-out
               zoom-frm-in)
    :init
    (progn
      (defun bootstrap::zoom-frm-powerline-reset ()
        (when (fboundp 'powerline-reset)
          (setq-default powerline-height (bootstrap:compute-powerline-height))
          (powerline-reset)))

      (defun bootstrap::zoom-frm-do (arg)
        "Perform a zoom action depending on ARG value."
        (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                                 ((< arg 0) 'zoom-frm-out)
                                 ((> arg 0) 'zoom-frm-in)))
              (fm (cdr (assoc 'fullscreen (frame-parameters))))
              (fwp (* (frame-char-width) (frame-width)))
              (fhp (* (frame-char-height) (frame-height))))
          (when (equal fm 'maximized)
            (toggle-frame-maximized))
          (funcall zoom-action)
          (set-frame-size nil fwp fhp t)
          (when (equal fm 'maximized)
            (toggle-frame-maximized))))

      (defun bootstrap:zoom-frm-in ()
        "zoom in frame, but keep the same pixel size"
        (interactive)
        (bootstrap::zoom-frm-do 1))

      (defun bootstrap:zoom-frm-out ()
        "zoom out frame, but keep the same pixel size"
        (interactive)
        (bootstrap::zoom-frm-do -1))

      (defun bootstrap:zoom-frm-unzoom ()
        "Unzoom current frame, keeping the same pixel size"
        (interactive)
        (bootstrap::zoom-frm-do 0))

      ;; Font size, either with ctrl + mouse wheel
      (global-set-key (kbd "<C-wheel-up>") 'bootstrap:zoom-frm-in)
      (global-set-key (kbd "<C-wheel-down>") 'bootstrap:zoom-frm-out))))

(defvar *bootstrap-mode-line-unicode-symbols* (unicode-p))
(defvar *bootstrap-mode-line-unicode-separators* (unicode-p))
(defvar *bootstrap-mode-line-minor-modes-p* t)
(defvar *bootstrap-mode-line-major-mode-p* t)
(defvar *bootstrap-mode-line-version-control-p* t)
(defvar *bootstrap-mode-line-display-point-p* nil)
(defvar *bootstrap-mode-line-org-clock-current-task-p* nil)
(defvar *bootstrap-mode-line-org-clock-format-fn 'org-clock-get-clock-string)
(defvar *bootstrap-mode-line-left*
  '(((workspace-number window-number)
     :separator "|"
     :face font-lock-keyword-face)
    anzu
    (buffer-modified buffer-size buffer-id remote-host)
    major-mode
    ((flycheck-errors flycheck-warnings flycheck-infos)
     :when active)
    ((minor-modes process)
     :when active)
    (erc-track :when active)
    (version-control :when active)
    (org-pomodoro :when active)
    (org-clock :when active)
    nyan-cat))
(defvar *bootstrap-mode-line-right*
  '((battery :when active)
    selection-info
    ((buffer-encoding-abbrev
      point-position
      line-column)
     :separator " | ")
    ((global-mode)
     :when active)
    buffer-position
    hud))

(defun emacs-base:init-powerline ()
  (use-package powerline
    :init
    (progn
      (if *bootstrap-mode-line-unicode-separators*
          (setq powerline-default-separator 'utf-8)
        (if (display-graphic-p)
            (setq-default powerline-default-separator 'wave)
          (setq-default powerline-default-separator 'utf-8)))
      
      (defpowerline bootstrap-powerline-minor-modes
        (mapconcat (lambda (mm)
                     (propertize
                      mm
                      'mouse-face 'mode-line-highlight
                      'help-echo
                      (concat "Minor mode\n"
                              " mouse-1: Display minor mode menu\n "
                              "mouse-2: Show help for minor mode\n "
                              "mouse-3: Toggle minor modes")
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map
                                     [mode-line down-mouse-1]
                                     (powerline-mouse 'minor 'menu mm))
                                   (define-key map
                                     [mode-line mouse-2]
                                     (powerline-mouse 'minor 'help mm))
                                   (define-key map
                                     [mode-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu mm))
                                   (define-key map
                                     [header-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu mm))
                                   map)))
                   (split-string (format-mode-line minor-mode-alist))
                   (concat (propertize
                            (if *bootstrap-mode-line-unicode-symbols*
                                " "
                              "")
                            'face face)
                           (unless *bootstrap-mode-line-unicode-symbols*
                             "|"))))
      (defun bootstrap::mode-line-file-encoding ()
        "Return the file encoding to be displayed in the mode-line."
        (let ((buf-coding (format "%s" buffer-file-coding-system)))
          (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
              (match-string 1 buf-coding)
            buf-coding)))
      
      (defun bootstrap:customize-powerline-faces ()
        "Alter powerline face to make them work with more themes."
        (set-face-attribute 'powerline-inactive2 nil
                            :inherit 'font-lock-comment-face))
      (bootstrap:customize-powerline-faces)

      (defmacro bootstrap:define-mode-line-segment (name value &rest props)
        "Defines a modeline segment called `NAME' whose value is
computed by the form `VALUE'. The optional keyword argument `WHEN'
defines a condition required for the segment to be shown.

This macro defines a function `bootstrap::mode-line-NAME' which
returns a list of modeline objects (strings or images). If the
form `VALUE' does not result in a list, the return value will be
wrapped as a singleton list.

All properties are stored in a plist attached to the symbol, to be
inspected at evaluation time by `bootstrap::eval-mode-line-segment'."
        (declare (indent 1))
        (let* ((wrapper-func (intern (format "bootstrap::mode-line-%S" name)))
               (wrapper-func-available (intern (format "%S-available"
                                                       wrapper-func)))
               (condition (if (plist-member props :when)
                              (plist-get props :when)
                             t)))
          `(progn
             (defun ,wrapper-func ()
               (when ,condition
                 (let ((value ,value))
                   (cond ((bootstrap::image-p value)
                          (list value))
                         ((listp value) value)
                         ((and (stringp value)
                               (= 0 (length value)))
                          nil)
                         (t (list value))))))
             (setplist ',wrapper-func ',props))))

      ;; An intermediate representation of the value of a modeline segment.
      (defstruct segment
        objects face-left face-right tight-left tight-right)

       (defun column-number-at-pos (pos)
        "Analog to line-number-at-pos."
        (save-excursion (goto-char pos) (current-column)))

      (defun selection-info ()
        "Info on the current selection for the mode-line.

It is a string holding:
- the number of columns in the selection if it covers only one line,
- the number of lines in the selection if if covers several full lines
- or rowsxcols if it's a block selection."
        (let* ((lines (count-lines (region-beginning) (min (1+ (region-end))
                                                           (point-max))))
               (chars (- (1+ (region-end)) (region-beginning)))
               (cols (1+ (abs (- (column-number-at-pos (region-end))
                                 (column-number-at-pos (region-beginning)))))))
            (if (> lines 1)
                (format "%d lines" lines)
              (format "%d chars" chars))))

      (bootstrap:define-mode-line-segment workspace-number
        (bootstrap:workspace-number)
        :when (and (bound-and-true-p eyebrowse-mode)
                   (bootstrap:workspace-number)))

      (bootstrap:define-mode-line-segment window-number
        (bootstrap:window-number)
        :when (and (bound-and-true-p window-numbering-mode)
                   (bootstrap:window-number)))

      (bootstrap:define-mode-line-segment anzu
        (anzu--update-mode-line)
        :when (and active (bound-and-true-p anzu--state)))

      (bootstrap:define-mode-line-segment buffer-modified "%*")
      (bootstrap:define-mode-line-segment buffer-size
        (powerline-buffer-size))
      (bootstrap:define-mode-line-segment buffer-id
        (powerline-buffer-id))
      (bootstrap:define-mode-line-segment remote-host
        (concat "@" (file-remote-p default-directory 'host))
        :when (file-remote-p default-directory 'host))

      (bootstrap:define-mode-line-segment major-mode
        (powerline-major-mode)
        :when *bootstrap-mode-line-major-mode-p*)
      (bootstrap:define-mode-line-segment minor-modes
        (bootstrap-powerline-minor-modes)
        :when *bootstrap-mode-line-minor-modes-p*)
      (bootstrap:define-mode-line-segment process
        (powerline-raw mode-line-process)
        :when (bootstrap::mode-line-nonempty mode-line-process))

      (bootstrap:define-mode-line-segment erc-track
        (let* ((buffers (mapcar 'car erc-modified-channels-alist))
               (long-names (mapconcat (lambda (buf)
                                        (or (buffer-name buf) ""))
                                      buffers " ")))
          long-names)
        :when (bound-and-true-p erc-track-mode))

      (bootstrap:define-mode-line-segment version-control
        (s-trim (powerline-vc))
        :when (and (powerline-vc)
                   *bootstrap-mode-line-version-control-p*))

      (bootstrap:define-mode-line-segment selection-info
        (selection-info)
        :when mark-active)

      (bootstrap:define-mode-line-segment buffer-encoding
        (format "%s" buffer-file-coding-system))
      (bootstrap:define-mode-line-segment buffer-encoding-abbrev
        (bootstrap::mode-line-file-encoding))

      (bootstrap:define-mode-line-segment point-position
        (format "%d" (point))
        :when *bootstrap-mode-line-display-point-p*)
      (bootstrap:define-mode-line-segment line-column "%l:%2c")
      (bootstrap:define-mode-line-segment buffer-position "%p")

      (bootstrap:define-mode-line-segment hud
        (powerline-hud font-lock-keyword-face default-face)
        :tight t
        :when (string-match "\%" (format-mode-line "%p")))

      (bootstrap:define-mode-line-segment nyan-cat
        (powerline-raw (nyan-create) default-face)
        :when (bound-and-true-p nyan-mode))

      (bootstrap:define-mode-line-segment global-mode
        (powerline-raw (-difference global-mode-string
                                    *bootstrap-global-mode-line-excludes*))
        :when (bootstrap::mode-line-nonempty global-mode-string))

      (bootstrap:define-mode-line-segment battery
        (powerline-raw (s-trim (fancy-battery-default-mode-line))
                       (fancy-battery-powerline-face))
        :when (bound-and-true-p fancy-battery-mode))

      ;; flycheck-errors, flycheck-warnings, flycheck-infos
      (dolist (type '(error warning info))
        (let ((segment-name (intern (format "flycheck-%ss" type)))
              (face (intern (format "bootstrap-mode-line-flycheck-%s-face" type))))
          (eval
           `(bootstrap:define-mode-line-segment ,segment-name
              (powerline-raw (s-trim (bootstrap:custom-flycheck-lighter ,type)) ',face)
              :when (and (bound-and-true-p flycheck-mode)
                         (or flycheck-current-errors
                             (eq 'running flycheck-last-status-change))
                         (bootstrap:custom-flycheck-lighter ,type))))))

      (bootstrap:define-mode-line-segment org-clock
        (substring-no-properties (funcall bootstrap-mode-line-org-clock-format-function))
        :when (and *bootstrap-mode-line-org-clock-current-task-p*
                   (fboundp 'org-clocking-p)
                   (org-clocking-p)))
      (push 'org-mode-line-string *bootstrap-global-mode-line-excludes*)

      (bootstrap:define-mode-line-segment org-pomodoro
        (nth 1 org-pomodoro-mode-line)
        :when (and (fboundp 'org-pomodoro-active-p)
                   (org-pomodoro-active-p)))
      (push 'org-pomodoro-mode-line *bootstrap-global-mode-line-excludes*)

      (defun bootstrap::eval-mode-line-segment (segment-spec &rest outer-props)
        "Evaluates a modeline segment given by `SEGMENT-SPEC' with
additional properties given by `OUTER-PROPS'.

`SEGMENT-SPEC' may be either:
- A literal value (number or string, for example)
- A symbol previously defined by `spacemacs|define-mode-line-segment'
- A list whose car is a segment-spec and whose cdr is a plist of properties
- A list of segment-specs

The properties applied are, in order of priority:
- Those given by `SEGMENT-SPEC', if applicable
- The properties attached to the segment symbol, if applicable
- `OUTER-PROPS'

Valid properties are:
- `:tight-left' => if true, the segment should be rendered with no padding
  or separator on its left side
- `:tight-right' => corresponding option for the right side
- `:tight' => shorthand option to set both `:tight-left' and `:tight-right'
- `:when' => condition that determines whether this segment is shown
- `:fallback' => segment to evaluate if this segment produces no output
- `:separator' => string with which to separate nested segments
- `:face' => the face with which to render the segment

When calling nested or fallback segments, the full property list is passed
as `OUTER-PROPS', with the exception of `:fallback'. This means that more
deeply specified properties, as a rule, override the higher level ones.
The exception is `:when', which must be true at all levels.

The return vaule is a `segment' struct. Its `OBJECTS' list may be nil."

        ;; We get a property list from `SEGMENT-SPEC' if it's a list
        ;; with more than one element whose second element is a symbol
        ;; starting with a colon
        (let* ((input (if (and (listp segment-spec)
                               (cdr segment-spec)
                               (keywordp (cadr segment-spec)))
                          segment-spec
                        (cons segment-spec nil)))
               (segment (car input))
               (segment-symbol (when (symbolp segment)
                                 (intern (format "bootstrap::mode-line-%S"
                                                 segment))))

               ;; Assemble the properties in the correct order
               (props (append (cdr input)
                              (when (symbolp segment)
                                (symbol-plist segment-symbol))
                              outer-props))

               ;; Property list to be passed to nested or fallback segments
               (nest-props (append '(:fallback nil) (cdr input) outer-props))

               ;; Parse property list
               (condition (if (plist-member props :when)
                              (eval (plist-get props :when))
                            t))
               (face (eval (or (plist-get props :face) 'default-face)))
               (separator (powerline-raw (or (plist-get props :separator)
                                             " ")
                                         face))
               (tight-left (or (plist-get props :tight)
                               (plist-get props :tight-left)))
               (tight-right (or (plist-get props :tight)
                                (plist-get props :tight-right)))

               ;; Final output
               (result (make-segment :objects nil
                                     :face-left face
                                     :face-right face
                                     :tight-left tight-left
                                     :tight-right tight-right)))

          ;; Evaluate the segment based on its type
          (when condition
            (cond
             ;; A list of segments
             ((listp segment)
              (let ((results (remove-if-not
                              'segment-objects
                              (mapcar (lambda (s)
                                        (apply
                                         'bootstrap::eval-mode-line-segment
                                         s
                                         nest-props))
                                      segment))))
                (when results
                  (setf (segment-objects result)
                        (apply 'append (bootstrap::intersperse
                                        (mapcar 'segment-objects results)
                                        (list separator))))
                  (setf (segment-face-left result)
                        (segment-face-left (car results)))
                  (setf (segment-face-right result)
                        (segment-face-right (car (last results))))
                  (setf (segment-tight-left result)
                        (segment-tight-left (car results)))
                  (setf (segment-tight-right result)
                        (segment-tight-right (car (last results)))))))
             ;; A single symbol
             ((symbolp segment)
              (setf (segment-objects result)
                    (mapcar (lambda (s)
                              (if (bootstrap::image-p s)
                                  s
                                (powerline-raw s face)))
                            (funcall segment-symbol))))
             ;; A literal value
             (t (setf (segment-objects result)
                      (list (powerline-raw (format "%s" segment) face))))))

          (cond
           ;; This segment produced output, so return it
           ((segment-objects result) result)
           ;; Return the fallback segment, if any
           ((plist-get props :fallback)
            (apply 'bootstrap::eval-mode-line-segment
                   (plist-get props :fallback) nest-props))
           ;; No output (objects = nil)
           (t result))))

      (defun bootstrap::mode-line-prepare-any (spec side)
        "Prepares one side of the modeline. `SPEC' is a list of segment
specifications (see `bootstrap::eval-mode-line-segment'), and `SIDE' is
one of `l' or `r'."
        (let* ((active (powerline-selected-window-active))
               (line-face (if active
                              'powerline-active2
                            'powerline-inactive2))
               (default-face (if active
                                 'powerline-active1
                               'powerline-inactive1))
               (other-face (if active
                               'mode-line
                             'mode-line-inactive))
               ;; Loop through the segments and collect the results
               (segments (loop with result
                               for s in spec
                               do (setq result
                                        (bootstrap::eval-mode-line-segment s))
                               if (segment-objects result)
                                 collect result
                                 and do (rotatef default-face other-face)))

               (dummy (make-segment :face-left line-face :face-right line-face))
               (separator-style (format "powerline-%S"
                                        powerline-default-separator))
               (default-separator (intern
                                   (format
                                    "%s-%S"
                                    separator-style
                                    (car powerline-default-separator-dir))))
               (other-separator (intern
                                 (format
                                  "%s-%S"
                                  separator-style
                                  (cdr powerline-default-separator-dir)))))

          ;; Collect all segment values and add separators
          (apply 'append
                 (mapcar
                  (lambda (pair)
                    (let* ((lhs (car pair))
                           (rhs (cdr pair))
                           (objs (if (eq 'l side) lhs rhs))
                           (add-sep (not (or (segment-tight-right lhs)
                                             (segment-tight-left rhs)))))
                      (rotatef default-separator other-separator)
                      (append
                       (when (and (eq 'r side) add-sep)
                         (list (funcall default-separator
                                        (segment-face-right lhs)
                                        (segment-face-left rhs))))
                       (unless (segment-tight-left objs)
                         (list (powerline-raw " " (segment-face-left objs))))
                       (segment-objects objs)
                       (unless (segment-tight-right objs)
                         (list (powerline-raw " " (segment-face-right objs))))
                       (when (and (eq 'l side) add-sep)
                         (list (funcall default-separator
                                        (segment-face-right lhs)
                                        (segment-face-left rhs)))))))
                  (-zip (if (eq 'l side) segments (cons dummy segments))
                        (if (eq 'l side) (append (cdr segments) (list dummy)) segments))))))

      (defun bootstrap::mode-line-prepare-left ()
        (bootstrap::mode-line-prepare-any *bootstrap-mode-line-left* 'l))

      (defun bootstrap::mode-line-prepare-right ()
        (bootstrap::mode-line-prepare-any *bootstrap-mode-line-right* 'r))

      (defun bootstrap::mode-line-prepare ()
        ;; diminish the lighters
        (when *bootstrap-mode-line-minor-modes-p*
          (let ((unicodep (bootstrap:symbol-value
                           *bootstrap-mode-line-unicode-symbols*)))
            (dolist (mm *bootstrap-diminished-minor-modes*)
              (let ((mode (car mm)))
                (when (and (boundp mode) (symbol-value mode))
                  (let* ((unicode (cadr mm))
                         (ascii (caddr mm))
                         (dim (if unicodep
                                  unicode
                                (if ascii
                                    ascii
                                  unicode))))
                    (diminish mode dim)))))))
        (let* ((active (powerline-selected-window-active))
               (lhs (bootstrap::mode-line-prepare-left))
               (rhs (bootstrap::mode-line-prepare-right))
               (line-face (if active 'powerline-active2 'powerline-inactive2)))
          ;; create the line
          (concat (powerline-render lhs)
                  (powerline-fill line-face (powerline-width rhs))
                  (powerline-render rhs))))

      (setq-default mode-line-format
                    '("%e" (:eval (bootstrap::mode-line-prepare))))

      (defun bootstrap::restore-powerline (buffer)
        "Restore the powerline in buffer"
        (with-current-buffer buffer
              (setq-local mode-line-format
                          '("%e" (:eval (bootstrap::mode-line-prepare))))
              (powerline-set-selected-window)
              (powerline-reset)))

      (defun bootstrap::set-powerline-for-startup-buffers ()
        "Set the powerline for buffers created when Emacs starts."
        (unless *bootstrap-layer-error-count*
          (dolist (buffer '("*Messages*" "*bootstrap*" "*Compile-Log*"))
            (when (and (get-buffer buffer)
                       (bootstrap-layer:package-used-p 'powerline))
              (bootstrap::restore-powerline buffer)))))
      (add-hook 'emacs-startup-hook
                'bootstrap::set-powerline-for-startup-buffers))))

(defun emacs-base:init-bind-key ())

(defun emacs-base:init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (setq bookmark-default-file (concat +bootstrap-cache-directory+ "bookmarks")
          bookmark-save-flag 1)))

(defun emacs-base:init-diminish ()
  (use-package diminish
    :init
    (progn
      (when (display-graphic-p)
        (eval-after-load "eproject"
          '(diminish 'eprojectmode " ᵋⓅ"))
        (eval-after-load "flymake"
          '(diminish 'flymake-mode " Ⓕ²")))
      (eval-after-load
          'elisp-slime-nav
        '(diminish 'elisp-slime-nav-mode))
      (eval-after-load "hi-lock"
        '(diminish 'hi-lock-mode))
      (eval-after-load "abbrev"
        '(diminish 'abbrev-mode))
      (eval-after-load "subword"
        '(when (eval-when-compile (version< "24.3.1" emacs-version))
           (diminish 'subword-mode))))))

(defun emacs-base:init-eldoc ()
  (use-package eldoc
    :defer t
    :config
    (progn
      (require 'eldoc)
      (bootstrap:diminish eldoc-mode " E" " e")
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      (add-hook 'ielm-mode-hook #'eldoc-mode))))

(defun emacs-base:init-electric-indent-mode ()
  (electric-indent-mode))

(defun emacs-base:init-ediff ()
  (use-package ediff
    :defer t
    :init
    (progn
      (setq-default
       ediff-window-setup-function 'ediff-setup-windows-plain
       ediff-split-window-function 'split-window-horizontally
       ediff-merge-split-window-function 'split-window-horizontally))))

(defun emacs-base:init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize))))

(defun emacs-base:init-fill-column-indicator ()
  (use-package fill-column-indicator
    :defer t
    :init
    (progn
      (setq fci-rule-width 1)
      (setq fci-rule-color "#32457D")
      (push '(fci-mode) minor-mode-alist)
      (defun bootstrap::fci-hook ()
        (fci-mode))
      (add-hook 'prog-mode-hook 'bootstrap::fci-hook)
      (fci-mode)
      (fci-mode -1))
    :config
    (progn
      (bootstrap:diminish fci-mode "" "")
      (bootstrap:hide-lighter fci-mode))))

(defvar *bootstrap-helm-resize* nil)
(defvar *bootstrap-helm-position* 'bottom)
(defvar *bootstrap-helm-no-header* nil)

(defun emacs-base:init-helm ()
  (use-package helm
    :defer 1
    :commands (bootstrap:helm-find-files)
    :config
    (progn
      (when (and *bootstrap-helm-resize*
                  (or (eq *bootstrap-helm-position* 'bottom)
                      (eq *bootstrap-helm-position* 'top)))
        (setq helm-autoresize-min-height 10)
        (helm-autoresize-mode 1))

      ;; https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
      (defvar helm-source-header-default-background
        (face-attribute 'helm-source-header :background))

      (defvar helm-source-header-default-foreground
        (face-attribute 'helm-source-header :foreground))

      (defvar helm-source-header-default-box
        (face-attribute 'helm-source-header :box))

      (defvar helm-source-header-default-height
        (face-attribute 'helm-source-header :height))

      (defun helm-toggle-header-line ()
        "Hide the `helm' header is there is only one source."
        (when *bootstrap-helm-no-header*
          (if (> (length helm-sources) 1)
              (set-face-attribute
               'helm-source-header
               nil
               :foreground helm-source-header-default-foreground
               :background helm-source-header-default-background
               :box helm-source-header-default-box
               :height helm-source-header-default-height)
            (set-face-attribute
             'helm-source-header
             nil
             :foreground (face-attribute 'helm-selection :background)
             :background (face-attribute 'helm-selection :background)
             :box nil
             :height 0.1))))
      (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

      (defun bootstrap:helm-find-files (arg)
        "Custom spacemacs implementation for calling helm-find-files-1.

Removes the automatic guessing of the initial value based on thing at
 point."
        (interactive "P")
        (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
                (default-input hist )
                (input (cond ((and (eq major-mode 'dired-mode)
                                   default-input)
                              (file-name-directory default-input))
                             ((and (not (string= default-input ""))
                                   default-input))
                             (t (expand-file-name
                                 (helm-current-directory))))))
            (set-text-properties 0 (length input) nil input)
            (helm-find-files-1 input ))))
    :init
    (progn
      (setq helm-prevent-escaping-from-minibuffer t
            helm-bookmark-show-location t
            helm-display-header-line nil
            helm-split-window-in-side-p t
            helm-always-two-windows t
            helm-echo-input-in-header-line t
            helm-imenu-execute-action-at-once-if-one nil)

      ;; hide minibuffer in Helm session, since we use the header line already
      (defun helm-hide-minibuffer-maybe ()
        (when (with-helm-buffer helm-echo-input-in-header-line)
          (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
            (overlay-put ov 'window (selected-window))
            (overlay-put ov 'face (let ((bg-color (face-background
                                                   'default nil)))
                                    `(:background ,bg-color
                                      :foreground ,bg-color)))
            (setq-local cursor-type nil))))
      (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

      ;; fuzzy matching setting
      (setq helm-M-x-fuzzy-match t
            helm-apropos-fuzzy-match t
            helm-file-cache-fuzzy-match t
            helm-imenu-fuzzy-match t
            helm-lisp-fuzzy-completion t
            helm-recentf-fuzzy-match t
            helm-semantic-fuzzy-match t
            helm-buffers-fuzzy-matching t)

      ;; helm-locate uses es (from everything on windows, which doesnt
      ;; like fuzzy)
      (setq helm-locate-fuzzy-match (executable-find "locate"))

      (defun bootstrap::helm-do-grep-region-or-symbol
          (&optional targs use-region-or-symbol-p)
        "Version of `helm-do-grep' with a default input."
        (interactive)
        (require 'helm)
        (cl-letf*
            (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
             ((symbol-function 'helm-do-grep-1)
              (lambda (targets &optional recurse zgrep exts default-input
                               region-or-symbol-p)
                (let* ((new-input (when region-or-symbol-p
                                    (if (region-active-p)
                                        (buffer-substring-no-properties
                                         (region-beginning) (region-end))
                                      (thing-at-point 'symbol t))))
                       (quoted-input (when new-input
                                       (rxt-quote-pcre new-input))))
                  (this-fn targets
                           recurse
                           zgrep
                           exts
                           default-input
                           quoted-input))))
             (preselection (or (dired-get-filename nil t)
                               (buffer-file-name (current-buffer))))
             (targets (if targs
                          targs
                        (helm-read-file-name
                         "Search in file(s): "
                         :marked-candidates t
                         :preselect
                         (and helm-do-grep-preselect-candidate
                              (if helm-ff-transformer-show-only-basename
                                  (helm-basename preselection)
                                preselection))))))
          (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

      (defun bootstrap:helm-file-do-grep ()
        "Search in current file with `grep' using a default input."
        (interactive)
        (bootstrap::helm-do-grep-region-or-symbol
         (list (buffer-file-name (current-buffer))) nil))

      (defun bootstrap:helm-file-do-grep-region-or-symbol ()
        "Search in current file with `grep' using a default input."
        (interactive)
        (bootstrap::helm-do-grep-region-or-symbol
         (list (buffer-file-name (current-buffer))) t))

      (defun bootstrap:helm-files-do-grep ()
        "Search in files with `grep'."
        (interactive)
        (bootstrap::helm-do-grep-region-or-symbol nil nil))

      (defun bootstrap:helm-files-do-grep-region-or-symbol ()
        "Search in files with `grep' using a default input."
        (interactive)
        (bootstrap::helm-do-grep-region-or-symbol nil t))

      (defun bootstrap:helm-buffers-do-grep ()
        "Search in opened buffers with `grep'."
        (interactive)
        (let ((buffers (cl-loop for buffer in (buffer-list)
                                when (buffer-file-name buffer)
                                collect (buffer-file-name buffer))))
          (bootstrap::helm-do-grep-region-or-symbol buffers nil)))

      (defun bootstrap:elm-buffers-do-grep-region-or-symbol ()
        "Search in opened buffers with `grep' with a default input."
        (interactive)
        (let ((buffers (cl-loop for buffer in (buffer-list)
                                when (buffer-file-name buffer)
                                collect (buffer-file-name buffer))))
          (bootstrap::helm-do-grep-region-or-symbol buffers t)))

      (defun bootstrap:last-search-buffer ()
        "open last helm-ag or hgrep buffer."
        (interactive)
        (if (get-buffer "*helm ag results*")
            (switch-to-buffer-other-window "*helm ag results*")
            (if (get-buffer "*hgrep*")
                (switch-to-buffer-other-window "*hgrep*")
                (message "No previous search buffer found"))))

      ;; use helm by default for M-x
      (unless (bootstrap-layer:package-used-p 'smex)
        (global-set-key (kbd "M-x") 'helm-M-x))

      ;; Use helm by default for C-x C-f too.
      (unless (bootstrap-layer:package-used-p 'smex)
        (global-set-key (kbd "C-x C-f") 'helm-find-files))

      (defvar *bootstrap-helm-display-help-buffer-regexp*
        '("*.*Helm.*Help.**"))

      (defvar *bootstrap-helm-display-buffer-regexp*
        `("*.*helm.**"
          (display-buffer-in-side-window)
          (inhibit-same-window . t)
          (side . ,*bootstrap-helm-position*)
          (window-width . 0.6)
          (window-height . 0.4)))

      (defvar *bootstrap-display-buffer-alist* nil)

      (defun bootstrap::helm-prepare-display ()
        "Prepare necessary settings to make Helm display properly."
        ;; avoid Helm buffer being diplaye twice when user
        ;; sets this variable to some function that pop buffer to
        ;; a window. See https://github.com/syl20bnr/bootstrap:issues/1396
        (let ((display-buffer-base-action '(nil)))
          (setq *bootstrap-display-buffer-alist* display-buffer-alist)
          ;; the only buffer to display is Helm, nothing else we must set
          ;; this otherwise Helm cannot reuse its own windows for
          ;; copyinng/deleting etc... because of existing popwin buffers
          ;; in the alist
          (setq display-buffer-alist nil)
          (popwin-mode -1)))

      (defun bootstrap::display-helm-window (buffer)
        (let ((display-buffer-alist
               (list *bootstrap-helm-display-help-buffer-regexp*
 ;;; this or any specialized case of Helm buffer must be added AFTER
 ;;; `spacemacs-helm-display-buffer-regexp'. Otherwise,
 ;;; `spacemacs-helm-display-buffer-regexp' will be used before
 ;;; `spacemacs-helm-display-help-buffer-regexp' and display
 ;;; configuration for normal Helm buffer is applied for helm help
 ;;; buffer, making the help buffer unable to be displayed.
                     *bootstrap-helm-display-buffer-regexp*)))
          (helm-default-display-buffer buffer)))

      (setq helm-display-function 'bootstrap::display-helm-window)

      (defun bootstrap::restore-previous-display-config ()
        (popwin-mode 1)
;;; we must enable popwin-mode first then restore `display-buffer-alist'
;;; Otherwise, popwin keeps adding up its own buffers to
;;; `display-buffer-alist' and could slow down Emacs as the list grows
        (setq display-buffer-alist *bootstrap-display-buffer-alist*))

      (add-hook 'helm-after-initialize-hook 'bootstrap::helm-prepare-display)
      ;;  Restore popwin-mode after a Helm session finishes.
      (add-hook 'helm-cleanup-hook
                'bootstrap::restore-previous-display-config)

      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l")
        'helm-minibuffer-history)

      (defun bootstrap::helm-cleanup ()
        "Cleanup some helm related states when quitting."
        ;; deactivate any running transient map (micro-state)
        (setq overriding-terminal-local-map nil))
      (add-hook 'helm-cleanup-hook 'bootstrap::helm-cleanup))
    :config
    (progn
      (helm-mode +1)
      (defun bootstrap::set-dotted-directory ()
        "Set the face of diretories for `.' and `..'"
        (set-face-attribute 'helm-ff-dotted-directory
                            nil
                            :foreground nil
                            :background nil
                            :inherit 'helm-ff-directory))
      (add-hook 'helm-find-files-before-init-hook
                'bootstrap::set-dotted-directory)

      ;; alter helm-bookmark key bindings to be simpler
      (defun simpler-helm-bookmark-keybindings ()
        (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
        (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
        (define-key helm-bookmark-map (kbd "C-f")
          'helm-bookmark-toggle-filename)
        (define-key helm-bookmark-map (kbd "C-o")
          'helm-bookmark-run-jump-other-window)
        (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
      (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)

      (defun bootstrap:helm-edit ()
        "Switch in edit mode depending on the current helm buffer."
        (interactive)
        (cond
         ((string-equal "*helm-ag*" helm-buffer)
          (helm-ag-edit))))

      ;; Swap default TAB and C-z commands.
      ;; For GUI.
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      ;; For terminal.
      (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-z") 'helm-select-action)

      (bootstrap:hide-lighter helm-mode)
      )))

(defun emacs-base:init-helm-descbinds ()
  (use-package helm-descbinds
    :defer t
    :init
    (progn
      (setq helm-descbinds-window-style 'split)
      (add-hook 'helm-mode-hook 'helm-descbinds-mode))))

(defun emacs-base:init-helm-projectile ()
  (use-package helm-projectile
    :commands (helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-grep
               helm-projectile
               helm-projectile-switch-project)
    :init
    (progn
      (setq projectile-switch-project-action 'helm-projectile)

      (defconst *bootstrap-use-helm-projectile* t)

      (defalias 'bootstrap:helm-project-do-grep
        'helm-projectile-grep)

      (defalias 'bootstrap:helm-project-do-grep-region-or-symbol
        'helm-projectile-grep))))

(defun emacs-base:init-ido ()
  (when *bootstrap-use-ido*
    (ido-mode t)
    (setq ido-save-directory-list-file
          (concat +bootstrap-cache-directory+ "ido.last")
          ido-enable-flex-matching t)))

(defvar *bootstrap-use-ido* nil)
(defun emacs-base:init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :init
    (progn
      (when *bootstrap-use-ido*
        (ido-vertical-mode t)))

    (defun bootstrap::ido-setup ()
      (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
      (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
      (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
      (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
      (define-key ido-completion-map (kbd "C-S-h") 'ido-prev-match-dir)
      (define-key ido-completion-map (kbd "C-S-j") 'next-history-element)
      (define-key ido-completion-map (kbd "C-S-k") 'previous-history-element)
      (define-key ido-completion-map (kbd "C-S-l") 'ido-next-match-dir)
      (define-key ido-completion-map (kbd "C-S-n") 'next-history-element)
      (define-key ido-completion-map (kbd "C-S-p") 'previous-history-element)
      (define-key ido-completion-map (kbd "C-o")
        'bootstrap:ido-invoke-in-other-window)
      (define-key ido-completion-map (kbd "C-s")
      'bootstrap:ido-invoke-in-vertical-split)
      (define-key ido-completion-map (kbd "C-t")
      'bootstrap:ido-invoke-in-new-frame)
      (define-key ido-completion-map (kbd "C-v")
        'bootstrap:ido-invoke-in-horizontal-split))
    (add-hook 'ido-setup-hook 'bootstrap::ido-setup)

    (defun bootstrap:ido-invoke-in-other-window ()
      (interactive)
      (setq ido-exit-minibuffer-target-window 'other)
      (ido-exit-minibuffer))

    (defun bootstrap:ido-invoke-in-horizontal-split ()
      (interactive)
      (setq ido-exit-minibuffer-target-window 'horizontal)
      (ido-exit-minibuffer))

    (defun bootstrap:ido-invoke-in-vertical-split ()
      (interactive)
      (setq ido-exit-minibuffer-target-window 'vertical)
      (ido-exit-minibuffer))

    (defun bootstrap:ido-invoke-in-new-frame ()
      (interactive)
      (setq ido-exit-minibuffer-target-window 'frame)
      (ido-exit-minibuffer))

    (defadvice ido-read-internal
        (around ido-read-internal-with-minibuffer-other-window-active)
      (let* (ido-exit-minibuffer-target-window
             (this-buffer (current-buffer))
             (result ad-do-it))
        (cond ((equal ido-exit-minibuffer-target-window 'other)
               (if (= 1 (count-windows))
                   (bootstrap:split-window-horizontally-and-switch)
                 (other-window 1)))
              ((equal ido-exit-minibuffer-target-window 'horizontal)
               (bootstrap:split-window-horizontally-and-switch))
              ((equal ido-exit-minibuffer-target-window 'vertical)
               (bootstrap:split-window-vertically-and-switch))
              ((equal ido-exit-minibuffer-target-window 'frame)
               (make-frame)))
        ;; why? Some ido commands, such as textmate.el's
        ;; textmate-goto-symbol don't switch the current buffer
        (switch-to-buffer this-buffer)
        result))))

(defun emacs-base:init-page-break-lines ()
  (use-package page-break-lines
    :defer t))

(defun emacs-base:init-popup ()
  (use-package popup
    :defer t))

(defun emacs-base:init-popwin ()
  (use-package popwin
    :config
    (progn
      (popwin-mode 1)
      (setq popwin:special-display-config nil)
      (push '("*Help*"
              :dedicated t
              :position bottom
              :stick t
              :noselect nil
              :height 0.4)
            popwin:special-display-config)
      (push '("*compilation*"
              :dedicated t
              :position bottom
              :stick t
              :noselect t
              :height 0.4)
            popwin:special-display-config)
      (push '("*Shell Command Output*"
              :dedicated t
              :position bottom
              :stick t
              :noselect nil)
            popwin:special-display-config)
      (push '("*Async Shell Command*"
              :dedicated t
              :position bottom
              :stick t
              :noselect nil)
            popwin:special-display-config)
      (push '(" *undo-tree*"
              :dedicated t
              :position bottom
              :stick t
              :noselect nil
              :height 0.4)
            popwin:special-display-config)
      (push '("*ert*"
              :dedicated t
              :position bottom
              :stick t
              :noselect nil)
            popwin:special-display-config)
      (push '("*grep*"
              :dedicated t
              :position bottom
              :stick t
              :noselect nil)
            popwin:special-display-config)
      (push '("*nosetests*"
              :dedicated t
              :position bottom
              :stick t
              :noselect nil)
            popwin:special-display-config)
      (push '("^\*WoMan.+\*$"
              :regexp t
              :position bottom)
            popwin:special-display-config)

      (defun bootstrap:remove-popwin-display-config (str)
        (setq popwin:special-display-config
              (-remove (lambda (x)
                         (if (and (listp x)
                                  (stringp (car x)))
                             (string-match str (car x))))
                       popwin:special-display-config))))))

(defun emacs-base:init-process-menu ())

(defun emacs-base:init-projectile ()
  (use-package projectile
    :commands (projectile-ack
               projectile-ag
               projectile-compile-project
               projectile-dired
               projectile-grep
               projectile-find-dir
               projectile-find-file
               projectile-find-tag
               projectile-find-test-file
               projectile-invalidate-cache
               projectile-kill-buffers
               projectile-multi-occur
               projectile-project-root
               projectile-recentf
               projectile-regenerate-tags
               projectile-replace
               projectile-run-async-shell-command-in-root
               projectile-run-shell-command-in-root
               projectile-switch-project
               projectile-switch-to-buffer
               projectile-vc)
    :init
    (progn
      (require 'projectile)
      
      ;; note for Windows: GNU find or Cygwin find must be in path
      ;; default parameters are not supported on Windows, we default
      ;; to simplest call to find.
      (when (and (windows-p)
                 (executable-find "find"))
        (setq projectile-indexing-method 'alien
              projectile-generic-command "find . -type f"))
      (setq projectile-enable-caching t
            projectile-indexing-method 'alien
            projectile-sort-order 'recentf
            projectile-cache-file (concat +bootstrap-cache-directory+
                                          "projectile.cache")
            projectile-known-projects-file
              (concat +bootstrap-cache-directory+
                      "projectile-bookmarks.eld")))
    :config
    (progn
      (projectile-global-mode)
      (bootstrap:hide-lighter projectile-mode))))

(defun emacs-base:init-savehist ()
  (use-package savehist
    :init
    (progn
      ;; Minibuffer history
      (setq savehist-file (concat +bootstrap-cache-directory+ "savehist")
            enable-recursive-minibuffers t ; Allow commands in minibuffers
            history-length 1000
            savehist-additional-variables '(mark-ring
                                            global-mark-ring
                                            search-ring
                                            regexp-search-ring
                                            extended-command-history)
            savehist-autosave-interval 60)
      (savehist-mode t))))

(defun emacs-base:init-saveplace ()
  (use-package saveplace
    :init
    (progn
      ;; Save point position between sessions
      (setq save-place t
            save-place-file (concat +bootstrap-cache-directory+ "places")))))

(defun emacs-base:init-spacemacs-theme ()
  (use-package spacemacs-theme
    :defer t
    :init
    (progn
      (setq spacemacs-theme-comment-bg t)
      (setq spacemacs-theme-org-height t))))

(defun emacs-base:init-subword ()
  (unless (version< emacs-version "24.4")
    (use-package subword
      :defer t
      :init
      (progn
        (unless (category-docstring ?U)
          (define-category ?U "Uppercase")
          (define-category ?u "Lowercase"))
        (modify-category-entry (cons ?A ?Z) ?U)
        (modify-category-entry (cons ?a ?z) ?u))
      :config
      (bootstrap:diminish subword-mode " Ⓒ" " c"))))

(defun emacs-base:init-undo-tree ()
  (use-package undo-tree
    :init
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    :config
    (progn
      (bootstrap:hide-lighter undo-tree-mode))))

(defun emacs-base:init-uniquify ()
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        uniquify-ignore-buffers-re "^\\*"))

(defun emacs-base:init-use-package ()
  (package-install 'use-package)
  (require 'use-package))

(defvar *bootstrap-which-key-delay* 0.4)
(defvar *bootstrap-which-key-position* 'bottom)
(defun emacs-base:init-which-key ()
  (use-package which-key
    :init
    (progn
      (require 'which-key)
      
      (when (not (windows-p))
        (let ((new-descriptions
               '(("bootstrap:\\(.+\\)" . "\\1")
                 ("select-window-\\([0-9]\\)" . "window \\1")
                 ("bootstrap:alternate-buffer" . "last buffer")
                 ("bootstrap:toggle-mode-line-\\(.+\\)" . "\\1")
                 ("avy-goto-word-or-subword-1" . "avy word")
                 ("shell-command" . "shell cmd")
                 ("bootstrap:default-pop-shell" . "open shell")
                 ("bootstrap:helm-project-smart-do-search-region-or-symbol" .
                  "smart search")
                 ("helm-descbinds" . "show keybindings")
                 ("sp-split-sexp" . "split sexp")
                 ("avy-goto-line" . "avy line")
                 ("universal-argument" . "universal arg")
                 ("er/expand-region" . "expand region")
                 ("helm-apropos" . "apropos"))))
          (dolist (nd new-descriptions)
            ;; ensure the target matches the whole string
            (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
                  which-key-description-replacement-alist))))
      ;; disable special key handling for spacemacs, since it can be
      ;; disorienting if you don't understand it
      (pcase *bootstrap-which-key-position*
        (`right (which-key-setup-side-window-right))
        (`bottom (which-key-setup-side-window-bottom))
        (`right-then-bottom (which-key-setup-side-window-right-bottom)))
      (setq which-key-special-keys nil
            which-key-use-C-h-for-paging t
            which-key-prevent-C-h-from-cycling t
            which-key-echo-keystrokes 0.02
            which-key-max-description-length 32
            which-key-idle-delay *bootstrap-which-key-delay*)
      (which-key-mode)
      (bootstrap:diminish which-key-mode " Ⓚ" " K"))))

(defvar *bootstrap-show-trailing-whitespace* t)
(defun emacs-base:init-whitespace ()
  (use-package whitespace
    :defer t
    :init
    (progn
      (setq *bootstrap-show-trailing-whitespace* t)
      (defun bootstrap::show-trailing-whitespace ()
        (when *bootstrap-show-trailing-whitespace*
          (set-face-attribute 'trailing-whitespace nil
                              :background
                              (face-attribute 'font-lock-comment-face
                                              :foreground))
          (setq show-trailing-whitespace 1)))
      (add-hook 'prog-mode-hook 'bootstrap::show-trailing-whitespace)
      (defun bootstrap::set-whitespace-style-for-diff ()
        "Whitespace configuration for `diff-mode'"
        (setq-local whitespace-style '(face
                                       tabs
                                       tab-mark
                                       spaces
                                       space-mark
                                       trailing
                                       indentation::space
                                       indentation::tab
                                       newline
                                       newline-mark)))
      (add-hook 'diff-mode-hook 'whitespace-mode)
      (add-hook 'diff-mode-hook 'bootstrap::set-whitespace-style-for-diff))
    :config
    (set-face-attribute 'whitespace-space nil
                        :background nil
                        :foreground (face-attribute 'font-lock-warning-face
                                                    :foreground))
    (set-face-attribute 'whitespace-tab nil
                        :background nil)
    (set-face-attribute 'whitespace-indentation nil
                        :background nil)
    (bootstrap:diminish whitespace-mode " ⓦ" " w")
    (bootstrap:diminish global-whitespace-mode " Ⓦ" " W")))

(defvar *bootstrap-winner-boring-buffers* nil)
(defun emacs-base:init-winner ()
  (use-package winner
    :init
    (progn
      (winner-mode t)
      (setq *bootstrap-winner-boring-buffers* '("*Completions*"
                                              "*Compile-Log*"
                                              "*inferior-lisp*"
                                              "*Fuzzy Completions*"
                                              "*Apropos*"
                                              "*Help*"
                                              "*cvs*"
                                              "*Buffer List*"
                                              "*Ibuffer*"
                                              "*esh command on file*"
                                              ))
      (setq winner-boring-buffers
            (append winner-boring-buffers *bootstrap-winner-boring-buffers*))
      (winner-mode t))))

(defun emacs-base:init-quelpa ())

(defun emacs-base:init-recentf ()
  (use-package recentf
    :defer t
    :init
    (add-hook 'find-file-hook (lambda ()
                                (unless recentf-mode
                                  (recentf-mode)
                                  (recentf-track-opened-file))))
    :config
    (add-to-list 'recentf-exclude (expand-file-name +bootstrap-cache-directory+))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (setq recentf-save-file (concat +bootstrap-cache-directory+ "recentf"))
    (setq recentf-max-saved-items 100)
    (setq recentf-auto-cleanup 'never)
    (setq recentf-auto-save-timer
          (run-with-idle-timer 600 t 'recentf-save-list))))

;;(defun emacs-base:init-folding ()
;;  (use-package folding
;;    :defer t
;;    :config
;;    (progn
;;      (folding-add-to-marks-list 'lisp-mode             ";;;{{{" ";;;}}}")
;;      (folding-add-to-marks-list 'emacs-lisp-mode       ";;;{{{" ";;;}}}")
;;      (folding-add-to-marks-list 'lisp-interaction-mode ";;;{{{" ";;;}}}")
;;      (folding-add-to-marks-list 'scheme-mode           ";;;{{{" ";;;}}}"))))

(defun emacs-base:init-htmlize ()
  (use-package htmlize
    :defer t))

(defun emacs-base:init-template ()
  (use-package template
    :defer t
    :init
    (progn
      (require 'template)
      (template-initialize)
      (setq template-default-directories
            (append (list (concat user-home-directory
                                  ".emacs.d/templates/"))
                    template-default-directories)))))
