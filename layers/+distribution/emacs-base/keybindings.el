
;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

;; Auto-indent on return
(define-key global-map (kbd "RET") 'newline-and-indent)

;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
;; can do the same thing and with fuzzy matching and other features.
(eval-after-load 'dired
  '(define-key dired-mode-map "j" 'bootstrap:helm-find-files))

;; alternate binding to search next occurrence with isearch without
;; exiting isearch
(define-key isearch-mode-map (kbd "S-<return>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "M-S-<return>") 'isearch-repeat-backward)
;; Escape from isearch-mode("/" and "?" in evil-mode) like vim
(define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

;; linum margin bindings-------------------------------------------------------
(global-set-key (kbd "<left-margin> <down-mouse-1>")
                'bootstrap:md-select-linum)

(global-set-key (kbd "<left-margin> <mouse-1>")
                'bootstrap:mu-select-linum)

(global-set-key (kbd "<left-margin> <double-mouse-1>")
                'bootstrap:select-current-block)

(global-set-key (kbd "<left-margin> <drag-mouse-1>")
                'bootstrap:mu-select-linum)

(eval-after-load "shell"
  '(progn
    (define-key comint-mode-map [(shift) (up)] 'comint-previous-input)
    (define-key comint-mode-map [(shift) (down)] 'comint-next-input)))

;; ^c-w - What line am I on?
(global-set-key "\C-cw" 'what-line)

;; ^c-d - Insert current date and time.
(global-set-key "\C-cp" 'bootstrap:insert-date-string)

;; Nudge window sizes.
(when (not (nextstep-p))
  ;;
  ;; ^c-<up> - Shrink/Grow window up.
  (global-set-key [(control c) (up)]
                  (lambda ()
                    (interactive)
                    (shrink-window -1)))
  ;;
  ;; ^c-<down> - Shrink/Shrink window down.
  (global-set-key [(control c) (down)]
                  (lambda ()
                    (interactive)
                    (shrink-window 1)))
  ;;
  ;; ^c-<left> - Shrink/Grow window left.
  (global-set-key [(control c) (left)]
                  (lambda ()
                    (interactive)
                    (shrink-window-horizontally -1)))
  ;;
  ;; ^c-<right> - Shrink/grow window left.
  (global-set-key [(control c) (right)]
                  (lambda ()
                    (interactive)
                    (shrink-window-horizontally 1))))

;; ^c-e - Insert a Euro symbol.
(global-set-key "\C-ce"
                (lambda ()
                  (interactive)
                  (ucs-insert #x20ac)))

;; Windows keys
(when (windows-p)
  (setq w32-pass-lwindow-to-system nil
        w32-pass-rwindow-to-system nil
        w32-lwindow-modifier 'super
        w32-rwindow-modifier 'hyper))

;; MacOS X keys
(when (mac-os-x-p)
  (global-unset-key (kbd "M-3"))      ; Unbind M-3 first.
  (global-set-key (kbd "M-3")         ; Bind so the UK keyboard can
                  (lambda ()          ; generate the hash symbol.
                    (interactive)
                    (insert "#"))))

(message "Base keybindings loaded.")

