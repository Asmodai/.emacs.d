

(defconst +bootstrap-buffer-name+ "*bootstrap*"
  "The name of the bootstrap buffer.")

(defconst +bootstrap-banner-length+ 50
  "Width of a banner.")

(defconst +bootstrap-banner-directory+
  (expand-file-name (concat +bootstrap-core-directory+ "banners/")))

(defconst +bootstrap-buffer-cache-file+
  (expand-file-name (concat +bootstrap-cache-directory+ "bootstrap-buffer.el"))
  "Cache file for various persistent data for the startup buffer.")

(defvar bootstrap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]          'widget-forward)
    (define-key map (kbd "C-i")    'widget-forward)
    (define-key map [backtab]      'widget-backward)
    (define-key map (kbd "RET")    'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Key map for bootstrap mode.")

(define-derived-mode bootstrap-mode special-mode "Bootstrap"
  "Bootstrap major mode for the startup screen.

\\<bootstrap-mode-map>"
  :group 'bootstrap
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t))

(defun bootstrap-buffer:set-mode-line (format)
  "Sets the modeline of the buffer named in `+bootstrap-buffer-name+' to the
value of FORMAT."
  (with-current-buffer (get-buffer-create +bootstrap-buffer-name+)
    (setq mode-line-format format)))

(defun bootstrap-buffer:message (msg &rest args)
  "Displays a message.

The message shall only be displayed if `*bootstrap-verbose*' is non-NIL."
  (when *bootstrap-verbose*
    (message "(Bootstrap) %s"
             (apply 'format msg args))))

(defun bootstrap-buffer:warning (msg &rest args)
  "Displays a warning."
  (message "(Bootstrap) Warning: %s"
           (apply 'format msg args)))

(defun bootstrap-buffer:append (msg &optional msgbuf)
  "Appends the text in MSG to the buffer named in `+bootstrap-buffer-name+'.

If MSGBUF is non-NIL, then the contents of MSG are also messaged to the user via
`message'."
  (with-current-buffer (get-buffer-create +bootstrap-buffer-name+)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if msgbuf
          (message "(Bootstrap) %s" msg)))
    (bootstrap-buffer:set-mode-line "")))

(defun bootstrap-buffer:replace-last-line (msg &optional msgbuf)
  "Replace the last line of the bootstrap buffer with MSG.

If MSGBUF is non-NIL then the message is also written to the message buffer."
  (with-current-buffer (get-buffer-create +bootstrap-buffer-name+)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if msgbuf
          (message "(Bootstrap) %s" msg)))
    (bootstrap-buffer:set-mode-line "")))

(defun bootstrap-buffer:insert-page-break ()
  "Insert a page break character."
  (bootstrap-buffer:append "\n\n\n"))

(defun bootstrap-buffer::render-framed-line (line hpadding)
  (let* ((len (length line))
         (fill (- fill-column len)))
    (when (>= fill 0)
      (concat "│"
              (make-string hpadding ?\s)
              line
              (make-string fill ?\s)
              (make-string hpadding ?\s)
              "│\n"))))

(defun bootstrap-buffer::render-framed-text (content
                                             &optional width caption hpadding
                                             fill justify nosqueeze)
  (with-temp-buffer
    (if (not (file-exists-p content))
        (insert content)
      (insert-file-contents content)
      (goto-char (point-max))
      (delete-char -1))
    (let* ((hpadding (or hpadding 1))
           (fill-column (if width
                            (- width (+ 2 (* 2 hpadding)))
                          fill-column))
           (sentence-end-double-space nil)
           (ncaption (concat "┤ " caption " ├"))
           (caption-len (length ncaption)))
      (when fill
        (fill-region (point-min) (point-max) justify nosqueeze))
      (concat
       "┌─"
       (if ncaption
           (concat ncaption
                   (make-string (+ (- fill-column caption-len 1)
                                   hpadding) ?─))
         (make-string fill-column ?─))
       (make-string hpadding ?─)
       "┐\n"
       (bootstrap-buffer::render-framed-line "" hpadding)
       (mapconcat (lambda (x)
                    (bootstrap-buffer::render-framed-line x hpadding))
                  (split-string (buffer-string) "\n" t) "")
       (bootstrap-buffer::render-framed-line "" hpadding)
       "└"
       (make-string hpadding ?─)
       (make-string fill-column ?─)
       (make-string hpadding ?─)
       "┘"))))

(defun bootstrap-buffer:loading-animation ()
  (when *bootstrap-loading-progress-bar*
    (incf *bootstrap-loading-counter*)
    (when (>= *bootstrap-loading-counter*
              *bootstrap-loading-dots-chunk-threshold*)
      (setf *bootstrap-loading-counter* 0
            *bootstrap-loading-string*
              (concat *bootstrap-loading-string*
                      (make-string +bootstrap-loading-dots-chunk-size+
                                   +bootstrap-loading-char+)))
      (bootstrap-buffer:set-mode-line *bootstrap-loading-string*)
      (bootstrap:redisplay))))

(defsubst bootstrap-buffer::choose-text-banner ()
  "Choose a random text banner to display to the user."
  (let* ((files (directory-files +bootstrap-banner-directory+ t))
         (count (length files))
         (choice (+ 2 (random (- count 2)))))
    (nth choice files)))

(defmacro bootstrap-buffer::insert-shortcut (shortcut-char
                                             search-label
                                             &optional no-next-line)
  `(define-key bootstrap-mode-map
     ,shortcut-char
     (lambda ()
       (interactive)
       (unless (search-forward ,search-label (point-max) t)
         (search-backward ,search-label (point-min) t))
       ,@(unless no-next-line
           '((forward-line 1)))
       (back-to-indentation))))

(defun bootstrap-buffer::insert-file-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (message "OK!")
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create
             'push-button
             :action `(lambda (&rest junk)
                        (find-file-existing ,el))
             :mouse-face 'highlight
             :follow-link "\C-m"
             :button-prefix ""
             :button-suffix ""
             :format "%[%t%]"
             (abbreviate-file-name el)))
          list)))

(defun bootstrap-buffer::insert-bookmark-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create
             'push-button
             :action `(lambda (&rest junk)
                        (bookmark-jump ,el))
             :mouse-face 'highlight
             :follow-link "\C-m"
             :button-prefix ""
             :button-suffix ""
             :format "%[%t%]"
             (format "%s - %s"
                     el
                     (abbreviate-file-name
                      (bookmark-get-filename el)))))
          list)))

(defun bootstrap-buffer:insert-startupify-lists ()
  (interactive)
  (with-current-buffer (get-buffer-create +bootstrap-buffer-name+)
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (goto-char (point-max))
      (when (featurep 'page-break-lines)
        (page-break-lines-mode))
      (bootstrap-buffer:insert-page-break)
      (mapc (lambda (el)
              (cond ((eq el 'recents)
                     (recentf-mode)
                     (when (bootstrap-buffer::insert-file-list
                            "Recent Files:"
                            (recentf-elements 5))
                       (bootstrap-buffer::insert-shortcut "r" "Recent Files:")
                       (insert list-separator)))
                    ((eq el 'bookmarks)
                     (helm-mode)
                     (when (bootstrap-buffer::insert-bookmark-list
                            "Bookmarks:"
                            (bookmark-all-names))
                       (bootstrap-buffer::insert-shortcut "m" "Bookmarks:")
                       (insert list-separator)))
                    ((eq el 'projects)
                     (projectile-mode)
                     (when (bootstrap-buffer::insert-file-list
                            "Projects:"
                            (projectile-relevant-known-projects))
                       (bootstrap-buffer::insert-shortcut "p" "Projects:")
                       (insert list-separator)))))
            *bootstrap-startup-lists*))))

(defun bootstrap-buffer::insert-buttons ()
  "Insert quick link buttons into the bootstrap-mode buffer."
  (goto-char (point-max))
  (insert "     ")
  (widget-create
   'push-button
   :help-echo "Update all ELPA packages to the latest versions."
   :action (lambda (&rest junk)
             )
   :mouse-face 'highlight
   :follow-link "\C-m"
   (propertize "Update" 'face 'font-lock-keyword-face))

  (insert " ")
  (widget-create
   'push-button
   :help-echo "Roll back ELP package upgrades."
   :action (lambda (&rest junk)
             )
   :mouse-face 'highlight
   :follow-link "\C-m"
   (propertize "Rollback" 'face 'font-lock-keyword-face))

  ;; TODO: wrap this around a condition.
  (insert "\n\n     ")
  (widget-create
   'push-button
   :help-echo "Connect to Lisp via SLIME."
   :action (lambda (&rest junk)
             )
   :mouse-face 'highlight
   :follow-link "\C-m"
   (propertize "Common Lisp" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create
   'push-button
   :help-echo "Start an EShell session."
   :action (lambda (&rest junk)
             (eshell))
   :mouse-face 'highlight
   :follow-link "\C-m"
   (propertize "EShell" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create
   'push-button
   :help-echo "Start an Interactive Emacs Lisp session."
   :action (lambda (&rest junk)
             (ielm))
   :mouse-face 'highlight
   :follow-link "\C-m"
   (propertize "IELM" 'face 'font-lock-keyword-face))
  (insert "\n"))

(defun bootstrap-buffer::generate-info-text ()
  (format (concat "%s: %s (%s)\n"
                  "%s: %s\n"
                  "%s: %s\n")
          (propertize "User"
                      'face 'font-lock-comment-face)
          (propertize user-login-name
                      'face 'font-lock-function-name-face)
          (propertize user-full-name
                      'face 'font-lock-function-name-face)
          (propertize "Host"
                      'face 'font-lock-comment-face)
          (propertize (system-name)
                      'face 'font-lock-function-name-face)
          (propertize "Mode"
                      'face 'font-lock-comment-face)
          (propertize (cond ((and (terminal-p)
                                  (256-colour-p))
                             "Terminal/256")
                            ((and (terminal-p)
                                  (not (256-colour-p)))
                             "Terminal")
                            (t
                             "Graphical"))
                      'face 'font-lock-function-name-face)))

(defun bootstrap-buffer::generate-info ()
  "Generate Emacs information for the startup screen."
  (let ((string (bootstrap-buffer::generate-info-text))
        (caption (format "%s %s"
                         (propertize "GNU Emacs"
                                     'face 'font-lock-keyword-face)
                         (propertize emacs-version
                                     'face 'font-lock-function-name-face))))
    (bootstrap-buffer::render-framed-text
     string
     (- 80 +bootstrap-banner-length+)
     caption
     1)))

(defun bootstrap-buffer::insert-version ()
  "Insert various Emacs information into the startup screen."
  (with-current-buffer (get-buffer-create +bootstrap-buffer-name+)
    (save-excursion
      (let* ((lines (split-string (bootstrap-buffer::generate-info) "\n"))
             (count 1)
             (maxcol +bootstrap-banner-length+)
             (buffer-read-only nil))
        (dolist (line lines)
          (beginning-of-buffer)
          (forward-line count)
          (move-to-column maxcol t)
          (insert line)
          (incf count))))))
  
(defun bootstrap-buffer::insert-banner ()
  "Insert the banner into the startup screen."
  (let ((banner (bootstrap-buffer::choose-text-banner))
        (buffer-read-only nil))
    (erase-buffer)
    (when banner
      (bootstrap-buffer:message (format "Banner: %s" banner))
      (insert-file-contents banner)
      (bootstrap-buffer::insert-version))
    (bootstrap-buffer::insert-buttons)
    (goto-char (point-min))))

(defun bootstrap-buffer:startup-screen ()
  "Creates the startup screen and inserts the banner."
  (switch-to-buffer (get-buffer-create +bootstrap-buffer-name+))
  (bootstrap-buffer:set-mode-line "")
  (bootstrap-buffer::insert-banner)
  (bootstrap-mode)
  ;; Display useful lists of items
  (when *bootstrap-startup-lists*
    (bootstrap-buffer:insert-startupify-lists))
  (bootstrap:redisplay))

(provide 'bootstrap-buffer)

;; EOF
