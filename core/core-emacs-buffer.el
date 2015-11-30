
(defconst bootstrap-buffer-name "*bootstrap*"
  "The name of the Emacs buffer.")

(defconst bootstrap-buffer--banner-length 75
  "Width of a banner.")

(defconst bootstrap-buffer--cache-file
  (expand-file-name
   (concat bootstrap-cache-directory "bootstrap-buffer.el"))
  "Cache file for various persistent data for the Emacs startup buffer.")

(defun bootstrap-buffer/insert-banner-and-buttons ()
  "Choose a banner according to `bootstrap-startup-banner' and insert it into the
Emacs buffer along with buttons underneath."
  (let ((banner (bootstrap-buffer//choose-banner))
        (buffer-read-only nil))
    (progn
      (when banner
        (bootstrap-buffer/message (format "Banner: %s" banner))
        (insert-file-contents banner)
        (bootstrap-buffer//inject-version))
      (bootstrap-buffer//insert-buttons)
      (if (file-exists-p bootstrap-buffer--cache-file)
          (load bootstrap-buffer--cache-file)
        (unless (file-exists-p bootstrap-loader-filepath)
          (setq bootstrap-buffer--version bootstrap-version)
          (bootstrap/dump-vars-to-file
           '(bootstrap-buffer--version)
           bootstrap-buffer--cache-file)))
      (bootstrap//redisplay))))

(defun bootstrap-buffer//choose-banner ()
  "Return the fill path of a banner based on the bootstrap value."
  (when bootstrap-loader-startup-banner
    (concat bootstrap-banner-directory "banner.txt")))

(defun bootstrap-buffer//inject-version ()
  "Inject the current version of the Emacs configuration into the first line of
the buffer, right adjusted."
  (with-current-buffer (get-buffer-create bootstrap-buffer-name)
    (save-excursion
      (let* ((maxcol bootstrap-buffer--banner-length)
             (injected (format "(%s)" bootstrap-version))
             (pos (- maxcol (length injected)))
             (buffer-read-only nil))
        (beginning-of-buffer)
        (let ((buffer-read-only nil))
          (end-of-line)
          (kill-line (- maxcol)))
        (beginning-of-buffer)
        (when (< (line-end-position) maxcol)
          (end-of-line)
          (insert-char ?\s (- maxcol (line-end-position))))
        (goto-char pos)
        (delete-char (length injected))
        (insert injected)))))

(defun bootstrap-buffer/set-mode-line (format)
  "Set the mode-line format for the Emacs buffer."
  (with-current-buffer (get-buffer-create bootstrap-buffer-name)
    (setq mode-line-format format)))

(defun bootstrap-buffer/message (msg &rest args)
  "Display MSG as a message prepended with `(Emacs)'

The message is displayed only if `bootstrap-loader-verbose-loading' is non-NIL."
  (when bootstrap-loader-verbose-loading
    (message "(Emacs) %s"
             (apply 'format msg args))))

(defun bootstrap-buffer/warning (msg &rest args)
  "Display MSG as a warning in buffer `*Messages*'.

The message is always displayed."
  (message "(Emacs) %s"
           (apply 'format msg args)))

(defun bootstrap-buffer/insert-page-break ()
  "Inserts a page break in the Emacs buffer."
  (bootstrap-buffer/append "\n\C-Q\C-L\n"))

(defun bootstrap-buffer/append (msg &optional msgbuf)
  "Append MSG to the Emacs buffer.  If MSGBUF is non-NIL then MSG is also written
to the message buffer."
  (with-current-buffer (get-buffer-create bootstrap-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if msgbuf
          (message "(Emacs) %s" msg)))
    (bootstrap-buffer/set-mode-line "")))

(defun bootstrap-buffer/replace-last-line (msg &optional msgbuf)
  "Replace the last line of the Emacs buffer with MSG.  If MSGBUF is non-NIL
then MSG is also written in the message buffer."
  (with-current-buffer (get-buffer-create bootstrap-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if msgbuf
          (message "(Emacs) %s" msg)))
    (bootstrap-buffer/set-mode-line "")))

(defun bootstrap-buffer/insert-framed-text (msg &optional caption hpadding)
  "Insert MSG into Emacs buffer within a frame of width
`bootstrap-buffer--banner-length'.

See `bootstrap//render-framed-text' for documentation."
  (with-current-buffer (get-buffer-create bootstrap-buffer-name)
    (let ((buffer-read-only nil))
      (insert (bootstrap//render-framed-text
               msg
               bootstrap-buffer--banner-length
               caption
               hpadding)))))

(defun bootstrap-buffer/insert-framed-text-from-file (fpath
                                                  &optional caption hpadding)
  "Insert the contents of FPATH at the current point in the Emacs buffer.
The inserted text will be framed."
  (when (file-exists-p filepath)
    (with-current-buffer (get-buffer-create bootstrap-buffer-name)
      (let ((buffer-read-only nil))
        (insert (bootstrap//render-framed-text
                 filepath
                 bootstrap-buffer--banner-length
                 caption
                 hpadding))))))

(defun bootstrap//render-framed-text (content
                                         &optional width caption hpadding)
  "Return a formated string framed with plained lines of width FILL-COLUMN.
CONTENT can be a text or a filepath.

WIDTH set the `fill-column' variable.

If CAPTION is non nil string then it is included in at the top of the frame.

If CAPTION length is greater than FILL-COLUMN minus 5 the function returns
nil.

HPADDING is the horizontal spacing between the text and the frame.

The vertical spacing is always one line."
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
           (caption-len (length caption)))
      (fill-region (point-min) (point-max) 'justify 'nosqueeze)
      (concat
       "╭─"
       (if caption
           (concat caption
                   (make-string (+ (- fill-column caption-len 1)
                                   hpadding) ?─))
         (make-string fill-column ?─))
       (make-string hpadding ?─) "╮\n"
       (bootstrap//render-framed-line "" hpadding)
       (mapconcat (lambda (x)
                    (bootstrap//render-framed-line x hpadding))
                  (split-string (buffer-string) "\n" nil) "")
       (bootstrap//render-framed-line "" hpadding)
       "╰" (make-string hpadding ?─)
       (make-string fill-column ?─)
       (make-string hpadding ?─) "╯"))))

(defun bootstrap//render-framed-line (line hpadding)
  "Return a formated LINE with borders of a frame on each side and
with width FILL-COLUMN.
If length of LINE is bigger than FILL-COLUMN it returns nil.
HPADDING is the horizontal spacing betwee the content line and the frame border."
  (let* ((len (length line))
         (fill (- fill-column len)))
    (when (>= fill 0)
      (concat "│" (make-string hpadding ?\s)
              line (make-string fill ?\s)
              (make-string hpadding ?\s) "│\n"))))

(defun bootstrap-buffer/loading-animation ()
  "Display the progress bar by chunk of size `bootstrap--loading-dots-chunk-threshold'."
  (when bootstrap-loading-progress-bar
    (setq bootstrap-loading-counter (1+ bootstrap-loading-counter))
    (when (>= bootstrap-loading-counter bootstrap-loading-dots-chunk-threshold)
      (setq bootstrap-loading-counter 0)
      (setq bootstrap-loading-string
            (concat bootstrap-loading-string
                    (make-string bootstrap-loading-dots-chunk-size
                                 bootstrap-loading-char)))
      (bootstrap-buffer/set-mode-line bootstrap-loading-string)
      (bootstrap//redisplay))))

(defmacro bootstrap//insert--shortcut (shortcut-char search-label &optional no-next-line)
  `(define-key bootstrap-mode-map
     ,shortcut-char
     (lambda ()
       (interactive)
       (unless (search-forward ,search-label (point-max) t)
         (search-backward ,search-label (point-min) t))
       ,@(unless no-next-line
           '((forward-line 1)))
       (back-to-indentation))))

(defun bootstrap-buffer//insert-buttons ()
  (goto-char (point-max))
  (insert "     ")
  (widget-create 'push-button
                 :help-echo "Update all ELPA packages to the latest versions."
                 :action (lambda (&rest ignore)
                           (configuration-layer/update-packages))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Rollback ELPA package upgrades if something got borked."
                 :action (lambda (&rest ignore)
                           (call-interactively 'configuration-layer/rollback))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Rollback" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Search in Emacs" 'face 'font-lock-function-name-face)
                 :help-echo "Find Emacs package and layer configs using helm-bootstrap."
                 :action (lambda (&rest ignore)
                           (call-interactively 'helm-bootstrap))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert "\n\n"))

(defun bootstrap-buffer//insert-file-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun bootstrap-buffer//insert-bookmark-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (bookmark-jump ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (format "%s - %s" el (abbreviate-file-name
                                                 (bookmark-get-filename el)))))
          list)))


(defun bootstrap-buffer/insert-startupify-lists ()
  (interactive)
  (with-current-buffer (get-buffer-create bootstrap-buffer-name)
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (goto-char (point-max))
      (page-break-lines-mode)
      (bootstrap-buffer/insert-page-break)
      (mapc (lambda (el)
              (cond
               ((eq el 'recents)
                (recentf-mode)
                (when (bootstrap-buffer//insert-file-list
                       "Recent Files:"
                       (recentf-elements 5))
                  (bootstrap//insert--shortcut "r" "Recent Files:")
                  (insert list-separator)))
               ((eq el 'bookmarks)
                (helm-mode)
                (when (bootstrap-buffer//insert-bookmark-list
                       "Bookmarks:"
                       (bookmark-all-names))
                  (bootstrap//insert--shortcut "m" "Bookmarks:")
                  (insert list-separator)))
               ((eq el 'projects)
                (projectile-mode)
                (when (bootstrap-buffer//insert-file-list
                       "Projects:"
                       (projectile-relevant-known-projects))
                  (bootstrap//insert--shortcut "p" "Projects:")
                  (insert list-separator))))) dotbootstrap-startup-lists))))

(defun bootstrap-buffer/goto-link-line ()
  "Move the point to the beginning of the link line."
  (interactive)
  (with-current-buffer bootstrap-buffer-name
    (goto-char (point-min))
    (re-search-forward "Homepage")
    (beginning-of-line)
    (widget-forward 1)))

(provide 'core-emacs-buffer)
