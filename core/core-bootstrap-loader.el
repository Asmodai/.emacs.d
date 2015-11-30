
(defconst bootstrap-loader-template-directory
  (expand-file-name (concat bootstrap-core-directory "templates/"))
  "Template directory.")

(defconst bootstrap-loader-test-results-buffer
  "*bootstrap-loader-test-results*"
  "Name of the buffer in which test results are displayed.")

(defconst bootstrap-loader-directory
  (expand-file-name
   (concat user-emacs-directory "loader.d/"))
  "Directory in which bootstrap-loaders are located.")

(defconst bootstrap-loader-filepath
  (expand-file-name
   (concat user-emacs-directory "loader.el")))

(defvar bootstrap-loader-verbose-loading nil
  "If non-NIL then display loading progress in `*Messages*' buffer.")

(defvar bootstrap-loader-startup-banner t
  "Non-NIL if a banner should be displayed on startup.")

(defvar bootstrap-loader-conf-layer-path '()
  "List of additional paths in which to look for configuration layers.")

(defvar bootstrap-loader-additional-packages '()
  "List of additional packages that will be installed without being wrapped
in a layer.  If you need some configuration for these packages then
consider creating a layer.")

(defvar bootstrap-loader-configuration-layers '(emacs-lisp)
  "List of configuration layers to load.  If it is the symbol `all' instead
of a list, then all discovered layers will be installed.")

(defvar bootstrap-loader-themes '(spacemacs-dark)
  "List of themes, the first of the list is loaded when Emacs starts.")

(defvar bootstrap-loader-default-font '("Ubuntu Mono")
  "Default font to use.")

(defvar bootstrap-loader-auto-save-file-location 'cache
  "Location to which auto-save files are writtten.

Possible values are:

  `original' - auto-save the file in-place.
  `cache'    - auto-save the file to the cache.
  `nil'      - disable auto-save.")

(defvar bootstrap-loader-which-key-delay 0.4
  "Delay, in seconds, starting from the last keystroke after which the
which-key buffer will be shown.")

(defvar bootstrap-loader-which-key-position 'bottom
  "Location of the which-key popup buffer.

Possible choices are: `bottom', `right', and `right-then-bottom'.")

(defvar bootstrap-loader-loading-progress-bar t
  "If non-NIL, a progress bar is displayed when Emacs is loading.")

(defvar bootstrap-loader-fullscreen-at-startup nil
  "If non-NIL, the Emacs frame will be full-screen when Emacs starts up.

This requires Emacs 24.4 and above.")

(defvar bootstrap-loader-fullscreen-use-non-native nil
  "If non-NIL, `bootstrap/toggle-fullscreen' will not use native fullscreen.
Use to disable fullscreen animations in OSX.")

(defvar bootstrap-loader-maximised-at-startup nil
  "If non-NIL, the Emacs frame will be maximised when Emacs starts up.  This
will only take effect if `bootstrap-loader-fullscreen-at-startup' is nil.

Requires Emacs 24.4 and above.")

(defvar bootstrap-loader-active-transparency 90
  "A value in the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it is active or selected.

Transparency can be toggled through `bootstrap/toggle-transparency'.")

(defvar bootstrap-loader-inactive-transparency 90
  "A value in the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it is inactive.

Transparency can be toggled through `bootstrap/toggle-transparency'.")

(defvar bootstrap-loader-mode-line-unicode-symbols t
  "If non-NIL then unicode symbols are displayed in the mode-line.")

(defvar bootstrap-loader-smooth-scrolling t
  "If non-NIL then smooth scrolling is enabled.")

(defvar bootstrap-loader-persistent-server nil
  "If non-NIL then the quit functions are advised to keep the server open when
exiting Emacs.")

(defvar bootstrap-loader-spartparens-strict-mode nil
  "If non-NIL then smartparens-strict-mode will be enabled in all programming
modes.")

(defvar bootstrap-loader-highlight-delimiters 'all
  "Select a scope to highlight delimiters.  Possible values are `any',
`current', `all' or `nil'.")

(defvar bootstrap-loader-delete-orphans-packages t
  "If non-NIL then Emacs will delete any orphan packages.")

(defvar bootstrap-loader-search-tools '("ag" "pt" "ack" "grep")
  "List of search tool executable names.

Supported tools are `ag', `pt', `ack', and `grep'.")

(defvar bootstrap-loader-default-package-repository 'melpa-stable
  "The default package repository used if no explicit repository has been
specified with an installed package.")

(defvar bootstrap-loader-startup-lists '(recents projects)
  "List of items to show in the startup buffer.  If NIL then the list is
disabled.

Possible values are: `recents', `bookmarks', `projects'.")

(defvar bootstrap-loader-exluded-packages '()
  "A list of packages and/or extensions that will not be installed and
loaded.")

(defun bootstrap-loader/sync-configuration-layers (&optional arg)
  "Synchronise declared layers in a bootstrap-loader file with Emacs."
  (interactive "P")
  (when (file-exists-p bootstrap-loader-filepath)
    (with-current-buffer (find-file-noselect bootstrap-loader-filepath)
      (let ((bootstrap-loader-loading-progress-bar nil))
        (setq bootstrap-loader-loading-string "")
        (save-buffer)
        (let ((tests-ok (or (equal arg '(16))
                            (bootstrap-loader/test-bootstrap-loader t))))
          (if tests-ok
              (progn
                (load-file buffer-file-name)
                (bootstrap-loader|call-func bootstrap-loader/init
                                            "Calling bootstrap-loader init...")
                (bootstrap-conf-layer/sync)
                (if (member arg '(4 16))
                    (message (concat
                              "Done (`bootstrap-loader/user-config' function "
                              "has been skipped)."))
                  (when (fboundp 'bootstrap-loader/user-config)
                    (bootstrap-loader|call-func
                       bootstrap-loader/user-config
                       "Calling bootstrap-loader user config..."))
                  (message "Done."))
                (when (configuration-layer/package-used-p 'powerline)
                  (emacs//restore-powerline (current-buffer))))
            (switch-to-buffer-other-window
              bootstrap-loader-test-results-buffer)
            (emacs-buffer/warning "Some tests failed, check `%s' buffer."
                                  bootstrap-loader-test-results-buffer)))))))

(defun bootstrap-loader/get-variable-string-list ()
  "Return a list of all the variables as strings."
  (all-completions "" obarray
                   (lambda (x)
                     (and (boundp x)
                          (not (keywordp x))
                          (string-prefix-p "bootstrap-loader"
                                           (symbol-name x))))))

(defun bootstrap-loader/get-variable-list ()
  "Return a list of all bootstrap-loader variable symbols."
  (mapcar 'intern (bootstrap-loader/get-variable-string-list)))

(defmacro bootstrap-loader|symbol-value (symbol)
  "Return the value of SYMBOL corresponding to a bootstrap-loader variable.

If SYMBOL value is `display-graphic-p' then return the result of
`(display-graphic-p)', otherwise return the value of the symbol."
  `(if (eq ,symbol 'display-graphic-p)
       (display-graphic-p)
     ,symbol))

(defun bootstrap-loader/location ()
  "Return the absolute path to the bootstrap-loader file."
  bootstrap-loader-filepath)

(defun bootstrap-loader/copy-template ()
  "Copy `bootstrap-loader.template'."
  (interactive)
  (let* ((copy? (if (file-exists-p bootstrap-loader-filepath)
                    (y-or-n-p
                     (format "%s already exists. Do you want to overwrite it? "
                             bootstrap-loader-filepath))
                  t)))
    (when copy?
      (copy-file (concat bootstrap-loader-template-directory
                         "loader.template")
                 bootstrap-loader-filepath
                 t)
      (message "%s has been installed." bootstrap-loader-filepath))))

(defun bootstrap-loader//ido-completing-read (prompt candidates)
  "Call `ido-completing-read' with a CANDIDATES alist where the key is a
display string and the value is the actual value to return."
  (let ((ido-max-window-height (1+ (length candidates))))
    (cadr (assoc (ido-completing-read prompt (mapcar 'car candidates))
                 candidates))))

(defun bootstrap-loader/install ()
  "Install the bootstrap-loader and return non-NIL if the install was successful."
  (interactive "P")
  (with-current-buffer (find-file-noselect
                        (concat bootstrap-loader-template-directory
                                "loader.template"))
    (let ((install
           (if (file-exists-p bootstrap-loader-filepath)
               (y-or-n-p
                (format "%s already exists. Do you want to overwrite it? "
                        bootstrap-loader-filepath))
             t)))
      (when install
        (write-file bootstrap-loader-filepath)
        (message "%s has been installed." bootstrap-loader-filepath)
        t))))

(defun bootstrap-loader/load-file ()
  "Load the bootstrap-loader file if it exists."
  (let ((bootstrap-loader (bootstrap-loader/location)))
    (if (file-exists-p bootstrap-loader)
        (unless (ignore-errors (load bootstrap-loader))
          (bootstrap-loader/safe-load)))))

(defun bootstrap-loader/safe-load ()
  "Error recovery from a malformed bootstrap-loader file."
  (load (concat bootstrap-loader-template-directory
                "loader.template"))
  (defadvice bootstrap-loader/layers
      (after error-recover-preserve-packages activate)
    (progn
      (setq-default bootstrap-loader-delete-orphan-packages nil)
      (ad-disable-advice 'bootstrap-loader/layers 'after
                         'error-recover-preserve-packages)
      (ad-activate 'bootstrap-loader/layers))))

(defmacro bootstrap-loader|call-func (func &optional msg)
  "Call the function from the bootstrap-loader only if it is bound.

If MSG is non-NIL then display a message in `*Messages*'."
  `(progn
     (when ,msg
       (bootstrap-buffer/message ,msg))
     (if (fboundp ',func)
         (,func))))

(defun bootstrap-loader//test-bootstrap-loader/layers ()
  "Test for `bootstrap-loader/layers'."
  (insert
   (format (concat "\n* Testing settings in bootstrap-loader/layers "
                   "[[file:%s::bootstrap-loader/layers][Show in File]]\n")
           bootstrap-loader-filepath))
  (let (bootstrap-loader-configuration-layer-path
        bootstrap-loader-configuration-layers
        bootstrap-loader-additional-packages
        bootstrap-loader-excluded-packages
        bootstrap-loader-delete-orphan-packages
        (passed-tests 0)
        (total-tests 0))
    (load bootstrap-loader-filepath)
    (bootstrap-loader/layers)
    (bootstrap//test-list 'stringp
                             'bootstrap-loader-configuration-layer-path
                             "is a string"
                             "path")
    (bootstrap//test-list 'file-directory-p
                             'bootstrap-loader-configuration-layer-path
                             "exists in filesystem"
                             "path")
    (setq bootstrap-loader-configuration-layers
          (mapcar (lambda (l)
                    (if (listp l)
                        (car l)
                      l))
                  bootstrap-loader-configuration-layers))
    (bootstrap//test-list 'configuration-layer/get-layer-path
                             'bootstrap-loader-configuration-layers
                             "can be found"
                             "layer")
    (insert (format
             (concat
              "** RESULTS: "
              "[[file:%s::bootstrap-loader/layers][bootstrap-loader/layers]]"
              "passed %s out of %s tests.\n")
             bootstrap-loader-filepath
             passed-tests
             total-tests))
    (equal passed-tests total-tests)))

(defmacro bootstrap-loader||let-init-test (&rest body)
  "Macro to protect bootstrap-loader variables."
  `(let ((fpath bootstrap-loader-filepath)
         ,@(bootstrap-loader/get-variable-list)
         (passed-tests 0)
         (total-tests 0))
     (setq bootstrap-loader-filepath fpath)
     (load bootstrap-loader-filepath)
     ,@body))

(defun bootstrap-loader//test-bootstrap-loader/init ()
  "Tests for `bootstrap-loader/init'"
  (insert (format (concatenate
                   "\n* Testing settings in bootstrap-loader/init "
                   "[[file:%s::bootstrap-loader/init][Show in File]]\n")
                  bootstrap-loader-filepath))
  (bootstrap-loader||let-init-test
   (bootstrap-loader/init)
   (bootstrap//test-var (lambda (x)
                             (member x '(original cache nil)))
                           'bootstrap-loader-auto-save-file-location
                           "is one of \'original, \'cache or nil")
   (bootstrap//test-var (lambda (x)
                             (member x '(all any current nil)))
                           'bootstrap-loader-highlight-delimiters
                           "is one of \'all, \'any, \'current or nil")
   (bootstrap//test-list (lambda (x)
                              (member x '(recents bookmarks projects)))
                            'bootstrap-loader-startup-lists
                            (concat "includes only \'recents, "
                                    "\'bookmarks or \'projects"))
   (bootstrap//test-var 'stringp
                           'bootstrap-loader-leader-key
                           "is a string")
   (bootstrap//test-var 'stringp
                           'bootstrap-loader-emacs-leader-key
                           "is a string")
   (bootstrap//test-var 'stringp
                           'bootstrap-loader-major-mode-leader-key
                           "is a string")
   (bootstrap//test-var 'stringp
                           'bootstrap-loader-command-key
                           "is a string")
   (insert (format
            (concat
             "** RESULTS: "
             "[[file:%s::bootstrap-loader/init][bootstrap-loader/init]] "
             "passed %s out of %s tests\n")
            bootstrap-loader-filepath
            passed-tests
            total-tests))
   (equal passed-tests total-tests)))

(defun bootstrap-loader/test-bootstrap-loader (&optional hide-buffer)
  "Test the settings in the bootstrap-loader file for correctness.

Returns nin-NIL if all the tests have passed."
  (interactive)
  (let ((min-version "0.0"))
    (if nil
        (error (format (concat "error: bootstrap-loader/test-dotfile requires "
                               "bootstrap-loader-version %s") min-version))
      (with-current-buffer (get-buffer-create
                              bootstrap-loader-test-results-buffer)
        (unless hide-buffer
          (switch-to-buffer-other-window bootstrap-loader-test-results-buffer))
        (org-mode)
        (org-indent-mode)
        (view-mode)
        (when (bound-and-true-p flyspell-mode)
          (flyspell-mode -1))
        (let (buffer-read-only)
          (erase-buffer)
          (insert (format "* Running tests on [[file:%s][%s]] (v%s)\n"
                          bootstrap-loader-filepath
                          bootstrap-loader-filepath
                          "0.0"))
          (prog1
              (reduce (lambda (x y)
                        (and (funcall y) x))
                      '(bootstrap-loader//test-bootstrap-loader/layers
                        bootstrap-loader//test-bootstrap-loader/init)
                      :initial-value t)
            (goto-char (point-min))))))))

(provide 'core-bootstrap-loader)
