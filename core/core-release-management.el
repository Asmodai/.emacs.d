(defconst bootstrap-repository ".emacs.d"
  "Name of the Bootstrap remote repository.")

(defconst bootstrap-repository-owner "asmodai"
  "Name of the Bootstrap remote repository owner.")

(defconst bootstrap-checkversion-remote "origin"
  "Name of the remote repository used to check for new version.")

(defconst bootstrap-checkversion-branch "master"
  "Name of the branch used to check for new version.")

;; new version variables
(defvar bootstrap-new-version nil
  "If non-nil a new Bootstrap version is available.")

(defvar bootstrap-version-check-timer nil
  "The current timer for new version check.")

(defvar bootstrap-version-check-interval "6 hours"
  "Time between two version checks.")

(defvar bootstrap-version-check-lighter "[+]"
  "Text displayed in the mode-line when a new version is available.")

(defun bootstrap/switch-to-version (&optional version)
  "Switch bootstrap to VERSION.
VERSION is a string with the format `x.x.x'.
IMPORTANT: The switch is performed by hard resetting the current branch.
If VERSION is nil then a prompt will ask for a version number.
If the current version is not `master' and not `develop' then
a prompt will ask for confirmation before actually switching to the
specified version.
It is not possible to switch version when you are on `develop' branch,
users on `develop' branch must manually pull last commits instead."
  (interactive)
  (let ((branch (bootstrap/git-get-current-branch)))
    (if (string-equal "develop" branch)
        (message (concat "Cannot switch version because you are on develop.\n"
                         "You have to manually `pull --rebase' last commits."))
      (unless version (setq version (read-string "version: "
                                                 (bootstrap/get-last-version))))
      (when (or (string-equal "master" branch)
                (yes-or-no-p (format (concat "You are not on master, are you "
                                             "sure that you want to switch to "
                                             "version %s ? ") version)))
        (let ((tag (concat "v" version)))
          (if (bootstrap/git-hard-reset-to-tag tag)
              (progn
                (setq bootstrap-version version)
                (message "Succesfully switched to version %s" version))
            (message "An error occurred while switching to version %s"
                     version)))))))

(defun bootstrap/check-for-new-version (&optional interval)
  "Periodicly check for new for new Bootstrap version.
Update `bootstrap-new-version' variable if any new version has been
found."
  (if (string-equal "develop" (bootstrap/git-get-current-branch))
      (message "Skipping check for new version because you are on develop.")
    (message "Start checking for new version...")
    (async-start
     (lambda ()
       (load-file (concat user-emacs-directory "core/core-load-paths.el"))
       (require 'core-bootstrap)
       (bootstrap/get-last-version))
     (lambda (result)
       (if result
           (if (or (version< result bootstrap-version)
                   (string= result bootstrap-version)
                   (if bootstrap-new-version
                       (string= result bootstrap-new-version)))
               (message "Bootstrap is up to date.")
             (message "New version of Bootstrap available: %s" result)
             (setq bootstrap-new-version result))
         (message "Unable to check for new version."))))
    (when interval
      (setq bootstrap-version-check-timer
            (run-at-time t (timer-duration interval)
                         'bootstrap/check-for-new-version)))))

(defun bootstrap/get-last-version ()
  "Return the last tagged version."
  (bootstrap//get-last-version bootstrap-repository
                               bootstrap-repository-owner
                               bootstrap-checkversion-remote
                               bootstrap-checkversion-branch))

(defun bootstrap//get-last-version (repo owner remote branch)
  "Return the last tagged version of BRANCH on REMOTE repository from
OWNER REPO."
  (let ((url (format "https://github.com/%s/%s" owner repo)))
    (bootstrap/git-remove-remote remote)
    (bootstrap/git-add-remote remote url)
    (bootstrap/git-fetch-remote remote)
    (bootstrap/git-fetch-tags remote branch))
  (let ((version (bootstrap/git-latest-tag remote branch)))
    (when version
      (save-match-data
        (string-match "^.*\\([0-9]+\\.[0-9]+\\.[0-9]+\\)$" version)
        (match-string 1 version)))))

(defun bootstrap/get-new-version-lighter-face (current new)
  "Return the new version lighter face given the difference between the CURRENT
version and the NEW version."
  (let* ((lcur (version-to-list current))
         (lnew (version-to-list new))
         (scur (bootstrap//compute-version-score lcur))
         (snew (bootstrap//compute-version-score lnew))
         (diff (- snew scur)))
    (cond
     ((< diff 3000) 'bootstrap-mode-line-new-version-lighter-success-face)
     ((< diff 5000) 'bootstrap-mode-line-new-version-lighter-warning-face)
     (t 'bootstrap-mode-line-new-version-lighter-error-face))))

(defun bootstrap/git-has-remote (remote)
  "Return non nil if REMOTE is declared."
  (let((proc-buffer "git-has-remote")
       (default-directory (file-truename user-emacs-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil "remote"))
        (with-current-buffer proc-buffer
          (prog2
              (goto-char (point-min))
              (re-search-forward (format "^%s$" remote) nil 'noerror)
            (kill-buffer proc-buffer))))))

(defun bootstrap/git-add-remote (remote url)
  "Add a REMOTE with URL, return t if no error."
  (let((proc-buffer "git-add-remote")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "add" remote url))
      (kill-buffer proc-buffer))))

(defun bootstrap/git-remove-remote (remote)
  "Remove a REMOTE, return t if no error."
  (let((proc-buffer "git-remove-remote")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "remote" "remove" remote))
      (kill-buffer proc-buffer))))

(defun bootstrap/git-fetch-remote (remote)
  "Fetch last commits from REMOTE, return t if no error."
  (let((proc-buffer "git-remove-remote")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote))
      (kill-buffer proc-buffer))))

(defun bootstrap/git-fetch-tags (remote branch)
  "Fetch the tags for BRANCH in REMOTE repository."
  (let((proc-buffer "git-fetch-tags")
       (default-directory (file-truename user-emacs-directory)))
    (prog2
        ;; seems necessary to fetch first
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" remote branch))
        ;; explicitly fetch the new tags
        (eq 0 (process-file "git" nil proc-buffer nil
                            "fetch" "--tags" remote branch))
      (kill-buffer proc-buffer))))

(defun bootstrap/git-hard-reset-to-tag (tag)
  "Hard reset the current branch to specifed TAG."
  (let((proc-buffer "git-hard-reset")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "reset" "--hard" tag))
      (kill-buffer proc-buffer))))

(defun bootstrap/git-latest-tag (remote branch)
  "Returns the latest tag on REMOTE/BRANCH."
  (let((proc-buffer "git-latest-tag")
       (default-directory (file-truename user-emacs-directory))
       (where (format "%s/%s" remote branch)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "describe" "--tags" "--abbrev=0"
                              "--match=v*" where "FETCH_HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-string)
                (end-of-buffer)
                (forward-line -1)
                (replace-regexp-in-string
                 "\n$" ""
                 (buffer-substring (line-beginning-position)
                                   (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun bootstrap/git-checkout (branch)
  "Checkout the given BRANCH. Return t if there is no error."
  (let((proc-buffer "git-checkout")
       (default-directory (file-truename user-emacs-directory)))
    (prog1
        (eq 0 (process-file "git" nil proc-buffer nil
                            "checkout" branch))
      (kill-buffer proc-buffer))))

(defun bootstrap/git-get-current-branch ()
   "Return the current branch. Return nil if an error occurred."
   (let((proc-buffer "git-get-current-branch")
        (default-directory (file-truename user-emacs-directory)))
     (when (eq 0 (process-file "git" nil proc-buffer nil
                               "symbolic-ref" "--short" "-q" "HEAD"))
       (with-current-buffer proc-buffer
         (prog1
             (when (buffer-string)
               (goto-char (point-min))
               (replace-regexp-in-string
                "\n$" ""
                (buffer-substring (line-beginning-position)
                                  (line-end-position))))
           (kill-buffer proc-buffer))))))

(defun bootstrap/git-get-current-branch-rev ()
  "Returns the hash of the head commit on the current branch.
Returns nil if an error occurred."
  (let((proc-buffer "git-get-current-branch-head-hash")
       (default-directory (file-truename user-emacs-directory)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "rev-parse" "--short" "HEAD"))
      (with-current-buffer proc-buffer
        (prog1
            (when (buffer-string)
              (goto-char (point-min))
              (replace-regexp-in-string
               "\n$" ""
               (buffer-substring (line-beginning-position)
                                 (line-end-position))))
          (kill-buffer proc-buffer))))))

(defun bootstrap//deffaces-new-version-lighter (state)
  "Define a new version lighter face for the given STATE."
  (let* ((fname (intern (format "bootstrap-mode-line-new-version-lighter-%s-face"
                                (symbol-name state))))
         (foreground (face-foreground state)))
    (eval `(defface ,fname '((t ()))
             ,(format "Color for new version lighter in mode line (%s)."
                      (symbol-name state))
             :group 'bootstrap))
    (set-face-attribute fname nil
                        :foreground foreground
                        :box (face-attribute 'mode-line :box))))

(defun bootstrap/set-new-version-lighter-mode-line-faces ()
  "Define or set the new version lighter mode-line faces."
  (mapcar 'bootstrap//deffaces-new-version-lighter
          '(error warning success)))
(bootstrap/set-new-version-lighter-mode-line-faces)

(defun bootstrap//compute-version-score (version)
  "Returns an integer from the version list.
Example: (1 42 3) = 1 042 003"
  (let ((i -1))
    (reduce '+ (mapcar (lambda (n) (setq i (1+ i)) (* n (expt 10 (* i 3))))
                       (reverse version)))))

(provide 'core-release-management)
