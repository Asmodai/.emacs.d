
(defun bootstrap/load-or-install-protected-package (pkg &optional
                                                    log file-to-load)
  "Load PKG package and protect it against being deleted as an orphan."
  (push pkg bootstrap-conf-layer--protected-packages)
  (bootstrap/load-or-install-package pkg log file-to-load))

(defun bootstrap/load-or-install-package (pkg &optional log file-to-load)
  "Load PKG package.  PKG will be installed if it is not already installed.

Whenever the initial require fails, the absolute path to the package
directory is returned.

If LOG is non-NIL, a message is displayed in the bootstrap-mode buffer.

FILE-TO-LOAD is an explicit file to load after the initialisation."
  (let ((warning-minimum-level :error))
    (condition-case nil
        (require pkg)
      (error
       (require 'cl)
       (let ((pkg-elpa-dir (bootstrap//get-package-directory pkg)))
         (if pkg-elpa-dir
             (add-to-list 'load-path pkg-elpa-dir)
           (when log
             (bootstrap-buffer/append
              (format "(Bootstrap) Installing %s...\n" pkg))
             (bootstrap//redisplay))
           (package-refresh-contents)
           (package-install pkg)
           (setq pkg-elpa-dir (bootstrap//get-package-directory pkg)))
         (require pkg nil 'noerror)
         (when file-to-load
           (load-file (concat pkg-elpa-dir file-to-load)))
         pkg-elpa-dir)))))

(defun bootstrap//get-package-directory (pkg)
  "Return the directory of PKG.  Return NIL if not found."
  (let ((elpa-dir (concat user-emacs-directory "elpa/")))
    (when (file-exists-p elpa-dir)
      (let ((dir (reduce (lambda (x y)
                           (if x
                               x
                             y))
                         (mapcar (lambda (x)
                                   (when (string-match
                                          (concat "/"
                                                  (symbol-name pkg)
                                                  "-[0-9]+")
                                          x)
                                     x))
                                 (directory-files elpa-dir 'full))
                         :initial-value nil)))
        (when dir
          (file-name-as-directory dir))))))

(defun bootstrap/mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail)
                (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail)
                (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun bootstrap/mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.
If there are multiple properties with the same keyword, only the first
property and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail)
                (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail)
                  (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

;; From http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun bootstrap/dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (bootstrap/dump varlist buf)
      (make-directory (file-name-directory filename) t)
      (save-buffer)
      (kill-buffer))))

;; From http://stackoverflow.com/questions/2321904/elisp-how-to-save-data-in-a-file
(defun bootstrap/dump (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

(defvar bootstrap--init-redisplay-count 0
  "The number of calls to `redisplay'")

(defun bootstrap//redisplay ()
  "`redisplay' wrapper."
  (setq bootstrap--init-redisplay-count (1+ bootstrap--init-redisplay-count))
  (redisplay))

(defun bootstrap/view-org-file (file &optional anchor-text expand-scope)
  "Open the change log for the current version."
  (interactive)
  (find-file file)
  (org-indent-mode)
  (view-mode)
  (goto-char (point-min))

  (when anchor-text
    (re-search-forward anchor-text))
  (beginning-of-line)

  (cond
   ((eq expand-scope 'subtree)
    (show-subtree))
   ((eq expand-scope 'all)
    (show-all))
   (t nil))

  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\n")
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components)

  (setq-local org-emphasis-alist '(("*" bold)
                                   ("/" italic)
                                   ("_" underline)
                                   ("=" org-verbatim verbatim)
                                   ("~" org-kbd)
                                   ("+"
                                    (:strike-through t))))

  (setq-local org-hide-emphasis-markers t))

(defun bootstrap//test-var (pred var test-desc)
  "Test PRED against VAR and print test result, incrementing
passed-tests and total-tests."
  (let ((var-name (symbol-name var))
        (var-val (symbol-value var)))
    (when (boundp 'total-tests)
      (setq total-tests (1+ total-tests)))
    (insert (format "** TEST: [[file:%s::%s][%s]] %s\n"
                    bootstrap-loader-filepath
                    var-name
                    var-name
                    test-desc))
    (if (funcall pred var-val)
        (progn
          (when (boundp 'passed-tests)
            (setq passed-tests (1+ passed-tests)))
          (insert (format "*** PASS: %s\n" var-val)))
      (insert (propertize (format "*** FAIL: %s\n" var-val)
                          'font-lock-face
                          'font-lock-warning-face)))))

(defun bootstrap//test-list (pred varlist test-desc &optional element-desc)
  "Test PRED against each element of VARLIST and print test
result, incrementing passed-tests and total-tests."
  (let ((varlist-name (symbol-name varlist))
        (varlist-val (symbol-value varlist)))
    (if element-desc
        (insert (format "** TEST: Each %s in [[file:%s::%s][%s]] %s\n"
                        element-desc
                        bootstrap-loader-filepath
                        varlist-name
                        varlist-name
                        test-desc))
      (insert (format "** TEST: Each element of [[file:%s::%s][%s]] %s\n"
                      bootstrap-loader-filepath
                      varlist-name
                      varlist-name
                      test-desc)))
    (dolist (var varlist-val)
      (when (boundp 'total-tests)
        (setq total-tests (1+ total-tests)))
      (if (funcall pred var)
          (progn
            (when (boundp 'passed-tests)
              (setq passed-tests (1+ passed-tests)))
            (insert (format "*** PASS: %s\n" var)))
        (insert (propertize (format "*** FAIL: %s\n" var)
                            'font-lock-face
                            'font-lock-warning-face))))))

(provide 'core-funcs)
