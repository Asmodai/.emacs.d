
(defvar *bootstrap-init-redisplay-count* 0
  "The number of calls to `redisplay'")

(defun bootstrap:redisplay ()
  "`redisplay' wrapper."
  (incf *bootstrap-init-redisplay-count*)
  (redisplay))

(defun bootstrap:list-variables (&optional prefix suffix)
  (setq prefix (or prefix
                   "*bootstrap-"))
  (setq suffix (or suffix
                   ""))
  (all-completions "" obarray
                   (lambda (x)
                     (let ((name (symbol-name x)))
                       (and (boundp x)
                            (not (keywordp x))
                            (string-prefix-p prefix name)
                            (string-suffix-p suffix name))))))

(defun bootstrap:paths ()
  (let ((buffer "*bootstrap-paths*"))
    (with-current-buffer (get-buffer-create buffer)
      (interactive)
      (erase-buffer)
      (insert
       (string-join
        (mapcar (lambda (x)
                  (format "%s = %s\n"
                          (symbol-name (intern x))
                          (symbol-value (intern x))))
                (bootstrap:list-variables "+bootstrap-"
                                          "-directory+")))))
    (switch-to-buffer buffer)))

(provide 'bootstrap-funs)

