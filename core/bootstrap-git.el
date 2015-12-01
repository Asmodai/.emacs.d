

(defun bootstrap:git-head-short-hash ()
  (let ((proc-buffer "git-current-hash")
        (default-directory (file-truename +bootstrap-core-directory+)))
    (when (eq 0 (process-file "git" nil proc-buffer nil
                              "rev-parse" "--short" "HEAD"))
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
