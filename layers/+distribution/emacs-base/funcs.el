
(defun bootstrap:emacsbin-path ()
  (interactive)
  (concat exec-directory
          (if (windows-p)
              "bin/")
          "emacs"))

(defun bootstrap:add-to-hooks (fun hooks)
  "Add a function to the given hooks."
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun bootstrap:add-all-to-hook (hook &rest funs)
  "Add all given functions to a hook."
  (dolist (fun funs)
    (add-hook hook fun)))

