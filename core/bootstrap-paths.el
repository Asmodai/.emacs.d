
;(defvar *bootstrap-debug* t)

(defconst +bootstrap-directory+
  (if (and (boundp '*bootstrap-debug*)
           *bootstrap-debug*)
      "~/emacs.d/"
    user-emacs-directory))

(defconst +bootstrap-core-directory+
  (expand-file-name (concat +bootstrap-directory+ "core/")))

(defconst +bootstrap-library-directory+
  (expand-file-name (concat +bootstrap-core-directory+ "libs/")))

(defconst +bootstrap-cache-directory+
  (expand-file-name (concat +bootstrap-directory+ "cache/")))

(defconst +bootstrap-banner-directory+
  (expand-file-name (concat +bootstrap-core-directory+ "banners/")))

(defconst +bootstrap-auto-save-directory+
  (expand-file-name (concat +bootstrap-cache-directory+ "auto-save/")))

(defconst +bootstrap-bytecode-cache-directory+
  (expand-file-name (concat +bootstrap-cache-directory+ "bytecode/")))

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

(defconst user-dropbox-directory
  (let ((dropbox (expand-file-name
                  (concat user-home-directory "Dropbox/"))))
    (if (file-exists-p dropbox)
        dropbox
      "")))



(defun add-to-load-path (dir)
  (add-to-list 'load-path dir))



(mapc 'add-to-load-path
      `(,+bootstrap-core-directory+
        ,+bootstrap-library-directory+
        ,(concat user-dropbox-directory "emacs/")))

;; EOF
