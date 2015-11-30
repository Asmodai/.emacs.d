
(defun add-to-load-path (dir)
  (add-to-list 'load-path dir))

(defconst bootstrap-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "Bootstrap core directory.")

(defconst bootstrap-info-directory
  (expand-file-name (concat bootstrap-core-directory "info/"))
  "Bootstrap info files directory")

(defconst bootstrap-banner-directory
  (expand-file-name (concat bootstrap-core-directory "banners/"))
  "Bootstrap banners directory.")

(defconst bootstrap-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Bootstrap storage area for persistent files")

(defconst bootstrap-auto-save-directory
  (expand-file-name (concat bootstrap-cache-directory "auto-save/"))
  "Bootstrap auto-save directory")

(defconst bootstrap-docs-directory
  (expand-file-name (concat user-emacs-directory "doc/"))
  "Bootstrap documentation directory.")

(defconst bootstrap-test-directory
  (expand-file-name (concat user-emacs-directory "tests/"))
  "Bootstrap tests directory.")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

(defconst pcache-directory
  (concat bootstrap-cache-directory "pcache"))

(unless (file-exists-p bootstrap-cache-directory)
    (make-directory bootstrap-cache-directory))

(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")

;; load paths
(mapc 'add-to-load-path
      `(,(concat user-emacs-directory "core/")
        ,(concat user-emacs-directory "core/libs/")
        ,(concat user-dropbox-directory "emacs/")))
