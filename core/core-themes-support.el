
(defconst emacs-built-in-themes (custom-available-themes)
  "List of built-in themes.")

(defface org-kbd
  '((t (:background "LemonChiffon1"
        :foreground "black"
        :box (:line-width 2 :color nil :style released-button))))
  "Face for displaying key bindings."
  :group 'org-faces)

(defconst bootstrap-theme-name-to-package
  '((spacemacs-dark . spacemacs-theme))
  "An alist matching a theme name with its package name.")

(defun bootstrap//get-theme-package (theme)
  "Returns the package theme for the given THEME name."
  (cond ((memq theme emacs-built-in-themes)
         nil)
        ((assq theme bootstrap-theme-name-to-package)
         (cdr (assq theme bootstrap-theme-name-to-package)))
        (t
         (intern (format "%S-theme" theme)))))

(defun bootstrap/load-theme (theme)
  "Load the given THEME."
  (unless (memq theme (custom-available-themes))
    (cond ((eq 'spacemacs-dark theme)
           (bootstrap/load-or-install-package 'spacemacs-theme)
           (add-to-list 'load-path (bootstrap//get-package-directory
                                    'spacemacs-theme))
           (require 'spacemacs-common)
           (deftheme spacemacs-dark "Lisp Hacker theme, the Dark Side."))
          (t
           (let ((pkg (bootstrap//get-theme-package theme)))
             (bootstrap/load-or-install-package pkg)))))
  (load-theme theme t))

(defadvice load-theme (after bootstrap/load-theme-adv activate)
  "Perform post load processing."
  (let ((theme (ad-get-arg 0)))
    (setq bootstrap--cur-theme theme)
    (bootstrap/post-theme-init theme)))

(defun bootstrap/post-theme-init (theme)
  (interactive)
  (when (fboundp 'bootstrap/set-flycheck-mode-line-faces)
    (bootstrap/set-flycheck-mode-line-faces))
  (when (fboundp 'bootstrap/set-new-version-lighter-mode-line-faces)
    (bootstrap/set-new-version-lighter-mode-line-faces))
  (when (fboundp 'bootstrap/customise-powerline-faces)
    (bootstrap/customise-powerline-faces))
  (when (fboundp 'powerline-reset)
    (powerline-reset)))

(provide 'core-themes-support)
