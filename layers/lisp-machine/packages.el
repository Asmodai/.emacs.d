
(setq lisp-machine-packages
      '(help+
        help-fns+
        (symbolics :location local :step post)))

(defun lisp-machine:init-help+ ()
  (use-package help+
    :defer t))

(defun lisp-machine:init-help-fns+ ()
  (use-package help-fns+
    :defer t))

(defun lisp-machine:init-symbolics ()
  (use-package symbolics
    :defer t
    :init (require 'symbolics)
    :config (symbolics:install-keymap)))

