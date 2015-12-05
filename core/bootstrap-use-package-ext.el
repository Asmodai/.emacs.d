
(defconst +bootstrap-use-package-add-hook-keywords+
  '(:pre-init
    :post-init
    :pre-config
    :post-config))

(defmacro bootstrap:use-package-add-hook (name &rest plist)
  "Add post hooks to `:init' or `:config' arguments of an existing
configuration.

In order to use this macro the variable `use-package-inject-hooks'
must be non-nil.

This is useful in the dotfile to override the default configuration
of a package.

Usage:

  (use-package-add-hook package-name
     [:keyword [option]]...)

:pre-init      Code to run before the default `:init' configuration.
:post-init     Code to run after the default `:init' configuration.
:pre-config    Code to run before the default `:config' configuration.
:post-config   Code to run after the default `:config' configuration.

In practice the most useful hook is the `:post-config' where you can
override lazy-loaded settings."
  (declare (indent 1))
  (let ((name-symbol (if (stringp name)
                         (intern name)
                       name))
        (expanded-forms '()))
    (dolist (keyword +bootstrap-use-package-add-hook-keywords+)
      (let ((body (bootstrap:mplist-get plist keyword)))
        (when body
          (let ((hook (intern (format "use-package--%S--%s-hook"
                                      name-symbol
                                      (substring (format "%s" keyword) 1)))))
            (push `(add-hook ',hook (lambda nil ,@body)) expanded-forms)))))
    `(progn ,@expanded-forms)))

(provide 'bootstrap-use-package-ext)
