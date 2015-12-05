
(require 'cl-lib)
(require 'eieio)
(require 'package)
(require 'warnings)
(require 'ht)
(require 'bootstrap-funs)
(require 'bootstrap-buffer)

;; Configure and initialise `pakage', unless it has been initialised already.
(unless package--initialized
  (setq package-archives '(("melpa" . "http://melpa.org/packages/")
                           ("org"   . "http://orgmode.org/elpa/")
                           ("gnu"   . "http://elpa.gnu.org/packages/")))
  (setq package-enable-at-startup nil)
  (package-initialize 'noactivate)
  (unless (or (package-installed-p 'python)
              (version< emacs-version "24.3"))
    (add-to-list 'package-archives
                 '("marmalade" . "https://marmalade-repo.org/packages/"))))

(defconst +bootstrap-layer-template-directory+
  (expand-file-name (concat +bootstrap-core-directory+ "templates/"))
  "Bootstrap layer templates directory.")

(defconst +bootstrap-layer-directory+
  (expand-file-name (concat +bootstrap-directory+ "layers/"))
  "Bootstrap layers base directory.")

(defconst +bootstrap-layer-private-directory+
  (expand-file-name (concat +bootstrap-directory+ "private/"))
  "Bootstrap private layers base directory.")

(defconst +bootstrap-layer-rollback-directory+
  (expand-file-name (concat +bootstrap-cache-directory+ "rollback/"))
  "Bootstrap rollback directory.")

(defconst bootstrap-layer-rollback-info "rollback-info"
  "Bootstrap rollback information file.")

(defconst +bootstrap-distribution-layer+ 'emacs-base)

(defvar *bootstrap-delete-orphan-packages* t)

(defclass cfgl-layer ()
  ((name
    :initarg :name
    :type symbol
    :documentation "The name of the layer.")
   (dir
    :initarg :dir
    :type string
    :documentation "Absolute path to the layer.")
   (variables
    :initarg :variables
    :initform nil
    :type list
    :documentation "A list of variable-value pairs.")
   (disabled
    :initarg :disabled-for
    :initform nil
    :type list
    :documentation "A list of layers where this layer is disabled."))
  "A configuration layer.")

(defclass cfgl-package ()
  ((name
    :initarg :name
    :type symbol
    :documentation "Nae of the package.")
   (owner
    :initarg :owner
    :initform nil
    :type symbol
    :documentation "The layer definining the init function.")
   (pre-layers
    :initarg :pre-layers
    :initform '()
    :type list
    :documentation "Layers with a pre-init function.")
   (post-layers
    :initarg :post-layers
    :initform '()
    :type list
    :documentation "Layers with a post-init function.")
   (location
    :initarg :location
    :initform elpa
    :type (satisfies (lambda (x)
                       (or (member x '(built-in local elpa))
                           (and (listp x)
                                (eq 'recipe (car x))))))
    :documentation "Location of the package.")
   (step
    :initarg :step
    :initform nil
    :type (satisfies (lambda (x)
                       (member x '(nil pre post))))
    :documentation "Initialisation step.")
   (excluded
    :initarg :excluded
    :initform nil
    :type boolean
    :documentation "If non-NIL then this package is excluded from all
layers.")))

(defvar *bootstrap-layer-layers* '()
  "A list of `cfgl-layer' objects.")

(defvar *bootstrap-layer-packages* '()
  "A list of `cfgl-package' objects.")

(defvar *bootstrap-layer-used-distant-packages* '()
  "A list of all packages that are effectively used.")

(defvar *bootstrap-layer-skipped-packages* nil
  "A list of packages that were skipped during the last update attempt.")

(defvar *bootstrap-layer-protected-packages* nil
  "A list of packages that will be protected from removal as orphans.")

(defvar *bootstrap-layer-error-count* nil
  "The number of errors that occured during the installation or
initialisation of a layer.")

(defvar *bootstrap-layer-paths* (make-hash-table :size 256)
  "A hash table of layer locations.  The key is a layer symbol and the value
is the path for the layer.")

(defvar *bootstrap-layer-categories* '()
  "A list of strings corresponding to category names.  A category is a
directory with a name starting with `+'.")

(defun bootstrap-layer:init ()
  "Initialise layers."
  (setq *bootstrap-layer-layers* (bootstrap-layer::declare-layers))
  (bootstrap-layer::configure-layers *bootstrap-layer-layers*)
  (setq *bootstrap-layer-packages* (bootstrap-layer::declare-packages
                                    *bootstrap-layer-layers*))
  (setq *bootstrap-layer-used-distant-packages*
        (bootstrap-layer::get-distant-used-packages
         *bootstrap-layer-packages*))
  (bootstrap-layer::load-packages *bootstrap-layer-packages*)

  ;; Clear out orphans.
  (when *bootstrap-delete-orphan-packages*
    (bootstrap-layer:delete-orphan-packages *bootstrap-layer-packages*)))

(defun bootstrap-layer:layer-used-p (name)
  (not (null (object-assoc name
                           :name *bootstrap-layer-packages*))))

(defun bootstrap-layer:make-layer (layer)
  "Return a `cfgl-layer' object based on LAYER."
  (let* ((name-sym (if (listp layer)
                       (car layer)
                     layer))
         (name-str (symbol-name name-sym))
         (base-dir (bootstrap-layer:get-layer-path name-sym))
         (disabled (when (listp layer)
                     (bootstrap:mplist-get layer :disabled-for)))
         (variables (when (listp layer)
                      (bootstrap:mplist-get layer :variables))))
    (if base-dir
        (let* ((dir (format "%s%s/" base-dir name-str)))
          (cfgl-layer name-str
                      :name name-sym
                      :dir dir
                      :disabled-for disabled
                      :variables variables))
      (progn
        (bootstrap-buffer:warning "Cannot find layer %S!" name-sym)
        nil))))

(defun bootstrap-layer::make-layers (symbols)
  "Make `cfgl-layer' objects from the passed layer symbols."
  (delq nil (mapcar 'bootstrap-layer/make-layer symbols)))

(defun bootstrap-layer:make-package (pkg &optional obj)
  "Return a `cfgl-package' object based on PKG.

If OBJ is non-NIL then copy PKG properties into OBJ, otherwise create a new
object.

Properties that can be copied are `:location', `:step', and `:excluded'."
  (let* ((name-sym (if (listp pkg)
                       (car pkg)
                     pkg))
         (name-str (symbol-name name-sym))
         (location (when (listp pkg)
                     (plist-get (cdr pkg) :location)))
         (step (when (listp pkg)
                 (plist-get (cdr pkg) :step)))
         (excluded (when (listp pkg)
                     (plist-get (cdr pkg) :excluded)))
         (obj (if obj
                  obj
                (cfgl-package name-str
                              :name name-sym))))
    (when location
      (oset obj :location location))
    (when step
      (oset obj :step step))
    (oset obj :excluded excluded)
    obj))

(defun bootstrap-layer:get-layer-path (layer)
  (ht-get *bootstrap-layer-paths* layer))

(defun bootstrap-layer::get-packages (layers)
  "Read the package lists of LAYERS and DOTFILE and return a list of packages."
  (let (result)
    (dolist (layer layers)
      (let* ((name (oref layer :name))
             (dir (oref layer :dir))
             (packages-file (concat dir "packages.el")))

        ;; Packages
        (when (file-exists-p packages-file)
          (eval `(defvar ,(intern (format "%S-packages" name)) nil))
          (unless (bootstrap-layer:layer-used-p name)
            ;; XXX
            ;;(bytecode:compile-and-load packages-file))
            (load packages-file))
          (dolist (pkg (symbol-value (intern (format "%S-packages" name))))
            (let* ((pkg-name (if (listp pkg)
                                 (car pkg)
                               pkg))
                   (init-func (intern (format "%S:init-%S"
                                              name
                                              pkg-name)))
                   (pre-init-func (intern (format "%S:pre-init-%S"
                                                  name
                                                  pkg-name)))
                   (post-init-func (intern (format "%S:post-init-%S"
                                                   name
                                                   pkg-name)))
                   (obj (object-assoc pkg-name :name result)))
              (if obj
                  (setq obj (bootstrap-layer:make-package pkg obj))
                (setq obj (bootstrap-layer:make-package pkg))
                (push obj result))
              (when (fboundp init-func)
                (when (oref obj :owner)
                  (bootstrap-buffer:warning
                   (format (concat "More than one init function found for "
                                   "package %S.  Previous owner was %S, "
                                   "replacing it with layer %S.")
                           pkg
                           (oref obj :owner)
                           name)))
                (oset obj :owner name))
              (when (fboundp pre-init-func)
                (push name (oref obj :pre-layers)))
              (when (fboundp post-init-func)
                (push name (oref obj :post-layers)))))
          (let ((xvar (intern (format "%S-excluded-packages" name))))
            (when (boundp xvar)
              (dolist (xpkg (symbol-value xvar))
                (let ((obj (object-assoc xpkg :name result)))
                  (unless obj
                    (setq obj (bootstrap-layer:make-package xpkg))
                    (push obj result))
                  (oset obj :excluded t))))))))
    result))

(defun bootstrap-layer::sort-packages (packages)
  "Return a sorted list of package objects."
  (sort packages (lambda (x y)
                   (string< (symbol-name (oref x :name))
                            (symbol-name (oref y :name))))))

(defun bootstrap-layer:filter-objects (objects ffunc)
  "Return a list of filtered objects."
  (reverse (reduce (lambda (acc x)
                     (if (funcall ffunc x)
                         (push x acc)
                       acc))
                   objects
                   :initial-value nil)))

(defun bootstrap-layer::get-distant-used-packages (packages)
  "Return the distant packages that are effectively used."
  (bootstrap-layer:filter-objects
   packages
   (lambda (x)
     (and (not (null (oref x :owner)))
          (not (memq (oref x :location) '(built-in local)))
          (not (oref x :excluded))))))

(defun bootstrap-layer::directory-type (path)
  "Return the type of directory pointed to by PATH.

Possible return values:

   layer    - the directory is a layer.
   category - the directory is a category.
   nil      - the directory is a regular directory."
  (when (file-directory-p path)
    (if (string-match "^+" (file-name-nondirectory
                            (directory-file-name
                             (concat +bootstrap-layer-directory+ path))))
        'category
      (let ((files (directory-files path)))
        (when (or (member "packages.el" files)
                  (member "config.el" files)
                  (member "keybindings.el" files)
                  (member "funcs.el" files))
          'layer)))))

(defun bootstrap-layer::get-category-from-path (dirpath)
  "Return a category symbol from the given DIRPATH.

The directory must start with a `+'.

Returns NIL if the directory is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name
                     (file-name-directory
                      (concat dirpath "/empty"))))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defun bootstrap-layer::discover-layers ()
  "Return a hash table where the key is the layer symbol and the value is its
  path."
  (let ((search-paths (append (list +bootstrap-layer-directory+)
                              (list +bootstrap-layer-private-directory+)))
        (discovered '())
        (result (make-hash-table :size 256)))
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub (directory-files current-path t nil 'nosort))
          (unless (or (string-equal ".." (substring sub -2))
                      (string-equal "." (substring sub -1))
                      (not (file-directory-p sub)))
            (let ((type (bootstrap-layer::directory-type sub)))
              (cond ((eq type 'category)
                     (let ((category (bootstrap-layer::get-category-from-path
                                      sub)))
                       (bootstrap-buffer:message "-> Discovered category: %S"
                                                 category)
                       (push category *bootstrap-layer-categories*)
                       (setq search-paths (cons sub search-paths))))
                    ((eq type 'layer)
                     (let ((layer-name (file-name-nondirectory sub))
                           (layer-dir (file-name-directory sub)))
                       (bootstrap-buffer:message "-> Discovered layer: %s"
                                                 layer-name)
                       (push (cons (intern layer-name) layer-dir) discovered)))
                    (t
                     (setq search-paths (cons sub search-paths)))))))))
    (mapc (lambda (l)
            (if (ht-contains? result (car l))
                (unless (string-equal (ht-get result (car l)) (cdr l))
                  (bootstrap-buffer:warning
                   (concat "Duplicated layer %s detected in directory \"%s\", "
                           "keeping only the layer in directory \"%s\".")
                   (car l)
                   (cdr l)
                   (ht-get result (car l))))
              (puthash (car l) (cdr l) result)))
          discovered)
    result))

(defun bootstrap-layer::declare-layers ()
  (setq *bootstrap-layer-layers* nil)
  (setq *bootstrap-layer-paths* (bootstrap-layer::discover-layers))
  (dolist (layer (ht-keys *bootstrap-layer-paths*))
    (let ((layer-name (if (listp layer)
                          (car layer)
                        layer)))
      (if (ht-contains? *bootstrap-layer-paths* layer-name)
          (unless (string-match-p "+distribution"
                                  (ht-get *bootstrap-layer-paths* layer-name))
            (push (bootstrap-layer:make-layer layer)
                  *bootstrap-layer-layers*))
        (bootstrap-buffer:warning "Unknown layer %s." layer-name))))
  (setq *bootstrap-layer-layers* (reverse *bootstrap-layer-layers*))
  (push (bootstrap-layer:make-layer +bootstrap-distribution-layer+)
        *bootstrap-layer-layers*))

(defun bootstrap-layer::load-layer-files (layer files)
  (dolist (file files)
    (let ((file (concat (oref layer :dir) file)))
      (if (file-exists-p file)
          (load file)))))

(defun bootstrap-layer::load-layers-files (layers files)
  (dolist (layer layers)
    (bootstrap-layer::load-layer-files layer files)))

(defun bootstrap-layer::set-layer-variables (layer)
  (let ((variables (oref layer :variables)))
    (while variables
      (let ((var (pop variables)))
        (if (consp variables)
            (condition-case err
                (set-default var (eval (pop variables)))
              ('error
               (bootstrap-layer::set-error)
               (bootstrap-buffer:append
                (format (concat "An error occured whilst setting layer "
                                "variable %s (error: %s).")
                        var
                        err))))
          (bootstrap-buffer:warning "Missing value for variable %s!" var))))))

(defun bootstrap-layer:package-used-p (name)
  (let ((obj (object-assoc name
                           :name *bootstrap-layer-packages*)))
    (when obj
      (oref obj :owner))))

(defun bootstrap-layer::configure-layer (layer)
  (bootstrap-layer::set-layer-variables layer)
  (bootstrap-layer::load-layer-files layer '("funcs.el"
                                             "config.el"
                                             "keybindings.el")))

(defun bootstrap-layer::configure-layers (layers)
  (let ((warning-minimum-level :error))
    (dolist (l layers)
      (bootstrap-layer::configure-layer l))))

(defun bootstrap-layer::declare-packages (layers)
  (let ((layers2 layers)
        (warning-minimum-level :error))
    (bootstrap-layer::load-layers-files layers2 '("packages.el"))
    (bootstrap-layer::sort-packages
     (bootstrap-layer::get-packages layers2))))

(defun bootstrap-layer::configured-packages-count ()
  (length *bootstrap-layer-packages*))

(defun bootstrap-layer::load-packages (packages)
  (setq *bootstrap-loading-dots-chunk-threshold*
        (/ (bootstrap-layer::configured-packages-count)
           +bootstrap-loading-dots-count+))
  (bootstrap-layer::install-packages packages)
  (bootstrap-layer::configure-packages packages))

(defun bootstrap-layer::configure-packages (packages)
  (bootstrap-layer::configure-packages-2
   (bootstrap-layer:filter-objects
    packages
    (lambda (x)
      (eq 'pre (oref x :step)))))
  (bootstrap-layer::configure-packages-2
   (bootstrap-layer:filter-objects
    packages
    (lambda (x)
      (null (oref x :step)))))
  (bootstrap-layer::configure-packages-2
   (bootstrap-layer:filter-objects
    packages
    (lambda (x)
      (eq 'post (oref x :step))))))

(defun bootstrap-layer::configure-packages-2 (packages)
  (dolist (pkg packages)
    (bootstrap-buffer:loading-animation)
    (let ((pkg-name (oref pkg :name)))
      (cond ((oref pkg :excluded)
             (bootstrap-buffer:message
              (format "%S ignored since it has been excluded." pkg-name)))
            ((null (oref pkg :owner))
             (bootstrap-buffer:message
              (format "%S ignored since it has no owner layer." pkg-name)))
            (t
             (when (eq 'local (oref pkg :location))
               (if (eq 'dotfile (oref pkg :owner))
                   (push (file-name-as-directory
                          (concat +bootstrap-layer-private-directory+
                                  "local/"
                                  (symbol-name (oref pkg :name))))
                         load-path)
                 (let* ((owner (object-assoc (oref pkg :owner)
                                             :name *bootstrap-layer-layers*))
                        (dir (when owner
                               (oref owner :dir))))
                   (bootstrap-buffer:message
                    (format "  -> Loading from %slocal/%S/"
                            dir
                            pkg-name))
                   (push (format "%slocal/%S/" dir pkg-name) load-path))))
             (cond ((eq 'dotfile (oref pkg :owner))
                    (bootstrap-layer::activate-package pkg-name)
                    (bootstrap-buffer:message
                     (format "%S is configured in the dotfile." pkg-name)))
                   (t
                    (bootstrap-layer::activate-package pkg-name)
                    (bootstrap-layer::configure-package pkg))))))))

(defun bootstrap-layer::activate-package (pkg)
  (if (version< emacs-version "24.3.50")
      (package-activate pkg '(0 0 0 0))
    (package-activate pkg)))

(defun bootstrap-layer::configure-package (pkg)
  (let* ((pkg-name (oref pkg :name))
         (owner (oref pkg :owner))
         (owner-layer (object-assoc owner :name *bootstrap-layer-layers*))
         (disabled-for-layers (oref owner-layer :disabled-for)))
    (bootstrap-buffer:message (format "Configuring %S..." pkg-name))
    (mapc (lambda (layer)
            (if (memq layer disabled-for-layers)
                (bootstrap-buffer:message
                 (format " -> ignored pre-init (%S)..." layer))
              (bootstrap-buffer:message
               (format " -> pre-init (%S)..." layer))
              (condition-case err
                  (funcall (intern (format "%S:pre-init-%S" layer pkg-name)))
                ('error
                 (bootstrap-layer::set-error)
                 (bootstrap-buffer:append
                  (format
                   (concat "An error occured whilst pre-configuring %S "
                           "in payer %S (error: %s)\n")
                   pkg-name
                   layer
                   err))))))
          (oref pkg :pre-layers))
    (bootstrap-buffer:message (format " -> init (%S:init-%S)..."
                                      owner
                                      pkg-name))
    (funcall (intern (format "%S:init-%S" owner pkg-name)))
    (mapc (lambda (layer)
            (if (memq layer disabled-for-layers)
                (bootstrap-buffer:message
                 (format " -> ignored post-init (%S)..." layer))
              (bootstrap-buffer:message
               (format " -> post-init (%S)..." layer))
              (condition-case err
                  (funcall (intern (format "%S:post-init-%S" layer pkg-name)))
                ('error
                 (bootstrap-buffer:append
                  (format
                   (concat "An error occured whilst post-configuring %S "
                           "in layer %S (error: %s)\n")
                   pkg-name
                   layer
                   err))))))
          (oref pkg :post-layers))))

(defun bootstrap-layer::install-packages (packages)
  (interactive)
  (let* ((noinst-pkg-names
          (bootstrap-layer::get-uninstalled-packages
           (mapcar 'car
                   (object-assoc-list
                    :name *bootstrap-layer-used-distant-packages*))))
         (noinst-count (length noinst-pkg-names))
         (installed-count 0))
    (when noinst-pkg-names
      (bootstrap-buffer:append
       (format "Found %s new package(s) to install...\n" noinst-count))
      (bootstrap-buffer:append
       "--> Fetching new package repository indexes...\n")
      (bootstrap:redisplay)
      (package-refresh-contents)
      (setq installed-count 0)
      (dolist (pkg-name noinst-pkg-names)
        (incf installed-count)
        (let* ((pkg (object-assoc pkg-name :name *bootstrap-layer-packages*))
               (layer (when pkg
                        (oref pkg :owner)))
               (location (when pkg
                           (oref pkg :location))))
          (bootstrap-buffer:replace-last-line
           (format "--> Installing %s%s... [%s/%s]"
                   (if layer
                       (format "%S:" layer)
                     "dependency ")
                   pkg-name
                   installed-count
                   noinst-count)
           t)
          (unless (package-installed-p pkg-name)
            (condition-case err
                (cond ((or (null pkg)
                           (eq 'elpa location))
                       (bootstrap-layer::install-from-elpa pkg-name))
                      ((and (listp location)
                            (eq 'recipe (car location)))
                       (bootstrap-layer::install-from-recipe pkg))
                      (t
                       (bootstrap-buffer:warning "Cannot install package %S."
                                                 pkg-name)))
              ('error
               (bootstrap-layer::set-error)
               (bootstrap-buffer:append
                (format (concat "An error occured whilst installing %s "
                                "(error: %s)\n")
                        pkg-name
                        err))))))
        (bootstrap:redisplay))
      (bootstrap-buffer:append "\n"))))

(defun bootstrap-layer::install-from-elpa (pkg-name)
  (if (not (assq pkg-name package-archive-contents))
      (bootstrap-buffer:append
       (format (concat "\nPackage %s is unavailable. "
                       "Is the package name misspelled?\n")
               pkg-name))
    (dolist (dep (bootstrap-layer::get-package-deps-from-archive pkg-name))
      (bootstrap-layer::activate-package (car dep)))
    (package-install pkg-name)))

(defun bootstrap-layer::install-from-recipe (pkg)
  (let* ((pkg-name (oref pkg :name))
         (layer (oref pkg :owner))
         (recipe (cons pkg-name (cdr (oref pkg :location)))))
    (if recipe
        (quelpa recipe)
      (bootstrap-buffer:warning
       (concat "Cannot find any recipe for package %S!  Be sure to "
               "add a recipe for it in alist %S.")
       pkg-name
       recipes-var))))

(defun bootstrap-layer::filter-packages-with-deps
    (pkg-names filter &optional use-archive)
  (when pkg-names
    (let (result)
      (dolist (pkg-name pkg-names)
        (let* ((deps
                (if use-archive
                    (bootstrap-layer::get-package-deps-from-archive pkg-name)
                  (bootstrap-layer::get-package-deps-from-alist pkg-name)))
               (install-deps (when deps
                               (bootstrap-layer::filter-packages-with-deps
                                (mapcar 'car deps)
                                filter))))
          (when install-deps
            (setq result (append install-deps result))))
        (when (funcall filter pkg-name)
          (add-to-list 'result pkg-name t)))
      (delete-dups result))))

(defun bootstrap-layer::get-uninstalled-packages (pkg-name)
  (bootstrap-layer::filter-packages-with-deps
   pkg-name
   (lambda (x)
     (not (package-installed-p x)))))

(defun bootstrap-layer::package-has-recipe-p (pkg-name)
  (when (object-assoc pkg-name :name *bootstrap-layer-packages*)
    (let* ((pkg (object-assoc pkg-name :name *bootstrap-layer-packages*))
           (location (oref pkg :location)))
      (and (listp location)
           (eq 'recipe (car location))))))

(defun bootstrap-layer::get-package-recipe (pkg-name)
  (let ((pkg (object-assoc pkg-name :name *bootstrap-layer-packages*)))
    (when pkg
      (let ((location (oref pkg :location)))
        (when (and (listp location)
                   (eq 'recipe (car location)))
          location)))))

(defun bootstrap-layer::new-version-available-p (pkg-name)
  (let ((recipe (bootstrap-layer::get-package-recipe pkg-name))
        (cur-version (bootstrap-layer::get-package-version-string pkg-name))
        new-version)
    (when cur-version
      (setq new-version
            (if recipe
                (quelpa-checkout recipe (expand-file-name
                                         (symbol-name pkg-name)
                                         quelpa-build-dir))
              (bootstrap-layer::get-lastest-package-version-string pkg-name)))
      (if new-version
          (version< cur-version new-version)
        (cl-pushnew pkg-name *bootstrap-layer-skipped-packages*
                    :test #'eq)
        nil))))

(defun bootstrap-layer::get-packages-to-update (pkg-names)
  (bootstrap-layer::filter-packages-with-deps
   pkg-names
   'bootstrap-layer::new-version-available-p
   'use-archive))
                

(defun bootstrap-layer::get-implicit-packages (packages)
  (let (imp-pkgs)
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (unless (object-assoc pkg-sym
                              :name packages)
          (add-to-list 'imp-pkgs pkg-sym))))
    imp-pkgs))

(defun bootstrap-layer::get-package-deps-from-alist (pkg-name)
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (cond ((version< emacs-version "24.3.50")
             (aref (cdr pkg-desc) 1))
            (t
             (package-desc-reqs (cadr pkg-desc)))))))

(defun bootstrap-layer::get-package-deps-from-archive (pkg-name)
  (let* ((pkg-arch (assq pkg-name package-archive-contents))
         (reqs (when pkg-arch
                 (if (version< emacs-version "24.3.50")
                     (aref (cdr pkg-arch) 1)
                   (package-desc-reqs (cadr pkg-arch))))))
    (dolist (req reqs)
      (let* ((pkg-name2 (car req))
             (reqs2 (bootstrap-layer::get-package-deps-from-archive pkg-name2)))
        (when reqs2
          (setq reqs (append reqs2 reqs)))))
    reqs))

(defun bootstrap-layer::get-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (cond ((version< emacs-version "24.3.50")
             (package-version-join (aref (cdr pkg-desc) 0)))
            (t
             (package-version-join (package-desc-version (cadr pkg-desc))))))))

(defun bootstrap-layer::get-package-version (pkg-name)
  "Return the version list for package with name PKG-NAME."
  (let ((version-string (bootstrap-layer::get-package-version-string pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun bootstrap-layer::get-latest-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-arch (assq pkg-name package-archive-contents)))
    (when pkg-arch
      (cond ((version< emacs-version "24.3.50")
             (package-version-join (aref (cdr pkg-arch) 0)))
            (t
             (package-version-join (package-desc-version (cadr pkg-arch))))))))

(defun bootstrap-layer::get-latest-package-version (pkg-name)
  "Return the versio list for package with name PKG-NAME."
  (let ((version-string
         (bootstrap-layer::get-latest-package-version-string pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun bootstrap-layer::get-packages-dependencies ()
  (let ((result (make-hash-table :size 512)))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (bootstrap-layer::get-package-deps-from-alist pkg-sym)))
        (dolist (dep deps)
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value
                         (add-to-list 'value pkg-sym)
                       (list pkg-sym))
                     result)))))
    result))

(defun bootstrap-layer::is-package-orphan (pkg-name dist-pkgs deps)
  (unless (or (object-assoc pkg-name :name dist-pkgs)
              (memq pkg-name *bootstrap-layer-protected-packages*))
    (if (ht-contains? deps pkg-name)
        (let ((parents (ht-get deps pkg-name)))
          (reduce (lambda (x y)
                    (and x y))
                  (mapcar (lambda (p)
                            (bootstrap-layer::is-package-orphan p dist-pkgs deps))
                          parents)
                  :initial-value t))
      (not (object-assoc pkg-name :name dist-pkgs)))))

(defun bootstrap-layer::get-orphan-packages (dist-pkgs implicit-pkgs deps)
  (let (result)
    (dolist (imp-pkg implicit-pkgs)
      (when (bootstrap-layer::is-package-orphan imp-pkg dist-pkgs deps)
        (add-to-list 'result imp-pkg)))))

(defun bootstrap-layer:delete-orphan-packages (packages)
  (interactive)
  (let* ((dependencies (bootstrap-layer::get-packages-dependencies))
         (implicit-packages (bootstrap-layer::get-implicit-packages
                             *bootstrap-layer-used-distant-packages*))
         (orphans (bootstrap-layer::get-orphan-packages
                   *bootstrap-layer-used-distant-packages*
                   implicit-packages
                   dependencies))
         (deleted-count 0)
         (orphans-count (length orphans)))
    (if orphans
        (progn
          (bootstrap-buffer:append
           (format "Found %s orphan package(s) to delete...\n"
                   orphans-count))
          (setq deleted-count 0)
          (dolist (orphan orphans)
            (incf deleted-count)
            (bootstrap-buffer:replace-last-line
             (format "--> deleting %s... [%s/%s]"
                     oprhan
                     deleted-count
                     orphans-count)
             t)
            (bootstrap-layer::package-delete orphan)
            (bootstrap:redisplay))
          (bootstrap-buffer:append "\n"))
      (bootstrap-buffer:message "No orphan packages to delete."))))

(defun bootstrap-layer::set-error ()
  (if *bootstrap-layer-error-count*
      (incf *bootstrap-layer-error-count*)
    (face-remap-add-relative 'mode-line '((:background "red") mode-line))
    (setq *bootstrap-layer-error-count* 1)))

(defun bootstrap-layer::package-delete (pkg-name)
  (cond ((version< emacs-version "24.3.50")
         (let ((v (bootstrap-layer::get-package-version-string pkg-name)))
           (when v
             (package-delete (symbol-name pkg-name) v)))
         ((version<= "25.0.50" emacs-version)
          (let ((p (cadr (assq pkg-name package-alist))))
            (when p
              (package-delete p)))))))

(provide 'bootstrap-layers)
