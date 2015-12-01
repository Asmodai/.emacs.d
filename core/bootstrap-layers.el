
(require 'cl-lib)
(require 'eieio)
(require 'package)
(require 'warnings)
(require 'ht)
(require 'bootstrap-loader)
(require 'bootstrap-funcs)
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

(defconst bootstrap-layer-template-directory
  (expand-file-name (concat bootstrap-core-directory "templates/"))
  "Bootstrap layer templates directory.")

(defconst bootstrap-layer-directory
  (expand-file-name (concat bootstrap-directory "layers/"))
  "Bootstrap layers base directory.")

(defconst bootstrap-layer-private-directory
  (expand-file-name (concat bootstrap-directory "private/"))
  "Bootstrap private layers base directory.")

(defconst bootstrap-layer-rollback-directory
  (expand-file-name (concat bootstrap-cache-directory "rollback/"))
  "Bootstrap rollback directory.")

(defconst bootstrap-layer-rollback-info "rollback-info"
  "Bootstrap rollback information file.")



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

(defclass clgl-package ()
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
                       (member x '(nil pre))))
    :documentation "Initialisation step.")
   (excluded
    :initarg :excluded
    :initform nil
    :type boolean
    :documentation "If non-NIL then this package is excluded from all
layers.")))

(defvar bootstrap-layer--layers '()
  "A list of `cfgl-layer' objects.")

(defvar bootstrap-layer--packages '()
  "A list of `cfgl-package' objects.")

(defvar bootstrap-layer--used-distant-packages '()
  "A list of all packages that are effectively used.")

(defvar bootstrap-layer--skipped-packages nil
  "A list of packages that were skipped during the last update attempt.")

(defvar bootstrap-layer--protected-packages nil
  "A list of packages that will be protected from removal as orphans.")

(defvar bootstrap-layer-error-count nil
  "The number of errors that occured during the installation or
initialisation of a layer.")

(defvar bootstrap-layer-paths (make-hash-table :size 256)
  "A hash table of layer locations.  The key is a layer symbol and the value
is the path for the layer.")

(defvar bootstrap-layer-categories '()
  "A list of strings corresponding to category names.  A category is a
directory with a name starting with `+'.")

(defun bootstrap-layer/sync ()
  "Synchronise declared layers with bootstrap."
  (bootstrap-loader|call-func bootstrap/layers
                              "Calling bootstrap/layers...")

  (when (bootstrap-buffer//choose-banner)
    (bootstrap-buffer//inject-version))

  ;; Layers.
  (setq bootstrap-layer--layers (bootstrap-layer//declare-layers))
  (bootstrap-layer//configure-layers bootstrap-layer--layers)

  ;; Packages.
  (setq bootstrap-layer--packages (boostrap-layer//declare-packages
                                   bootstrap-layer--layers))
  (setq bootstrap-layer--used-distant-packages
        (bootstrap-layer//get-distant-used-packages bootstrap-layer--packages))
  (bootstrap-layer//load-packages bootstrap-layer--packages)

  ;; Clear out orphans.
  (when bootstrap-delete-oprhan-packages
    (bootstrap-layer/delete-orphan-packages bootstrap-layer--packages)))

(defun bootstrap-layer/create-layer ()
  "Ask the user for a configuration layer name and the layer directory to
use.  Create a layer with this name in the selected layer directory."
  (interactive)
  (let* ((current-layer-paths (mapcar (lambda (dir)
                                        (expand-file-name dir))
                                      (cl-pushnew
                                       bootstrap-layer-private-directory
                                       bootstrap-layer-path)))
         (other-choice "Another directory...")
         (helm-lp-source
          `((name       . "Layer paths")
            (candidates . ,(append current-layer-paths
                                   (list other-choice)))
            (action     . (lambda (x)
                            x))))
         (layer-path-sel (helm :sources helm-lp-source
                               :prompt "Layer path: "))
         (layer-path (cond ((string-equal layer-path-sel other-choice)
                            (read-directory-name (concat "Other configuration "
                                                         "layer path: ")
                                                 "~/"))
                           ((member layer-path-sel current-layer-paths)
                            layer-path-sel)
                           (t
                            (error "Please select an option from the list."))))
         (name (read-from-minibuffer "Configuration layer name: "))
         (layer-dir (concat layer-path "/" name)))
    (cond ((string-equal "" name)
           (message "Cannot create a layer without a name."))
          ((file-exists-p layer-dir)
           (message (concat "Cannot create layer \"%s\", "
                            "this layer already exists.") name))
          (t
           (make-directory layer-dir t)
           (bootstrap-layer//copy-template name "extensions" layer-dir)
           (bootstrap-layer//copy-template name "packages" layer-dir)
           (message "Layer \"%s\" successfully created." name)))))

(defun bootstrap-layer/make-layer (layer)
  "Return a `cfgl-layer' object based on LAYER."
  (let* ((name-sym (if (listp layer)
                       (car layer)
                     layer))
         (name-str (symbol-name name-sym))
         (base-dir (bootstrap-layer/get-layer-path name-sym))
         (disabled (when (listp layer)
                     (bootstrap/mplist-get layer :disabled-for)))
         (variables (when (listp layer)
                      (bootstrap/mplist-get layer :variables))))
    (if base-dir
        (let* ((dir (format "%s%s/" base-dir name-str)))
          (cfgl-layer name-str
                      :name name-sym
                      :dir dir
                      :disabled-for disabled
                      :variables variables))
      (progn
        (bootstrap-buffer/warning "Cannot find layer %S!" name-sym)
        nil))))

(defun bootstrap-layer//make-layers (symbols)
  "Make `cfgl-layer' objects from the passed layer symbols."
  (delq nil (mapcar 'bootstrap-layer/make-layer symbols)))

(defun bootstrap-layer/make-package (pkg &optional obj)
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

(defun boostrap-layer/get-packages (layers)
  "Read the package lists of LAYERS and DOTFILE and return a list of packages."
  (let (result)
    (dolist (layer layers)
      (let* ((name (oref layer :name))
             (dir (oref layer :dir))
             (packages-file (concat dir "packages.el")))

        ;; Packages
        (when (file-exists-p packages-file)
          (eval `(defvar ,(intern (format "%S-packages" name)) nil))
          (unless (bootstrap-layer/layer-used-p name)
            (bytecode/compile-and-load packages-file))
          (dolist (pkg (symbol-value (intern (format "%S-packages" name))))
            (let* ((pkg-name (if (listp pkg)
                                 (car pkg)
                               pkg))
                   (init-func (intern (format "%S/init-%S" name pkg)))
                   (pre-init-func (intern (format "%S/pre-init-%S" name pkg)))
                   (post-init-func (intern (format "%S/post-init-%S" name pkg)))
                   (obj (object-assoc pkg-name :name result)))
              (if obj
                  (setq obj (bootstrap-layer/make-package pkg obj))
                (setq obj (bootstrap-layer/make-pckage pkg))
                (push obj result))
              (when (fboundp init-func)
                (when (oref obj :owner)
                  (bootstrap-buffer/warning
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
                    (setq obj (bootstrap-layer/make-package xpkg))
                    (push obj result))
                  (oset obj :excluded t))))))))
    result))

(defun bootstrap-layer//sort-packages (packages)
  "Return a sorted list of package objects."
  (sort packages (lambda (x y)
                   (string< (symbol-name (oref x :name))
                            (symbol-name (oref y :name))))))

(defun bootstrap-layer/filter-objects (objects ffunc)
  "Return a list of filtered objects."
  (reverse (reduce (lambda (acc x)
                     (if (funcall ffunc x)
                         (push x acc)
                       acc))
                   objects
                   :initial-value nil)))

(defun bootstrap-layer//get-distant-used-packages (packages)
  "Return the distant packages that are effectively used."
  (bootstrap-layer/filter-objects
   packages
   (lambda (x)
     (and (not (null (oref x :owner)))
          (not (memq (oref x :location) 'built-in local))
          (not (oref x :excluded))))))

(defun bootstrap-layer//get-private-layer-dir (name)
  "Return an absolute path to the private configuration layer string NAME."
  (file-name-as-directory
   (concat bootstrap-layer-private-directory name)))

(defun bootstrap-layer//copy-template (name template &optional layer-dir)
  "Copy and replace special values of TEMPLATE to layer string NAME.

If LAYER-DIR is NIL, then the private directory is used."
  (let ((src (concat bootstrap-layer-template-directory
                     (format "%s.template" template)))
        (dst (if layer-dir
                 (concat layer-dir "/" (format "%s.el" template))
               (concat bootstrap-layer-private-directory
                       (format "%s.el" template)))))
    (copy-file src dst)
    (find-file dst)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward "%LAYERNAME%" nil t)
          (replace-match name t))))
    (save-buffer)))

(defun bootstrap-layer//directory-type (path)
  "Return the type of directory pointed to by PATH.

Possible return values:

   layer    - the directory is a layer.
   category - the directory is a category.
   nil      - the directory is a regular directory."
  (when (file-directory-p path)
    (if (string-match "^+" (file-name-nondirectory
                            (directory-file-name
                             (concat bootstrap-layer-directory path))))
        'category
      (let ((files (directory-files path)))
        (when (or (member "packages.el" files)
                  (member "config.el" files)
                  (member "keybindings.el" files)
                  (member "funcs.el" files))
          'layer)))))

(defun bootstrap-layer//get-category-from-path (dirpath)
  "Return a category symbol from the given DIRPATH.

The directory must start with a `+'.

Returns NIL if the directory is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-directory
                    (directory-file-name
                     (concat bootstrap-layer-directory dirpath)))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defun bootstrap-layer//discover-layers ()
  "Return a hash table where the key is the layer symbol and the value is its
  path."
  (let ((search-paths (append (list bootstrap-layer-directory)
                              bootstrap-layer-path
                              (list bootstrap-layer-private-directory)))
        (discovered '())
        (result (make-hash-table :size 256)))
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub (directoryu-files current-path t nil 'nosort))
          (unless (or (string-equal ".." (substring sub -2))
                      (string-equal "." (substring sub -1))
                      (not (file-directory-p sub)))
            (let ((type (bootstrap-layer//directory-type sub)))
              (cond ((eq type 'category)
                     (let ((category (bootstrap-layer//get-category-from-path
                                      sub)))
                       (bootstrap-buffer/message "-> Discovered category: %S"
                                                 category)
                       (push category bootstrap-layers-categories)
                       (setq search-paths (cons sub search-paths))))
                    ((eq type 'layer)
                     (let ((layer-name (file-name-nondirectory sub))
                           (layer-dir (file-name-directory sub)))
                       (bootstrap-buffer/message "-> Discovered layer: %s"
                                                 layer-name)
                       (push (cons (intern layer-name) layer-dir) discovered)))
                    (t
                     (setq search-paths (cons sub search-paths)))))))))
    (mapc (lambda (l)
            (if (ht-contains? result (car l))
                (unless (string-equal (ht-get result (car l)) (cdr l))
                  (bootstrap-buffer/warning
                   (concat "Duplicated layer %s detected in directory \"%s\", "
                           "keeping only the layer in directory \"%s\".")
                   (car l)
                   (cdr l)
                   (ht-get result (car l))))
              (puthash (car l) (cdr l) result)))
          discovered)
    result))

