

(defun bootstrap::bytecode-filename (file)
  "Returns the name of the bytecode filename in the cache directory."
  (expand-file-name
   (concat file ".elc")
   +bootstrap-bytecode-cache-directory+))

(defun bootstrap::bytecode-is-stale-p (file)
  "Returns T if the given FILE has state bytecode; otherwise NIL is returned.

In order to determine whether bytecode is stale, we compare the Emacs Lisp file
creation timestamp with that of the bytecode file.  If the Emacs Lisp file is
newer, then the bytecode is stale."
  (let* ((sourcefile (locate-library file))
         (bytefile (bootstrap::bytecode-filename file)))
    (and (file-exists-p bytefile)
         (file-newer-than-file-p sourcefile bytefile))))

(defun bootstrap::byte-compile-to (input output)
  "Like `byte-compile-file', but puts the result in OUTPUT.  Returns the result
of `byte-compile-file'."
  (require 'bytecomp)
  (let ((saved-dest-func (symbol-function 'byte-compile-dest-file))
        (byte-compile-verbose nil)
        (font-lock-verbose nil)
        (byte-compile-warnings '()))
    (unwind-protect
        (progn
          (fset 'byte-compile-dest-file
                '(lambda (src)
                   (if (equal src input)
                       output
                     (funcall saved-dest-func src))))
          (byte-compile-file input))
      (fset 'byte-compile-dest-file saved-dest-func))))

(defun bootstrap:compile-load (file)
  "Load in the given FILE's bytecode.  If the given FILE has no bytecode, then
invoke the  compier and load the resulting bytecode."
  (let ((cache-file (bootstrap::bytecode-filename file)))
    (when (or (bootstrap::bytecode-is-stale-p file)
              (not (file-exists-p cache-file)))
      (let ((el-file (locate-library file)))
        (if el-file
            (bootstrap::byte-compile-to el-file cache-file)
          (error "Could not locate %s for library %s."
                 el-file
                 file))))
    (load cache-file)))

(provide 'bootstrap-bytecompiler)
