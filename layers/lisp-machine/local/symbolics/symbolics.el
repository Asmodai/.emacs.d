
(eval-when-compile
  (require 'cl)
  (require 'cl-lib)
  (require 'help+)
  (require 'help-fns+))

(defgroup symbolics nil
  "Symbolics customisations."
  :group 'convenience
  :prefix 'symbolics-)

(defcustom *symbolics-debug* nil
  "Non-NIL if debugging is required.")

(defvar *symbolics-map* (make-sparse-keymap)
  "Symbolics key map.")

(defun symbolics-display-keyboard-mapping ()
  (interactive)
  (with-help-window (help-buffer)
    (princ (format (concat "Symbolics keyboard mapping:\n\n"
                           "  Key   \tMapping\n"
                           "  ------\t-------------------------\n")))
    (mapcar (lambda (x)
              (princ (format "  %6s\t%s\n"
                             (car x)
                             (cdr x))))
            (reverse (cdr *symbolics-map*)))
    nil))

(defun symbolics-display-function-bindings ()
  (interactive)
  (describe-bindings [function]))

(defun symbolics-describe-function-keymap ()
  (interactive)
  (describe-keymap *symbolics-function-map*))

(defun symbolics-display-select-bindings ()
  (interactive)
  (describe-bindings [select]))

(defun symbolics-describe-select-keymap ()
  (interactive)
  (describe-keymap *symbolics-select-map*))

(defmacro symbolics-define-function-key (key fun)
  `(define-key *symbolics-function-map* ,key ,fun))

(defmacro symbolics-define-select-key (key fun)
  `(define-key *symbolics-select-map* ,key ,fun))

;; Define a new symbol key.
(defmacro define-symbol-key (key symbol charname)
  "Define a key sequence that results with KEY, when pressed with the
`symbol' key, prints the given unicode SYMBOL to the buffer."
  (declare (indent 1))
  (let* ((docstr (concat "Inserts a(n) " charname " at the current point."))
         (str (if (symbolp symbol)
                  (symbol-name symbol)
                symbol))
         (mname (intern (concat "ucs-insert-"
                                (replace-regexp-in-string
                                 "\\([[:space:]\n]\\)"
                                 "-"
                                 (downcase charname))))))
    `(progn
       (defun ,mname (&rest ignore)
         ,docstr
         (interactive "P")
         (ucs-insert ,symbol))
       (define-key *symbolics-symbol-map* ,key ',mname))))

(defun symbolics-display-symbol-bindings ()
  (interactive)
  (describe-bindings [symbol]))

(defun symbolics-describe-symbol-keymap ()
  (interactive)
  (describe-keymap *symbolics-symbol-map*))

(defun symbolics::install-prefix-maps ()
  (define-prefix-command '*symbolics-hyper-map*)
  (define-prefix-command '*symbolics-function-map*)
  (define-prefix-command '*symbolics-select-map*)
  (define-prefix-command '*symbolics-square-map*)
  (define-prefix-command '*symbolics-circle-map*)
  (define-prefix-command '*symbolics-triangle-map*)
  (define-prefix-command '*symbolics-symbol-map*))

(defun symbolics::install-fkey-mapping ()
  (define-key *symbolics-map* (kbd "<f1>")    [select])   ; meta
  (define-key *symbolics-map* (kbd "<f2>")    [function]) ; meta
  (define-key *symbolics-map* (kbd "<f3>")    [symbol])   ; meta
  (define-key *symbolics-map* (kbd "<f4>")    [hyper])    ; meta
  (define-key *symbolics-map* (kbd "<f5>")    [square])   ; meta
  (define-key *symbolics-map* (kbd "<f6>")    [circle])   ; meta
  (define-key *symbolics-map* (kbd "<f7>")    [triangle]) ; meta
  (define-key *symbolics-map* (kbd "<f8>")    [cut])
  (define-key *symbolics-map* (kbd "<f9>")    [copy])
  (define-key *symbolics-map* (kbd "<f10>")   [paste])
  (define-key *symbolics-map* (kbd "<f11>")   [help])
  (define-key *symbolics-map* (kbd "<f12>")   [complete])
  
  (define-key *symbolics-map* (kbd "S-<f1>")  [abort])
  (define-key *symbolics-map* (kbd "S-<f2>")  [undo])
  (define-key *symbolics-map* (kbd "S-<f3>")  [redo])
  ;;(define-key *symbolics-map* (kbd "S-<f4>")  [])
  ;;(define-key *symbolics-map* (kbd "S-<f5>")  [])
  ;;(define-key *symbolics-map* (kbd "S-<f6>")  [])
  (define-key *symbolics-map* (kbd "S-<f7>")  [find])
  (define-key *symbolics-map* (kbd "S-<f8>")  [insert])
  (define-key *symbolics-map* (kbd "S-<f9>")  [home])
  (define-key *symbolics-map* (kbd "S-<f10>") [end])
  (define-key *symbolics-map* (kbd "S-<f11>") [symbol-help])
  (define-key *symbolics-map* (kbd "S-<f12>") [clear-input]))

(defun symbolics::install-keys ()
  (define-key function-key-map [super] 'event-apply-super-modifier)
  (define-key function-key-map [hyper] 'event-apply-hyper-modifier)
  (global-set-key [copy]        'kill-ring-save)
  (global-set-key [cut]         'kill-ring)
  (global-set-key [paste]       'yank)
  (global-set-key [abort]       'keyboard-quit)
  (global-set-key [clear-input] 'backward-kill-sentence))

(defun symbolics::install-meta-maps ()
  (global-set-key [hyper]    *symbolics-hyper-map*)
  (global-set-key [function] *symbolics-function-map*)
  (global-set-key [select]   *symbolics-select-map*)
  (global-set-key [square]   *symbolics-square-map*)
  (global-set-key [circle]   *symbolics-circle-map*)
  (global-set-key [triangle] *symbolics-triangle-map*)
  (global-set-key [symbol]   *symbolics-symbol-map*))

(defun symbolics::install-function-map ()
  (symbolics-define-function-key "e"    'eval-defun)
  (symbolics-define-function-key "r"    'redraw-display)
  (symbolics-define-function-key "="    'symbolics-display-function-bindings)
  (symbolics-define-function-key [help] 'symbolics-describe-function-keymap))

(defun symbolics::install-select-map ()
  (symbolics-define-select-key "l"    'ielm)
  (symbolics-define-select-key "s"    'eshell)
  (symbolics-define-select-key "="    'symbolics-display-select-bindings)
  (symbolics-define-select-key [help] 'symbolics-describe-select-keymap))


(defun symbolics::install-symbol-map ()
  (global-set-key [symbol-help] 'symbolics-display-symbol-bindings)
  (define-key *symbolics-symbol-map* [help] 'symbolics-describe-symbol-keymap)
  (define-symbol-key "'" #x22C5 "dot operator")
  (define-symbol-key "A" #x03B1 "Greek small letter alpha")
  (define-symbol-key "q" #x2227 "logical AND")
  (define-symbol-key "E" #x03B5 "Greek small letter epsilon")
  (define-symbol-key "L" #x03BB "Greek small letter lambda")
  (define-symbol-key "D" #x03B4 "Greek small letter delta")
  (define-symbol-key ":" #x00B1 "plus-minus sign")
  (define-symbol-key "i" #x221E "infinity")
  (define-symbol-key "t" #x2282 "subset of")
  (define-symbol-key "e" #x2229 "intersection")
  (define-symbol-key "u" #x2200 "for all")
  (define-symbol-key "*" #x2297 "circled times")
  (define-symbol-key "j" #x2190 "leftwards arrow")
  (define-symbol-key "=" #x2260 "not equal to")
  (define-symbol-key "," #x2264 "less-than or equal to")
  (define-symbol-key "`" #x2261 "identical to")
  (define-symbol-key "/" #x222B "integral")
  (define-symbol-key "h" #x2193 "downwards arrow")
  (define-symbol-key "B" #x03B2 "Greek small letta beta")
  (define-symbol-key "-" #x00AC "not")
  (define-symbol-key "P" #x03C0 "Greek small letter pi")
  (define-symbol-key "G" #x03B3 "Greek small letter gamma")
  (define-symbol-key "g" #x2191 "upwards arrow")
  (define-symbol-key "+" #x2295 "circled plus")
  (define-symbol-key "p" #x2202 "partial differential")
  (define-symbol-key "y" #x2283 "superset of")
  (define-symbol-key "r" #x222A "union")
  (define-symbol-key "o" #x2203 "there exists")
  (define-symbol-key "l" #x21C4 "rightwards arrow over leftwards arrow")
  (define-symbol-key "k" #x2192 "rightwards arrow")
  (define-symbol-key [escape] #x25CA "lozenge")
  (define-symbol-key "." #x2265 "greater-than or equal to")
  (define-symbol-key "w" #x2228 "logical OR"))

(defun symbolics:install-keymap ()
  (symbolics::install-fkey-mapping)
  (let ((map (copy-keymap *symbolics-map*)))
    (set-keymap-parent map (keymap-parent key-translation-map))
    (set-keymap-parent key-translation-map map))
  (symbolics::install-prefix-maps)
  (symbolics::install-keys)
  (symbolics::install-meta-maps)
  (symbolics::install-function-map)
  (symbolics::install-select-map)
  (symbolics::install-symbol-map))

(provide 'symbolics)

