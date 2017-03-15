;;; $Header: /home/rpg/emacs/RCS/cl-indent-patches.el,v 1.1 2003/06/30 15:37:28 rpg Exp $

;; Extentions for more cl compatible indenting
;; this was in cl-indent.el but it conflicted with the distribution

;;; Load the distribution lisp-mode and cl-indent.el BEFORE loading this file.

;;; Original version by rst
;;; Change Log
;;;  10/6/88 salem - Fixes for CL indentation style (CLtM as reference)
;;;  6/13/89 bromley - fixed common-lisp-indent-function to indent backquoted forms correctly.
;;;  6/15/89 bromley - removed bogus indentations for flet and labels

(provide 'cl-indent-patches)
;;wish every file had a provide...

(if (not (boundp 'lisp-indent-maximum-backtracking))
    (load-library "cl-indent"))

;; Don't use this for Emacs Lisp
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))

(defun common-lisp-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    ;; Walk up list levels until we see something
    ;;  which does special things with subforms.
    (let ((depth 0)
          ;; Path describes the position of point in terms of
          ;;  list-structure with respect to contining lists.
          ;; `foo' has a path of (0 4 1) in `((a b c (d foo) f) g)'
          (path ())
          ;; set non-nil when somebody works out the indentation to use
          calculated
          (last-point indent-point)
          ;; the position of the open-paren of the innermost containing list
          (containing-form-start (elt state 1))
          ;; the column of the above
          sexp-column)
      ;; Move to start of innermost containing list
      (goto-char containing-form-start)
      (setq sexp-column (current-column))
      ;; Look over successively less-deep containing forms
      (while (and (not calculated)
                  (< depth lisp-indent-maximum-backtracking))
        (let ((containing-sexp (point)))
          (forward-char 1)
          (parse-partial-sexp (point) indent-point 1 t)
          ;; Move to the car of the relevant containing form
          (let (tem function method)
            (if (not (looking-at "\\sw\\|\\s_"))
                ;; This form doesn't seem to start with a symbol
                (setq function nil method nil)
              (setq tem (point))
              (forward-sexp 1)
              (setq function (downcase (buffer-substring tem (point))))
              (goto-char tem)
              (setq tem (intern-soft function)
                    method (get tem 'common-lisp-indent-function))
              (cond ((and (null method)
                          (string-match ":[^:]+" function))
                     ;; The pleblisp package feature
                     (setq function (substring function
                                               (1+ (match-beginning 0)))
                           method (get (intern-soft function)
                                       'common-lisp-indent-function)))
                    ((and (null method))
                     ;; backwards compatibility
                     (setq method (get tem 'lisp-indent-function)))))
            (let ((n 0))
              ;; How far into the containing form is the current form?
              (if (< (point) indent-point)
                  (while (condition-case ()
                             (progn
                               (forward-sexp 1)
                               (if (>= (point) indent-point)
                                   nil
                                 (parse-partial-sexp (point)
                                                     indent-point 1 t)
                                 (setq n (1+ n))
                                 t))
                           (error nil))))
              (setq path (cons n path)))

            ;; backwards compatibility.
            (cond ((null function))
                  ((null method)
                   (if (null (cdr path))
                       ;; (package prefix was stripped off above)
                       (setq method 
                             (cond ((and (string-match "\\`def" function)
                                         (not (string-match "\\`default-" function)))
                                    '(4 (&whole 4 &rest 1) &body))
                                   ((string-match "\\`\\(with\\|do\\)-"
                                                  function)
                                    '(4 &body))))))
                  ;; backwards compatibility.  Bletch.
                  ((eq method 'defun)
                   (setq method '(4 (&whole 4 &rest 1) &body))))

            (cond ((and (eql (char-after (1- containing-sexp)) ?\') ; patched to only do this for ' and not `.  
                        (not (eql (char-after (- containing-sexp 2)) ?\#)))
                   ;; No indentation for "'(...)" elements
                   (setq calculated (1+ sexp-column)))
                  ((eql (char-after (1- containing-sexp)) ?\#)
                   ;; "#(...)"
                   (setq calculated (1+ sexp-column)))
                  ((null method))
                  ((integerp method)
                   ;; convenient top-level hack.
                   ;;  (also compatible with lisp-indent-function)
                   ;; The number specifies how many `distinguished'
                   ;;  forms there are before the body starts
                   ;; Equivalent to (4 4 ... &body)
                   (setq calculated (cond ((cdr path)
                                           normal-indent)
                                          ((<= (car path) method)
                                           ;; `distinguished' form
                                           (list (+ sexp-column 4)
                                                 containing-form-start))
                                          ((= (car path) (1+ method))
                                           ;; first body form.
                                           (+ sexp-column lisp-body-indent))
                                          (t
                                           ;; other body form
                                           normal-indent))))
                  ((symbolp method)
                   (setq calculated (funcall method
                                             path state indent-point
                                             sexp-column normal-indent)))
                  (t
                   (setq calculated (lisp-indent-259
                                      method path state indent-point
                                      sexp-column normal-indent)))))

          (goto-char containing-sexp)
          (setq last-point containing-sexp)
          (if (not calculated)
              (condition-case ()
                   (progn (backward-up-list 1)
                          (setq depth (1+ depth)))
                (error (setq depth lisp-indent-maximum-backtracking))))))
      calculated)))

(defun rst-indent-sexp (count)
  "Indent each line of the sexp after point.  With arg, indents
that many sexps.  Redefined by rst to not reindent trailing comments
on lines."
  (interactive "p")
  (save-excursion
    (let ((beginning (point)))
      (forward-sexp count)
      (indent-region beginning (point) nil))))

;;; Emacs-lisp backquote does exist (he finds out later)
;;; but it's pretty darn crude ...
(defmacro def-lisp-indentation (sym indentation-hook)
  (list 'put
        (list 'quote sym)
        ''common-lisp-indent-function
        (list 'quote indentation-hook)))

(def-lisp-indentation block 1)
(def-lisp-indentation case 1)
(def-lisp-indentation catch 1)
(def-lisp-indentation ccase 1)
(def-lisp-indentation compiler-let 1)
(def-lisp-indentation cond 0)
(def-lisp-indentation ctypecase 1)
(def-lisp-indentation defconstant (13 2 2))
(def-lisp-indentation defconst (10 2 2))
(def-lisp-indentation define-setf-method defun)
(def-lisp-indentation defparameter (14 2 2))
(def-lisp-indentation defsetf 3)
(def-lisp-indentation defstruct 1)
(def-lisp-indentation deftype defun)
(def-lisp-indentation defvar (8 2 2))
(def-lisp-indentation do 2)
(def-lisp-indentation do-all-symbols 1)
(def-lisp-indentation do-external-symbols 1)
(def-lisp-indentation do-symbols 1)
(def-lisp-indentation dolist 1)
(def-lisp-indentation dotimes 1)
(def-lisp-indentation ecase 1)
(def-lisp-indentation etypecase 1)
(def-lisp-indentation eval-when 1)
;;the default one is better -rpg
;(def-lisp-indentation macrolet 1)
(def-lisp-indentation multiple-value-bind 2)
(def-lisp-indentation multiple-value-prog1 0)
(def-lisp-indentation multiple-value-setq 2)
(def-lisp-indentation prog 1)
(def-lisp-indentation prog* 1)
(def-lisp-indentation prog1 0)
(def-lisp-indentation prog2 0)
(def-lisp-indentation progv 2)
(def-lisp-indentation tagbody 0)
(def-lisp-indentation typecase 1)
(def-lisp-indentation unless 1)
(def-lisp-indentation unwind-protect 1)
(def-lisp-indentation when 1)
(def-lisp-indentation with-input-from-string 1)
(def-lisp-indentation with-open-stream 1)
(def-lisp-indentation with-output-to-string 1)
(def-lisp-indentation handler-bind 1)

;; TMC internal macros
(def-lisp-indentation letd 1)           ;added by cal
(def-lisp-indentation letd* 1)          ;added by cal
(def-lisp-indentation def-file-set 1)
       
;; CLtL2
(def-lisp-indentation generic-labels
    ((&whole 4 &rest (&whole 1 (&whole 4 &rest 1) &body))
     &body))
(def-lisp-indentation with-accessors 2)
(def-lisp-indentation define-condition (10 5 2 2))
(def-lisp-indentation handler-case (4 &rest (&whole 2 &lambda &body)))
(def-lisp-indentation restart-case (4 &rest (&whole 2 &lambda &body)))
(def-lisp-indentation defpackage 1)
  
;; CLOS
(def-lisp-indentation defclass (6 4 (&whole 2 &rest 1) (&whole 2 &rest 1)))
(def-lisp-indentation defgeneric defun)
(def-lisp-indentation define-method-combination (8 8 4 2))
(def-lisp-indentation defmethod (11 4 2 2))
;; the following isn't right, but could be fixed to be right
(def-lisp-indentation with-slots (12 12 3))
(def-lisp-indentation make-instance (15 3))
           
   
;; Symbolics
(def-lisp-indentation once-only 1)
(def-lisp-indentation condition-case (16 5 3))
(def-lisp-indentation condition-case-if (18 18 5 3))
(def-lisp-indentation defflavor (11 5 5 2)) ;; ??

;; *lisp
(def-lisp-indentation *defun defun)
(def-lisp-indentation *let 1)
(def-lisp-indentation let-vp-set 1)
(def-lisp-indentation *let* 1)
(def-lisp-indentation *cond 0)
(def-lisp-indentation with-css-saved 0)
(def-lisp-indentation do-for-selected-processors 1)
(def-lisp-indentation *all 0)
(def-lisp-indentation *when 1)
(def-lisp-indentation *unless 1)
(def-lisp-indentation *defvar (8 2 2))
(def-lisp-indentation *with-vp-set 1)

;; CM
(def-lisp-indentation with-any-vp-fields 1)
(def-lisp-indentation with-vp-fields 1)
(def-lisp-indentation with-vp-fields-in-cm 1)
(def-lisp-indentation with-vp-fields-for-read-or-write 1)
(def-lisp-indentation let-paris-stack 1)
(def-lisp-indentation let-paris-stack-vp-set 1)
(def-lisp-indentation without-safety-checking 0)
(def-lisp-indentation with-vp-set-and-flags-saved 0)
(def-lisp-indentation with-vp-set-saved 0)
(def-lisp-indentation with-flags-saved 1)
(def-lisp-indentation with-all-flags-saved 0)

;; Custom
(def-lisp-indentation defenumeration (8 4 2))
(def-lisp-indentation defbitfield (8 4 2))

(def-lisp-indentation loop cl-indent-indent-loop-macro)

(defun cl-indent-parse-state-depth (parse-state)
  (car parse-state))

(defun cl-indent-parse-state-start (parse-state)
  (car (cdr parse-state)))

(defun cl-indent-parse-state-prev (parse-state)
  (car (cdr (cdr parse-state))))

;; Regexps matching various varieties of loop macro keyword ...
(defvar cl-indent-body-introducing-loop-macro-keyword
  "do\\|finally\\|initially"
  "Regexp matching loop macro keywords which introduce body-forms")

;; This is so "and when" and "else when" get handled right
;; (not to mention "else do" !!!)
(defvar cl-indent-prefix-loop-macro-keyword
  "and\\|else"
  "Regexp matching loop macro keywords which are prefixes")

(defvar cl-indent-clause-joining-loop-macro-keyword
  "and"
  "Regexp matching 'and', and anything else there ever comes to be
like it ...")

;; This is handled right, but it's incomplete ...
;; (It could probably get arbitrarily long if I did *every* iteration-path)
(defvar cl-indent-indented-loop-macro-keyword
  "into\\|by\\|upto\\|downto\\|above\\|below\\|on\\|being\\|=\\|first\\|then\\|from\\|to"
  "Regexp matching keywords introducing loop subclauses.  Always indented two")

(defvar cl-indent-indenting-loop-macro-keyword
  "when\\|unless\\|if"
  "Regexp matching keywords introducing conditional clauses.
Cause subsequent clauses to be indented")

(defvar cl-indent-loop-macro-else-keyword "else")

;;; Attempt to indent the loop macro ...

(defun cl-indent-indent-loop-macro
    (path parse-state indent-point sexp-column normal-indent)
  (list (cl-indent-indent-loop-macro-1 parse-state indent-point)
        (cl-indent-parse-state-start parse-state)))

(defun cl-indent-indent-loop-macro-1 (parse-state indent-point)
  (catch 'return-indentation
    (save-excursion

      ;; Find first clause of loop macro, and use it to establish
      ;; base column for indentation
      
      (goto-char (cl-indent-parse-state-start parse-state))
      (let ((loop-start-column (current-column)))
        (cl-indent-loop-advance-past-keyword-on-line)
        (if (eolp)
            (progn
              (forward-line 1)
              (end-of-line)
              
              ;; If indenting first line after "(loop <newline>"
              ;; cop out ...
              
              (if (<= indent-point (point))
                  (throw 'return-indentation (+ 2 loop-start-column)))
              (back-to-indentation)))
        
        (let* ((case-fold-search t)
               (loop-macro-first-clause (point))
               (previous-expression-start (cl-indent-parse-state-prev parse-state))
               (default-value (current-column))
               (loop-body-p nil)
               (loop-body-indentation nil)
               (indented-clause-indentation (+ 2 default-value)))
          ;; Determine context of this loop clause, starting with the
          ;; expression immediately preceding the line we're trying to indent
          (goto-char previous-expression-start)

          ;; Handle a body-introducing-clause which ends a line specially.
          (if (looking-at cl-indent-body-introducing-loop-macro-keyword)
              (let ((keyword-position (current-column)))
                (setq loop-body-p t)
                (setq loop-body-indentation
                      (if (cl-indent-loop-advance-past-keyword-on-line)
                          (current-column)
                        (back-to-indentation)
                        (if (/= (current-column) keyword-position)
                            (+ 2 (current-column))
                          (- keyword-position 3)))))
              
            (back-to-indentation)
            (if (< (point) loop-macro-first-clause)
                (goto-char loop-macro-first-clause))

            ;; If there's an "and" or "else," advance over it.
            ;; If it is alone on the line, the next "cond" will treat it
            ;; as if there were a "when" and indent under it ...
            (let ((exit nil))
              (while (and (null exit)
                          (looking-at cl-indent-prefix-loop-macro-keyword))
                (if (null (cl-indent-loop-advance-past-keyword-on-line))
                    (progn (setq exit t)
                           (back-to-indentation)))))

            ;; Found start of loop clause preceding the one we're trying to indent.
            ;; Glean context ...
            (cond
              ((looking-at "(")
               ;; We're in the middle of a clause body ...
               (setq loop-body-p t)
               (setq loop-body-indentation (current-column)))
              ((looking-at cl-indent-body-introducing-loop-macro-keyword)
               (setq loop-body-p t)
               ;; Know there's something else on the line (or would
               ;; have been caught above)
               (cl-indent-loop-advance-past-keyword-on-line)
               (setq loop-body-indentation (current-column)))
              (t
               (setq loop-body-p nil)
               (if (or (looking-at cl-indent-indenting-loop-macro-keyword)
                       (looking-at cl-indent-prefix-loop-macro-keyword))
                   (setq default-value (+ 2 (current-column))))
               (setq indented-clause-indentation (+ 2 (current-column)))
               ;; We still need loop-body-indentation for "syntax errors" ...
               (goto-char previous-expression-start)
               (setq loop-body-indentation (current-column)))))
      
        ;; Go to first non-blank character of the line we're trying to indent.
        ;; (if none, wind up poised on the new-line ...)
        (goto-char indent-point)
        (back-to-indentation)
        (cond
          ((looking-at "(")
           ;; Clause body ... 
           loop-body-indentation)
          ((or (eolp) (looking-at ";"))
           ;; Blank line.  If body-p, indent as body, else indent as
           ;; vanilla clause.
           (if loop-body-p
               loop-body-indentation
             default-value))
          ((looking-at cl-indent-indented-loop-macro-keyword)
           indented-clause-indentation)
          ((looking-at cl-indent-clause-joining-loop-macro-keyword)
           (let ((stolen-indent-column nil))
             (forward-line -1)
             (while (and (null stolen-indent-column)
                         (> (point) loop-macro-first-clause))
               (back-to-indentation)
               (if (and (< (current-column) loop-body-indentation)
                        (looking-at "\\sw"))
                   (progn
                     (if (looking-at cl-indent-loop-macro-else-keyword)
                         (cl-indent-loop-advance-past-keyword-on-line))
                     (setq stolen-indent-column
                           (current-column)))
                 (forward-line -1)))
             (if stolen-indent-column
                 stolen-indent-column
               default-value)))
          (t default-value)))))))

(defun cl-indent-loop-advance-past-keyword-on-line ()
  (forward-word 1)
  (while (and (looking-at "\\s-") (not (eolp)))
    (forward-char 1))
  (if (eolp)
      nil
    (current-column)))

;; "validation"
'(loop for i from 0 below 2
       for j from 0 below 2
       when foo
         do (fubar)
            (bar)
            (moo)
         and collect cash
               into honduras
       else do ;; this is the body of the first else
               ;; the body is ...
               (indented to the above comment)
               (ZMACS gets this wrong)
            and do this
            and do that
            and when foo
                  do the-other
                  and cry
       when this-is-a-short-condition do
         (body code of the when)
       when here's something I used to botch do (here is a body)
                                                (rest of body indented same)
       do
    (exdented loop body)
    (I'm not sure I like this but it's compatible)
       when funny-predicate do ;; Here's a comment
                               (body filled to comment))

 '(loop when foo
         do this
            (that)
         and do the-other
       )


;;; EOF
