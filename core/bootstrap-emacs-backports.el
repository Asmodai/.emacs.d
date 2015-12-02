
(unless (featurep 'subr-x)
  ;; `subr-x' function for Emacs 24.3 and below.

  (defsubst string-join (strings &optional separator)
    "Join all STRINGS using SEPARATOR."
    (mapconcat 'identity strings separator))

  (defsubst string-trim-left (string)
    "Remove leading whitespace from STRING."
    (if (string-match "\\`[ \t\n\r]+" string)
        (replace-match "" t t string)
      string))

  (defsubst string-trim-right (string)
    "Remove trailing whitespace from STRING."
    (if (string-match "[ \t\n\r]+\\'" string)
        (replace-match "" t t string)
      string))

  (defsubst string-trim (string)
    "Remove leading and trailing whitespace from STRING."
    (string-trim-left (string-trim-right string)))

  (defsubst string-empty-p (string)
    "Check whether STRING is empty."
    (string= string ""))

  )                                     ; subr-x

(unless (fboundp 'with-eval-after-load)
  ;; `with-eval-after-load' function for Emacs 24.3 and below

  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1)
             (debug t))
    `(eval-after-load ,file
       (lambda ()
         ,@body)))

  )                                     ; with-eval-after-load

(provide 'bootstrap-emacs-backports)
