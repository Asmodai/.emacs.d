
(require 'core-predicates)
(require 'core-funcs)
(require 'core-emacs-buffer)

(defvar bootstrap--diminished-minor-modes nil
  "List of diminished minor modes to Unicode or ASCII values.")

(defun bootstrap/set-default-font (plist)
  "Set the font to the one given in the passed PLIST.

PLIST has the form (\"fontname\" :prop1 val1 ... :propN valN)"
  (let* ((font (car plist))
         (props (cdr plist))
         (scale (plist-get props :powerline-scale))
         (font-props (bootstrap/mplist-remove
                      (bootstrap/mplist-remove props :powerline-scale)
                      :powerline-offset))
         (fontspec (apply 'font-spec :name font font-props)))
    (bootstrap-buffer/message "Setting font \"%s\"..." font)
    (set-default-font fontspec nil t)
    (setq-default powerline-scale scale)
    (setq-default powerline-height (bootstrap/compute-powerline-height))
    ;; Fallback
    (pcase system-type
      (`gnu/linux
       (setf fallback-font-name  "UbuntuMono"
             fallback-font-name2 "UbuntuMono"))
      (`darwin
       (setf fallback-font-name  "Menlo"
             fallback-font-name2 "Menlo"))
      (`windows-nt
       (setf fallback-font-name  "Lucida Sans Unicode"
             fallback-font-name2 "Lucida Sans Unicode"))
      (`cygwin
       (setf fallback-font-name  "Lucida Sans Unicode"
             fallback-font-name2 "Lucida Sans Unicode"))
      (other
       (setf fallback-font-name  nil
             fallback-font-name2 nil)))
    (when (and fallback-font-name
               fallback-font-name2)
      (let* ((fallback-props (bootstrap/mplist-remove
                              (bootstrap/mplist-remove font-props :size)
                              :height))
             (fallback-spec (apply 'font-spec
                                   :name fallback-font-name
                                   fallback-props))
             (fallback-spec2 (apply 'font-spec
                                    :name fallback-font-name2
                                    fallback-props)))
        (set-fontset-font "fontset-default"
                          '(#x2776 . #x2793) fallback-spec nil 'prepend)
        ;; mode-line circled letters
        (set-fontset-font "fontset-default"
                          '(#x24b6 . #x24fe) fallback-spec nil 'prepend)
        ;; mode-line additional characters
        (set-fontset-font "fontset-default"
                          '(#x2295 . #x22a1) fallback-spec nil 'prepend)
        ;; new version lighter
        (set-fontset-font "fontset-default"
                          '(#x2190 . #x2200) fallback-spec2 nil 'prepend)))))

(defun bootstrap/compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale)
                        powerline-scale)
                   powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(defmacro bootstrap|diminish (mode unicode &optional ascii)
  "Diminish MODE name in mode line to UNICODE or ASCII depending on the value of
`bootstrap-loader-mode-line-unicode-symbols'.

If ASCII is not provided, then UNICODE is used instead."
  `(add-to-list 'bootstrap--diminished-minor-modes '(,mode ,unicode ,ascii)))

(defmacro bootstrap|hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load
       'diminish '(diminish ',mode)))

(provide 'core-fonts-support)

