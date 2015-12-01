
(defgroup bootstrap nil
  "Bootstrap customisations."
  :group 'starter-kit
  :prefix 'bootstrap-)

(defvar *bootstrap-loading-char* ?█
  "Progress bar character.")

(defvar *bootstrap-loading-string* ""
  "Progress bar string.")

(defvar *bootstrap-loading-counter* 0
  "Progress bar counter.")

(defconst +bootstrap-loading-dots-chunk-count+ 3
  "Number of 'dots' per chunk.")

(defconst +bootstrap-loading-dots-count+ (window-total-size nil 'width)
  "Number of positions in order to fill a window.")

(defconst +bootstrap-loading-dots-chunk-size+
  (/ +bootstrap-loading-dots-count+
     +bootstrap-loading-dots-chunk-count+)
  "Number of dot chunks.")

(provide 'bootstrap-core)
