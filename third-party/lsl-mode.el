;; This software has been given to the PUBLIC DOMAIN. Use it as you see
;; fit. As usual, no warrantees are available, neither expressed nor
;; implied. You use this software at your own risk.

;; To load this into emacs, add the following lines to your .emacs:
;
; (add-to-list 'load-path "/path/to/directory/containing/this/file")
; (autoload 'lsl-mode "lsl-mode" "Load LSL mode." t)
; (add-to-list 'auto-mode-alist '("\\.lsl$" . lsl-mode))
;
;; You should then be able to load up LSL files and get the pretty
;; colors and indentation.

;; This is derived from C-mode, since LSL is so similar. There'll
;; probably be a couple gotchas as a result. I haven't tested this
;; thorougly, but it works well enough for me. If you have any
;; improvements, feel free to contact Brain Curry in game.

(defcustom lsl-keywords
  '("for" "if" "else" "break" "while" "do" "return" "state" "default")
  "LSL keywords."
  :type 'list
  :group 'lsl-font-lock)

(defcustom lsl-types
  '("integer" "float" "string" "key" "list" "vector" "rotation")
  "LSL types."
  :type 'list
  :group 'lsl-font-lock)

(defcustom lsl-constants
  '("ACTIVE" "AGENT" "ALL_SIDES" "CHANGED_ALLOWED_DROP"
   "CHANGED_COLOR" "CHANGED_INVENTORY" "CHANGED_LINK"
   "CHANGED_OWNER" "CHANGED_REGION" "CHANGED_SCALE"
   "CHANGED_SHAPE" "CHANGED_TELEPORT" "CHANGED_TEXTURE"
   "CONTROL_BACK" "CONTROL_DOWN" "CONTROL_FWD"
   "CONTROL_LBUTTON" "CONTROL_LEFT" "CONTROL_ML_LBUTTON"
   "CONTROL_RIGHT" "CONTROL_ROT_LEFT" "CONTROL_ROT_RIGHT"
   "CONTROL_UP" "DEBUG_CHANNEL" "DEG_TO_RAD" "EOF"
   "FALSE" "HTTP_BODY_MAXLENGTH" "HTTP_BODY_TRUNCATED"
   "HTTP_METHOD" "HTTP_MIMETYPE" "HTTP_VERIFY_CERT"
   "INVENTORY_ALL" "INVENTORY_ANIMATION" "INVENTORY_BODYPART"
   "INVENTORY_CLOTHING" "INVENTORY_GESTURE"
   "INVENTORY_LANDMARK" "INVENTORY_NONE" "INVENTORY_NOTECARD"
   "INVENTORY_OBJECT" "INVENTORY_SCRIPT" "INVENTORY_SOUND"
   "INVENTORY_TEXTURE" "LINK_ALL_CHILDREN" "LINK_ALL_OTHERS"
   "LINK_ROOT" "LINK_SET" "LINK_THIS" "NULL_KEY"
   "PARCEL_MEDIA_COMMAND_AGENT"
   "PARCEL_MEDIA_COMMAND_AUTO_ALIGN"
   "PARCEL_MEDIA_COMMAND_LOOP"
   "PARCEL_MEDIA_COMMAND_PAUSE"
   "PARCEL_MEDIA_COMMAND_PLAY"
   "PARCEL_MEDIA_COMMAND_STOP"
   "PARCEL_MEDIA_COMMAND_TEXTURE"
   "PARCEL_MEDIA_COMMAND_TIME"
   "PARCEL_MEDIA_COMMAND_UNLOAD"
   "PARCEL_MEDIA_COMMAND_URL" "PASSIVE" "PAYMENT_INFO_ON_FILE"
   "PAYMENT_INFO_USED" "PAY_DEFAULT" "PAY_HIDE"
   "PERMISSION_ATTACH" "PERMISSION_CHANGE_LINKS"
   "PERMISSION_CONTROL_CAMERA" "PERMISSION_DEBIT"
   "PERMISSION_TAKE_CONTROLS" "PERMISSION_TRACK_CAMERA"
   "PERMISSION_TRIGGER_ANIMATION" "PI" "PY_BY_TWO"
   "PUBLIC_CHANNEL" "RAD_TO_DEG" "REMOTE_DATA_CHANNEL"
   "REMOTE_DATA_REPLY" "REMOTE_DATA_REQUEST" "SCRIPTED"
   "SQRT2" "TRUE" "TWO_PI" "ZERO_ROTATION" "ZERO_VECTOR")
  "LSL constants."
  :type 'list
  :group 'lsl-font-lock)

(defcustom lsl-events
  '("at_rot_target" "at_target" "attach" "changed" "collision"
    "collision_end" "collision_start" "control" "dataserver"
    "email" "land_collision" "land_collision_end"
    "land_collision_start" "link_message" "listen" "money"
    "moving_end" "moving_start" "no_sensor" "not_at_rot_target"
    "not_at_target" "object_rez" "on_rez" "remote_data"
    "run_time_permissions" "sensor" "state_entry" "state_exit"
    "timer" "touch" "touch_end" "touch_start")
  "LSL events."
  :type 'list
  :group 'lsl-font-lock)

(defvar lsl-builtin-regexp "\\<ll[A-Z][a-zA-Z]+\\>")
(defvar lsl-keywords-regexp (regexp-opt lsl-keywords 'words))
(defvar lsl-type-regexp (regexp-opt lsl-types 'words))
(defvar lsl-constant-regexp (regexp-opt lsl-constants 'words))
(defvar lsl-event-regexp (regexp-opt lsl-events 'words))
(defvar lsl-variable-name-regexp
  (concat "\\<integer\\>" "\\s+\\(\\<foo\\s +bar\\>\\)"))

(setq lsl-font-lock-keywords
  `(,lsl-keywords-regexp
    (,lsl-type-regexp . font-lock-type-face)
    ("\\(\\<\\S +\\>\\)\\s *(" 1 font-lock-function-name-face t)
    (,lsl-event-regexp 0 font-lock-builtin-face t)
    (,lsl-builtin-regexp 0 font-lock-builtin-face t)
    (,(concat lsl-type-regexp "\\W+\\(\\<\\w+\\>\\)") 2 font-lock-variable-name-face)
    (,lsl-constant-regexp . font-lock-constant-face)))

;;(define-derived-mode lsl-mode c-mode "LSL"
(define-derived-mode lsl-mode c++-mode "LSL"
  "Major mode for editing LSL.
\\{lsl-mode-map}"
  (setq font-lock-defaults '((lsl-font-lock-keywords) nil nil))
  (setq c-basic-offset 2))

(provide 'lsl-mode)

