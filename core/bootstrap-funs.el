
(defvar *bootstrap-init-redisplay-count* 0
  "The number of calls to `redisplay'")

(defun bootstrap:redisplay ()
  "`redisplay' wrapper."
  (incf *bootstrap-init-redisplay-count*)
  (redisplay))
