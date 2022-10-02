(in-package charje.triads)

(defclass key ()
  ((scale :type vector :initarg :scale :reader scale)))

(defmethod tonic ((obj key))
  (aref (scale obj) 0))

(defclass minor-key (key) ())

(defclass major-key (key) ())

(defmethod print-object ((obj major-key) stream)
  (format stream "~A Major" (tonic obj)))

(defmethod print-object ((obj minor-key) stream)
  (format stream "~A minor" (tonic obj)))

(defun make-key (string)
  (restart-case
      (let ((key-type (if (str:containsp "m" string)
                          'minor-key 'major-key))
            (key (str:upcase (unicode-accidental (remove #\m string)))))
        (unless (or (find key +sharps+ :test #'string=)
                    (find key +flats+ :test #'string=))
          (error 'key-error :bad-key string))
        (make-instance
         key-type
         :scale (generate-scale
                 (if (eq 'minor-key key-type)
                     (str:concat key "m")
                     key))))
    (use-new-key (new-key)
      :report "Enter a new key."
      :interactive (lambda ()
                     (format *debug-io* "New key: ")
                     (force-output *debug-io*)
                     (list (read-line *debug-io*)))
      :test key-error-p
      (make-key new-key))))
