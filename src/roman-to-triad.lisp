(in-package charje.triads)

(defvar +major-romans+
  '("I" "ii" "iii" "IV" "V" "vi" "vii"))

(defvar +minor-romans+
  '("i" "ii" "III" "iv" "v" "VI" "VII"))

(define-condition chord-error ()
  ((key :type key :initarg :key :reader key)
   (bad-chord :type string :initarg :bad-chord :reader bad-chord))
  (:report (lambda (condition stream)
             (format stream "~A is not a valid chord of ~A."
                     (bad-chord condition)
                     (key condition)))))

(defun chord-error-p (condition)
  (typep condition 'chord-error))

(defun roman-to-index (roman)
  (ecase (find-keyword (str:upcase roman))
    (:i 0)
    (:ii 1)
    (:iii 2)
    (:iv 3)
    (:v 4)
    (:vi 5)
    (:vii 6)))

(defun relative-key (key roman)
  (make-key
   (str:concat
    (aref (scale key) (roman-to-index roman))
    (if (every #'lower-case-p roman)
        "m" ""))))

(defmethod roman-to-triad ((obj key) roman)
  (let ((root (roman-to-index roman)))
    (mapcar (lambda (rank)
              (aref (scale obj) (mod rank 7)))
            (list root (+ 2 root) (+ 4 root)))))

(defmethod roman-to-triad ((obj major-key) roman)
  (unless (member roman +major-romans+ :test #'string=)
    (error 'chord-error :bad-chord roman :key obj))
  (call-next-method))

(defmethod roman-to-triad ((obj minor-key) roman)
  (if (not (member roman +minor-romans+ :test #'string=))
      (roman-to-triad (relative-key obj "I") roman)
      (call-next-method)))

(defun compound-roman-to-triad (key roman)
  (if (str:containsp "/" roman)
      (ematch (str:split-omit-nulls "/" roman)
        ((list base roman)
         (roman-to-triad (relative-key key base)
                         roman)))
      (roman-to-triad key roman)))
