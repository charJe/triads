(in-package charje.triads)

(defvar +note-names+ '("A" "B" "C" "D" "E" "F" "G"))

(defvar +note-name-circle+
  (let ((names (copy-seq +note-names+)))
    (setf (rest (last names)) names)
    names))

(define-condition key-error ()
  ((bad-key :type string :initarg :bad-key :reader bad-key))
  (:report (lambda (condition stream)
             (format stream "`~S' is not a key." (bad-key condition)))))

(defun key-error-p (condition)
  (typep condition 'key-error))

(defun names-starting-with (tonic)
  (let ((start (position tonic +note-names+
                         :test (flip #'str:starts-with-p))))
    (unless start
      (error 'key-error :bad-key tonic))
    (coerce (subseq +note-name-circle+ start (+ 7 start)) 'vector)))

(defvar +sharps+
  (vector "F" "C" "G" "D" "A" "E" "B" "F♯" "C♯" "G♯" "D♯" "A♯" "E♯" "B♯"))

(defvar +flats+
  (vector "B" "E" "A" "D" "G" "C" "F" "B♭" "E♭" "A♭" "D♭" "G♭" "C♭" "F♭"))

(defun how-many-sharps (key)
  (let ((pos (position (remove #\m key) +sharps+
                       :test #'string=)))
    (if (null pos)
        0
        (- pos (if (find #\m key)
                   4 1)))))

(defun how-many-flats (key)
  (let ((pos (position (remove #\m key) +flats+
                       :test #'string=)))
    (if (null pos)
        0
        (- pos (if (find #\m key)
                   2 5)))))

(defun sharps (key)
  (let ((how-many-sharps (how-many-sharps key)))
    (if (< how-many-sharps 1)
        (vector)
        (subseq +sharps+ 0 how-many-sharps))))

(defun flats (key)
  (let ((how-many-flats (how-many-flats key)))
    (if (< how-many-flats 1)
        (vector)
        (subseq +flats+ 0 how-many-flats))))

(defun generate-scale (key)
  (let ((names (names-starting-with key))
        (sharps (sharps key))
        (flats (flats key)))
    (map 'vector
         (lambda (name)
           (let ((sharpness (count name sharps :test #'str:starts-with-p))
                 (flatness (count name flats :test #'str:starts-with-p)))
             (str:concat
              name
              (cond
                ((= 1 sharpness) "♯")
                ((< 1 sharpness) "𝄪")
                ((= 1 flatness) "♭")
                ((< 1 flatness) "𝄫")
                (t "")))))
         names)))

(defun unicode-accidental (string)
  (if (< (length string) 2)
      string
      (let ((character (elt string 1)))
        (setf (elt string 1)
              (case character
                ((#\b) #\♭)
                ((#\#) #\♯)
                (otherwise character)))
        string)))
