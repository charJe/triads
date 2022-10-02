(in-package charje.triads)

(defun format-roman-and-triad (key roman)
  (restart-case
      (format t "~A:~,6T~{~A~,3T~}~%"
              roman (compound-roman-to-triad key roman))
    (use-new-roman (new-roman)
      :report "Enter a new roman numeral."
      :interactive (lambda ()
                     (format *debug-io* "New roman numeral: ")
                     (force-output *debug-io*)
                     (list (read-line *debug-io*)))
      :test chord-error-p
      (format-roman-and-triad key new-roman))))

(defun stream-to-chords (key in)
  (loop
    for line = (read-line in nil nil)
    while line do
      (loop for roman in (str:split-omit-nulls " " line) do
        (format-roman-and-triad key roman))))

(defun main (&aux (args (uiop:command-line-arguments)))
  (handler-case
      (ematch args
        ((list key)
         (let ((key (make-key key)))
           (format *error-output* "Generating triads from standard input in the key of ~A.~%"
                   key)
           (stream-to-chords key *standard-input*)))
        ((list key file)
         (if (not (uiop:file-exists-p file))
             (format *error-output* "`~A' does not exist." file)
             (let ((key (make-key key)))
               (format *error-output* "Generating triads from `~A' in the key of ~A.~%"
                       file key)
               (with-open-file (in file :direction :input)
                 (stream-to-chords key in)))))
        (_ (format *error-output* "Usage: triads key [file]~%")))
    (serious-condition (condition)
      (format *error-output* "Encountered `~A'; exiting.~%"
              condition)
      (uiop:quit))))
