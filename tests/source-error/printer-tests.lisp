(in-package #:source-error-tests)

(defvar *test-file*)

(defvar *test-notes*)

(setf *test-file*
      "Line 1: abcdefghijklmnopqrstuvwxyz
Line 2: bcdefghijklmnopqrstuvwxyza
Line 3: cdefghijklmnopqrstuvwxyzab
Line 4: defghijklmnopqrstuvwxyzabc
Line 5: efghijklmnopqrstuvwxyzabcd
Line 6: fghijklmnopqrstuvwxyzabcde
Line 7: ghijklmnopqrstuvwxyzabcdef
Line 8: hijklmnopqrstuvwxyzabcdefg
")

(setf *test-notes*
      '((20 80 "error 1")
        (80 140 "error 4")
        (60 120 "error 3")
        (40 100 "error 2")))

(defun make-notes ()
  (mapcar (lambda (notedef)
            (destructuring-bind (start end message) notedef
              (note :span (cons start end) :type :error :message message)))
          *test-notes*))

(defclass source-string ()
  ((string :initarg :string)))

(defmethod source-error:source-name ((source-string source-string))
  "String Input")

(defmethod source-error:source-stream ((source-string source-string))
  (make-string-input-stream (slot-value source-string 'string)))


(let* ((stream (make-string-output-stream))
       (source (make-instance 'source-string :string *test-file*))
       (error-info (source-error :type :error
                                 :location '(35 . 41)
                                 :source source
                                 :message "This is line 1")))
  (source-error::with-source-stream (source-stream error-info)
    (let ((printer-state (make-instance 'source-error::printer-state
                           :stream stream
                           :source-stream source-stream
                           :error-info error-info)))
      printer-state)))



(with-output-to-string (stream)
  (let* ((source (make-instance 'source-string :string *test-file*))
         (error-info (source-error :type :error
                                   :location '(35 . 41)
                                   :source source
                                   :message "This is line 1")))
    (source-error:report-source-error stream error-info)))
