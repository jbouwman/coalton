(in-package #:coalton-tests)

(deftest test-parser ()
  (labels ((test-files (pattern)
             (directory (merge-pathnames pattern (asdf:system-source-directory "coalton/tests"))))

           (parse-file (file)
             (source-error:with-source-file (stream file)
               (parser:with-reader-context stream
                 (parser:read-program stream :mode :file))))

           (error-string (condition)
             (with-output-to-string (stream)
               (source-error:report-source-condition condition stream)))

           (parse-error-text (file)
             (handler-case
                 (source-error:with-displaced-source-file (stream file "test" 0)
                   (parser:with-reader-context stream
                     (parser:read-program stream :mode :file))
                   nil)
               (error:coalton-error (c)
                 (error-string c)))))
    (loop :for file :in (test-files "tests/parser/*.bad.coalton")
          :do (let ((error-file (make-pathname :type "error"
                                               :defaults file)))
                (cond ((uiop:file-exists-p error-file)
                       (check-string= (format nil "expected error ~A (A) and generated error (B)" error-file)
                                      (alexandria:read-file-into-string error-file)
                                      (parse-error-text file)))
                      (t
                       (signals parser:parse-error
                         (parse-file file))))))

    (loop :for file :in (test-files "tests/parser/*.good.coalton")
          :do (parse-file file))))
