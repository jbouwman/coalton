(in-package #:coalton-tests)

(deftest test-parser ()
  (labels ((test-files (pattern)
             (directory (merge-pathnames pattern
                                         (asdf:system-source-directory "coalton/tests"))))

           (parse-file (file)
             (with-open-file (stream file
                                     :direction :input
                                     :element-type 'character)
               (parser:with-reader-context stream
                 (let ((file (error:make-coalton-file :stream stream
                                                      :name (namestring file))))
                   (parser:read-program stream file)))))

           (parse-error-text (file)
             (with-open-file (stream file
                                     :direction :input
                                     :element-type 'character)
               (handler-case
                   (parser:with-reader-context stream
                     (let ((file (error:make-coalton-file :stream stream :name "test")))
                       (parser:read-program stream file)))
                 (error:coalton-base-error (c)
                   (princ-to-string c))))))
    (dolist (file (test-files "tests/parser/*.bad.coalton"))
      (let ((error-file (make-pathname :type "error"
                                       :defaults file)))
        (cond ((uiop:file-exists-p error-file)
               (check-string= (format nil "expected error ~A (A) and generated error (B)"
                                      error-file)
                              (alexandria:read-file-into-string error-file)
                              (parse-error-text file)))
              (t
               (signals parser:parse-error
                 (parse-file file))))))

    (dolist (file (test-files "tests/parser/*.good.coalton"))
      (parse-file file))))
