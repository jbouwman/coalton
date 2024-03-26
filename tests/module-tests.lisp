(in-package #:cl-user)

(defun parsed-example ()
  (with-open-file (stream "library/optional.coalton")
    (coalton-impl/parser:with-reader-context stream
      (coalton-impl/parser:read-program
       stream
       (coalton-impl/error:make-coalton-file :stream stream
                                             :name "file")
       :mode :file))))

(defun compile-coalton (program file)
  (with-open-file (stream file
                          :direction :output
                          :element-type 'character
                          :if-exists :supersede)
    (prin1 (coalton-impl/entry:entry-point program) stream)
    (terpri stream)))

(defun compile-and-load (coalton-form)
  (uiop:with-temporary-file (:stream out-stream
                             :pathname input-file
                             :suffix "lisp"
                             :direction :output
                             :keep t)
    (prin1 coalton-form out-stream)
    (terpri out-stream)
    :close-stream
    (uiop:with-temporary-file (:pathname output-file
                               :type #+ccl (pathname-type ccl:*.fasl-pathname*)
                                     #+(not ccl) "fasl"
                               :keep t)
      (compile-file input-file :output-file output-file)
;;      (load output-file)
      (values input-file output-file))))


(compile-coalton (parsed-example) "/Users/jlbouwman/optional.lisp")

