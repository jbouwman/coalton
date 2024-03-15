(in-package #:cl-user)

(defun parse-example ()
  (with-open-file (stream "library/optional.coalton")
    (coalton-impl/parser:with-reader-context stream
      (coalton-impl/parser:read-program
       stream
       (coalton-impl/error:make-coalton-file :stream stream
                                             :name "file")
       :mode :file))))

(defun example-package ()
  (coalton-impl/parser/toplevel::program-package (parse-example))
  )


(export 'coalton-library/hash::Hash 'coalton-library/hash)

(find-package 'coalton-library/hash)
