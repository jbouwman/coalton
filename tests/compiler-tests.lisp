(in-package #:coalton-tests)

(deftest test-compile-file ()
  (with-open-file (input "library/types.coalton" :direction :input)
    (with-open-file (output "types.lisp"
                            :direction :output
                            :if-exists :supersede)
      (coalton-impl/compiler::%compile-file input output))))
