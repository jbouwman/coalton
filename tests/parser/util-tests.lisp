(fiasco:define-test-package #:coalton-impl/parser/util/tests
  (:use
   #:cl
   #:coalton-impl/parser/util)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)))

(in-package #:coalton-impl/parser/util/tests)

(defun read-cst (string)
  (with-input-from-string (stream string)
    (coalton-impl/parser/reader:with-reader-context stream
      (coalton-impl/parser/reader:maybe-read-form stream))))

(defmacro with-test-form ((form string) &body body)
  `(with-cst-form (,form (read-cst ,string))
     ,@body))

(deftest test-cst-helper ()
  (with-test-form (form "(1)")
    (is (next-value-p form))
    (is (= (cst:raw (consume-value form)) 1))
    (is (not (next-value-p form))))

  (with-test-form (form "(a b c)")
    (is (equalp (collect-symbols form)
                '(a b c)))))

(let ((p (coalton-impl/parser/toplevel::make-toplevel-package :name "test")))
  (with-cst-form (form (read-cst "(import test)"))
    (coalton-impl/parser/toplevel::parse-import p form)))
