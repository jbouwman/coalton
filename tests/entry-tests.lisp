(in-package #:coalton-tests)

(defun compile-test-file ()
  (test-file "examples/small-coalton-programs/src/fact-fib.coal"))

(deftest test-compile-to-lisp ()
  "Test that the Coalton compiler compiles a test file into something that looks like Lisp source."
  (with-open-file (input (compile-test-file) :direction ':input)
    (let ((source (entry:codegen input "test")))
      (with-input-from-string (stream source)
        (let ((form-types (loop :for form := (read stream nil nil) :while form :collect (first form))))
          (dolist (expect-type '(defpackage in-package defun eval-when let setf))
            (is (position expect-type form-types)
                "Missing expected ~A form in generated code" expect-type)))))))

(deftest test-compile-to-fasl ()
  "Test that the Coalton compiler compiles a test file into a working .fasl that persists environment updates."
  (flet ((test-sym ()
           (let ((p (find-package "SMALL-COALTON-PROGRAMS/FACT-FIB")))
             (when p
               (intern "FACT" p)))))
    (let ((fact (test-sym)))
      (fmakunbound fact)
      (setf entry:*global-environment* (tc:unset-function entry:*global-environment* fact)))
    (with-open-file (stream (compile-test-file))
      (entry:compile stream "test" :load t)
      (let ((fact (test-sym)))
        (is (fboundp fact)
            "Test function was bound as side effect of loading fasl")
        (is (= 120 (funcall fact 5))
            "Test function is callable")
        (is (equalp (tc:make-function-env-entry :name fact :arity 1)
                    (tc:lookup-function entry:*global-environment* fact :no-error t))
            "Environment was restored")))))
