;;;; Run the coalton/tests fiasco suite as a GOAL-025 regression check.
;;;;
;;;; Usage (from the coalton repo root, inside `nix develop`, with a
;;;; recursive CL_SOURCE_REGISTRY so the example test systems resolve):
;;;;   sbcl --script benchmark/run-tests.lisp
;;;;
;;;; The dev shell pins the kreisler "epsilon" SBCL fork (2.6.4); the
;;;; suite passes there, including tail-call-elimination-test1 (a
;;;; 10,000,000-deep mutual recursion that needs the typeclass-mutual-
;;;; recursion tail-call elimination fixed upstream in SBCL 2.5.7.38).

(require :asdf)

(handler-case
    (asdf:load-system :coalton/tests)
  (error (e)
    (format *error-output* "~&Failed to load coalton/tests: ~A~%" e)
    (finish-output *error-output*)
    (sb-ext:exit :code 2)))

(let ((ok (funcall (find-symbol "RUN-COALTON-TESTS" "COALTON-TESTS"))))
  (format t "~&;; coalton/tests result: ~:[FAIL~;PASS~]~%" ok)
  (finish-output)
  (sb-ext:exit :code (if ok 0 1)))
