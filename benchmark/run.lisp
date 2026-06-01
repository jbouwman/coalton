;;;; Load the benchmark harness and run a baseline.
;;;; Usage (from the coalton repo root, inside `nix develop`):
;;;;   sbcl --script benchmark/run.lisp [baseline-output-path]
;;;; or to diff against a pinned baseline:
;;;;   sbcl --non-interactive --eval '(load "benchmark/run.lisp")' ... (see Makefile)

(require :asdf)

(let* ((here (or *load-truename* *compile-file-truename*
                 (truename "benchmark/run.lisp")))
       (asd (merge-pathnames "coalton-benchmark.asd" here)))
  (asdf:load-asd asd))

(handler-case
    (asdf:load-system :coalton-benchmark)
  (error (e)
    (format *error-output* "~&Failed to load coalton-benchmark: ~A~%" e)
    (sb-ext:exit :code 1)))

(funcall (find-symbol "MAIN" "COALTON-BENCHMARK"))
(sb-ext:exit :code 0)
