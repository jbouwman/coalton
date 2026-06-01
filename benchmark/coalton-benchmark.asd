;;; Per-phase baseline harness for the Coalton compiler (GOAL-025 step 1,
;;; kreisler PLAN-308). Independent of coalton/tests and the existing
;;; benchmarks: it drives the compiler over a fixed corpus and reports
;;; per-phase correctness and cost. Depends on the full coalton system so
;;; the prelude is in the global environment when corpus files are typechecked.

(asdf:defsystem "coalton-benchmark"
  :description "Per-phase correctness and performance baseline for the Coalton compiler."
  :license "MIT"
  :depends-on ("coalton")
  :pathname "."
  :serial t
  :components ((:file "package")
               (:file "measure")
               (:file "phases")
               (:file "oracle")
               (:file "harness")))
