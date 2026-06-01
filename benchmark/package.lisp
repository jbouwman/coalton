(defpackage #:coalton-benchmark
  (:use #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:codegen #:coalton-impl/codegen)
   (#:analysis #:coalton-impl/analysis)
   (#:source #:coalton-impl/source)
   (#:entry #:coalton-impl/entry))
  (:export
   #:run-baseline                       ; FUNCTION -- run over the corpus, return results
   #:print-report                       ; FUNCTION -- human-readable per-phase table
   #:write-baseline                     ; FUNCTION -- pin correctness artifacts + cost to disk
   #:diff-baseline                      ; FUNCTION -- compare a fresh run against a pinned baseline
   #:main))                             ; FUNCTION -- script entry point
