;;;; Staged compiler driver.
;;;;
;;;; entry:entry-point bundles typecheck and codegen in one function and
;;;; parser:read-program bundles read and parse, so to time the phases
;;;; separately the harness reproduces the pipeline here with the boundaries
;;;; exposed. %typecheck below is a faithful copy of the body of
;;;; entry:entry-point (src/entry.lisp) up to, but not including, the
;;;; compile-translation-unit call; it is the one place this harness is
;;;; coupled to compiler internals and is expected to change in lockstep
;;;; with GOAL-025 steps 2, 3, and 5.

(in-package #:coalton-benchmark)

(defun parse-source (source)
  "Phase 1: read + parse SOURCE into a parser program AST."
  (with-open-stream (stream (source:source-stream source))
    (parser:with-reader-context stream
      (parser:read-program stream source ':file))))

(defstruct (tc-result (:constructor make-tc-result))
  "The output of the typecheck phase, threaded into codegen."
  translation-unit
  monomorphize-table
  inline-p-table
  env
  program)                              ; the renamed program

(defun %typecheck (program env)
  "Phase 2: run the typechecker over a freshly parsed PROGRAM against ENV.
Mirrors the body of entry:entry-point through analyze-translation-unit.
ENV is threaded functionally and is not mutated; the global environment is
left untouched, so this is repeatable across iterations."
  (let* ((*package* (parser:program-lisp-package program))
         (program (parser:rename-variables program)))

    (setf (parser:program-defines program)
          (tc:resolve-control-flow (parser:program-defines program)))
    (setf (parser:program-instances program)
          (tc:resolve-control-flow (parser:program-instances program)))

    (multiple-value-bind (type-definitions instances env)
        (tc:toplevel-define-type (parser:program-types program)
                                 (parser:program-structs program)
                                 (parser:program-type-aliases program)
                                 env)
      (multiple-value-bind (class-definitions env)
          (tc:toplevel-define-class (parser:program-classes program)
                                    env)
        (let ((all-instances
                (append instances
                        (parser:program-instances program)
                        (tc:derive-class-instances (parser:program-types program)
                                                   (parser:program-structs program)
                                                   env))))
          (multiple-value-bind (ty-instances env)
              (tc:toplevel-define-instance all-instances env)
            (multiple-value-bind (toplevel-definitions env)
                (tc:toplevel-define (parser:program-defines program)
                                    (parser:program-declares program)
                                    env)
              (multiple-value-bind (toplevel-instances)
                  (tc:toplevel-typecheck-instance ty-instances all-instances env)
                (setf env (tc:toplevel-specialize
                           (parser:program-specializations program) env))
                (let ((monomorphize-table (make-hash-table :test #'eq))
                      (inline-p-table (make-hash-table :test #'eq))
                      (translation-unit
                        (tc:make-translation-unit
                         :types type-definitions
                         :definitions toplevel-definitions
                         :classes class-definitions
                         :instances toplevel-instances
                         :lisp-forms (parser:program-lisp-forms program)
                         :package *package*)))
                  (loop :for define :in (parser:program-defines program)
                        :when (parser:toplevel-define-monomorphize define)
                          :do (setf (gethash (parser:node-variable-name
                                              (parser:toplevel-define-name define))
                                             monomorphize-table)
                                    t)
                        :when (parser:toplevel-define-inline define)
                          :do (setf (gethash (parser:node-variable-name
                                              (parser:toplevel-define-name define))
                                             inline-p-table)
                                    t))
                  (loop :for declare :in (parser:program-declares program)
                        :when (parser:toplevel-declare-monomorphize declare)
                          :do (setf (gethash (parser:identifier-src-name
                                              (parser:toplevel-declare-name declare))
                                             monomorphize-table)
                                    t)
                        :when (parser:toplevel-declare-inline declare)
                          :do (setf (gethash (parser:identifier-src-name
                                              (parser:toplevel-declare-name declare))
                                             inline-p-table)
                                    t))
                  (loop :for ty-instance :in ty-instances
                        :for method-codegen-inline-p
                          := (tc:ty-class-instance-method-codegen-inline-p ty-instance)
                        :do (loop :for (method-codegen-sym . inline-p)
                                    :in method-codegen-inline-p
                                  :do (when inline-p
                                        (setf (gethash method-codegen-sym inline-p-table) t))))
                  (analysis:analyze-translation-unit translation-unit env)
                  (make-tc-result :translation-unit translation-unit
                                  :monomorphize-table monomorphize-table
                                  :inline-p-table inline-p-table
                                  :env env
                                  :program program))))))))))

(defun typecheck-program (program)
  "Phase 2 against the pristine post-stdlib global environment."
  (%typecheck program entry:*global-environment*))

(defun codegen-tc-result (tcr)
  "Phase 3: lower a typecheck result to Lisp forms."
  (codegen:compile-translation-unit (tc-result-translation-unit tcr)
                                    (tc-result-monomorphize-table tcr)
                                    (tc-result-inline-p-table tcr)
                                    (tc-result-env tcr)))
