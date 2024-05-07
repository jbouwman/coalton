(defpackage #:coalton-impl/compiler
  (:use
   #:cl)
  (:shadow
   #:compile
   #:compile-file)
  (:local-nicknames
   (#:codegen #:coalton-impl/codegen)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:entry #:coalton-impl/entry))
  (:export
   #:codegen-ast-result
   #:codegen-result
   #:compile
   #:compile-toplevel
   #:generate-ast
   #:generate-code
   #:toplevel-result))

;;; Tailored compiler output
;;;
;;; The classes defined below implement the code generator's
;;; 'collector' protocol (codegen:emit, codegen:emit-env) in order to
;;; provide varied compiler output modes supporting:
;;;
;;; - The coalton-toplevel macro
;;; - Direct compilation of .coalton files
;;; - Raw code generation
;;; - AST printing

(in-package #:coalton-impl/compiler)

;;; class: compile-toplevel

;; Collect and emit the environment and code created by the
;; coalton-toplevel macro.
;;
;; The Coalton environment is serialized by quoting arguments that
;; implement make-load-form: see implementations of make-load-form for
;; the structures defined in typechecker/environment, codegen/types
;; and elsewhere
;;
;; When files are loaded after compilation, the environment
;; definitions are immediately replayed -- harmless, but doesn't need
;; to happen and might justify making load behavior configurable with
;; a dynamic variable.

(defun %adjustable-array ()
  "Return a zero length extensible array."
  (make-array 0 :adjustable t :fill-pointer 0))

(defclass compile-toplevel ()
  ((env :initform (%adjustable-array))
   (code :initform (%adjustable-array)))
  (:documentation
   "Compiler output collector that generates the value of the coalton-toplevel macro."))

;; Collect a compiled lisp form

(defmethod codegen:emit ((collector compile-toplevel) form)
  (vector-push-extend form (slot-value collector 'code)))

;; Collect an environment entry. Repeated code blocks emitted before
;; and during optimization are deduplicated at output time.

(defmethod codegen:emit-env ((collector compile-toplevel) name args)
  (vector-push-extend (cons name args) (slot-value collector 'env)))

(defun export-env-update (update)
  "Wrap an environment update in a form that destructively sets the global environment."
  (destructuring-bind (name . args) update
    `(setf entry:*global-environment*
           (,name entry:*global-environment*
                  ,@(mapcar #'util:runtime-quote args)))))

(defun code-update-eql (a b)
  "Compare environment updates, returning t for set-code updates of the same symbol."
  (and (eql (first a) 'coalton-impl/typechecker/environment:set-code)
       (eql (first b) 'coalton-impl/typechecker/environment:set-code)
       (eql (second a)
            (second b))))

(defun toplevel-result (collector)
  "Return the result of evaluating a coalton-toplevel form."
  (with-slots (env code) collector
    (cons 'progn
          (append (mapcar #'export-env-update
                          (remove-duplicates (coerce env 'list) :test #'code-update-eql))
                  (coerce code 'list)))))

;;; class: compile-file

;; Generate lisp source from coalton source. This is similar to
;; compile-toplevel, but serializes environment updates in a completely
;; different way, by emitting source forms, rather than load forms

(defclass compile-file ()
  ((stream :initarg :stream)))

(defmethod codegen:emit ((collector compile-file) form)
  (with-slots (stream) collector
    (let ((*package* (find-package :cl))
          (*print-case* :downcase)
          (*print-circle* nil))
      (prin1 form stream)
      (terpri stream)
      (terpri stream))))

(defmethod codegen:emit-comment ((collector compile-file) string)
  (with-slots (stream) collector
    (format stream "~%;; ~A~%~%" string)))

(defun set-value-type! (name value)
  (setf entry:*global-environment*
        (coalton-impl/typechecker:set-value-type entry:*global-environment*
                                                 name value)))

(defmethod codegen:emit-env ((collector compile-file) name args)
  (case name
    (coalton-impl/typechecker::set-value-type
     (destructuring-bind (name type) args
       (codegen:emit-comment collector (format nil "env: set-value-type ~A" name))
       (break)
       (codegen:emit collector `(set-value-type! ',name ',type))))
    (t
     ;; fixme -- source-persistable representation of environment modification
     (codegen:emit-comment collector (format nil "env: ~A" name))
     (codegen:emit collector
                   `(setf entry:*global-environment*
                          (,name entry:*global-environment*
                                 ,@(mapcar #'util:runtime-quote args)))))))

;;; class: generate-code

;; Collect generated code, ignoring environment updates, comments, etc.

(defclass generate-code ()
  ((forms :initform (%adjustable-array))))

(defmethod codegen:emit ((collector generate-code) form)
  (vector-push-extend form (slot-value collector 'forms)))

(defun codegen-result (collector)
  (util:runtime-quote (coerce (slot-value collector 'forms) 'list)))

;;; class: generate-ast

;; Collects AST, by emitting printed versions to a stream. Ignores all
;; other compiler activity.

(defclass generate-ast ()
  ((stream :initarg :stream)))

(defmethod codegen:emit-ast ((collector generate-ast) name type value)
  (with-slots (stream) collector
    (format stream "~A :: ~A~%~A~%~%~%" name type value)))

;;; compiler entry points

(defun %process-source (stream name collector)
  "Helper: read Coalton source from STREAM and process it with COLLECTOR.

NAME provides information on the stream's origin in error messages."
  (let* ((file (error:make-coalton-file :stream stream
                                        :name name))
         (program (parser:read-file stream file)))
    (entry:emit-prologue collector)
    (codegen:emit collector
                  (parser:generate-package (parser:program-package-src program)))
    (entry:entry-point program collector)))

(defun compile (input-stream output-stream)
  "Read Coalton source from INPUT-STREAM and write Lisp source to OUTPUT-STREAM."
  (parser:with-reader-context input-stream
    (let ((collector (make-instance '%compile-file :stream output-stream)))
      (codegen:emit-comment collector "Generated from XYZ")
      (setf entry:*global-environment* (%process-source input-stream "string" collector))
      (values))))

(defun compile-file (input-file output-file)
  (with-open-file (input-stream input-file
                          :direction :input
                          :element-type 'character)
    (with-open-file (output-stream output-file
                                   :direction :output
                                   :element-type 'character
                                   :if-exists :supersede)
      (compile input-stream output-stream))))

(defun compile-example ()
  (compile "/Users/jbouwman/git/coalton/diff.coalton"
           "/Users/jbouwman/git/coalton/diff.lisp"))

(defun generate-ast (stream)
  "Read Coalton source from STREAM and return string representation of ast."
  (parser:with-reader-context stream
    (with-output-to-string (ast-stream)
      (let ((collector (make-instance 'generate-ast :stream ast-stream)))
        (%process-source stream "string" collector)))))
