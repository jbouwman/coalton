(defpackage #:coalton-impl/compiler
  (:use
   #:cl)
  (:local-nicknames
   (#:codegen #:coalton-impl/codegen)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:entry #:coalton-impl/entry))
  (:export
   #:collector-result
   #:compile-toplevel
   #:generate-ast
   #:generate-code))

(in-package #:coalton-impl/compiler)

(defun process-source (source-stream source-name collector)
  (let* ((file (error:make-coalton-file :stream source-stream
                                        :name source-name))
         (program (parser:read-file source-stream file)))
    (entry:emit-prologue collector)
    (codegen:emit collector (parser:generate-package (parser:program-package-src program)))
    (entry:entry-point program collector)))

(defgeneric collector-result (collector)
  (:method (collector)
    (declare (ignore collector))
    (values)))

(defun emit-env (name args)
  (ecase name
    ((coalton-impl/typechecker/environment:set-function-source-parameter-names
      coalton-impl/typechecker/environment:set-method-inline)
     (destructuring-bind (key value) args
       `(setf entry:*global-environment*
              (,name entry:*global-environment*',key ',value))))
    ((coalton-impl/typechecker/environment:add-instance
      coalton-impl/typechecker/environment:set-class
      coalton-impl/typechecker/environment:set-code
      coalton-impl/typechecker/environment:set-constructor
      coalton-impl/typechecker/environment:set-function
      coalton-impl/typechecker/environment:set-name
      coalton-impl/typechecker/environment:set-type
      coalton-impl/typechecker/environment:set-value-type)
     (destructuring-bind (key value) args
       `(setf entry:*global-environment*
              (,name entry:*global-environment*',key
                     ,(coalton-impl/generics::emit-load-form value)))))))

(defclass compile-toplevel ()
  ((forms :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod codegen:emit ((collector compile-toplevel) form)
  (vector-push-extend form (slot-value collector 'forms)))

(defmethod codegen:emit-env ((collector compile-toplevel) name args)
  (codegen:emit collector (emit-env name args)))

(defmethod collector-result ((collector compile-toplevel))
  `(progn ,@(coerce (slot-value collector 'forms) 'list)))

(defclass %compile-file ()
  ((stream :initarg :stream)))

(defmethod codegen:emit ((collector %compile-file) form)
  (with-slots (stream) collector
    (let ((*package* (find-package :cl))
          (*print-case* :downcase)
          (*print-circle* nil))
      (prin1 form stream)
      (terpri stream)
      (terpri stream))))

(defmethod codegen:emit-comment ((collector %compile-file) string)
  (with-slots (stream) collector
    (format stream "~%;; ~A~%~%" string)))

(defmethod codegen:emit-env ((collector %compile-file) name args)
  (codegen:emit collector (emit-env name args)))

(defclass generate-code ()
  ((forms :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod codegen:emit ((collector generate-code) form)
  (vector-push-extend form (slot-value collector 'forms)))

(defmethod codegen:emit-env ((collector generate-code) name args)
  (codegen:emit collector (emit-env name args)))

(defmethod collector-result ((collector generate-code))
  (util:runtime-quote (coerce (slot-value collector 'forms) 'list)))

(defclass generate-ast ()
  ((stream :initarg :stream)))

(defmethod codegen:emit-ast ((collector generate-ast) name type value)
  (with-slots (stream) collector
    (format stream "~A :: ~A~%~A~%~%~%" name type value)))

(defun generate-ast (stream)
  (parser:with-reader-context stream
    (with-output-to-string (ast-stream)
      (let ((collector (make-instance 'generate-ast :stream ast-stream)))
        (process-source stream "string" collector)
        (collector-result collector))))) ; result is stringified ast

(defun %compile-file (stream lisp-stream)
  (parser:with-reader-context stream
    (let ((collector (make-instance '%compile-file :stream lisp-stream)))
      (codegen:emit-comment collector "Generated from XYZ")
      (setf entry:*global-environment* (process-source stream "string" collector))
      t)))
