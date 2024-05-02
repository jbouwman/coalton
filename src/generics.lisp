(defpackage #:coalton-impl/generics
  (:use #:cl)
  (:export
   #:make-source-form))

(in-package #:coalton-impl/generics)

(defgeneric make-source-form (object)
  (:documentation "emit the source form of an object: a form which, when evaluated, will rebuild the input"))

(defmethod make-source-form ((object symbol))
  `',object)

(defmethod make-source-form (object)
  (error "Refusing to dump type ~A" (type-of object)))
