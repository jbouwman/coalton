(defpackage #:coalton-impl/generics
  (:use
   #:cl)
  (:export
   #:make-source-form))

(in-package #:coalton-impl/generics)

(defgeneric make-source-form (object))
