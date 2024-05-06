(defpackage #:coalton-impl/generics
  (:use #:cl)
  (:export
   #:make-source-alist
   #:make-source-form
   #:make-source-hash))

(in-package #:coalton-impl/generics)

(defgeneric make-source-form (object)
  (:documentation "emit the source form of an object: a form which, when evaluated, will rebuild the input"))

(defmethod make-source-form ((object symbol))
  `',object)

(defmethod make-source-form ((object number))
  object)

(defmethod make-source-form (object)
  (error "Refusing to dump type ~A" (type-of object)))

(defmethod make-source-form ((self cons))
  `(list ,@(mapcar #'make-source-form self)))

(defmethod make-source-form ((self hash-table))
  `(let ((hash-out (make-hash-table)))
     ,@(loop :for k :being :the :hash-keys :in self
             :collect `(setf (gethash ',k hash-out)
                             ,(make-source-form (gethash k self))))
     hash-out))

(defun make-source-hash (self)
  `(let ((hash-out (make-hash-table)))
     ,@(loop :for k :being :the :hash-keys :in self
             :collect `(setf (gethash ',k hash-out)
                             ,(gethash k self)))
     hash-out))

(defun make-source-alist (list-in)
  `(let ((list-out nil))
    ,@(loop :for (k . v) :in list-in
            :collect `(setf list-out (cons (cons ',k ,(make-source-form v)) list-out)))
     list-out))
