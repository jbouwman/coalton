(defpackage #:coalton-impl/generics
  (:use
   #:cl)
  (:export
   #:emit-load-form                     ; GENERIC
   #:emit-load-hash-form                ; FUNCTION
   #:emit-hash-form                     ; FUNCTION
   #:emit-load-forms                    ; FUNCTION
   ))

(in-package #:coalton-impl/generics)

;;;
;;; Generics
;;;

(defgeneric emit-load-form (self))

(defun emit-load-forms (list)
  `(list ,@(mapcar #'emit-load-form list)))

(defun emit-hash-form (hash-in)
  `(let ((hash-out (make-hash-table)))
     ,@(loop :for k :being :the :hash-keys :in hash-in
             :collect `(setf (gethash ',k hash-out)
                             ,(gethash k hash-in)))
     hash-out))

(defun emit-load-hash-form (hash-in)
  `(let ((hash-out (make-hash-table)))
     ,@(loop :for k :being :the :hash-keys :in hash-in
             :collect `(setf (gethash ',k hash-out)
                             ,(emit-load-form (gethash k hash-in))))
     hash-out))
