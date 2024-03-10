(defpackage #:coalton-impl/error
  (:use
   #:cl)
  (:export
   #:coalton-internal-condition
   #:coalton-error
   #:coalton-warning
   #:coalton-internal-type-error))

(in-package #:coalton-impl/error)

(define-condition coalton-internal-condition (error)
  ()
  (:documentation "An internal Coalton condition for use in signaling. Internal conditions should always be caught.")
  (:report
   (lambda (c s)
     (declare (ignore c))
     (format s "Unhandled internal condition.~%~%If you are seeing this, please file an issue on Github."))))

(define-condition coalton-internal-type-error (coalton-internal-condition)
  ())

(define-condition coalton-error (source-error:source-error)
  ())

(define-condition coalton-warning (source-error:source-warning)
  ())
