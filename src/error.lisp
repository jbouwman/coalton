(defpackage #:coalton-impl/error
  (:use #:cl)
  (:export
   #:coalton-internal-condition         ; CONDITION
   #:coalton-base-error                 ; CONDITION
   #:coalton-base-warning               ; CONDITION
   #:coalton-warning-err                ; ACCESSOR
   #:coalton-internal-type-error        ; CONDITION
   #:coalton-file                       ; TYPE
   #:coalton-file-name                  ; ACCESSOR
   #:deferred-note                      ; MACRO
   #:make-coalton-error-note            ; FUNCTION
   #:make-coalton-error-help            ; FUNCTION
   #:make-coalton-error-context         ; FUNCTION
   #:make-coalton-file                  ; FUNCTION
   #:make-coalton-ide-file              ; FUNCTION
   #:make-coalton-string                ; FUNCTION
   #:*coalton-error-context*            ; VARIABLE
   #:coalton-error                      ; MACRO
   #:coalton-error-location             ; ACCESSOR
   #:coalton-error-file                 ; ACCESSOR
   #:display-coalton-error              ; FUNCTION
   ))

(in-package #:coalton-impl/error)


(define-condition coalton-internal-condition (error)
  ()
  (:documentation "An internal Coalton condition for use in signaling. Internal conditions should always be caught.")
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Unhandled internal condition.~%~%If you are seeing this, please file an issue on Github."))))

(define-condition coalton-internal-type-error (coalton-internal-condition)
  ())

(define-condition coalton-base-error (source-error:source-error)
  ())

(define-condition coalton-base-warning (source-error:source-warning)
  ())

;; Support direct compilation of files and strings, and slime's method
;; of writing out a tempfile containing a form taken from within an
;; original source file.

(defclass coalton-file () ())

(defclass coalton-source-file (coalton-file)
  ((source-filename :initarg :source-filename
                    :reader source-filename
                    :documentation "The file containing source to be compiled.")
   (original-filename :initarg :original-filename
                      :reader original-filename
                      :initform nil
                      :reader coalton-file-name
                      :documentation "The original name of the source file, if source was copied iinto place for compilation.")
   (original-position :initarg :original-position
                      :initform 0)))

(defun make-coalton-file (filename)
  (make-instance 'coalton-source-file
    :source-filename filename
    :original-filename filename))

(defun make-coalton-ide-file (source-filename original-filename original-position)
  (make-instance 'coalton-source-file
    :source-filename source-filename
    :original-filename original-filename
    :original-position original-position))

(defmethod source-error:source-name ((object coalton-source-file))
  (or (original-filename object)
      (source-filename object)))

(defmethod source-error:source-stream ((object coalton-source-file))
  (open (source-filename object)))

(defclass coalton-source-string (coalton-file)
  ((source-string :initarg :source-string
                  :reader source-string)))

(defmethod coalton-file-name ((object coalton-source-string))
  "string")

(defun make-coalton-string (string)
  (make-instance 'coalton-source-string :source-string string))

(defmethod source-error:source-name ((object coalton-source-string))
  "string input")

(defmethod source-error:source-stream ((object coalton-source-string))
  (make-string-input-stream (source-string object)))

(defvar *coalton-error-context* nil)

(defun coalton-error (&key (type :error) span highlight file message primary-note notes help-notes)
  (source-error:source-error :type type
                             :location (if (eql :end highlight)
                                           (cdr span)
                                           span)
                             :source file
                             :message message
                             :primary-note primary-note
                             :notes notes
                             :help help-notes
                             :context *coalton-error-context*))

(defun make-coalton-error-help (&key span replacement message)
  (source-error:help :span span :replacement replacement :message message))

(defun make-coalton-error-note (&key type span message)
  (source-error:note :type type :span span :message message))

(defun make-coalton-error-context (&key message)
  (source-error:context :message message))

(defmacro deferred-note (&rest args)
  (source-error:deferred-note `(lambda () (note ,@args))))
