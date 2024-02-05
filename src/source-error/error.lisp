(in-package #:source-error)

;; Source-aware error and warning condition types and helper functions
;;
;; These are the condition classes used by applications. Error and
;; warning conditions both have an `error` slot of type
;; `error-info` that contains detailed information about the
;; source of the error and optional explanatory notes and
;; messages. `print-source-error` uses this structure to emit an
;; annotated error description when either of these conditions is
;; printed.

;; source info protocol
;;
;; The generic functions `source-name` and `source-stream` define a
;; protocol that gives the condition printer access to an input source
;; related to the condition, via the `source` slot on a
;; error-info struct (see below). These provide, respectively,
;; the name, and contents of a source.

;; source annotation structures
;;
;; These are classes that hold for input span-associated error
;; annotations. The top-level `error-info` structure may
;; contain sets of optional `note` and
;; `source-error-help` messages that refer to source spans, which
;; `print-source-error` will resolve to character ranges in an input
;; source.

(defun report-error (condition stream)
  (with-slots (error) condition
    (print-error stream error)))

(define-condition source-error (error)
  ((error :initarg :error))
  (:documentation "The type for user-facing errors.")
  (:report report-error))

(define-condition source-warning (style-warning)
  ((error :initarg :error))
  (:documentation "The type for user-facing warnings.")
  (:report report-error))

(defclass span-mixin ()
  ((span :initarg :span)))

(defun span-start (span-mixin)
  (car (slot-value span-mixin 'span)))

(defun span-end (span-mixin)
  (cdr (slot-value span-mixin 'span)))

(defclass note (span-mixin)
  ((type :initarg :type
         :reader note-type)
   (message :initarg :message)))

(defclass help (span-mixin)
  ((replacement :initarg :replacement)
   (message :initarg :message)))

(defclass deferred-note ()
  ((fn :initarg :fn)
   (rendered)))

(defclass error-context ()
  ((message :initarg :message
            :reader error-context-message)))

(defclass error-info ()
  ((type :initarg :type)
   (source :initarg :source
           :reader error-source)
   (location :initarg :location)
   (message :initarg :message)
   (notes :initarg :notes
          :reader notes)
   (help :initarg :help)
   (context :initarg :context
            :reader error-context)))

(defmethod initialize-instance :after ((error-info error-info) &rest initargs)
  (declare (ignore initargs))
  (with-slots (notes) error-info
    (setf notes (stable-sort notes #'< :key #'span-start))))

;; Public functions

(defun note (&key span type message)
  (make-instance 'note
    :span span
    :type type
    :message message))

(defun help (&key span replacement message)
  (make-instance 'help
    :span span
    :replacement replacement
    :message message))

(defun deferred-note (fn)
  (make-instance 'deferred-note :fn fn))

(defun make-deferred-error-note (fn)
  (make-instance 'deferred-note :fn fn))

(defun context (&key message)
  (make-instance 'error-context :message message))

(defun source-error (&key (type :error) source location message primary-note notes help context)
  "Construct an ERROR-INFO instance with a message and primary note attached to the provided form.

MESSAGE and PRIMARY-NOTE must be supplied string arguments.
NOTES and HELP may optionally be supplied notes and help messages."
  (let ((span (etypecase location
                (cons location)
                (integer (cons (1- location) location)))))
    (make-instance 'error-info
      :type type
      :source source
      :location (etypecase location
                  (cons (car location))
                  (integer location))
      :message message
      :notes (list* (note :type :primary
                          :span span
                          :message primary-note)
                    notes)
      :help help
      :context context)))

(defgeneric source-name (source)
  (:documentation "Returns a string that names a source, for reporting in conditions. In
the case of files, this is the filename path."))

(defgeneric source-stream (source)
  (:documentation "A seekable stream providing access to the original contents of a
source, for reporting in condition printers. WARNING: it is the
responsibility of the caller to close the stream, if not using the
with-source-stream convenience macro."))

(defmacro with-source-stream ((stream error-info) &body body)
  `(with-open-stream (,stream (source-stream (error-source ,error-info)))
    ,@body))
