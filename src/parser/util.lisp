;;; Utilities for working with CST nodes

(defpackage #:coalton-impl/parser/util
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:export
   #:collect-symbols
   #:cons-form-p
   #:consume-string
   #:consume-symbol
   #:consume-value
   #:cst-form
   #:cst-form-pointer
   #:next-value
   #:next-value-p
   #:require-symbol
   #:symbol-form-p
   #:syntax-error
   #:syntax-error-span
   #:with-cst-form))

(in-package #:coalton-impl/parser/util)

(defstruct cst-form
  form                                  ; the entire form that is being read
  pointer)                              ; current pointer when reading sequentially

(define-condition syntax-error (error)
  ((span :initarg :span
         :reader error-span)
   (note :initarg :note
         :reader error-note)))

(defun syntax-error-span (span note)
  (error 'syntax-error
         :span span
         :note note))

(defun syntax-error (form note &key pointer)
  (declare (type cst-form form))
  (let ((span (cst:source (or pointer (cst-form-pointer form)))))
    (syntax-error-span span note)))

(defun next-value (form)
  "Return the next value from iterator FORM."
  (declare (type cst-form form))
  (let ((pointer (cst-form-pointer form)))
    (unless (cst:consp pointer)
      (syntax-error-span (cons (cdr (cst:source pointer))
                               (cdr (cst:source pointer)))
                         "attempt to read past end of list"))
    (let ((value (cst:first pointer)))
      (unless value
        (syntax-error form "empty list"))
      (values value (cst:rest pointer)))))

(defun next-value-p (form)
  "Return T if the next value of iterator CST-FORM is a nonempty cons cell."
  (declare (type cst-form form))
  (let ((pointer (cst-form-pointer form)))
    (and (cst:consp pointer)
         (cst:first pointer))))

(defun consume-value (form)
  "Consume and return the next value in FORM."
  (declare (type cst-form form))
  (multiple-value-bind (value rest)
      (next-value form)
    (setf (cst-form-pointer form) rest)
    value))

(defun %consume-symbol (form missing not-symbol)
  "Return the next value in FORM, a CST:NODE. If the next value is not a symbol, signal PARSE-ERROR."
  (declare (type cst-form form))
  (let ((end-of-list (cdr (cst:source (cst-form-pointer form)))))
    (unless (next-value-p form)
      (syntax-error-span (cons (1- end-of-list) end-of-list)
                         (or missing "expected a symbol")))
    (let ((value (consume-value form)))
      (unless (symbolp (cst:raw value))
        (syntax-error form
                      (or not-symbol "expected a symbol")
                      :pointer value))
      value)))

(defun consume-symbol (form &key missing not-symbol)
  "Return the next value in FORM, a CST:NODE. If the next value is not a symbol, signal PARSE-ERROR."
  (declare (type cst-form form))
  (cst:raw (%consume-symbol form missing not-symbol)))

(defun require-symbol (form value &optional message)
  "Consume the next symbol in FORM, requiring it to have VALUE."
  (declare (type cst-form form))
  (let ((symbol (%consume-symbol form message message)))
    (unless (string-equal value (cst:raw symbol))
      (syntax-error form
                    (or message
                        (format nil "expected '~A'" value))
                    :pointer symbol))
    symbol))

(defun consume-string (form &key optional)
  "Return the next value in FORM as a string. If next value is not a string and optional is null, signal PARSE-ERROR."
  (declare (type cst-form form))
  (let ((value (and (next-value-p form)
                    (next-value form))))
    (cond ((stringp (and value (cst:raw value)))
           (consume-value form)
           (cst:raw value))
          ((not optional)
           (syntax-error form "expected a string")))))

(defun collect-symbols (form)
  (declare (type cst-form form))
  (loop :while (next-value-p form)
        :collect (consume-symbol form)))

(defun cons-form-p (form)
  (declare (type cst-form form))
  (cst:consp (cst-form-pointer form)))

(defun symbol-form-p (form)
  (declare (type cst-form form))
  (symbolp (cst:raw (cst-form-pointer form))))

(defmacro with-cst-form ((form cst) &body body)
  `(let ((,form (make-cst-form :form ,cst :pointer ,cst)))
     ,@body))
