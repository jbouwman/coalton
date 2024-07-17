;;; Utilities for working with CST nodes

(defpackage #:coalton-impl/parser/util
  (:use
   #:cl)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:se #:source-error)
   (#:util #:coalton-impl/util))
  (:export
   ))

(in-package #:coalton-impl/parser/util)

(declaim (inline package-syntax-error-span))
(defun package-syntax-error-span (span file note)
  (error 'parse-error
         :err (se:source-error :span span
                               :file file
                               :message "Malformed package declaration"
                               :primary-note note)))

(declaim (inline package-syntax-error))
(defun package-syntax-error (form file note)
  (package-syntax-error-span (cst:source form) file note))

(declaim (inline make-iter))
(defun make-iter (form file)
  (cons form file))

(defun next-value (iter)
  "Return the next value from iterator ITER. The underlying value must be a nonempty cons cell."
  (destructuring-bind (form . file) iter
    (unless (cst:consp form)
      (package-syntax-error form file "expected a list"))
    (let ((value (cst:first form)))
      (unless value
        (package-syntax-error form file "empty list"))
      (values value (cst:rest form)))))

(defun next-value-p (iter)
  "Return T if the next value of iterator ITER is a nonempty cons cell."
  (and (cst:consp (car iter))
       (cst:first (Car iter))))

(defun consume-value (iter)
  "Consume and return the next value in ITER."
  (multiple-value-bind (value rest) (next-value iter)
    (rplaca iter rest)
    value))

(defun consume-symbol (iter &key missing not-symbol)
  "Return the next value in ITER, a CST:NODE. If the next value is not a symbol, signal PARSE-ERROR."
  (let ((end-of-list (cdr (cst:source (car iter)))))
    (unless (next-value-p iter)
      (package-syntax-error-span (cons (1- end-of-list) end-of-list)
                                 (cdr iter)
                                 (or missing "expected a symbol")))
    (let ((value (consume-value iter)))
      (unless (identifierp (cst:raw value))
        (package-syntax-error-span (cst:source value)
                                   (cdr iter)
                                   (or not-symbol "expected a symbol")))
      value)))

(defun require-symbol (iter value &optional message)
  "Consume the next symbol in ITER, requiring it to have VALUE."
  (let ((symbol (consume-symbol iter)))
    (unless (string-equal value (cst:raw symbol))
      (package-syntax-error symbol (cdr iter)
                            (or message
                                (format nil "expected '~A'" value))))
    symbol))

(defun consume-string (iter &key optional)
  "Return the next value in ITER as a string. If next value is not a string and optional is null, signal PARSE-ERROR."
  (let ((value (and (next-value-p iter)
                    (next-value iter))))
    (cond ((stringp (and value (cst:raw value)))
           (consume-value iter)
           (cst:raw value))
          ((not optional)
           (package-syntax-error (car iter) (cdr iter) "expected a string")))))

(defun collect-symbols (iter)
  (loop :while (next-value-p iter)
        :collect (symbol-name (cst:raw (consume-symbol iter)))))

(defstruct cst-form
  form
  pointer
  context)

(defvar *current-form*)

(defmacro with-cst-form ((var form context) &body body)
  `(let* ((*current-form* (make-cst-form :form form :pointer form :context context))
          (,var *current-form*))
     ,@body))
