(defpackage #:coalton-impl/parser/cursor
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:se #:source-error))
  (:export
   #:collect-symbols
   #:cursor-form
   #:do-every
   #:empty-p
   #:make-cursor
   #:make-note
   #:make-parse-error
   #:next-if
   #:next-symbol
   #:peek
   #:span-error
   #:syntax-error
   ))

(in-package #:coalton-impl/parser/cursor)

(defstruct (cursor
            (:constructor %make-cursor))
  form                                  ; current form
  pointer)                              ; pointer into current form

(defun make-cursor (form)
  (%make-cursor :form form :pointer form))

(defun peek (cursor)
  (cst:raw (cursor-pointer cursor)))

(defun peek-value (cursor)
  (cst:raw (cst:first (cursor-pointer cursor))))

(defstruct note
  span
  text
  (type ':primary))

(define-condition syntax-error (error)
  ((note :initarg :notes
         :reader error-notes)))

(defun primary-note (notes)
  (first notes))

(defun secondary-notes (notes)
  (remove-if (lambda (note)
               (eql ':help (note-type note)))
             (rest notes)))

(defun help-notes (notes)
  (remove-if-not (lambda (note)
                   (eql ':help (note-type note)))
                 notes))

(defun pointer-span (cursor)
  (declare (type cursor cursor))
  (let ((pointer (cursor-pointer cursor))
        (cons-p (has-cons-p cursor))
        (empty-p (empty-p cursor)))
    (cond ((and cons-p (not empty-p))
           (cst:source (cst:first pointer)))
          (cons-p
           (cons (1- (cdr (cst:source pointer)))
                 (cdr (cst:source pointer))))
          (t
           (cst:source pointer)))))

(declaim (inline span-error))
(defun span-error (span note)
  (error 'syntax-error :notes (list (make-note :span span
                                               :text note))))

(declaim (inline syntax-error))
(defun syntax-error (cursor note)
  (declare (type cursor cursor))
  (span-error (pointer-span cursor) note))

(defun make-parse-error (file syntax-error)
  (let* ((notes (error-notes syntax-error))
         (primary-note (primary-note notes)))
    (error 'coalton-impl/parser/base:parse-error
           :err (se:source-error :span (note-span primary-note)
                                 :file file
                                 :message "Malformed package declaration"
                                 :primary-note (note-text primary-note)
                                 :notes (secondary-notes notes)
                                 :help-notes (mapcar (lambda (note)
                                                       (se:make-source-error-help
                                                        :span (note-span note)
                                                        :replacement #'identity
                                                        :message (note-text note)))
                                                     (help-notes notes))))))

(defun %incf (cursor)
  (declare (type cursor cursor))
  (setf (cursor-pointer cursor)
        (cst:rest (cursor-pointer cursor))))

(defun has-cons-p (cursor)
  "T if CURSOR is pointing at a cons."
  (declare (type cursor cursor))
  (or (cst:consp (cursor-pointer cursor))
      (null (peek cursor))))
  
(defun empty-p (cursor)
  "T if CURSOR has no next value."
  (declare (type cursor cursor))
  (let ((pointer (cursor-pointer cursor)))
    (or (not (cst:consp pointer))
        (null (cst:first pointer)))))

(defun next-if (cursor pred)
  (when (not (has-cons-p cursor))
    (syntax-error cursor "not a list"))
  (when (empty-p cursor)
    (syntax-error cursor "attempt to read past end of list"))
  (let ((value (peek-value cursor)))
    (when (funcall pred value)
      (%incf cursor)
      value)))

(defun next-value (cursor)
  (when (not (has-cons-p cursor))
    (syntax-error cursor (or missing "not a list")))
  (when (empty-p cursor)
    (syntax-error cursor "attempt to read past end of list"))
  (let ((value (cst:first (cursor-pointer cursor))))
    (%incf cursor)
    value))

(defun next-symbol (cursor &key missing require message not-symbol)
  (when (not (has-cons-p cursor))
    (syntax-error cursor "not a list"))
  (when (empty-p cursor)
    (syntax-error cursor (or missing "attempt to read past end of list")))
  (let ((value (peek-value cursor)))
    (when (not (symbolp value))
      (syntax-error cursor (or not-symbol "not a symbol")))
    (when (and require (not (string-equal require value)))
      (syntax-error cursor (or message (format nil "expected ~A" require))))
    (when value
      (%incf cursor))
    value))

(defun collect-symbols (cursor)
  (loop :until (empty-p cursor)
        :collect (next-symbol cursor)))

(defun do-every (cursor fn)
  (loop :until (empty-p cursor)
        :do (funcall fn (make-cursor (next-value cursor)))))
