;;; This package provides utilities for incremental consumption of
;;; concrete syntax tree values.  A cursor struct maintains a
;;; reference to the current value and a pointer within it, if it is
;;; cons-valued. The functions 'next' and 'empty-p' are sufficient to
;;; build more specialized parsing functions that don't need knowledge
;;; of the cst package.

(defpackage #:coalton-impl/parser/cursor
  (:use
   #:cl)
  (:shadow
   #:error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util))
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:atom-p
   #:collect-symbols
   #:cursor-location
   #:cursor-message
   #:cursor-pointer
   #:cursor-source
   #:cursor-value
   #:discard-symbol
   #:each
   #:each-value
   #:empty-p
   #:error
   #:make-cursor
   #:next
   #:next-symbol
   #:peek))

(in-package #:coalton-impl/parser/cursor)

;; The value field can be any type returned by cst, generally a cons
;; or atom.  When value is a cst:cons, pointer initially points to
;; that value, and is destructively updated as values are 'popped'.

(defstruct (cursor (:constructor %make-cursor))
  "A CST node-valued cursor."
  (value   (util:required 'value)   :type cst:cst)           ; current value
  (pointer (util:required 'value)   :type cst:cst)           ; pointer into current value
  (last    nil                      :type (or null cst:cst)) ; pointer to most recently consumed value
  (source  (util:required 'source))                          ; the source:location of cursor
  (message (util:required 'message) :type string))           ; a message providing context for errors

;;; The implementation of source:location for a cursor returns the
;;; entire span of the cursor.

(defmethod source:location ((self cursor))
  (source:make-location (cursor-source self)
                        (cst:source (cursor-value self))))

(defun make-cursor (value source message)
  "Make a cursor that points at VALUE."

  (%make-cursor :value value
                :pointer value
                :source source
                :message message))

(defun peek (cursor &key (unwrap t))
  "Peek at the value of CURSOR without changing any state."
  (declare (type cursor cursor))
  (unless (empty-p cursor)
    (let ((value (cst:first (cursor-pointer cursor))))
      (when unwrap
        (setf value (cst:raw value)))
      value)))

(defun atom-p (cursor)
  "Return T if cursor is pointing at an atom."
  (not (consp (cst:raw (cursor-pointer cursor)))))

(defun empty-p (cursor)
  "T if CURSOR has no next value."
  (declare (type cursor cursor))
  (let ((pointer (cursor-pointer cursor)))
    (or (not (cst:consp pointer))
        (null (cst:first pointer)))))

(defun cursor-location (cursor)
  "Return the location of the value that CURSOR is pointing at."
  (source:make-location (cursor-source cursor)
                        (cond ((cursor-last cursor)
                               (cst:source (cursor-last cursor)))
                              (t
                               (let ((s (cst:source (cursor-value cursor))))
                                 (cons (1+ (car s))
                                       (1+ (car s))))))))

(defun error (cursor position note)
  "Signal a PARSE-ERROR related to the current value of a cursor.
If END is T, indicate the location directly following the current value."
  (parse-error (cursor-message cursor)
               (source:note
                (ecase position
                  (:last (cursor-location cursor))
                  (:next (source:make-location (cursor-source cursor)
                                               (cst:source (cst:first (cursor-pointer cursor)))))
                  (:form (source:make-location (cursor-source cursor)
                                               (cst:source (cursor-value cursor))))
                  (:after-last (source:end-location (cursor-location cursor))))
                note)))

(defun next (cursor &key (pred nil) (unwrap t))
  "Return the next value from a nonempty cursor.

If PRED is non-NIL, only consume a value if it is true.
If UNWRAP is NIL, return the CST node, otherwise, return the raw value."
  (declare (type cursor cursor))
  (when (empty-p cursor)
    ;; Finding empty-p = t here this would indicate that the compiler
    ;; writer hasn't checked for emptiness in the calling context in
    ;; order to construct a more specific error message.
    (error cursor ':after-last "attempt to read past end of list"))
  (let ((value (cst:first (cursor-pointer cursor))))
    (when (or (null pred)
              (funcall pred (cst:raw value)))
      (setf (cursor-pointer cursor)
            (cst:rest (cursor-pointer cursor))
            (cursor-last cursor)
            value)
      (if unwrap (cst:raw value) value))))

(defun each (cursor fn)
  (loop :until (empty-p cursor)
        :do (funcall fn (next cursor :unwrap t))))

;;; Utilities

(defun next-symbol (cursor missing not-symbol)
  (when (empty-p cursor)
    (error cursor ':after-last missing))
  (let ((name (next cursor)))
    (unless (symbolp name)
      (error cursor ':last not-symbol))
    name))

(defun discard-symbol (cursor &optional symbol message)
  (let ((s (next-symbol cursor (or message (format nil "expected ~A" symbol))
                        (or message "must be a symbol"))))
    (when (and symbol (not (string-equal s symbol)))
      (error cursor ':last (or message (format nil "expected ~A" symbol))))
    s))

(defun collect-symbols (cursor)
  (loop :until (empty-p cursor)
        :unless (let ((value (peek cursor)))
                  (and value (symbolp value)))
          :do (error cursor ':next "expected symbol")
        :collect (next cursor :unwrap t)))

(defun each-value (cursor f)
  "For each element in cons-valued CURSOR, create a subcursor and apply F."
  (loop :until (empty-p cursor)
        :do (let* ((value (next cursor :unwrap nil))
                   (cursor (%make-cursor :value value
                                         :pointer value
                                         :source (cursor-source cursor)
                                         :message (cursor-message cursor))))
              (funcall f cursor))))
