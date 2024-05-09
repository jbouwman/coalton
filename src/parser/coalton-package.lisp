(defpackage #:coalton-impl/parser/package
  (:use
   #:cl
   #:coalton-impl/error
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:export
   #:build-package
   #:generate-package
   #:make-package-info
   #:package-info
   #:parse-package))

;;; Parser for Coalton package forms

(in-package #:coalton-impl/parser/package)

(defmacro parse-error (file span message note &optional help)
  `(error 'parse-error
          :err (coalton-error :span ,span
                              :file ,file
                              :message ,message
                              :primary-note ,note
                              :help-notes ,help)))

(defclass package-info ()
  ((name :initarg :name)
   (imports :initform nil)
   (exports :initform nil)))

(defmethod print-object ((self package-info) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (name imports exports) self
      (format stream "~A imports ~D exports ~D"
              name (length imports) (length exports)))))

(defun make-package-info (name)
  (make-instance 'package-info :name name))

(defclass import-info ()
  ((package :initarg :package)
   (symbols :initarg :symbols)))

(defun add-import (package-info package symbols)
  (push (make-instance 'import-info
          :package package
          :symbols symbols)
        (slot-value package-info 'imports)))

(defun add-export (package-info symbols)
  (with-slots (exports) package-info
    (setf exports (append exports symbols))))

(defun parse-symbol (sequence index file &key missing-error type-error)
  (let ((form (cst:nth index sequence)))
    (unless form
      (parse-error file (cst:source form)
                   "Malformed reference"
                   (or missing-error "missing symbol")))
    (unless (identifierp (cst:raw form))
      (parse-error file (cst:source form)
                   "Malformed reference"
                   (or type-error "Not a symbol")))
    (symbol-name (cst:raw form))))


(defun map-forms (f form)
  (loop :for clauses := form :then (cst:rest clauses)
        :while (cst:consp clauses)
        :collect (funcall f (cst:first clauses))))


(defun do-forms (f form)
  (loop :for clauses := form :then (cst:rest clauses)
        :while (cst:consp clauses)
        :do (funcall f (cst:first clauses))))


(defun parse-import-statement (package-info form file)
  (cond ((cst:consp form)
         (let ((source-package (parse-symbol form 0 file)))
           (flet ((parse-import-symbol (import)
                    (cond ((string= "*" (cst:raw import))
                           (return-from parse-import-statement
                             `(:import-all ,source-package)))
                          ((identifierp (cst:raw import))
                           (symbol-name (cst:raw import)))
                          (t
                           (parse-error file (cst:source import)
                                        "Malformed import statement"
                                        "Unrecognized import type")))))
             (add-import package-info
                        source-package
                        (map-forms #'parse-import-symbol (cst:rest form))))))
        (t
         (add-import package-info
                     (symbol-name (cst:raw form))
                     nil))))


(defun parse-package-clause (package-info clause file)
  "Parses a coalton package clause of the form of ({operation} {symbol}* ...)"
  (unless (cst:consp clause)
    (parse-error file (cst:source clause)
                 "Malformed package declaration"
                 "malformed package clause"))
  (let* ((head (cst:first clause))
         (body (cst:rest clause))
         (clause-type (cst:raw head)))
    (cond ((string= clause-type "IMPORT")
           (do-forms (lambda (form)
                       (parse-import-statement package-info form file))
             body))
          ((string= clause-type "EXPORT")
           (add-export package-info (map-forms (lambda (form)
                                                 (symbol-name (cst:raw form)))
                                               body)))
          (t
           (parse-error file (cst:source head)
                        "Malformed package declaration"
                        "Unknown package clause"
                        (list (make-coalton-error-help
                               :span (cst:source head)
                               :replacement #'identity
                               :message "Must be one of import or export")))))))


(defun parse-package (form file)
  "Parse a coalton package declaration in the form of (package {name})"

  ;; Package declarations must start with "PACKAGE"
  (unless (string= (cst:raw (cst:first form)) "PACKAGE")
    (parse-error file (cst:source (cst:first form))
                 "Malformed package declaration"
                 "package declarations must start with `package`"))

  ;; Package declarations must have a name
  (unless (cst:consp (cst:rest form))
    (parse-error file (cst:source (cst:first form))
                 "Malformed package declaration"
                 "missing package name"))

  ;; Remaining forms are package clauses
  (let ((name-form (cst:nth 1 form)))
    (unless name-form
      (parse-error file (cst:source name-form) "Malformed package declaration"
                   "missing package name"))
    (unless (identifierp (cst:raw name-form))
      (parse-error file (cst:source name-form) "Malformed package declaration"
                   "package name must be a symbol"))
    (let ((package-info (make-package-info (symbol-name (cst:raw name-form)))))
      (do-forms (lambda (form)
                  (parse-package-clause package-info form file))
        (cst:rest (cst:rest form)))
      package-info)))


(defun apply-import (package import)
  )

(defun apply-export (package export)
  )

;; (use-package (find-package name) package))
;; (intern sym (find-package name)))

(defun build-package (package-info)
  (with-slots (name imports exports) package-info
    (let ((package (uiop:ensure-package name)))
      (use-package (find-package "COALTON") package)
      (use-package (find-package "COALTON-PRELUDE") package)
      (dolist (import imports)
        (apply-import package import))
      (dolist (export exports)
        (apply-export package export))
      package)))

(defun generate-package (package-info)
  (with-slots (name import export) package-info
  `(defpackage ,name
     ;; ...
     )))
