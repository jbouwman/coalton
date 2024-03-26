(defpackage #:coalton-impl/parser/module
  (:use
   #:cl
   #:coalton-impl/error
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:a #:alexandria)
   (#:cst #:concrete-syntax-tree))
  (:export
   #:parse-package))

(in-package #:coalton-impl/parser/module)

(defun mapcar-form (f form)
  (loop :for clauses := form :then (cst:rest clauses)
        :while (cst:consp clauses)
        :collect (funcall f (cst:first clauses))))

(defmacro parse-error (form file message note &optional help)
  `(error 'parse-error
          :err (coalton-error :span (cst:source ,form)
                              :file ,file
                              :message ,message
                              :primary-note ,note
                              :help-notes ,help)))

(defun parse-symbol (sequence index file)
  (let ((form (cst:nth index sequence)))
    (unless form
      (parse-error form file "Malformed reference" "Missing symbol"))
    (unless (identifierp (cst:raw form))
      (parse-error form file "Malformed reference" "Not a symbol"))
    (symbol-name (cst:raw form))))

(defun parse-import-statement (form file)
  (let ((source-package (parse-symbol form 0 file)))
    (flet ((parse-import-symbol (import)
             (cond ((string= "*" (cst:raw import))
                    (return-from parse-import-statement
                      `(:import-all ,source-package)))
                   ((identifierp (cst:raw import))
                    (symbol-name (cst:raw import)))
                   (t
                    (parse-error import file
                                 "Malformed import statement"
                                 "Unrecognized import type")))))
      `(:import ,source-package
                ,@(mapcar-form #'parse-import-symbol
                               (cst:rest form))))))

(defun parse-package-clause (clause file)
  "Parses a coalton package clause of the form of ({operation} {symbol}* ...)"
  (let* ((head (cst:first clause))
         (body (cst:rest clause))
         (clause-type (cst:raw head)))
    (cond ((string= clause-type "IMPORT")
           (mapcar-form (a:rcurry #'parse-import-statement file) body))
          ((string= clause-type "EXPORT")
           (list (cons :export (mapcar-form (a:compose #'symbol-name #'cst:raw) body))))
          ((string= clause-type "REFER")
           (mapcar-form (lambda (form)
                          `(:refer ,(parse-symbol form 0 file)
                                   ,(parse-symbol form 1 file)))
                        body))
          (t
           (parse-error head file
                        "Malformed package declaration"
                        "Unknown package clause"
                        (list (make-coalton-error-help
                               :span (cst:source head)
                               :replacement #'identity
                               :message "Must be one of refer, import or export")))))))

(defun parse-package-form (form file)
  "Parses a coalton package declaration in the form of (package {name})"

  ;; Package declarations must start with "PACKAGE"
  (unless (string= (cst:raw (cst:first form)) "PACKAGE")
    (parse-error (cst:first form) file
                 "Malformed package declaration"
                 "package declarations must start with `package`"))

  ;; Package declarations must have a name
  (unless (cst:consp (cst:rest form))
    (parse-error (cst:rest form) file
                 "Malformed package declaration"
                 "missing package name"))

  ;; The remaining forms are package clauses
  (cons `(:package ,(parse-symbol form 1 file))
        (apply #'append
               (mapcar-form (a:rcurry #'parse-package-clause file)
                            (cst:rest (cst:rest form))))))

(defun do-package-clause (name)
  (let ((p (or (find-package name)
               (make-package name))))
    (use-package "COALTON" p)
    (use-package "COALTON-PRELUDE" p)
    p))

(defun do-import-all-clause (package name)
  (use-package (find-package name) package))

(defun do-import-clause (package name &rest symbols)
  (import (mapcar (a:rcurry #'intern (find-package name)) symbols) package))

(defun do-export-clause (package &rest symbols)
  (import (mapcar (a:rcurry #'intern package) symbols) package))

(defun do-refer-clause (package name nickname)
  (sb-ext:add-package-local-nickname nickname (find-package name) package))

(defun parse-package (form file)
  "Parses a coalton package declaration in the form of (package {name})"
  (let ((package nil))
    (dolist (step (parse-package-form form file))
      (destructuring-bind (op . args) step
        (ecase op
          (:package
           (setf package (apply #'do-package-clause args)))
          (:refer
           (apply #'do-refer-clause package args))
          (:import-all
           (apply #'do-import-all-clause package args))
          (:import
           (apply #'do-import-clause package args))
          (:export
           (apply #'do-export-clause package args))
          )))
    package))
