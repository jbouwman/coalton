(defpackage #:coalton-impl/parser/module
  (:use
   #:cl
   #:coalton-impl/error
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util))
  (:export
   #:parse-package))

(in-package #:coalton-impl/parser/module)

(defun map-form (f form)
  (loop :for clauses := form :then (cst:rest clauses)
        :while (cst:consp clauses)
        :do (funcall f (cst:first clauses))))

(defun mapcar-form (f form)
  (loop :for clauses := form :then (cst:rest clauses)
        :while (cst:consp clauses)
        :collect (funcall f (cst:first clauses))))

(defun unwrap-form (form)
  (mapcar-form #'identity form))

;; convention: 'forms' names a list of cst structures

(defun assert-length (form n file)
  (unless (null (cst:nthrest (1+ n) form))
    (error 'parse-error
           :err (coalton-error
                 :span (cst:source form)
                 :file file
                 :message "Malformed declaration"
                 :primary-note "extra input"))))

(defun parse-package-ref (sequence index file)
  (let ((form (cst:nth index sequence)))
    (unless form
      (error 'parse-error
             :err (coalton-error
                   :span (cst:source form)
                   :highlight :end
                   :file file
                   :message "Missing package reference"
                   :primary-note "Missing package name")))
    (unless (identifierp form)
      (error 'parse-error
             :err (coalton-error
                   :span (cst:source form)
                   :file file
                   :message "Malformed package reference"
                   :primary-note "Not a package name")))
    (let ((package (find-package (cst:raw form))))
      (unless package
        (error 'parse-error
               :err (coalton-error
                     :span (cst:source form)
                     :file file
                     :message "Unknown package"
                     :primary-note "Unknown package")))
      package)))

(defun parse-import-clause (package form file)
  (let ((source-package (parse-package-ref form 0 file))
        (import-all nil)
        (import-syms nil))
    (map-form (lambda (import)
                (cond ((string= "*" (cst:raw import))
                       (setf import-all t))
                      ((identifierp import)
                       (push (cst:raw import) import-syms))
                      (t
                       (error 'parse-error
                                :err (coalton-error
                                      :span (cst:source import)
                                      :file file
                                      :message "Malformed import statement"
                                      :primary-note "Unrecognized import type")))))
              (cst:rest form))
    (cond (import-all
           (use-package (list source-package) package))
          (t
           (import (mapcar (lambda (symbol)
                             (intern symbol source-package))
                           package))))))

(defun parse-refer-clause (package clause file)
  (map-form (lambda (subclause)
              (let ((source-package (parse-package-ref subclause 0 file))
                    (nickname (parse-symbol-ref subclause 1 file)))
                (assert-length subclause 2 file)
                (sb-ext:add-package-local-nickname nickname source-package package)))
            clause))

(defun parse-export-clause (package forms file)
  (declare (ignore file))
  (export (mapcar (lambda (term)
                    (intern (symbol-name (cst:raw term)) package))
                  forms)
          package))

(defun clause-parser (clause file)
  (let* ((head (cst:first clause))
         (clause-type (cst:raw head)))
    (cond ((string= clause-type "IMPORT")
           #'parse-import-clause)
          ((string= clause-type "EXPORT")
           #'parse-export-clause)
          ((string= clause-type "REFER")
           #'parse-refer-clause)
          (t
           (error 'parse-error
                  :err (coalton-error
                        :span (cst:source head)
                        :file file
                        :message "Malformed package declaration"
                        :primary-note "Unknown package clause"
                        :help-notes
                        (list
                         (make-coalton-error-help
                          :span (cst:source head)
                          :replacement #'identity
                          :message "Must be one of refer, import or export"))))))))

(defun parse-package-clause (package clause file)
  "Parses a coalton package clause of the form of ({operation} {symbol}* ...)"
  (funcall (clause-parser clause file)
           package (cst:rest clause) file))

(defun parse-package (form file)
  "Parses a coalton package declaration in the form of (package {name})"
  (declare (type cst:cst form)
           (type coalton-file file)
           (values package))

  ;; Package declarations must start with "PACKAGE"
  (unless (string= (cst:raw (cst:first form)) "PACKAGE")
    (error 'parse-error
           :err (coalton-error
                 :span (cst:source (cst:first form))
                 :file file
                 :message "Malformed package declaration"
                 :primary-note "package declarations must start with `package`")))

  ;; Package declarations must have a name
  (unless (cst:consp (cst:rest form))
    (error 'parse-error
           :err (coalton-error
                 :span (cst:source (cst:first form))
                 :file file
                 :message "Malformed package declaration"
                 :primary-note "missing package name")))

  (unless (identifierp (cst:raw (cst:second form)))
    (error 'parse-error
           :err (coalton-error
                 :span (cst:source (cst:second form))
                 :file file
                 :message "Malformed package declaration"
                 :primary-note "package name must be a symbol")))
  
  (let* ((package-name (symbol-name (cst:raw (cst:second form))))
         (package (or (find-package package-name)
                      (make-package package-name :use '("COALTON" "COALTON-PRELUDE")))))
    ;; The remaining forms are package clauses that side-effect 'package
    (map-form (lambda (clause)
                (parse-package-clause package clause file))
              (cst:rest (cst:rest form)))
    package))
