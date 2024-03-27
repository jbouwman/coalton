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
   #:ensure-package
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
  (unless (cst:consp clause)
    (parse-error head file
                 "Malformed package declaration"
                 "Malformed package clause"))
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

;; Parse a module declaration and return a list of package definition
;; 'teps' -- these can be used to build ap akge inplace , or to build
;; a defpackage form.

(defun parse-package (form file)
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

(defun ensure-package (parsed-package)
  "Parses a coalton package declaration in the form of (package {name})"
  (let ((package nil))
    (dolist (step parsed-package)
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
           (apply #'do-export-clause package args)))))
    (unless package
      (error "oops"))
    package))



(defun package-decl-name (parsed-package)
  (a:if-let ((package (cadr (find-if (lambda (x)
                                       (eql (car x) :package))
                                     parsed-package))))
    (string-downcase package)
    (error "Missing package in ~A" parsed-package)))

;;(package-decl-name '((:package "x")))

(defun package-decl-use (parsed-package)
  (append '("coalton")
          (mapcar (lambda (import)
                    (string-downcase (cadr import)))
                  (remove-if-not (lambda (x)
                                   (eql (car x) :import-all))
                                 parsed-package))))

(defun package-decl-local-nicknames (parsed-package)
  (mapcar (lambda (clause)
            (list (string-downcase (cadr clause))
                  (string-downcase (caddr clause))))
          (remove-if-not (lambda (x)
                           (eql (car x) :refer))
                         parsed-package)))

(defun package-decl-export (parsed-package)
  (mapcan (lambda (clause)
            (mapcar #'string-downcase (cdr clause)))
          (remove-if-not (lambda (x)
                           (eql (car x) :export))
                         parsed-package)))

;; (package-decl-use '((:import-all "x")))

(defun generate-defpackage (stream package-decl)
  (format stream "(defpackage #:~A"
          (package-decl-name package-decl))
  (a:when-let ((use (package-decl-use package-decl)))
    (format stream "~%  (:use~%   ~{#:~A~^~%   ~})" use))
  (a:when-let ((nn (package-decl-local-nicknames package-decl)))
    (format stream "~%  (:local-nicknames")
    (dolist (n nn)
      (format stream "~%   (#:~A #:~A)" (car n) (cadr n)))
    (format stream ")"))
  (a:when-let ((export (package-decl-export package-decl)))
    (format stream "~%  (:export~%   ~{#:~A~^~%   ~})" export))
  (terpri stream)
  (terpri stream))
