(defpackage #:coalton-impl/parser/types
  (:use
   #:cl
   #:coalton-impl/source
   #:coalton-impl/parser/base)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util))
  (:export
   #:ty                                 ; STRUCT
   #:ty-location                        ; ACCESSOR
   #:ty-list                            ; TYPE
   #:tyvar                              ; STRUCT
   #:make-tyvar                         ; CONSTRUCTOR
   #:tyvar-p                            ; FUNCTION
   #:tyvar-name                         ; ACCESSOR
   #:tyvar-list                         ; TYPE
   #:tycon                              ; STRUCT
   #:make-tycon                         ; CONSTRUCTOR
   #:tycon-p                            ; FUNCTION
   #:tycon-name                         ; ACCESSOR
   #:tycon-list                         ; TYPE
   #:tapp                               ; STRUCT
   #:make-tapp                          ; CONSTRUCTOR
   #:tapp-p                             ; FUNCTION
   #:tapp-from                          ; ACCESSOR
   #:tapp-to                            ; ACCESSOR
   #:ty-predicate                       ; STRUCT
   #:make-ty-predicate                  ; CONSTRUCTOR
   #:ty-predicate-class                 ; ACCESSOR
   #:ty-predicate-types                 ; ACCESSOR
   #:ty-predicate-location              ; ACCESSOR
   #:ty-predicate-list                  ; TYPE
   #:qualified-ty                       ; STRUCT
   #:make-qualified-ty                  ; CONSTRUCTOR
   #:qualified-ty-predicates            ; ACCESSOR
   #:qualified-ty-type                  ; ACCESSOR
   #:qualified-ty-location              ; ACCESSOR
   #:qualified-ty-list                  ; TYPE
   #:parse-qualified-type               ; FUNCTION
   #:parse-type                         ; FUNCTION
   #:parse-predicate                    ; FUNCTION
   ))

(in-package #:coalton-impl/parser/types)

;;;; # Type Parsing
;;;;
;;;; tyvar := <a keyword symbol>
;;;;
;;;; tycon := <a lisp symbol>
;;;;
;;;; class := <a lisp symbol>
;;;;
;;;; type-list := ty ty+
;;;;            | ty+ "->" type-list
;;;;
;;;; ty := tyvar
;;;;     | tycon
;;;;     | "(" type-list ")"
;;;;
;;;; ty-predicate := class ty+ 
;;;;
;;;; qualified-ty := ty
;;;;               | "(" ty-predicate "=>" type-list ")"
;;;;               | "(" ( "(" ty-predicate ")" )+ "=>" type-list ")"

(defstruct (ty (:constructor nil)
               (:copier nil))
  (location (util:required 'location) :type location :read-only t))

(defun ty-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-p x)))

(deftype ty-list ()
  '(satisfies ty-list-p))

(defstruct (tyvar (:include ty)
                  (:copier nil))
  (name (util:required 'name) :type keyword :read-only t))

(defun tyvar-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'tyvar-p x)))

(deftype tyvar-list ()
  '(satisfies tyvar-list-p))

(defstruct (tycon (:include ty)
                  (:copier nil))
  (name (util:required 'name) :type identifier :read-only t))

(defun tycon-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'tycon-p x)))

(deftype tycon-list ()
  '(satisfies tycon-list-p))

(defstruct (tapp (:include ty)
                 (:copier nil))
  ;; The type being applied to
  (from (util:required 'from) :type ty :read-only t)
  ;; The type argument
  (to   (util:required 'to)   :type ty :read-only t))

(defstruct (ty-predicate
            (:copier nil))
  (class    (util:required 'class)    :type identifier-src :read-only t)
  (types    (util:required 'types)    :type ty-list        :read-only t)
  (location (util:required 'location) :type location       :read-only t))

(defun ty-predicate-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-predicate-p x)))

(deftype ty-predicate-list ()
  '(satisfies ty-predicate-list-p))

(defstruct (qualified-ty
            (:predicate nil)
            (:copier nil))
  (predicates (util:required 'predicates) :type ty-predicate-list :read-only t)
  (type       (util:required 'type)       :type ty                :read-only t)
  (location   (util:required 'location)   :type location          :read-only t))

(defun parse-qualified-type (form source)
  (declare (type cst:cst form))

  (if (cst:atom form)

      (make-qualified-ty
       :predicates nil
       :type (parse-type form source)
       :location (form-location form source))

      (multiple-value-bind (left right)
          (util:take-until (lambda (cst)
                             (and (cst:atom cst)
                                  (eq (cst:raw cst) 'coalton:=>)))
                           (cst:listify form))
        (cond
          ;; no predicates
          ((null right)
           (make-qualified-ty
            :predicates nil
            :type (parse-type-list left (form-location form source))
            :location (form-location form source)))

          ;; (=> T -> T)
          ((and (null left) right)
           (error 'parse-error
                  :message "Malformed type"
                  :notes (list (make-note (form-location (cst:first form) source)
                                          "unnecessary `=>`"))
                  :help-notes
                  (cond
                    ;; If this is the only thing in the list then don't suggest anything
                    ((cst:atom (cst:rest form))
                     nil)
                    ;; If there is nothing to the right of C then emit without list
                    ((cst:atom (cst:rest (cst:rest form)))
                     (list
                      (make-help
                       :location (form-location form source)
                       :replacement
                       (lambda (existing)
                         (subseq existing 4 (1- (length existing))))
                       :message "remove `=>`")))
                    (t
                     (list
                      (make-help
                       :location (form-location form source)
                       :replacement
                       (lambda (existing)
                         (concatenate 'string
                                      (subseq existing 0 1)
                                      (subseq existing 4)))
                       :message "remove `=>`"))))))

          ;; (... =>)
          ((null (rest right))
           (simple-parse-error source (cst:second form)
                               "Malformed type"
                               "missing type after `=>`"))

          (t
           (let (predicates)
             (if (cst:atom (first left))
                 (setf predicates (list (parse-predicate
                                         left
                                         (make-location :source source
                                                        :span (cons (car (cst:source (first left)))
                                                                    (cdr (cst:source (car (last left)))))))))

                 (loop :for pred :in left
                       :unless (cst:consp pred)
                         :do (simple-parse-error source (cst:second form)
                                                 "Malformed type predicate"
                                                 "expected predicate")
                       :do (push (parse-predicate (cst:listify pred)
                                                  (make-location :source source
                                                                 :span (cst:source form)))
                                 predicates)))

             (make-qualified-ty
              :predicates (reverse predicates)
              :type (parse-type-list
                     (cdr right)
                     (make-location :source source
                                    :span (cons (car (cst:source (second right)))
                                                (cdr (cst:source (car (last right)))))))
              :location (form-location form source))))))))

(defun parse-predicate (forms location)
  (declare (type util:cst-list forms)
           (type location location)
           (values ty-predicate))

  (assert forms)
  (let ((source (location-source location)))
    (cond
      ;; (T) ... => ....
      ((not (cst:atom (first forms)))
       (error 'parse-error
              :message "Malformed type predicate"
              :notes (list (make-note (form-location (first forms)
                                                     source)
                                      "expected class name"))
              :help-notes (list (make-help :location (form-location (first forms) source)
                                           :replacement
                                           (lambda (existing)
                                             (subseq existing 1 (1- (length existing))))
                                           :message "remove parentheses"))))

      ;; "T" ... => ...
      ((not (identifierp (cst:raw (first forms))))
       (simple-parse-error source (first forms)
                           "Malformed type predicate"
                           "expected identifier"))

      (t
       (let ((name (cst:raw (first forms))))
         (when (= 1 (length forms))
           (simple-parse-error source (first forms)
                               "Malformed type predicate"
                               "expected predicate"))

         (make-ty-predicate
          :class (make-identifier-src
                  :name name
                  :source (form-location (first forms) source))
          :types (loop :for form :in (cdr forms)
                       :collect (parse-type form source))
          :location (form-location (first forms) source)))))))

(defun parse-type (form source)
  (declare (type cst:cst form)
           (values ty &optional))

  (cond
    ((and (cst:atom form)
          (symbolp (cst:raw form))
          (cst:raw form))

     (if (equalp (symbol-package (cst:raw form)) util:+keyword-package+)
         (make-tyvar :name (cst:raw form) :location (form-location form source))
         (make-tycon :name (cst:raw form) :location (form-location form source))))

    ((cst:atom form)
     (simple-parse-error source form
                         "Malformed type"
                         "expected identifier"))

    ;; (T)
    ((cst:atom (cst:rest form))
     (simple-parse-error source form
                         "Malformed type"
                         "unexpected nullary type"))

    (t
     (parse-type-list (cst:listify form) (form-location form source)))))

(defun parse-type-list (forms location)
  (declare (type util:cst-list forms)
           (type location location)
           (values ty &optional))

  (assert forms)

  (if (= 1 (length forms))
      (parse-type (first forms) (location-source location))
      (multiple-value-bind (left right)
          (util:take-until (lambda (cst)
                             (and (cst:atom cst)
                                  (eq (cst:raw cst) 'coalton:->)))
                           forms)

        ;; (T ... ->)
        (cond
          ((and right (null (rest right)))
           (simple-parse-error (location-source location)
                               (car right)
                               "Malformed function type"
                               "missing return type"))

          ;; (-> ...)
          ((and (null left) right)
           (simple-parse-error (location-source location)
                               (car right)
                               "Malformed function type"
                               "invalid function syntax"))

          (t
           (let ((ty (parse-type (car left) (location-source location))))
             (loop :for form_ :in (cdr left)
                   :for ty_ := (parse-type form_ (location-source location))
                   :do (setf ty (make-tapp :from ty
                                           :to ty_
                                           :location location)))

             (if (null right)
                 ty

                 (make-tapp
                  :from (make-tapp
                         :from (make-tycon
                                :name 'coalton:Arrow
                                :location (form-location (first right)
                                                         (location-source location)))
                         :to ty
                         :location (make-location :source (location-source location)
                                                  :span (cons (car (location-span (ty-location ty)))
                                                              (cdr (cst:source (first right))))))
                  :to (parse-type-list
                       (cdr right)
                       (make-location :source (location-source location)
                                      :span (cons
                                             (car (cst:source (first right)))
                                             (cdr (cst:source (car (last right)))))))
                  :location location))))))))
