;;;;
;;;; Central environment managment. Coalton stores all persistent
;;;; state in a single immutable struct. State is partitioned into a
;;;; series of sub-environments. Each sub-environment is analogous to
;;;; a database table, and holds multiple entries which function like
;;;; database rows. The process of compiling Coalton code will require
;;;; many "writes" to the environment. Each write is an immutable
;;;; update which returns a new environment. The writes preformed when
;;;; compiling a single coalton-toplevel form are tracked in a log.
;;;; After compilation is finished the write log is added to the
;;;; generated lisp code. This allows replaying the environment
;;;; updates when the compiled fasl is loaded.
;;;;

(defpackage #:coalton-impl/typechecker/environment
  (:use
   #:cl
   #:coalton-impl/algorithm
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/unify)
  (:import-from
   #:coalton-impl/typechecker/substitutions
   #:apply-substitution
   #:substitution-list
   #:compose-substitution-lists)
  (:import-from
   #:coalton-impl/environment
   #:environment)
  (:import-from
   #:coalton-impl/typechecker/fundeps
   #:fundep
   #:fundep-from
   #:fundep-to
   #:fundep-list
   #:+fundep-max-depth+)
  (:local-nicknames
   (#:env #:coalton-impl/environment)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser))
  (:export
   #:*env-update-log*                       ; VARIABLE
   #:explicit-repr                          ; TYPE
   #:type-entry                             ; STRUCT
   #:make-type-entry                        ; CONSTRUCTOR
   #:type-entry-name                        ; ACCESSOR
   #:type-entry-runtime-type                ; ACCESSOR
   #:type-entry-type                        ; ACCESSOR
   #:type-entry-tyvars                      ; ACCESSOR
   #:type-entry-constructors                ; ACCESSOR
   #:type-entry-explicit-repr               ; ACCESSOR
   #:type-entry-enum-repr                   ; ACCESSOR
   #:type-entry-newtype                     ; ACCESSOR
   #:type-entry-docstring                   ; ACCESSOR
   #:type-entry-location                    ; ACCESSOR
   #:constructor-entry                      ; STRUCT
   #:make-constructor-entry                 ; ACCESSOR
   #:constructor-entry-name                 ; ACCESSOR
   #:constructor-entry-arity                ; ACCESSOR
   #:constructor-entry-constructs           ; ACCESSOR
   #:constructor-entry-classname            ; ACCESSOR
   #:constructor-entry-compressed-repr      ; ACCESSOR
   #:constructor-entry-list                 ; TYPE
   #:struct-entry                           ; STRUCT
   #:make-struct-entry                      ; CONSTRUCTOR
   #:struct-entry-name                      ; ACCESSOR
   #:struct-entry-fields                    ; ACCESSOR
   #:struct-entry-field-tys                 ; ACCESSOR
   #:struct-entry-field-idx                 ; ACCESSOR`
   #:struct-entry-list                      ; TYPE
   #:ty-class                               ; STRUCT
   #:make-ty-class                          ; CONSTRUCTOR
   #:ty-class-name                          ; ACCESSOR
   #:ty-class-predicate                     ; ACCESSOR
   #:ty-class-superclasses                  ; ACCESSOR
   #:ty-class-class-variables               ; ACCESSOR
   #:ty-class-class-variable-map            ; ACCESSOR
   #:ty-class-fundeps                       ; ACCESSOR
   #:ty-class-unqualified-methods           ; ACCESSOR
   #:ty-class-codegen-sym                   ; ACCESSOR
   #:ty-class-superclass-dict               ; ACCESSOR
   #:ty-class-superclass-map                ; ACCESSOR
   #:ty-class-docstring                     ; ACCESSOR
   #:ty-class-location                      ; ACCESSOR
   #:ty-class-list                          ; TYPE
   #:ty-class-instance                      ; STRUCT
   #:make-ty-class-instance                 ; CONSTRUCTOR
   #:ty-class-instance-constraints          ; ACCESSOR
   #:ty-class-instance-predicate            ; ACCESSOR
   #:ty-class-instance-codegen-sym          ; ACCESSOR
   #:ty-class-instance-method-codegen-syms  ; ACCESSOR
   #:ty-class-instance-docstring            ; ACCESSOR
   #:ty-class-instance-list                 ; TYPE
   #:instance-environment-instances         ; ACCESSOR
   #:function-env-entry                     ; STRUCT
   #:make-function-env-entry                ; CONSTRUCTOR
   #:function-env-entry-name                ; ACCESSOR
   #:function-env-entry-arity               ; ACCESSOR
   #:name-entry                             ; STRUCT
   #:make-name-entry                        ; CONSTRUCTOR
   #:name-entry-name                        ; ACCESSOR
   #:name-entry-type                        ; ACCESSOR
   #:name-entry-docstring                   ; ACCESSOR
   #:name-entry-location                    ; ACCESSOR
   #:specialization-entry                   ; STRUCT
   #:make-specialization-entry              ; CONSTRUCTOR
   #:specialization-entry-from              ; ACCESSOR
   #:specialization-entry-to                ; ACCESSOR
   #:specialization-entry-to-ty             ; ACCESSOR
   #:specialization-entry-list              ; TYPE
   #:make-default-environment               ; FUNCTION
   #:lookup-value-type                      ; FUNCTION
   #:set-value-type                         ; FUNCTION
   #:unset-value-type                       ; FUNCTION
   #:lookup-type                            ; FUNCTION
   #:set-type                               ; FUNCTION
   #:lookup-constructor                     ; FUNCTION
   #:set-constructor                        ; FUNCTION
   #:unset-constructor                      ; FUNCTION
   #:lookup-struct                          ; FUNCTION
   #:set-struct                             ; FUNCTION
   #:unset-struct                           ; FUNCTION
   #:set-constructor                        ; FUNCTION
   #:unset-constructor                      ; FUNCTION
   #:lookup-class                           ; FUNCTION
   #:set-class                              ; FUNCTION
   #:lookup-function                        ; FUNCTION
   #:set-function                           ; FUNCTION
   #:unset-function                         ; FUNCTION
   #:lookup-name                            ; FUNCTION
   #:set-name                               ; FUNCTION
   #:unset-name                             ; FUNCTION
   #:lookup-class-instances                 ; FUNCTION
   #:lookup-class-instance                  ; FUNCTION
   #:lookup-instance-by-codegen-sym         ; FUNCTION
   #:lookup-function-source-parameter-names ; FUNCTION
   #:set-function-source-parameter-names    ; FUNCTION
   #:unset-function-source-parameter-names  ; FUNCTION
   #:constructor-arguments                  ; FUNCTION
   #:add-class                              ; FUNCTION
   #:add-instance                           ; FUNCTION
   #:set-method-inline                      ; FUNCTION
   #:lookup-method-inline                   ; FUNCTION
   #:set-code                               ; FUNCTION
   #:lookup-code                            ; FUNCTION
   #:add-specialization                     ; FUNCTION
   #:lookup-specialization                  ; FUNCTION
   #:lookup-specialization-by-type          ; FUNCTION
   #:lookup-fundep-environment              ; FUNCTION
   #:update-instance-fundeps                ; FUNCTION
   #:solve-fundeps                          ; FUNCTION
   #:environment
   ))

(in-package #:coalton-impl/typechecker/environment)

(defvar *env-update-log* nil)

(defun make-update-record (name arg-list)
  `(setf env (,name env ,@(loop :for arg :in (cdr arg-list)
                                :collect (util:runtime-quote arg)))))

(defmacro define-env-updater (name arg-list &body body)
  `(defun ,name (&rest args)
     (push (make-update-record ',name args) *env-update-log*)
     (destructuring-bind ,arg-list args ,@body)))

;;;
;;; Value type environments
;;;

;;;
;;; Type environments
;;;

(deftype explicit-repr ()
  '(or null
    (member :enum :lisp :transparent)
    (cons (eql :native) (cons t null))))

(defstruct type-entry
  (name         (util:required 'name)         :type symbol           :read-only t)
  (runtime-type (util:required 'runtime-type) :type t                :read-only t)
  (type         (util:required 'type)         :type ty               :read-only t)
  (tyvars       (util:required 'tyvars)       :type tyvar-list       :read-only t)
  (constructors (util:required 'constructors) :type util:symbol-list :read-only t)

  ;; An explicit repr defined in the source, or nil if none was supplied. Computed repr will be reflected in
  ;; ENUM-REPR, NEWTYPE, and/or RUNTIME-TYPE.
  (explicit-repr (util:required 'explicit-repr) :type explicit-repr  :read-only t)

  ;; If this is true then the type is compiled to a more effecient
  ;; enum representation at runtime
  (enum-repr (util:required 'enum-repr)       :type boolean :read-only t)

  ;; If this is true then the type does not exist at runtime
  ;; See https://wiki.haskell.org/Newtype
  ;;
  ;; A type cannot be both enum repr and a newtype
  ;;
  ;; A type that is a newtype has another Coalton type as it's
  ;; runtime-type instead of a lisp type. This is to avoid issues with
  ;; recursive newtypes.
  (newtype (util:required 'newtype)           :type boolean :read-only t)

  (docstring (util:required 'docstring)       :type (or null string) :read-only t)
  (location  (util:required 'location)        :type t                :read-only t))

(defmethod make-load-form ((self type-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod kind-of ((entry type-entry))
  (kind-of (type-entry-type entry)))

(defvar *early-types*
  (list 'coalton:Boolean (make-type-entry
                          :name 'coalton:Boolean
                          :runtime-type 'cl:boolean
                          :type *boolean-type*
                          :tyvars nil
                          :constructors '(coalton:True coalton:False)
                          :explicit-repr '(:native cl:boolean)
                          :enum-repr t
                          :newtype nil
                          :docstring "Either true or false represented by `t` and `nil` respectively."
                          :location "")
        'coalton:Unit           (make-type-entry
                                 :name 'coalton:Unit
                                 :runtime-type '(member coalton::Unit/Unit)
                                 :type *unit-type*
                                 :tyvars nil
                                 :constructors '(coalton:Unit)
                                 :explicit-repr :enum
                                 :enum-repr t
                                 :newtype nil
                                 :docstring ""
                                 :location "")
        'coalton:Char           (make-type-entry
                                 :name 'coalton:Char
                                 :runtime-type 'cl:character
                                 :type *char-type*
                                 :tyvars nil
                                 :constructors nil
                                 :explicit-repr '(:native cl:character)
                                 :enum-repr nil
                                 :newtype nil
                                 :docstring "A single character represented as a `character` type."
                                 :location "")
        'coalton:Integer           (make-type-entry
                                    :name 'coalton:Integer
                                    :runtime-type 'cl:integer
                                    :type *integer-type*
                                    :tyvars nil
                                    :constructors nil
                                    :explicit-repr '(:native cl:integer)
                                    :enum-repr nil
                                    :newtype nil
                                    :docstring "Unbound integer. Uses `integer`."
                                    :location "")
        'coalton:Single-Float (make-type-entry
                               :name 'coalton:Single-Float
                               :runtime-type 'cl:single-float
                               :type *single-float-type*
                               :tyvars nil
                               :constructors nil
                               :explicit-repr '(:native cl:single-float)
                               :enum-repr nil
                               :newtype nil
                               :docstring "Single precision floating point numer. Uses `single-float`."
                               :location "")
        'coalton:Double-Float (make-type-entry
                               :name 'coalton:Double-Float
                               :runtime-type 'cl:double-float
                               :type *double-float-type*
                               :tyvars nil
                               :constructors nil
                               :explicit-repr '(:native cl:double-float)
                               :enum-repr nil
                               :newtype nil
                               :docstring "Double precision floating point numer. Uses `double-float`."
                               :location "")
        'coalton:String (make-type-entry
                         :name 'coalton:String
                         :runtime-type 'cl:string
                         :type *string-type*
                         :tyvars nil
                         :constructors nil
                         :explicit-repr '(:native cl:string)
                         :enum-repr nil
                         :newtype nil
                         :docstring "String of characters represented by Common Lisp `string`."
                         :location "")
        'coalton:Fraction           (make-type-entry
                                     :name 'coalton:Fraction
                                     :runtime-type 'cl:rational
                                     :type *fraction-type*
                                     :tyvars nil
                                     :constructors nil
                                     :explicit-repr '(:native cl:rational)
                                     :enum-repr nil
                                     :newtype nil
                                     :docstring "A ratio of integers always in reduced form."
                                     :location "")
        'coalton:Arrow (make-type-entry
                        :name 'coalton:Arrow
                        :runtime-type nil
                        :type *arrow-type*
                        :tyvars nil
                        :constructors nil
                        :explicit-repr nil
                        :enum-repr nil
                        :newtype nil
                        :docstring "Type constructor for function types."
                        :location "")
        'coalton:List (make-type-entry
                       :name 'coalton:List
                       :runtime-type 'cl:list
                       :type *list-type*
                       :tyvars (list (make-variable))
                       :constructors '(coalton:Cons coalton:Nil)
                       :explicit-repr '(:native cl:list)
                       :enum-repr nil
                       :newtype nil
                       :docstring "Homogeneous list of objects represented as a Common Lisp `list`."
                       :location "")))

;;;
;;; Constructor environment
;;;

(defstruct constructor-entry
  (name            (util:required 'name)            :type symbol                         :read-only t)
  (arity           (util:required 'arity)           :type alexandria:non-negative-fixnum :read-only t)
  (constructs      (util:required 'constructs)      :type symbol                         :read-only t)
  (classname       (util:required 'classname)       :type symbol                         :read-only t)

  ;; If this constructor constructs a compressed-repr type then
  ;; compressed-repr is the runtime value of this nullary constructor
  (compressed-repr (util:required 'compressed-repr) :type t                              :read-only t))

(defmethod make-load-form ((self constructor-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(defvar *early-constructors*
  (list 'coalton:True (make-constructor-entry
                       :name 'coalton:True
                       :arity 0
                       :constructs 'coalton:Boolean
                       :classname 'coalton::Boolean/True
                       :compressed-repr 't)
        'coalton:False (make-constructor-entry
                        :name 'coalton:False
                        :arity 0
                        :constructs 'coalton:Boolean
                        :classname 'coalton::Boolean/False
                        :compressed-repr 'nil)
        'coalton:Unit (make-constructor-entry
                       :name 'coalton:Unit
                       :arity 0
                       :constructs 'coalton:Unit
                       :classname 'coalton::Unit/Unit
                       :compressed-repr 'coalton::Unit/Unit)
        'coalton:Cons (make-constructor-entry
                       :name 'coalton:Cons
                       :arity 2
                       :constructs 'coalton:List
                       :classname nil
                       :compressed-repr 'nil)
        'coalton:Nil (make-constructor-entry
                      :name 'coalton:Nil
                      :arity 0
                      :constructs 'coalton:List
                      :classname nil
                      :compressed-repr 'nil)))

;;;
;;; Struct environment
;;;

(defstruct struct-entry
  (name      (util:required 'name)      :type symbol           :read-only t)
  (fields    (util:required 'fields)    :type util:string-list :read-only t)

  ;; Mapping of "field name" -> "field type"
  ;; Type variables are the same as in `type-entry-type'
  (field-tys (util:required 'field-tys) :type hash-table       :read-only t)

  ;; Mapping of "field name" -> "field index"
  (field-idx (util:required 'field-idx) :type hash-table       :read-only t)h)

(defmethod make-load-form ((self struct-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

;;;
;;; Class environment
;;;

(defstruct ty-class
  (name                (util:required 'name)                :type symbol              :read-only t)
  (predicate           (util:required 'predicate)           :type ty-predicate        :read-only t)
  (superclasses        (util:required 'superclasses)        :type ty-predicate-list   :read-only t)
  (class-variables     (util:required 'class-variables)     :type util:symbol-list    :read-only t)

  ;; Hash table mapping variable symbols to their index in the predicate
  (class-variable-map  (util:required 'class-variable-map)  :type hash-table          :read-only t)
  (fundeps             (util:required 'fundeps)             :type fundep-list         :read-only t)

  ;; Methods of the class containing the same tyvars in PREDICATE for
  ;; use in pretty printing
  (unqualified-methods (util:required 'unqualified-methods) :type scheme-binding-list :read-only t)
  (codegen-sym         (util:required 'codegen-sym)         :type symbol              :read-only t)
  (superclass-dict     (util:required 'superclass-dict)     :type list                :read-only t)
  (superclass-map      (util:required 'superclass-map)      :type hash-table          :read-only t)
  (docstring           (util:required 'docstring)           :type (or null string)    :read-only t)
  (location            (util:required 'location)            :type t                   :read-only t))

(defmethod make-load-form ((self ty-class) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod apply-substitution (subst-list (class ty-class))
  (make-ty-class
   :name (ty-class-name class)
   :predicate (apply-substitution subst-list (ty-class-predicate class))
   :superclasses (apply-substitution subst-list (ty-class-superclasses class))
   :class-variables (ty-class-class-variables class)
   :class-variable-map (ty-class-class-variable-map class)
   :fundeps (ty-class-fundeps class)
   :unqualified-methods (mapcar (lambda (entry)
                                  (cons (car entry)
                                        (apply-substitution subst-list (cdr entry))))
                                (ty-class-unqualified-methods class))
   :codegen-sym (ty-class-codegen-sym class)
   :superclass-dict (mapcar (lambda (entry)
                              (cons (apply-substitution subst-list (car entry))
                                    (cdr entry)))
                            (ty-class-superclass-dict class))
   :superclass-map (ty-class-superclass-map class)
   :docstring (ty-class-docstring class)
   :location (ty-class-location class)))

;;;
;;; Instance environment
;;;

(defstruct ty-class-instance
  (constraints         (util:required 'constraints)         :type ty-predicate-list :read-only t)
  (predicate           (util:required 'predicate)           :type ty-predicate      :read-only t)
  (codegen-sym         (util:required 'codegen-sym)         :type symbol            :read-only t)
  (method-codegen-syms (util:required 'method-codegen-syms) :type hash-table        :read-only t)
  (docstring           (util:required 'docstring)           :type (or null string)  :read-only t))

(defmethod make-load-form ((self ty-class-instance) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod apply-substitution (subst-list (instance ty-class-instance))
  (make-ty-class-instance
   :constraints (apply-substitution subst-list (ty-class-instance-constraints instance))
   :predicate (apply-substitution subst-list (ty-class-instance-predicate instance))
   :codegen-sym (ty-class-instance-codegen-sym instance)
   :method-codegen-syms (ty-class-instance-method-codegen-syms instance)
   :docstring (ty-class-instance-docstring instance)))

;;;
;;; Function environment
;;;

(defstruct function-env-entry
  (name  (util:required 'name)  :type symbol :read-only t)
  (arity (util:required 'arity) :type fixnum :read-only t))

(defmethod make-load-form ((self function-env-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

;;;
;;; Name environment
;;;

(defstruct name-entry
  (name      (util:required 'name)      :type symbol                               :read-only t)
  (type      (util:required 'type)      :type (member :value :method :constructor) :read-only t)
  (docstring (util:required 'docstring) :type (or null string)                     :read-only t)
  (location  (util:required 'location)  :type t                                    :read-only t))

(defmethod make-load-form ((self name-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

;;;
;;; Specialization Environment
;;;

(defstruct specialization-entry
  (from (util:required 'from)   :type symbol :read-only t)
  (to (util:required 'to)       :type symbol :read-only t)
  (to-ty (util:required 'to-ty) :type ty     :read-only t))

(defmethod make-load-form ((self specialization-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

;;;
;;; Environment
;;;

(defun make-default-environment ()
  (apply #'env:set
         (apply #'env:set nil :type *early-types*)
         :constructor *early-constructors*))

;;;
;;; Methods
;;;

(defun env-substitute-values (env subst-list)
  (env:update env :value (lambda (v)
                           (apply-substitution subst-list v))))

(defmethod type-variables ((env environment))
  (let ((out nil))
    (env:map env :value (lambda (k v)
                          (declare (ignore k))
                          (setf out (append (type-variables v) out))))
    (remove-duplicates out :test #'equalp)))

;;;
;;; Functions
;;;

(defun lookup-value-type (env symbol &key no-error)
  (or (env:get env :value symbol)
      (unless no-error
        (util:coalton-bug "Unknown binding ~S" symbol))))

(define-env-updater set-value-type (env symbol value)
  ;; Schemes stored in the environment are not allowed to have any free variables.
  (when (type-variables value)
    (util:coalton-bug "Unable to add type with free variables to environment ~S" value))

  (env:set env :value symbol value))

(define-env-updater unset-value-type (env symbol)
  (env:unset env :value symbol))

(defun lookup-type (env symbol &key no-error)
  (or (env:get env :type symbol)
      (unless no-error
        (util:coalton-bug "Unknown type ~S" symbol))))

(define-env-updater set-type (env symbol value)
  (env:set env :type symbol value))

(defun lookup-constructor (env symbol &key no-error)
  (or (env:get env :constructor symbol)
      (unless no-error
        (util:coalton-bug "Unknown constructor ~S." symbol))))

(define-env-updater set-constructor (env symbol value)
  (env:set env :constructor symbol value))

(define-env-updater unset-constructor (env symbol)
  (env:unset env :constructor symbol))

(defun lookup-struct (env symbol &key no-error)
  (or (env:get env :struct symbol)
      (unless no-error
        (util:coalton-bug "Unknown struct ~S" symbol))))

(define-env-updater set-struct (env symbol value)
  (env:set env :struct symbol value))

(define-env-updater unset-struct (env symbol)
  (env:unset env :struct symbol))

(defun lookup-class (env symbol &key no-error)
  (or (env:get env :class symbol)
      (unless no-error
        (util:coalton-bug "Unknown class ~S." symbol))))

(define-env-updater set-class (env symbol value)
  (env:set env :class symbol value))

(defun lookup-function (env symbol &key no-error)
  (or (env:get env :function symbol)
      (unless no-error
        (util:coalton-bug "Unknown function ~S." symbol))))

(define-env-updater set-function (env symbol value)
  (env:set env :function symbol value))

(define-env-updater unset-function (env symbol)
  (env:unset env :function  symbol))

(defun lookup-name (env symbol &key no-error)
  (or (env:get env :name symbol)
      (unless no-error
        (error "Unknown name ~S." symbol))))

(define-env-updater set-name (env symbol value)
  (let ((old-value (lookup-name env symbol :no-error t)))
    (when (and old-value (not (equalp (name-entry-type old-value) (name-entry-type value))))
      (error "Unable to change the type of name ~S from ~A to ~A."
             symbol
             (name-entry-type old-value)
             (name-entry-type value)))
    (if old-value
        env
        (env:set env :name symbol value))))

(define-env-updater unset-name (env symbol)
  (env:unset env :name symbol))

(defun lookup-class-instances (env class)
  (env:get env :instance class))

(defun lookup-class-instance (env pred &key no-error)
  (let ((pred-class (ty-predicate-class pred)))
    (dolist (instance (lookup-class-instances env pred-class))
      (handler-case
          (let ((subs (predicate-match (ty-class-instance-predicate instance) pred)))
            (return-from lookup-class-instance (values instance subs)))
        (predicate-unification-error () nil)))
    (unless no-error
      (error "Unknown instance for predicate ~S" pred))))

(defun lookup-instance-by-codegen-sym (env codegen-sym &key no-error)
  (or (env:get env :codegen-sym codegen-sym)
      (unless no-error
        (error "Unknown instance with codegen-sym ~A" codegen-sym))))

(defun lookup-function-source-parameter-names (env function-name)
  (env:get env :source-name function-name))

(define-env-updater set-function-source-parameter-names (env function-name source-parameter-names)
  (env:set env :source-name function-name source-parameter-names))

(define-env-updater unset-function-source-parameter-names (env function-name)
  (env:unset env :source-name function-name))

(defun constructor-arguments (name env)
  (lookup-constructor env name)
  (function-type-arguments (lookup-value-type env name)))

(define-env-updater add-class (env symbol value)
  ;; Ensure this class does not already exist
  (when (lookup-class env symbol :no-error t)
    (error "Class ~S already exists." symbol))
  ;; Ensure all super classes exist
  (dolist (sc (ty-class-superclasses value))
    (unless (lookup-class env (ty-predicate-class sc) :no-error t)
      (error "Superclass ~S is not defined." sc)))
  (set-class env symbol value))

(define-env-updater add-instance (env class value)
  ;; Ensure the class is defined
  (unless (lookup-class env class)
    (error "Class ~S does not exist." class))

  (let* ((instances (lookup-class-instances env class))
         (index (position-if (lambda (inst)
                               (handler-case (or (predicate-mgu (ty-class-instance-predicate value)
                                                                (ty-class-instance-predicate inst))
                                                 t)
                                 (predicate-unification-error () nil)))
                             instances)))
    (cond (index
           ;; If we have the same instance then simply overwrite the old one
           (let ((inst (nth index instances)))
             (assert (eql (ty-class-instance-codegen-sym value)
                          (ty-class-instance-codegen-sym inst)))
             (handler-case
                 (predicate-match (ty-class-instance-predicate value)
                                  (ty-class-instance-predicate inst))
               (predicate-unification-error ()
                 (error 'overlapping-instance-error
                        :inst1 (ty-class-instance-predicate value)
                        :inst2 (ty-class-instance-predicate inst))))
             (setf (nth index instances) value)))
          (t
           (push value instances)))
    (setf env (env:set env :instance class instances))
    (env:set env :codegen-sym (ty-class-instance-codegen-sym value) value)))

(define-env-updater set-method-inline (env method instance codegen-sym)
  (env:set env :method-inline (cons method instance) codegen-sym))

(defun lookup-method-inline (env method instance &key no-error)
  (or (env:get env :method-inline (cons method instance))
      (unless no-error
        (error "Unable to find inline method for method ~A on instance ~S." method instance))))

(define-env-updater set-code (env name code)
  (env:set env :code name code))

(defun lookup-code (env name &key no-error)
  (or (env:get env :code name)
      (unless no-error
        (error "Unable to find code for function ~A." name))))

(define-env-updater add-specialization (env entry)
  (let* ((from (specialization-entry-from entry))
         (to (specialization-entry-to entry))
         (to-ty (specialization-entry-to-ty entry))
         (to-scheme (quantify (type-variables to-ty)
                              (qualify nil to-ty)))
         (to-specializations (env:get env :specialization from)))
    (loop :for elem :across to-specializations
          :for index :from 0
          :do (let ((type (specialization-entry-to-ty elem)))
                (when (equalp to-scheme
                              (quantify (type-variables type)
                                        (qualify nil type)))
                  (setf (nth index to-specializations) entry)
                  (return-from add-specialization
                    (env:set env :specialization from to-specializations)))
                (handler-case
                    (progn
                      (unify nil to-ty (specialization-entry-to-ty elem))
                      (error 'overlapping-specialization-error
                             :new to
                             :existing (specialization-entry-to elem)))
                  (unification-error ()))))
    (env:set env :specialization from (cons entry to-specializations))))

(defun lookup-specialization (env from to &key (no-error nil))
  (dolist (elem (env:get env :specialization from))
    (when (eq to (specialization-entry-to elem))
      (return-from lookup-specialization elem)))

  (unless no-error
    (error "Unable to find specialization from ~A to ~A" from to)))

(defun lookup-specialization-by-type (env from ty)
  (dolist (elem (env:get env :specialization from))
    (handler-case
        (progn
          (match (specialization-entry-to-ty elem) ty)
          (return-from lookup-specialization-by-type elem))
      (error:coalton-internal-type-error (e)
        (declare (ignore e))))))

(defun lookup-fundep-environment (env class)
  (env:get env :fundep class))

(defstruct fundep-entry
  (from      (util:required 'from) :type ty-list :read-only t)
  (to        (util:required 'to)   :type ty-list :read-only t))

(defmethod make-load-form ((self fundep-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(define-env-updater update-instance-fundeps (env pred)
  (let* ((class (lookup-class env (ty-predicate-class pred)))
         (fundep-env (lookup-fundep-environment env (ty-predicate-class pred)))
         (class-variable-map (ty-class-class-variable-map class)))

    (loop :for fundep :in (ty-class-fundeps class)

          :for from-tys
            := (mapcar
                (lambda (var)
                  (nth (gethash var class-variable-map) (ty-predicate-types pred)))
                (fundep-from fundep))

          :for to-tys
            := (mapcar
                (lambda (var)
                  (nth (gethash var class-variable-map) (ty-predicate-types pred)))
                (fundep-to fundep))

          :do (block update-block
                ;; Try to find a matching relation for the current fundep
                (dolist (entry fundep-env)
                  ;; If the left side matches checking either direction
                  (when (or (handler-case
                                (progn
                                  (match-list (fundep-entry-from entry) from-tys)
                                  t)
                              (unification-error ()
                                nil))
                            (handler-case
                                (progn
                                  (match-list from-tys (fundep-entry-from entry))
                                  t)
                              (unification-error ()
                                nil)))
                    (handler-case
                        (progn
                          (match-list (fundep-entry-to entry) to-tys)
                          ;; Exit upon finding a match
                          (return-from update-block))

                      ;; If the right side does not match
                      ;; signal an error
                      (unification-error ()
                        (error-fundep-conflict
                         env
                         class
                         pred
                         fundep
                         (fundep-entry-from entry)
                         from-tys
                         (fundep-entry-to entry)
                         to-tys)))))

                ;; Insert a new relation if there wasn't a match
                (setf env
                      (env:set env :fundep class (cons
                                                  (make-fundep-entry :from from-tys :to to-tys)
                                                  (env:get env :fundep class)))))))
  env)

(defun error-fundep-conflict (env class pred fundep old-from-tys new-from-tys old-to-tys new-to-tys)
  "Finds a conflicting instance and signals an error"
  (let* ((class-name (ty-class-name class))

         (from-tys_ (copy-list new-from-tys))

         (vars
           (loop :for v :in (ty-class-class-variables class)
                 :if (find v (fundep-from fundep))
                   :collect (prog1
                              (car from-tys_)
                              (setf from-tys_ (cdr from-tys_)))
                 :else
                   :collect (make-variable)))

         (new-pred (make-ty-predicate :class class-name :types vars :source (ty-predicate-source pred))))

    (dolist (inst (lookup-class-instances env class-name))
      (handler-case
          (progn
            (predicate-mgu new-pred (ty-class-instance-predicate inst))
            (error 'fundep-conflict
                   :new-pred pred
                   :old-pred (ty-class-instance-predicate inst)
                   :fundep fundep
                   :class class-name
                   :class-vars (ty-class-class-variables class)
                   :class-fundeps (ty-class-fundeps class)
                   :old-from-tys old-from-tys
                   :new-from-tys new-from-tys
                   :old-to-tys old-to-tys
                   :new-to-tys new-to-tys))
        (predicate-unification-error () nil)))

    ;; If there was a fundep conflict, one of the instances should have matched
    (util:unreachable)))

(defun solve-fundeps (env preds subs)
  ;; If no predicates have fundeps, then exit early
  (unless (loop :for pred :in preds
                :for class-name := (ty-predicate-class pred)
                :for class := (lookup-class env class-name)
                :when (ty-class-fundeps class)
                  :collect class)
    (return-from solve-fundeps (values preds subs)))

  (loop :with new-subs := nil
        :with preds-generated := nil
        :for i :below +fundep-max-depth+
        :do
           (setf new-subs subs)

           (loop :for pred :in preds
                 :for class-name := (ty-predicate-class pred)
                 :for class := (lookup-class env class-name)
                 ;; If there are super-predicates then add those
                 ;; predicates into the list of preds and remove
                 ;; this predicate from the list since it will give
                 ;; us no new type information. Additionally,
                 ;; restart the current check to avoid terminating
                 ;; early when no subs are generated.
                 :for instance := (lookup-class-instance env pred :no-error t)

                 :when instance
                   ;; Since we allow for type variables in the
                   ;; constraints which do not appear in the
                   ;; predicate, we need to create a full fresh set of
                   ;; predicates, rather than just matching the head.
                   :do (let* ((fresh-instance-preds
                                (fresh-preds
                                 (cons (ty-class-instance-predicate instance)
                                       (ty-class-instance-constraints instance))))
                              (instance-head (car fresh-instance-preds))
                              (instance-context (cdr fresh-instance-preds))

                              (instance-subs (predicate-match instance-head pred new-subs)))
                         (loop :for new-pred :in instance-context :do
                           (push (make-ty-predicate
                                  :class (ty-predicate-class new-pred)
                                  :types (apply-substitution instance-subs (ty-predicate-types new-pred)))
                                 preds))

                         (setf preds (remove pred preds :test #'eq))
                         (setf preds-generated t)
                         (return))

                 :when (ty-class-fundeps class)
                   :do (setf new-subs (generate-fundep-subs% env (apply-substitution new-subs pred) new-subs)))
           (if (and (not preds-generated)
                    (or (equalp new-subs subs)
                        (null preds)))
               (return-from solve-fundeps (values preds subs))
               (setf subs new-subs))
           (setf preds-generated nil)
           (setf preds (apply-substitution subs preds))
        :finally (util:coalton-bug "Fundep solving failed to fixpoint")))


(defun generate-fundep-subs% (env pred subs)
  (let* ((class-name (ty-predicate-class pred))

         (class (lookup-class env class-name))

         (class-variable-map (ty-class-class-variable-map class))

         (fundep-env (lookup-fundep-environment env class-name)))

    (loop :for fundep :in (ty-class-fundeps class)
          :when fundep-env
            :do (setf subs (generate-fundep-subs-for-pred% pred fundep-env class-variable-map fundep subs)))

    subs))

(defun generate-fundep-subs-for-pred% (pred state class-variable-map fundep subs)
  (let* ((from-tys
           (mapcar
            (lambda (var)
              (nth (gethash var class-variable-map) (ty-predicate-types pred)))
            (fundep-from fundep)))

         (to-tys
           (mapcar
            (lambda (var)
              (nth (gethash var class-variable-map) (ty-predicate-types pred)))
            (fundep-to fundep))))

    (dolist (entry state)
      (handler-case
          (let* ((fresh-entry (fresh-fundep-entry entry))
                 (left-subs (match-list from-tys (fundep-entry-from fresh-entry)))
                 (right-side (apply-substitution left-subs (fundep-entry-to fresh-entry))))
            (return-from generate-fundep-subs-for-pred% (unify-list subs to-tys right-side)))
        (unification-error () nil))))

  subs)

(defun fresh-fundep-entry (entry)
  (let* ((from-pred (make-ty-predicate
                     :class nil
                     :types (fundep-entry-from entry)))
         (to-pred (make-ty-predicate
                   :class nil
                   :types (fundep-entry-to entry)))
         (fresh-preds (fresh-preds (list from-pred to-pred))))
    (make-fundep-entry
     :from (ty-predicate-types (first fresh-preds))
     :to (ty-predicate-types (second fresh-preds)))))
