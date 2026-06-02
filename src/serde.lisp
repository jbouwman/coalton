;;;; Environment-edit serialization boundary (GOAL-025 step 4).
;;;;
;;;; The environment replay (see entry.lisp, make-environment-updater)
;;;; records the edits a compilation makes to the global environment and
;;;; emits source that re-applies them at load time. Historically each edit
;;;; argument was embedded as a readably-printed literal (util:runtime-quote
;;;; + make-load-form), which couples replay to the in-memory representation
;;;; of every compiler structure it touches -- a CLOS standard-object is not
;;;; readably printable, so that path blocks converting the embedded families.
;;;;
;;;; This file is the one place allowed to know the internal representation.
;;;; It encodes each structured edit argument as representation-independent
;;;; plain data (symbols, keywords, numbers, strings, conses) and decodes it
;;;; back through the public constructors. Replay no longer depends on how a
;;;; type, scheme, entry, or pattern is represented in memory, only on its
;;;; accessor and constructor names -- which the struct->CLOS conversion
;;;; preserves. See docs/internals/design-docs/environment-replay-and-an-edit-ir.md.
;;;;
;;;; Internal (::) access is deliberate here: the boundary's purpose is to
;;;; read and rebuild compiler internals as data.
;;;;
;;;; Generic-variable identity. Within one type scheme the same quantified
;;;; variable appears in several places -- e.g. a constructor's stored type
;;;; (Array :t) -> (Wrapper :t) mentions :t twice -- and parts of the compiler
;;;; compare those occurrences by object identity, not by index (the
;;;; transparent-type case in lisp-type tests (eq inner-var outer-var)). The
;;;; previous readable-literal replay preserved that sharing through
;;;; *print-circle*. Here a scheme decodes its quantified variables once, into
;;;; an immutable vector indexed by variable index, and threads that vector
;;;; through type decoding; each (generic-variable INDEX) occurrence resolves
;;;; to the same object by lookup. No mutation and no shared decode state.

(defpackage #:coalton-impl/serde
  (:use #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:kinds #:coalton-impl/typechecker/kinds)
   (#:ty #:coalton-impl/typechecker/types)
   (#:scheme #:coalton-impl/typechecker/scheme)
   (#:pred #:coalton-impl/typechecker/predicate)
   (#:tcenv #:coalton-impl/typechecker/environment)
   (#:src #:coalton-impl/source)
   (#:pat #:coalton-impl/parser/pattern))
  (:export
   #:edit-arg-forms                     ; FUNCTION (compile-time)
   ;; Decoders, referenced by the emitted replay source (load-time).
   #:decode-scheme
   #:decode-predicate #:decode-predicates
   #:decode-pattern #:decode-patterns
   #:decode-name-entry
   #:decode-function-env-entry
   #:decode-specialization-entry
   #:decode-type-entry
   #:decode-constructor-entry
   #:decode-type-alias-entry
   #:decode-struct-entry
   #:decode-ty-class
   #:decode-ty-class-instance))

(in-package #:coalton-impl/serde)

;;;; ----------------------------------------------------------------------
;;;; Kinds: star | (arrow kind kind)

(defun encode-kind (k)
  (typecase k
    (kinds::kstar 'star)
    (kinds::kfun (list 'arrow (encode-kind (kinds::kfun-from k))
                              (encode-kind (kinds::kfun-to k))))
    (t (error "unencodable kind: ~S" k))))

(defun decode-kind (form)
  (cond
    ((eq form 'star) kinds::+kstar+)
    ((and (consp form) (eq (first form) 'arrow))
     (kinds::make-kfun :from (decode-kind (second form))
                       :to (decode-kind (third form))))
    (t (error "undecodable kind: ~S" form))))

;;;; ----------------------------------------------------------------------
;;;; Types.
;;;;
;;;; A decoded type is given the scheme's GENERIC-VARIABLES vector (empty when
;;;; the type is not part of a scheme, e.g. an entry's parameter type), so a
;;;; (generic-variable INDEX) occurrence resolves to the shared object.

(defun encode-keywords (entries open-p)
  (if (and (null entries) (not open-p))
      '()
      (list :keys (mapcar (lambda (e)
                            (list (ty::keyword-ty-entry-keyword e)
                                  (encode-type (ty::keyword-ty-entry-type e))))
                          entries)
            :open open-p)))

(defun decode-keywords (form generic-variables)
  (if (null form)
      (values '() nil)
      (destructuring-bind (&key keys open) form
        (values (mapcar (lambda (pair)
                          (ty::make-keyword-ty-entry
                           :keyword (first pair)
                           :type (decode-type (second pair) generic-variables)))
                        keys)
                open))))

(defun encode-type (type)
  (typecase type
    (ty::tycon  (list 'type-constructor (ty::tycon-name type)
                      (encode-kind (ty::tycon-kind type))))
    (ty::tgen   `(generic-variable ,(ty::tgen-id type) ,(ty::tgen-source-name type)
                                   ,@(when (ty::tgen-allow-result-p type) '(:result))))
    (ty::tapp   (list 'type-application (encode-type (ty::tapp-from type))
                                        (encode-type (ty::tapp-to type))))
    (ty::function-ty
     (list 'function-type
           (mapcar #'encode-type (ty::function-ty-positional-input-types type))
           (encode-keywords (ty::function-ty-keyword-input-types type)
                            (ty::function-ty-keyword-open-p type))
           (mapcar #'encode-type (ty::function-ty-output-types type))))
    (ty::result-ty
     (list 'result-type (mapcar #'encode-type (ty::result-ty-output-types type))))
    (ty::tyvar  `(type-variable ,(ty::tyvar-id type) ,(encode-kind (ty::tyvar-kind type))
                                ,(ty::tyvar-source-name type)
                                ,@(when (ty::tyvar-allow-result-p type) '(:result))))
    (t (error "unencodable type: ~S" type))))

(defun decode-type (form &optional (generic-variables #()))
  (ecase (first form)
    (type-constructor
     (ty::make-tycon :name (second form) :kind (decode-kind (third form))))
    (generic-variable
     (let ((index (second form)))
       (if (< index (length generic-variables))
           (aref generic-variables index)
           ;; Outside a scheme there is no binder; this should not occur for
           ;; stored schemes, but reconstruct independently if it does.
           (ty::make-tgen :id index :source-name (third form)
                          :allow-result-p (and (member :result (cdddr form)) t)))))
    (type-application
     (ty::make-tapp :from (decode-type (second form) generic-variables)
                    :to (decode-type (third form) generic-variables)))
    (function-type
     (destructuring-bind (inputs keywords outputs) (rest form)
       (multiple-value-bind (entries open-p) (decode-keywords keywords generic-variables)
         (ty::make-function-ty
          :positional-input-types (decode-types inputs generic-variables)
          :keyword-input-types entries
          :keyword-open-p open-p
          :output-types (decode-types outputs generic-variables)))))
    (result-type
     (ty::make-result-ty :output-types (decode-types (second form) generic-variables)))
    (type-variable
     (ty::make-tyvar :id (second form) :kind (decode-kind (third form))
                     :source-name (fourth form)
                     :allow-result-p (and (member :result (cddddr form)) t)))))

(defun encode-types (ts) (mapcar #'encode-type ts))
(defun decode-types (fs generic-variables)
  (mapcar (lambda (f) (decode-type f generic-variables)) fs))

;;;; ----------------------------------------------------------------------
;;;; Predicates and schemes

(defun encode-predicate (p)
  (list 'predicate (pred::ty-predicate-class p)
        (mapcar #'encode-type (pred::ty-predicate-types p))
        (encode-location (pred::ty-predicate-location p))))

(defun decode-predicate (form &optional (generic-variables #()))
  (pred::make-ty-predicate :class (second form)
                           :types (decode-types (third form) generic-variables)
                           :location (decode-location (fourth form))))

(defun encode-predicates (ps) (mapcar #'encode-predicate ps))
(defun decode-predicates (fs &optional (generic-variables #()))
  (mapcar (lambda (f) (decode-predicate f generic-variables)) fs))

(defun encode-scheme (s)
  (let ((q (scheme::ty-scheme-type s)))
    (list 'type-scheme
          (scheme::ty-scheme-explicit-p s)
          (mapcar #'encode-kind (scheme::ty-scheme-kinds s))
          (mapcar #'encode-predicate (pred::qualified-ty-predicates q))
          (encode-type (pred::qualified-ty-type q)))))

(defun collect-generic-variable-info (form acc)
  "Accumulate an alist index -> (allow-result-p source-name) over an encoded
type form. Occurrences of one index agree, so the first seen wins."
  (cond
    ((atom form) acc)
    ((eq (first form) 'generic-variable)
     (let ((index (second form)))
       (if (assoc index acc)
           acc
           (acons index (list (and (member :result (cdddr form)) t) (third form))
                  acc))))
    (t (reduce (lambda (a sub) (collect-generic-variable-info sub a))
               form :initial-value acc))))

(defun decode-scheme (form)
  (destructuring-bind (tag explicit kinds-form predicates-form type-form) form
    (declare (ignore tag))
    (let* ((arity (length kinds-form))
           (info (reduce (lambda (a f) (collect-generic-variable-info f a))
                         (cons type-form predicates-form) :initial-value '()))
           (generic-variables
             (coerce (loop :for index :below arity
                           :for (result-p source-name) := (cdr (assoc index info))
                           :collect (ty::make-tgen :id index
                                                   :allow-result-p result-p
                                                   :source-name source-name))
                     'vector)))
      (scheme::make-ty-scheme
       :explicit-p explicit
       :kinds (mapcar #'decode-kind kinds-form)
       :type (pred::make-qualified-ty
              :predicates (decode-predicates predicates-form generic-variables)
              :type (decode-type type-form generic-variables))))))

;;;; ----------------------------------------------------------------------
;;;; Source locations: a source reference (name) plus span, not a
;;;; reconstructed source-file. The loader rebuilds a named, empty-content
;;;; source string carrying the span; the original file text is not embedded
;;;; (intended -- it removes the path leak and the read-eval literal).

(defun encode-location (loc)
  (and loc (list 'location (src::source-name (src::location-source loc))
                          (src::location-span loc))))

(defun decode-location (form)
  (and form (src::make-location (src::make-source-string "" :name (second form))
                                (third form))))

;;;; ----------------------------------------------------------------------
;;;; Environment entries. Each encodes as a tagged plist; type-bearing slots
;;;; go through the encoders above, everything else (symbols, keywords,
;;;; strings, fixnums, CL type designators) is plain data carried verbatim.
;;;;
;;;; Entry parameter types use type variables, not the scheme's quantified
;;;; generic variables, so their decoders pass no generic-variable vector.

;;; name-entry

(defun encode-name-entry (e)
  (list :name-entry
        :name (tcenv::name-entry-name e)
        :type (tcenv::name-entry-type e)
        :docstring (tcenv::name-entry-docstring e)
        :location (encode-location (tcenv::name-entry-location e))))

(defun decode-name-entry (f)
  (destructuring-bind (&key name type docstring location) (cdr f)
    (tcenv::make-name-entry :name name :type type :docstring docstring
                            :location (decode-location location))))

;;; function-env-entry

(defun encode-function-env-entry (e)
  (list :function :name (tcenv::function-env-entry-name e)
                  :arity (tcenv::function-env-entry-arity e)
                  :inline-p (tcenv::function-env-entry-inline-p e)))

(defun decode-function-env-entry (f)
  (destructuring-bind (&key name arity inline-p) (cdr f)
    (tcenv::make-function-env-entry :name name :arity arity :inline-p inline-p)))

;;; specialization-entry

(defun encode-specialization-entry (e)
  (list :specialization :from (tcenv::specialization-entry-from e)
                        :to (tcenv::specialization-entry-to e)
                        :to-ty (encode-type (tcenv::specialization-entry-to-ty e))))

(defun decode-specialization-entry (f)
  (destructuring-bind (&key from to to-ty) (cdr f)
    (tcenv::make-specialization-entry :from from :to to :to-ty (decode-type to-ty))))

;;; type-entry

(defun encode-type-entry (e)
  (list :type-entry
        :name (tcenv::type-entry-name e)
        :source-name (tcenv::type-entry-source-name e)
        :runtime-type (tcenv::type-entry-runtime-type e)
        :type (encode-type (tcenv::type-entry-type e))
        :tyvars (encode-types (tcenv::type-entry-tyvars e))
        :variances (tcenv::type-entry-variances e)
        :constructors (tcenv::type-entry-constructors e)
        :explicit-repr (tcenv::type-entry-explicit-repr e)
        :enum-repr (tcenv::type-entry-enum-repr e)
        :newtype (tcenv::type-entry-newtype e)
        :docstring (tcenv::type-entry-docstring e)
        :location (encode-location (tcenv::type-entry-location e))
        :exception-p (tcenv::type-entry-exception-p e)
        :resumption-p (tcenv::type-entry-resumption-p e)))

(defun decode-type-entry (f)
  (destructuring-bind (&key name source-name runtime-type type tyvars variances
                            constructors explicit-repr enum-repr newtype docstring
                            location exception-p resumption-p)
      (cdr f)
    (tcenv::make-type-entry
     :name name :source-name source-name :runtime-type runtime-type
     :type (decode-type type) :tyvars (decode-types tyvars #()) :variances variances
     :constructors constructors :explicit-repr explicit-repr :enum-repr enum-repr
     :newtype newtype :docstring docstring :location (decode-location location)
     :exception-p exception-p :resumption-p resumption-p)))

;;; constructor-entry

(defun encode-constructor-entry (e)
  (list :constructor-entry
        :name (tcenv::constructor-entry-name e)
        :source-name (tcenv::constructor-entry-source-name e)
        :arity (tcenv::constructor-entry-arity e)
        :constructs (tcenv::constructor-entry-constructs e)
        :classname (tcenv::constructor-entry-classname e)
        :docstring (tcenv::constructor-entry-docstring e)
        :compressed-repr (tcenv::constructor-entry-compressed-repr e)))

(defun decode-constructor-entry (f)
  (destructuring-bind (&key name source-name arity constructs classname docstring
                            compressed-repr)
      (cdr f)
    (tcenv::make-constructor-entry
     :name name :source-name source-name :arity arity :constructs constructs
     :classname classname :docstring docstring :compressed-repr compressed-repr)))

;;; type-alias-entry

(defun encode-type-alias-entry (e)
  (list :type-alias-entry
        :name (tcenv::type-alias-entry-name e)
        :source-name (tcenv::type-alias-entry-source-name e)
        :tyvars (encode-types (tcenv::type-alias-entry-tyvars e))
        :type (encode-type (tcenv::type-alias-entry-type e))
        :docstring (tcenv::type-alias-entry-docstring e)))

(defun decode-type-alias-entry (f)
  (destructuring-bind (&key name source-name tyvars type docstring) (cdr f)
    (tcenv::make-type-alias-entry
     :name name :source-name source-name :tyvars (decode-types tyvars #())
     :type (decode-type type) :docstring docstring)))

;;; struct-field / struct-entry

(defun encode-struct-field (e)
  (list :struct-field :name (tcenv::struct-field-name e)
                      :type (encode-type (tcenv::struct-field-type e))
                      :index (tcenv::struct-field-index e)
                      :docstring (tcenv::struct-field-docstring e)))

(defun decode-struct-field (f)
  (destructuring-bind (&key name type index docstring) (cdr f)
    (tcenv::make-struct-field :name name :type (decode-type type)
                              :index index :docstring docstring)))

(defun encode-struct-entry (e)
  (list :struct-entry :name (tcenv::struct-entry-name e)
                      :source-name (tcenv::struct-entry-source-name e)
                      :fields (mapcar #'encode-struct-field (tcenv::struct-entry-fields e))
                      :docstring (tcenv::struct-entry-docstring e)))

(defun decode-struct-entry (f)
  (destructuring-bind (&key name source-name fields docstring) (cdr f)
    (tcenv::make-struct-entry
     :name name :source-name source-name
     :fields (mapcar #'decode-struct-field fields) :docstring docstring)))

;;; ty-class-method / ty-class

(defun encode-ty-class-method (e)
  (list :method :name (tcenv::ty-class-method-name e)
                :type (encode-scheme (tcenv::ty-class-method-type e))
                :outer-tvars (encode-types (tcenv::ty-class-method-outer-tvars e))
                :explicit-tvars (encode-types (tcenv::ty-class-method-explicit-tvars e))
                :docstring (tcenv::ty-class-method-docstring e)))

(defun decode-ty-class-method (f)
  (destructuring-bind (&key name type outer-tvars explicit-tvars docstring) (cdr f)
    (tcenv::make-ty-class-method
     :name name :type (decode-scheme type)
     :outer-tvars (decode-types outer-tvars #())
     :explicit-tvars (decode-types explicit-tvars #()) :docstring docstring)))

(defun encode-superclass-dict (dict)
  ;; alist of (ty-predicate . codegen-data); the cdr is plain data carried
  ;; verbatim (it survives the existing readable-print replay unchanged).
  (mapcar (lambda (pair) (cons (encode-predicate (car pair)) (cdr pair))) dict))

(defun decode-superclass-dict (form)
  (mapcar (lambda (pair) (cons (decode-predicate (car pair)) (cdr pair))) form))

(defun encode-ty-class (e)
  (list :class
        :name (tcenv::ty-class-name e)
        :source-name (tcenv::ty-class-source-name e)
        :predicate (encode-predicate (tcenv::ty-class-predicate e))
        :superclasses (encode-predicates (tcenv::ty-class-superclasses e))
        :class-variables (tcenv::ty-class-class-variables e)
        :fundeps (tcenv::ty-class-fundeps e)
        :unqualified-methods (mapcar #'encode-ty-class-method
                                     (tcenv::ty-class-unqualified-methods e))
        :codegen-sym (tcenv::ty-class-codegen-sym e)
        :superclass-dict (encode-superclass-dict (tcenv::ty-class-superclass-dict e))
        :superclass-map (tcenv::ty-class-superclass-map e)
        :docstring (tcenv::ty-class-docstring e)
        :location (encode-location (tcenv::ty-class-location e))))

(defun decode-ty-class (f)
  (destructuring-bind (&key name source-name predicate superclasses class-variables
                            fundeps unqualified-methods codegen-sym superclass-dict
                            superclass-map docstring location)
      (cdr f)
    (tcenv::make-ty-class
     :name name :source-name source-name :predicate (decode-predicate predicate)
     :superclasses (decode-predicates superclasses) :class-variables class-variables
     :fundeps fundeps
     :unqualified-methods (mapcar #'decode-ty-class-method unqualified-methods)
     :codegen-sym codegen-sym
     :superclass-dict (decode-superclass-dict superclass-dict)
     :superclass-map superclass-map :docstring docstring
     :location (decode-location location))))

;;; ty-class-instance

(defun encode-ty-class-instance (e)
  (list :instance
        :constraints (encode-predicates (tcenv::ty-class-instance-constraints e))
        :predicate (encode-predicate (tcenv::ty-class-instance-predicate e))
        :codegen-sym (tcenv::ty-class-instance-codegen-sym e)
        :method-codegen-syms (tcenv::ty-class-instance-method-codegen-syms e)
        :method-codegen-inline-p (tcenv::ty-class-instance-method-codegen-inline-p e)
        :docstring (tcenv::ty-class-instance-docstring e)
        :location (encode-location (tcenv::ty-class-instance-location e))))

(defun decode-ty-class-instance (f)
  (destructuring-bind (&key constraints predicate codegen-sym method-codegen-syms
                            method-codegen-inline-p docstring location)
      (cdr f)
    (tcenv::make-ty-class-instance
     :constraints (decode-predicates constraints) :predicate (decode-predicate predicate)
     :codegen-sym codegen-sym :method-codegen-syms method-codegen-syms
     :method-codegen-inline-p method-codegen-inline-p :docstring docstring
     :location (decode-location location))))

;;;; ----------------------------------------------------------------------
;;;; Parser patterns (the set-function-source-parameter-names payload).

(defun encode-pattern (p)
  (typecase p
    (pat::pattern-var
     (list 'pattern-variable (pat::pattern-var-name p) (pat::pattern-var-orig-name p)
           (encode-location (pat::pattern-location p))))
    (pat::pattern-literal
     (list 'pattern-literal (pat::pattern-literal-value p)
           (encode-location (pat::pattern-location p))))
    (pat::pattern-wildcard
     (list 'pattern-wildcard (encode-location (pat::pattern-location p))))
    (pat::pattern-binding
     (list 'pattern-binding (encode-pattern (pat::pattern-binding-var p))
           (encode-pattern (pat::pattern-binding-pattern p))
           (encode-location (pat::pattern-location p))))
    (pat::pattern-constructor
     (list 'pattern-constructor (pat::pattern-constructor-name p)
           (mapcar #'encode-pattern (pat::pattern-constructor-patterns p))
           (encode-location (pat::pattern-location p))))
    (t (error "unencodable pattern: ~S" p))))

(defun decode-pattern (f)
  (ecase (first f)
    (pattern-variable
     (pat::make-pattern-var :name (second f) :orig-name (third f)
                            :location (decode-location (fourth f))))
    (pattern-literal
     (pat::make-pattern-literal :value (second f)
                                :location (decode-location (third f))))
    (pattern-wildcard
     (pat::make-pattern-wildcard :location (decode-location (second f))))
    (pattern-binding
     (pat::make-pattern-binding :var (decode-pattern (second f))
                                :pattern (decode-pattern (third f))
                                :location (decode-location (fourth f))))
    (pattern-constructor
     (pat::make-pattern-constructor :name (second f)
                                    :patterns (mapcar #'decode-pattern (third f))
                                    :location (decode-location (fourth f))))))

(defun encode-patterns (ps) (mapcar #'encode-pattern ps))
(defun decode-patterns (fs) (mapcar #'decode-pattern fs))

;;;; ----------------------------------------------------------------------
;;;; Per-operation argument codecs.
;;;;
;;;; Each environment-update function (see typechecker/environment.lisp,
;;;; define-env-updater) maps to one codec keyword per logged value argument
;;;; (the environment argument is not logged). The codec names how to encode
;;;; that argument as data and which decoder rebuilds it at load time.
;;;;
;;;; :plain  -- carry through the existing literal path (symbols, fixnums,
;;;;            keywords, CL type designators): already representation-free.
;;;; :code   -- the codegen body (set-code); deferred to the literal path
;;;;            until the codegen node family is CLOS (GOAL-025 step 4).

(defparameter +arg-decoders+
  `((:scheme               . decode-scheme)
    (:predicate            . decode-predicate)
    (:predicate-list       . decode-predicates)
    (:pattern-list         . decode-patterns)
    (:name-entry           . decode-name-entry)
    (:function-entry       . decode-function-env-entry)
    (:specialization-entry . decode-specialization-entry)
    (:type-entry           . decode-type-entry)
    (:constructor-entry    . decode-constructor-entry)
    (:type-alias-entry     . decode-type-alias-entry)
    (:struct-entry         . decode-struct-entry)
    (:class                . decode-ty-class)
    (:instance             . decode-ty-class-instance))
  "Codec keyword -> decoder symbol, exported and referenced by emitted replay source.")

(defun arg-encoder (kind)
  "The compile-time encoder for a codec KIND, or NIL for the literal path."
  (ecase kind
    ((:plain :code)        nil)
    (:scheme               #'encode-scheme)
    (:predicate            #'encode-predicate)
    (:predicate-list       #'encode-predicates)
    (:pattern-list         #'encode-patterns)
    (:name-entry           #'encode-name-entry)
    (:function-entry       #'encode-function-env-entry)
    (:specialization-entry #'encode-specialization-entry)
    (:type-entry           #'encode-type-entry)
    (:constructor-entry    #'encode-constructor-entry)
    (:type-alias-entry     #'encode-type-alias-entry)
    (:struct-entry         #'encode-struct-entry)
    (:class                #'encode-ty-class)
    (:instance             #'encode-ty-class-instance)))

(defparameter +edit-op-codecs+
  (list
   (cons 'tcenv:set-value-type                       '(:plain :scheme))
   (cons 'tcenv:unset-value-type                     '(:plain))
   (cons 'tcenv:set-type                             '(:plain :type-entry))
   (cons 'tcenv:set-constructor                      '(:plain :constructor-entry))
   (cons 'tcenv:unset-constructor                    '(:plain))
   (cons 'tcenv:set-type-alias                       '(:plain :type-alias-entry))
   (cons 'tcenv:unset-type-alias                     '(:plain))
   (cons 'tcenv:set-struct                           '(:plain :struct-entry))
   (cons 'tcenv:unset-struct                         '(:plain))
   (cons 'tcenv:set-class                            '(:plain :class))
   (cons 'tcenv:set-function                         '(:plain :function-entry))
   (cons 'tcenv:unset-function                       '(:plain))
   (cons 'tcenv:set-name                             '(:plain :name-entry))
   (cons 'tcenv:unset-name                           '(:plain))
   (cons 'tcenv:set-function-source-parameter-names  '(:plain :pattern-list))
   (cons 'tcenv:unset-function-source-parameter-names '(:plain))
   (cons 'tcenv:add-instance                         '(:plain :instance))
   (cons 'tcenv:set-method-inline                    '(:plain :plain :plain))
   (cons 'tcenv:set-code                             '(:plain :code))
   (cons 'tcenv:add-specialization                   '(:specialization-entry))
   (cons 'tcenv:initialize-fundep-environment        '(:plain))
   (cons 'tcenv:update-instance-fundeps              '(:predicate :predicate-list)))
  "Environment-update function symbol -> codec list, one keyword per value argument.")

(defun encode-edit-arg (kind arg)
  "Return a form that reconstructs ARG at load time for codec KIND."
  (let ((encoder (arg-encoder kind)))
    (if (null encoder)
        (util:runtime-quote arg)
        (let ((decoder (cdr (assoc kind +arg-decoders+))))
          (list decoder (list 'quote (funcall encoder arg)))))))

(defun edit-arg-forms (op args)
  "Return forms reconstructing the logged value ARGS of update OP at load time.

Structured arguments are encoded now as representation-independent data and
wrapped in a decoder call; :plain/:code arguments keep the literal path. An
unrecognized OP falls back entirely to the literal path."
  (let ((codecs (cdr (assoc op +edit-op-codecs+))))
    (if (null codecs)
        (mapcar #'util:runtime-quote args)
        (loop :for arg :in args
              :for kind :in codecs
              :collect (encode-edit-arg kind arg)))))
