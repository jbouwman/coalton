;;;; Prototype of the edit-IR serialization boundary, for the type layer.
;;;;
;;;; GOAL-025 step 4 (see docs/internals/design-docs/environment-replay-and-an-edit-ir.md):
;;;; the environment replay should serialize edits as representation-independent
;;;; data instead of embedding compiler-internal structs. This file prototypes
;;;; the core of that boundary -- encode/decode for kinds, types, predicates,
;;;; and type schemes -- as plain s-expressions, and validates it by
;;;; round-tripping every value-type scheme in the loaded stdlib environment
;;;; against ty-scheme= (alpha-equivalence).
;;;;
;;;; Load on top of :coalton, then call (coalton-serde:roundtrip-report).
;;;; Internal (::) access is used deliberately: this is the one place that is
;;;; allowed to know the internal representation -- the whole point of the
;;;; boundary.

(defpackage #:coalton-serde
  (:use #:cl)
  (:local-nicknames
   (#:ty #:coalton-impl/typechecker/types)
   (#:kinds #:coalton-impl/typechecker/kinds)
   (#:scheme #:coalton-impl/typechecker/scheme)
   (#:pred #:coalton-impl/typechecker/predicate)
   (#:tcenv #:coalton-impl/typechecker/environment)
   (#:src #:coalton-impl/source)
   (#:pat #:coalton-impl/parser/pattern)
   (#:cgast #:coalton-impl/codegen/ast)
   (#:algo #:coalton-impl/algorithm)
   (#:entry #:coalton-impl/entry))
  (:export #:encode-scheme #:decode-scheme
           #:encode-type #:decode-type
           #:encode-pattern #:decode-pattern
           #:encode-node
           #:roundtrip-report #:size-report
           #:roundtrip-entries-report
           #:roundtrip-patterns-report #:code-encode-report))

(in-package #:coalton-serde)

;;; Kinds: *  |  (-> kind kind)

(defun encode-kind (k)
  (typecase k
    (kinds::kstar '*)
    (kinds::kfun (list '-> (encode-kind (kinds::kfun-from k))
                            (encode-kind (kinds::kfun-to k))))
    (t (error "unencodable kind: ~S" k))))

(defun decode-kind (form)
  (cond
    ((eq form '*) kinds::+kstar+)
    ((and (consp form) (eq (first form) '->))
     (kinds::make-kfun :from (decode-kind (second form))
                       :to (decode-kind (third form))))
    (t (error "undecodable kind: ~S" form))))

;;; Types

(defun encode-keywords (entries open-p)
  (if (and (null entries) (not open-p))
      '()
      (list :keys (mapcar (lambda (e)
                            (list (ty::keyword-ty-entry-keyword e)
                                  (encode-type (ty::keyword-ty-entry-type e))))
                          entries)
            :open open-p)))

(defun decode-keywords (form)
  (if (null form)
      (values '() nil)
      (destructuring-bind (&key keys open) form
        (values (mapcar (lambda (pair)
                          (ty::make-keyword-ty-entry
                           :keyword (first pair)
                           :type (decode-type (second pair))))
                        keys)
                open))))

(defun encode-type (type)
  (typecase type
    (ty::tycon  (list 'con (ty::tycon-name type) (encode-kind (ty::tycon-kind type))))
    (ty::tgen   `(gen ,(ty::tgen-id type)
                      ,@(when (ty::tgen-allow-result-p type) '(:result))))
    (ty::tapp   (list 'app (encode-type (ty::tapp-from type))
                           (encode-type (ty::tapp-to type))))
    (ty::function-ty
     (list 'fn
           (mapcar #'encode-type (ty::function-ty-positional-input-types type))
           (encode-keywords (ty::function-ty-keyword-input-types type)
                            (ty::function-ty-keyword-open-p type))
           (mapcar #'encode-type (ty::function-ty-output-types type))))
    (ty::result-ty
     (list 'result (mapcar #'encode-type (ty::result-ty-output-types type))))
    (ty::tyvar  `(var ,(ty::tyvar-id type) ,(encode-kind (ty::tyvar-kind type))
                      ,@(when (ty::tyvar-allow-result-p type) '(:result))))
    (t (error "unencodable type: ~S" type))))

(defun decode-type (form)
  (ecase (first form)
    (con (ty::make-tycon :name (second form) :kind (decode-kind (third form))))
    (gen (ty::make-tgen :id (second form)
                        :allow-result-p (and (member :result (cddr form)) t)))
    (app (ty::make-tapp :from (decode-type (second form))
                        :to (decode-type (third form))))
    (fn (destructuring-bind (ins kw outs) (rest form)
          (multiple-value-bind (entries open-p) (decode-keywords kw)
            (ty::make-function-ty
             :positional-input-types (mapcar #'decode-type ins)
             :keyword-input-types entries
             :keyword-open-p open-p
             :output-types (mapcar #'decode-type outs)))))
    (result (ty::make-result-ty :output-types (mapcar #'decode-type (second form))))
    (var (ty::make-tyvar :id (second form) :kind (decode-kind (third form))
                         :allow-result-p (and (member :result (cdddr form)) t)))))

;;; Predicates and schemes

(defun encode-pred (p)
  (list 'pred (pred::ty-predicate-class p)
        (mapcar #'encode-type (pred::ty-predicate-types p))))

(defun decode-pred (form)
  (pred::make-ty-predicate :class (second form)
                           :types (mapcar #'decode-type (third form))))

(defun encode-scheme (s)
  (let ((q (scheme::ty-scheme-type s)))
    (list 'scheme
          (scheme::ty-scheme-explicit-p s)
          (mapcar #'encode-kind (scheme::ty-scheme-kinds s))
          (mapcar #'encode-pred (pred::qualified-ty-predicates q))
          (encode-type (pred::qualified-ty-type q)))))

(defun decode-scheme (form)
  (destructuring-bind (tag explicit kinds-form preds-form type-form) form
    (declare (ignore tag))
    (scheme::make-ty-scheme
     :explicit-p explicit
     :kinds (mapcar #'decode-kind kinds-form)
     :type (pred::make-qualified-ty
            :predicates (mapcar #'decode-pred preds-form)
            :type (decode-type type-form)))))

;;; Round-trip the entire stdlib value environment against ty-scheme=.

(defun roundtrip-report ()
  (let* ((env entry::*global-environment*)
         (schemes (algo::immutable-map-values
                   (tcenv::environment-value-environment env)))
         (total 0) (ok 0) (mismatch '()) (errored '()))
    (dolist (s schemes)
      (incf total)
      (handler-case
          (let ((round (decode-scheme (encode-scheme s))))
            (if (scheme::ty-scheme= s round)
                (incf ok)
                (push s mismatch)))
        (error (e) (push (cons s e) errored))))
    (format t "~&edit-IR type serde round-trip over stdlib value schemes~%")
    (format t "~60,,,'-<~>~%")
    (format t "schemes:   ~D~%" total)
    (format t "round-trip ok (ty-scheme=): ~D~%" ok)
    (format t "mismatch:  ~D~%" (length mismatch))
    (format t "errored:   ~D~%" (length errored))
    (when errored
      (format t "~%first errors:~%")
      (dolist (e (subseq errored 0 (min 5 (length errored))))
        (format t "  ~A~%" (cdr e))))
    (when mismatch
      (format t "~%first mismatch (original vs round-tripped encoding):~%")
      (let ((s (first mismatch)))
        (format t "  orig:  ~S~%  round: ~S~%"
                (encode-scheme s)
                (encode-scheme (decode-scheme (encode-scheme s))))))
    ;; A couple of sample encodings, to show the IR is plain data.
    (format t "~%sample encodings:~%")
    (dolist (s (subseq schemes 0 (min 3 (length schemes))))
      (format t "  ~S~%" (encode-scheme s)))
    (values total ok (length mismatch) (length errored))))

;;; Size comparison: the IR encoding vs the current replay representation
;;; (the readably-printed struct, which is what runtime-quote + make-load-form
;;; emit). Printed the same way the replay is (downcase, circle on), so the
;;; comparison reflects emitted bytes.

(defun replay-chars (obj)
  (with-standard-io-syntax
    (let ((*print-case* :downcase) (*print-circle* t) (*print-readably* t)
          (*print-right-margin* 80))
      (length (prin1-to-string obj)))))

(defun ir-chars (form)
  (with-standard-io-syntax
    (let ((*print-case* :downcase) (*print-circle* t) (*print-right-margin* 80))
      (length (prin1-to-string form)))))

(defun size-report ()
  (let* ((env entry::*global-environment*)
         (schemes (algo::immutable-map-values
                   (tcenv::environment-value-environment env)))
         (ir 0) (replay 0))
    (dolist (s schemes)
      (incf ir (ir-chars (encode-scheme s)))
      (incf replay (replay-chars s)))
    (format t "~&edit-IR type serde size, over ~D stdlib value schemes~%" (length schemes))
    (format t "~60,,,'-<~>~%")
    (format t "current replay (readable #S struct): ~:D chars~%" replay)
    (format t "edit-IR encoding:                    ~:D chars~%" ir)
    (format t "ratio (replay / IR):                 ~,2Fx~%" (/ (float replay) ir))
    (values replay ir)))

;;;; ----------------------------------------------------------------------
;;;; Entry encoders.
;;;;
;;;; Each environment entry encodes as a tagged plist; type-bearing slots go
;;;; through the type/scheme/predicate/location encoders above, everything
;;;; else (symbols, keywords, strings, fixnums, CL type designators) is plain
;;;; data carried verbatim. The probe confirmed type-entry-runtime-type and
;;;; constructor-entry-compressed-repr are plain data (nil/t/symbol/cons), so
;;;; there is no opaque residual at the entry layer.

(defun encode-location (loc)
  (and loc (list 'loc (src::source-name (src::location-source loc))
                     (src::location-span loc))))

(defun decode-location (form)
  (and form (src::make-location (src::make-source-string "" :name (second form))
                                (third form))))

(defun encode-types (ts) (mapcar #'encode-type ts))
(defun decode-types (fs) (mapcar #'decode-type fs))
(defun encode-preds (ps) (mapcar #'encode-pred ps))
(defun decode-preds (fs) (mapcar #'decode-pred fs))

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

;;; function-env-entry (all plain data)

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
     :type (decode-type type) :tyvars (decode-types tyvars) :variances variances
     :constructors constructors :explicit-repr explicit-repr :enum-repr enum-repr
     :newtype newtype :docstring docstring :location (decode-location location)
     :exception-p exception-p :resumption-p resumption-p)))

;;; constructor-entry (all plain data)

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
     :name name :source-name source-name :tyvars (decode-types tyvars)
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
     :outer-tvars (decode-types outer-tvars)
     :explicit-tvars (decode-types explicit-tvars) :docstring docstring)))

(defun encode-superclass-dict (dict)
  ;; alist of (ty-predicate . codegen-data)
  (mapcar (lambda (pair) (cons (encode-pred (car pair)) (cdr pair))) dict))

(defun decode-superclass-dict (form)
  (mapcar (lambda (pair) (cons (decode-pred (car pair)) (cdr pair))) form))

(defun encode-ty-class (e)
  (list :class
        :name (tcenv::ty-class-name e)
        :source-name (tcenv::ty-class-source-name e)
        :predicate (encode-pred (tcenv::ty-class-predicate e))
        :superclasses (encode-preds (tcenv::ty-class-superclasses e))
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
     :name name :source-name source-name :predicate (decode-pred predicate)
     :superclasses (decode-preds superclasses) :class-variables class-variables
     :fundeps fundeps
     :unqualified-methods (mapcar #'decode-ty-class-method unqualified-methods)
     :codegen-sym codegen-sym
     :superclass-dict (decode-superclass-dict superclass-dict)
     :superclass-map superclass-map :docstring docstring
     :location (decode-location location))))

;;; ty-class-instance

(defun encode-ty-class-instance (e)
  (list :instance
        :constraints (encode-preds (tcenv::ty-class-instance-constraints e))
        :predicate (encode-pred (tcenv::ty-class-instance-predicate e))
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
     :constraints (decode-preds constraints) :predicate (decode-pred predicate)
     :codegen-sym codegen-sym :method-codegen-syms method-codegen-syms
     :method-codegen-inline-p method-codegen-inline-p :docstring docstring
     :location (decode-location location))))

;;; Round-trip the entry layer over the loaded environment, via
;;; encode = encode . decode . encode (decode faithfully inverts encode for
;;; everything encode captures; every slot is captured by construction).

(defun rt-consistent-p (enc dec obj)
  (let ((e1 (funcall enc obj)))
    (equal e1 (funcall enc (funcall dec e1)))))

(defun map-items (map) (algo::immutable-map-values map))

(defun listmap-items (lm)
  (let ((acc '()))
    (algo::immutable-listmap-foreach
     (lambda (key vals) (declare (ignore key)) (dolist (e vals) (push e acc)))
     lm)
    acc))

(defun roundtrip-entries-report ()
  (let* ((env entry::*global-environment*)
         (specs
           (list (list "name-entry" (map-items (tcenv::environment-name-environment env))
                       #'encode-name-entry #'decode-name-entry)
                 (list "function-env-entry" (map-items (tcenv::environment-function-environment env))
                       #'encode-function-env-entry #'decode-function-env-entry)
                 (list "type-entry" (map-items (tcenv::environment-type-environment env))
                       #'encode-type-entry #'decode-type-entry)
                 (list "constructor-entry" (map-items (tcenv::environment-constructor-environment env))
                       #'encode-constructor-entry #'decode-constructor-entry)
                 (list "type-alias-entry" (map-items (tcenv::environment-type-alias-environment env))
                       #'encode-type-alias-entry #'decode-type-alias-entry)
                 (list "struct-entry" (map-items (tcenv::environment-struct-environment env))
                       #'encode-struct-entry #'decode-struct-entry)
                 (list "ty-class" (map-items (tcenv::environment-class-environment env))
                       #'encode-ty-class #'decode-ty-class)
                 (list "specialization-entry" (listmap-items (tcenv::environment-specialization-environment env))
                       #'encode-specialization-entry #'decode-specialization-entry)))
         (grand-total 0) (grand-ok 0))
    (format t "~&edit-IR entry serde round-trip over the loaded environment~%")
    (format t "~64,,,'-<~>~%")
    (format t "~26A ~8@A ~8@A ~8@A~%" "entry" "count" "ok" "fail")
    (dolist (spec specs)
      (destructuring-bind (label items enc dec) spec
        (let ((n 0) (ok 0) (first-fail nil))
          (dolist (it items)
            (incf n)
            (handler-case
                (if (rt-consistent-p enc dec it) (incf ok)
                    (unless first-fail (setf first-fail "mismatch")))
              (error (e) (unless first-fail (setf first-fail (princ-to-string (type-of e)))))))
          (incf grand-total n) (incf grand-ok ok)
          (format t "~26A ~8D ~8D ~8D~@[  first-fail: ~A~]~%"
                  label n ok (- n ok) (and (plusp (- n ok)) first-fail)))))
    (format t "~64,,,'-<~>~%")
    (format t "TOTAL: ~D / ~D ok~%" grand-ok grand-total)
    (values grand-ok grand-total)))

;;;; ----------------------------------------------------------------------
;;;; Pattern encoder (the set-function-source-parameter-names payload, 3b).
;;;; A small tagged form per pattern variant, with a source location.

(defun encode-pattern (p)
  (typecase p
    (pat::pattern-var
     (list 'pvar (pat::pattern-var-name p) (pat::pattern-var-orig-name p)
           (encode-location (pat::pattern-location p))))
    (pat::pattern-literal
     (list 'plit (pat::pattern-literal-value p)
           (encode-location (pat::pattern-location p))))
    (pat::pattern-wildcard
     (list 'pwild (encode-location (pat::pattern-location p))))
    (pat::pattern-binding
     (list 'pbind (encode-pattern (pat::pattern-binding-var p))
           (encode-pattern (pat::pattern-binding-pattern p))
           (encode-location (pat::pattern-location p))))
    (pat::pattern-constructor
     (list 'pctor (pat::pattern-constructor-name p)
           (mapcar #'encode-pattern (pat::pattern-constructor-patterns p))
           (encode-location (pat::pattern-location p))))
    (t (error "unencodable pattern: ~S" p))))

(defun decode-pattern (f)
  (ecase (first f)
    (pvar (pat::make-pattern-var :name (second f) :orig-name (third f)
                                 :location (decode-location (fourth f))))
    (plit (pat::make-pattern-literal :value (second f)
                                     :location (decode-location (third f))))
    (pwild (pat::make-pattern-wildcard :location (decode-location (second f))))
    (pbind (pat::make-pattern-binding :var (decode-pattern (second f))
                                      :pattern (decode-pattern (third f))
                                      :location (decode-location (fourth f))))
    (pctor (pat::make-pattern-constructor :name (second f)
                                          :patterns (mapcar #'decode-pattern (third f))
                                          :location (decode-location (fourth f))))))

(defun roundtrip-patterns-report ()
  (let* ((env entry::*global-environment*)
         (lists (map-items (tcenv::environment-source-name-environment env)))
         (n 0) (ok 0) (first-fail nil))
    (dolist (plist lists)
      (dolist (p plist)
        (incf n)
        (handler-case
            (if (rt-consistent-p #'encode-pattern #'decode-pattern p) (incf ok)
                (unless first-fail (setf first-fail "mismatch")))
          (error (e) (unless first-fail (setf first-fail (princ-to-string (type-of e))))))))
    (format t "~&edit-IR pattern serde round-trip (source-parameter patterns)~%")
    (format t "~60,,,'-<~>~%")
    (format t "patterns: ~D  ok: ~D  fail: ~D~@[  first-fail: ~A~]~%"
            n ok (- n ok) (and (plusp (- n ok)) first-fail))
    (when (plusp n)
      (format t "sample: ~S~%" (encode-pattern (first (first lists)))))
    (values n ok)))

;;;; ----------------------------------------------------------------------
;;;; Code (set-code) payload: a reflective ENCODER over the codegen node
;;;; family. Encode reads slots generically (sb-mop:class-slots works on the
;;;; structure-classes), with nested types encoded compactly via encode-type.
;;;; This proves the optimized body is fully expressible as plain data and
;;;; lets us measure its size. Faithful generic DECODE wants a uniform
;;;; constructor over the node family -- which the stage-4 CLOS conversion
;;;; provides (make-instance + the MOP) -- so the code round-trip is
;;;; sequenced after that; here we validate the encode direction and size.

(defun encode-value (v)
  (cond
    ((typep v 'ty::ty) (list :ty (encode-type v)))
    ((typep v 'pat::pattern) (list :pat (encode-pattern v)))
    ((typep v 'structure-object) (encode-node v))
    ((consp v) (cons (encode-value (car v)) (encode-value (cdr v))))
    (t v)))

(defun encode-node (n)
  "Reflectively encode a codegen node (or any nested struct) to plain data."
  (list* :s (type-of n)
         (loop :for sd :in (sb-mop:class-slots (class-of n))
               :for name := (sb-mop:slot-definition-name sd)
               :collect (cons name (encode-value (slot-value n name))))))

(defun code-encode-report ()
  (let* ((env entry::*global-environment*)
         (bodies (map-items (tcenv::environment-code-environment env)))
         (n 0) (ok 0) (first-fail nil) (ir 0) (replay 0))
    (dolist (b bodies)
      (incf n)
      (handler-case
          (let ((enc (encode-node b)))
            (incf ok)
            (incf ir (ir-chars enc))
            (incf replay (replay-chars b)))
        (error (e) (unless first-fail (setf first-fail (princ-to-string e))))))
    (format t "~&edit-IR code (set-code) reflective encode over stored bodies~%")
    (format t "~60,,,'-<~>~%")
    (format t "bodies: ~D  encoded ok: ~D  fail: ~D~@[~%  first-fail: ~A~]~%"
            n ok (- n ok) (and (plusp (- n ok)) first-fail))
    (when (plusp ok)
      (format t "current replay (readable #S): ~:D chars~%" replay)
      (format t "edit-IR encoding:             ~:D chars~%" ir)
      (format t "ratio (replay / IR):          ~,2Fx~%" (/ (float replay) (max 1 ir))))
    (values n ok)))
