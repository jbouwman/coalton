(defpackage #:coalton-impl/parser/renamer
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern
   #:coalton-impl/parser/expression
   #:coalton-impl/parser/toplevel)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:import-from
   #:coalton-impl/parser/collect
   #:collect-type-variables)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm))
  (:export
   #:rename-variables                   ; FUNCTION
   #:rename-type-variables              ; FUNCTION
   ))

(in-package #:coalton-impl/parser/renamer)

(defun make-local-vars (vars &key (package *package*))
  (declare (type util:symbol-list vars))
  (loop :for var :in vars
        :collect (cons var (gentemp (concatenate 'string (symbol-name var) "-") package))))

(defun rename-variables (node)
  (rename-variables-ctx node (algo:make-immutable-map)))

(defgeneric rename-variables-ctx (node ctx))

(defmethod rename-variables-ctx (node ctx)
  (values node ctx))

(defmethod rename-variables-ctx ((node node-variable) ctx)
  (let ((new-name (algo:immutable-map-lookup ctx (node-variable-name node))))
    (values
     (if new-name
         (make-node-variable
          :name new-name
          :source (node-source node))
         node)
     ctx)))

(defmethod rename-variables-ctx ((node node-bind) ctx)
  (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name (pattern-variables (node-bind-pattern node)))))
         (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))
    (values
     (make-node-bind
      :pattern (rename-variables-ctx (node-bind-pattern node) new-ctx)

      ;; ctx is used instead of new-ctx because bind creates non-recursive bindings
      :expr (rename-variables-ctx (node-bind-expr node) ctx)
      :source (node-bind-source node))
     new-ctx)))

(defmethod rename-variables-ctx ((node node-body) ctx)
  (let ((new-ctx ctx))
    (values
     (make-node-body
      :nodes (loop :for elem :in (node-body-nodes node)
                   :collect (multiple-value-bind (elem new-ctx_)
                                (rename-variables-ctx elem new-ctx)
                              (setf new-ctx new-ctx_)
                              elem))
      :last-node (rename-variables-ctx (node-body-last-node node) new-ctx))
     ctx)))

(defmethod rename-variables-ctx ((node node-abstraction) ctx)
  (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name (pattern-variables (node-abstraction-params node)))))
         (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))
    (values
     (make-node-abstraction
      :params (rename-variables-ctx (node-abstraction-params node) new-ctx)
      :body (rename-variables-ctx (node-abstraction-body node) new-ctx)
      :source (node-source node))
     ctx)))

(defmethod rename-variables-ctx ((node node-let-binding) ctx)
  (values
   (make-node-let-binding
    :name (rename-variables-ctx (node-let-binding-name node) ctx)
    :value (rename-variables-ctx (node-let-binding-value node) ctx)
    :source (node-let-binding-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-let-declare) ctx)
  (values
   (make-node-let-declare
    :name (rename-variables-ctx (node-let-declare-name node) ctx)
    :type (node-let-declare-type node)
    :source (node-let-declare-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-let) ctx)
  (let* ((new-bindings (make-local-vars (mapcar (alexandria:compose
                                                 #'node-variable-name
                                                 #'node-let-binding-name)
                                                (node-let-bindings node))))
         (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))
    (values
     (make-node-let
      :bindings (rename-variables-ctx (node-let-bindings node) new-ctx)
      :declares (rename-variables-ctx (node-let-declares node) new-ctx)
      :body (rename-variables-ctx (node-let-body node) new-ctx)
      :source (node-source node))
     ctx)))

(defmethod rename-variables-ctx ((node node-lisp) ctx)
  (values
   (make-node-lisp
    :source (node-source node)
    :type (node-lisp-type node)
    :vars (rename-variables-ctx (node-lisp-vars node) ctx)
    :var-names (node-lisp-var-names node)
    :body (node-lisp-body node))
   ctx))

(defmethod rename-variables-ctx ((node node-match-branch) ctx)
  (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name
                                                (pattern-variables (node-match-branch-pattern node)))))
         (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))
    (values
     (make-node-match-branch
      :pattern (rename-variables-ctx (node-match-branch-pattern node) new-ctx)
      :body (rename-variables-ctx (node-match-branch-body node) new-ctx)
      :source (node-match-branch-source node))
     ctx)))

(defmethod rename-variables-ctx ((node node-match) ctx)
  (values
   (make-node-match
    :expr (rename-variables-ctx (node-match-expr node) ctx)
    :branches (rename-variables-ctx (node-match-branches node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-progn) ctx)
  (values
   (make-node-progn
    :body (rename-variables-ctx (node-progn-body node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-the) ctx)
  (values
   (make-node-the
    :type (node-the-type node)
    :expr (rename-variables-ctx (node-the-expr node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-return) ctx)
  (values
   (make-node-return
    :expr (if (node-return-expr node)
              (rename-variables-ctx (node-return-expr node) ctx)
              nil)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-application) ctx)
  (values
   (make-node-application
    :rator (rename-variables-ctx (node-application-rator node) ctx)
    :rands (rename-variables-ctx (node-application-rands node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-or) ctx)
  (values
   (make-node-or
    :nodes (rename-variables-ctx (node-or-nodes node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-and) ctx)
  (values
   (make-node-and
    :nodes (rename-variables-ctx (node-and-nodes node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-if) ctx)
  (values
   (make-node-if
    :expr (rename-variables-ctx (node-if-expr node) ctx)
    :then (rename-variables-ctx (node-if-then node) ctx)
    :else (rename-variables-ctx (node-if-else node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-when) ctx)
  (values
   (make-node-when
    :expr (rename-variables-ctx (node-when-expr node) ctx)
    :body (rename-variables-ctx (node-when-body node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-while) ctx)
  (values
   (make-node-while
    :expr (rename-variables-ctx (node-while-expr node) ctx)
    :label (node-while-label node)
    :body (rename-variables-ctx (node-while-body node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-while-let) ctx)
  (let* ((new-bindings
           (make-local-vars
            (mapcar #'pattern-var-name
                    (pattern-variables (node-while-let-pattern node)))))
         (new-ctx
           (algo:immutable-map-set-multiple ctx new-bindings)))
    (values
     (make-node-while-let
      :label (node-while-let-label node)
      :pattern (rename-variables-ctx (node-while-let-pattern node) new-ctx)
      :expr (rename-variables-ctx (node-while-let-expr node) ctx)
      :body (rename-variables-ctx (node-while-let-body node) new-ctx)
      :source (node-source node))
     new-ctx))) ;; XXX

(defmethod rename-variables-ctx ((node node-for) ctx)
  (let*
      ((new-bindings
         (make-local-vars
          (mapcar #'pattern-var-name
                  (pattern-variables (node-for-pattern node)))))

       (new-ctx
         (algo:immutable-map-set-multiple ctx new-bindings)))

    (values
     (make-node-for
      :label (node-for-label node)
      :pattern (rename-variables-ctx (node-for-pattern node) new-ctx)
      :expr (rename-variables-ctx (node-for-expr node) ctx)
      :body (rename-variables-ctx (node-for-body node) new-ctx)
      :source (node-source node))
     new-ctx)))                         ; XXX

(defmethod rename-variables-ctx ((node node-loop) ctx)
  (values
   (make-node-loop
    :source (node-source node)
    :label (node-loop-label node)
    :body (rename-variables-ctx (node-loop-body node) ctx))
   ctx))

(defmethod rename-variables-ctx ((node node-unless) ctx)
  (values
   (make-node-unless
    :expr (rename-variables-ctx (node-unless-expr node) ctx)
    :body (rename-variables-ctx (node-unless-body node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-cond-clause) ctx)
  (values
   (make-node-cond-clause
    :expr (rename-variables-ctx (node-cond-clause-expr node) ctx)
    :body (rename-variables-ctx (node-cond-clause-body node) ctx)
    :source (node-cond-clause-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-cond) ctx)
  (values
   (make-node-cond
    :clauses (rename-variables-ctx (node-cond-clauses node) ctx)
    :source (node-source node))
   ctx))

(defmethod rename-variables-ctx ((node node-do-bind) ctx)
  (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name (pattern-variables (node-do-bind-pattern node)))))

         (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

    (values
     (make-node-do-bind
      :pattern (rename-variables-ctx (node-do-bind-pattern node) new-ctx)
      :expr (rename-variables-ctx (node-do-bind-expr node) ctx)
      :source (node-do-bind-source node))
     new-ctx)))                         ; XXX

(defmethod rename-variables-ctx ((node node-do) ctx)
  (let ((new-ctx ctx))
    (values
     (make-node-do
      :nodes (loop :for elem :in (node-do-nodes node)
                   :collect (multiple-value-bind (elem new-ctx_)
                                (rename-variables-ctx elem new-ctx)
                              (setf new-ctx new-ctx_)
                              elem))
      :last-node (rename-variables-ctx (node-do-last-node node) new-ctx)
      :source (node-source node))
     ctx)))

(defmethod rename-variables-ctx ((pattern pattern-var) ctx)
  (let ((new-name (algo:immutable-map-lookup ctx (pattern-var-name pattern))))
    (values
     (if new-name
         (make-pattern-var
          :name new-name
          :orig-name (pattern-var-orig-name pattern)
          :source (pattern-source pattern))
         pattern)
     ctx)))

(defmethod rename-variables-ctx ((pattern pattern-literal) ctx)
  (values pattern ctx))

(defmethod rename-variables-ctx ((pattern pattern-wildcard) ctx)
  (values pattern ctx))

(defmethod rename-variables-ctx ((pattern pattern-constructor) ctx)
  (values
   (make-pattern-constructor
    :name (pattern-constructor-name pattern)
    :patterns (rename-variables-ctx (pattern-constructor-patterns pattern) ctx)
    :source (pattern-source pattern))
   ctx))

(defmethod rename-variables-ctx ((toplevel toplevel-define) ctx)
  (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name (pattern-variables (toplevel-define-params toplevel)))))

         (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

    (values
     (make-toplevel-define
      :name (toplevel-define-name toplevel)
      :params (rename-variables-ctx (toplevel-define-params toplevel) new-ctx)
      :orig-params (toplevel-define-orig-params toplevel)
      :docstring (toplevel-define-docstring toplevel)
      :body (rename-variables-ctx (toplevel-define-body toplevel) new-ctx)
      :source (toplevel-define-source toplevel)
      :monomorphize (toplevel-define-monomorphize toplevel))
     ctx)))

(defmethod rename-variables-ctx ((method instance-method-definition) ctx)
  (let* ((new-bindings (make-local-vars (mapcar #'pattern-var-name (pattern-variables (instance-method-definition-params method)))))

         (new-ctx (algo:immutable-map-set-multiple ctx new-bindings)))

    (values
     (make-instance-method-definition
      :name (instance-method-definition-name method)
      :params (rename-variables-ctx (instance-method-definition-params method) new-ctx)
      :body (rename-variables-ctx (instance-method-definition-body method) new-ctx)
      :source (instance-method-definition-source method))
     ctx)))

(defmethod rename-variables-ctx ((toplevel toplevel-define-instance) ctx)
  (values
   (make-toplevel-define-instance
    :context (toplevel-define-instance-context toplevel)
    :pred (toplevel-define-instance-pred toplevel)
    :methods (rename-variables-ctx (toplevel-define-instance-methods toplevel) ctx)
    :source (toplevel-define-instance-source toplevel)
    :head-src (toplevel-define-instance-head-src toplevel)
    :docstring (toplevel-define-instance-docstring toplevel)
    :compiler-generated (toplevel-define-instance-compiler-generated toplevel))
   ctx))

(defmethod rename-variables-ctx ((program program) ctx)
  (values
   (make-program
    :package (program-package program)
    :file (program-file program)
    :types (rename-type-variables (program-types program))
    :structs (rename-type-variables (program-structs program))
    :declares (program-declares program)
    :defines (rename-variables-ctx (program-defines program) ctx)
    :classes (program-classes program) ; Class type variables are renamed during kind inference
    :instances (rename-variables-ctx (program-instances program) ctx)
    :specializations (program-specializations program) ; Renaming type variables in specializations is not valid
    )
   ctx))

(defmethod rename-variables-ctx ((list list) ctx)
  (values
   (mapcar
    (lambda (node)
      (rename-variables-ctx node ctx))
    list)
   ctx))



(defvar *ctx* nil)

(defun ctx-get (name)
  (when *ctx*
    (algo:immutable-map-lookup *ctx* name)))

(defun ctx-get-all ()
  (algo:immutable-map-keys *ctx*)) ; FIXME accessor
         
(defmacro with-ctx (bindings &body body)
  `(let ((*ctx* (algo:immutable-map-set-multiple
                 (or *ctx* (algo:make-immutable-map))
                 ,bindings)))
     ,@body))

(defgeneric rename-type-variables (ty))

(defmethod rename-type-variables (obj)
  obj)

(defmethod rename-type-variables ((ty tyvar))
  (let ((new-name (ctx-get (tyvar-name ty))))
    (if new-name
        (make-tyvar :name new-name
                    :source (ty-source ty))
        ty)))

(defmethod rename-type-variables ((ty tapp))
  (make-tapp :from (rename-type-variables (tapp-from ty))
             :to (rename-type-variables (tapp-to ty))
             :source (ty-source ty)))

(defmethod rename-type-variables ((pred ty-predicate))
  (make-ty-predicate :class (ty-predicate-class pred)
                     :types (rename-type-variables (ty-predicate-types pred))
                     :source (ty-predicate-source pred)))

(defmethod rename-type-variables ((qual-ty qualified-ty))
  (make-qualified-ty :predicates (rename-type-variables (qualified-ty-predicates qual-ty)) 
                     :type (rename-type-variables (qualified-ty-type qual-ty))
                     :source (qualified-ty-source qual-ty)))

(defmethod rename-type-variables ((ctor constructor))
  (make-constructor :name (constructor-name ctor)
                    :fields (rename-type-variables (constructor-fields ctor))
                    :source (constructor-source ctor)))

(defmethod rename-type-variables ((keyword keyword-src))
  (let ((new-name (ctx-get (keyword-src-name keyword))))
    (if new-name
        (make-keyword-src :name new-name
                          :source (keyword-src-source keyword))
        keyword)))

(defmethod rename-type-variables ((toplevel toplevel-define-type))
  (let ((tvars (mapcar #'keyword-src-name (toplevel-define-type-vars toplevel))))
    (with-ctx (make-local-vars tvars :package util:+keyword-package+)
      (make-toplevel-define-type :name (toplevel-define-type-name toplevel)
                                 :vars (rename-type-variables (toplevel-define-type-vars toplevel))
                                 :docstring (toplevel-define-type-docstring toplevel)
                                 :ctors (rename-type-variables (toplevel-define-type-ctors toplevel))
                                 :source (toplevel-define-type-source toplevel)
                                 :repr (toplevel-define-type-repr toplevel)
                                 :head-src (toplevel-define-type-head-src toplevel)))))

(defmethod rename-type-variables ((field struct-field))
  (make-struct-field :name (struct-field-name field)
                     :type (rename-type-variables (struct-field-type field))
                     :source (struct-field-source field)))

(defmethod rename-type-variables ((toplevel toplevel-define-struct))
  (let ((tvars (mapcar #'keyword-src-name (toplevel-define-struct-vars toplevel))))
    (with-ctx (make-local-vars tvars :package util:+keyword-package+)
      (make-toplevel-define-struct :name (toplevel-define-struct-name toplevel)
                                   :vars (rename-type-variables (toplevel-define-struct-vars toplevel))
                                   :docstring (toplevel-define-struct-docstring toplevel)
                                   :fields (rename-type-variables (toplevel-define-struct-fields toplevel))
                                   :source (toplevel-define-struct-source toplevel)
                                   :repr (toplevel-define-struct-repr toplevel)
                                   :head-src (toplevel-define-struct-head-src toplevel)))))

(defmethod rename-type-variables ((fundep fundep))
  (make-fundep :left (rename-type-variables (fundep-left fundep))
               :right (rename-type-variables (fundep-right fundep))
               :source (fundep-source fundep)))

(defmethod rename-type-variables ((method method-definition))
  (let* ((tvars (mapcar #'tyvar-name (collect-type-variables method)))
         (new-tvars (set-difference tvars (ctx-get-all) :test #'eq)))
    (with-ctx (make-local-vars new-tvars :package util:+keyword-package+)
      (make-method-definition :name (method-definition-name method)
                              :type (rename-type-variables (method-definition-type method))
                              :source (method-definition-source method)))))

(defmethod rename-type-variables ((toplevel toplevel-define-class))
  (let* ((tvars (mapcar #'keyword-src-name (toplevel-define-class-vars toplevel))))
    (with-ctx (make-local-vars tvars :package util:+keyword-package+)
      (make-toplevel-define-class :name (toplevel-define-class-name toplevel)
                                  :vars (rename-type-variables (toplevel-define-class-vars toplevel))
                                  :preds (rename-type-variables (toplevel-define-class-preds toplevel))
                                  :fundeps (rename-type-variables (toplevel-define-class-fundeps toplevel))
                                  :docstring (toplevel-define-class-docstring toplevel)
                                  :methods (rename-type-variables (toplevel-define-class-methods toplevel))
                                  :source (toplevel-define-class-source toplevel)
                                  :head-src (toplevel-define-class-head-src toplevel)))))

(defmethod rename-type-variables ((list list))
  (mapcar #'rename-type-variables list))
