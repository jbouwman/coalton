(defpackage #:coalton-impl/ir
  (:use
   #:cl
   #:coalton-impl/generics
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/pattern
   #:coalton-impl/typechecker/fundeps
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/types)
  (:local-nicknames
   (#:pp #:coalton-impl/parser/pattern)
   (#:env #:coalton-impl/typechecker/environment))
  (:export
   #:node->code
   #:code->node))

(in-package #:coalton-impl/ir)

(defgeneric node->code (type))

;; the node types that appear as arguments to environment updates:

(defmethod make-source-form ((node node-abstraction))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node node-variable))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node node-let))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node node-literal))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node node-seq))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node node-application))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node node-direct-application))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node node-lisp))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node ty-scheme))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node pattern-wildcard))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node pp:pattern-constructor))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node pp:pattern-var))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node pp:pattern-wildcard))
  `(code->node ',(node->code node)))

(defmethod make-source-form ((node ty-predicate))
  `(code->node ',(node->code node)))

(defvar *cc*)

(setf *cc* (make-hash-table))

(defun code->node (code)
  (etypecase code
    (number
     code)
    (symbol
     (ecase code
       (* +kstar+)))
    (cons
     (destructuring-bind (op &rest args)
         code
       (apply (or (gethash op *cc*)
                  (error "missing operator ~A" op))
                  args)))))

;;; Fundep

(defmethod node->code ((node fundep))
  (list :fdep                           ; TODO macro (below)
        (fundep-from node)              
        (fundep-to node)))

(defun code->fundep (from to)
  (make-fundep :from from
               :to to))

(setf (gethash :fdep *cc*) #'code->fundep) ; TODO macro


;;; Kind

(defmethod node->code ((node kstar))
  '*)

(defmethod node->code ((node kfun))
  (list :->
        (node->code (kfun-from node))
        (node->code (kfun-to node))))

(defun code->kfun (from to)
  (make-kfun :from (code->node from)
             :to (code->node to)))

(setf (gethash :-> *cc*) #'code->kfun)  ; fixme

(defun code->kind (code)
  (etypecase code
    (symbol
     (ecase code
       (* +kstar+)))
    (cons
     (ecase (car code)
       (:-> (make-kfun :from (code->kind (cadr code))
                       :to (code->kind (caddr code))))))))

;;; Type

;; application

(defmethod node->code ((node tapp))
  (list :a
        (node->code (tapp-from node))
        (node->code (tapp-to node))))

(defun code->tapp (from to)
  (make-tapp :from (code->node from)
             :to (code->node to)))

(setf (gethash :a *cc*) #'code->tapp)

;; generic - see thih, section 8

(defmethod node->code ((node tgen))
  (list :tg (tgen-id node)))

(defun code->tgen (id)
  (make-tgen :id id))

(setf (gethash :tg *cc*) #'code->tgen)


;; constructor

(defmethod node->code ((node tycon))
  (list :c
        (tycon-name node)
        (node->code (tycon-kind node))))

(defun code->tycon (name kind)
  (make-tycon :name name
              :kind (code->node kind)))

(setf (gethash :c *cc*) #'code->tycon)

;; variable

(defmethod node->code ((node tyvar))
  (list :v
        (tyvar-id node)
        (node->code (tyvar-kind node))))

(defun code->tyvar (id kind)
  (make-tyvar :id id
              :kind (code->kind kind)))

(setf (gethash :v *cc*) #'code->tyvar)

;; predicate

(defmethod node->code ((node ty-predicate))
  (list :ty-p
        (ty-predicate-class node)
        (mapcar #'node->code (ty-predicate-types node))
        (ty-predicate-source node)))

(defun code->ty-predicate (class types source)
  (make-ty-predicate :class class
                     :types (mapcar #'code->node types)
                     :source source))

(setf (gethash :ty-p *cc*) #'code->ty-predicate)

;; qualified type

(defmethod node->code ((node qualified-ty))
  (list :ty-q
        (mapcar #'node->code (qualified-ty-predicates node))
        (node->code (qualified-ty-type node))))

(defun code->qualified-ty (predicates type)
  (make-qualified-ty :predicates (mapcar #'code->node predicates)
                     :type (code->node type)))

(setf (gethash :ty-q *cc*) #'code->qualified-ty)

;; scheme

(defmethod node->code ((node ty-scheme))
  (list :ty-s
        (mapcar #'node->code (ty-scheme-kinds node))
        (node->code (ty-scheme-type node))))

(defun code->ty-scheme (kinds type)
  (make-ty-scheme :kinds (mapcar #'code->node kinds)
                  :type (code->node type)))

(setf (gethash :ty-s *cc*) #'code->ty-scheme)

;;; Pattern (codegen)

(defmethod node->code ((node pattern-var))
  (list :pv
        (node->code (pattern-type node))
        (pattern-var-name node)))

(defun code->pattern-var (type name)
  (make-pattern-var :type (code->node type)
                    :name name))

(setf (gethash :pv *cc*) #'code->pattern-var)

(defmethod node->code ((node pattern-literal))
  (list :pl
        (node->code (pattern-type node))
        (pattern-literal-value node)))

(defun code->pattern-literal (type value)
  (make-pattern-literal :type (code->node type)
                        :value value))

(setf (gethash :pl *cc*) #'code->pattern-literal)

(defmethod node->code ((node pattern-wildcard))
  (list :pw
        (node->code (pattern-type node))))

(defun code->pattern-wildcard (type)
  (make-pattern-wildcard :type (code->node type)))

(setf (gethash :pw *cc*) #'code->pattern-wildcard)

(defmethod node->code ((node pattern-constructor))
  (list :pc
        (node->code (pattern-type node))
        (pattern-constructor-name node)
        (mapcar #'node->code (pattern-constructor-patterns node))))

(defun code->pattern-constructor (type name patterns)
  (make-pattern-constructor :type (code->node type)
                            :name name
                            :patterns (mapcar #'code->node patterns)))

(setf (gethash :pc *cc*) #'code->pattern-constructor)


;;; Pattern (parser)

(defmethod node->code ((node pp:pattern-var))
  (list :pp-pv
        (pp:pattern-source node)
        (pp:pattern-var-name node)
        (pp:pattern-var-orig-name node)))

(defun code->pp-pattern-var (source name orig-name)
  (pp:make-pattern-var :source source
                       :name name
                       :orig-name orig-name))

(setf (gethash :pp-pv *cc*) #'code->pp-pattern-var)

(defmethod node->code ((node pp:pattern-literal))
  (list :pp-pl
        (pp:pattern-source node)
        (pp:pattern-literal-value node)))

(defun code->pp-pattern-literal (source value)
  (pp:make-pattern-literal :source source
                           :value value))

(setf (gethash :pp-pl *cc*) #'code->pp-pattern-literal)

(defmethod node->code ((node pp:pattern-wildcard))
  (list :pp-pw (pp:pattern-source node)))

(defun code->pp-pattern-wildcard (source)
  (pp:make-pattern-wildcard :source source))

(setf (gethash :pp-pw *cc*) #'code->pp-pattern-wildcard)

(defmethod node->code ((node pp:pattern-constructor))
  (list :pp-pc
        (pp:pattern-source node)
        (pp:pattern-constructor-name node)
        (mapcar #'node->code (pp:pattern-constructor-patterns node))))

(defun code->pp-pattern-constructor (source name patterns)
  (pp:make-pattern-constructor :source source
                               :name name
                               :patterns (mapcar #'code->node patterns)))

(setf (gethash :pp-pc *cc*) #'code->pp-pattern-constructor)

;;; Node

;; abstraction

(defmethod node->code ((node node-literal))
  (list :nl
        (node->code (node-type node))
        (node-literal-value node)))

(defun code->node-literal (type value)
  (make-node-literal :type (code->node type)
                     :value value))

(setf (gethash :nl *cc*) #'code->node-literal)

;; variable

(defmethod node->code ((node node-variable))
  (list :nv
        (node->code (node-type node))
        (node-variable-value node)))

(defun code->node-variable (type value)
  (make-node-variable :type (code->node type)
                      :value value))

(setf (gethash :nv *cc*) #'code->node-variable)

;; application

(defmethod node->code ((node node-application))
  (list :nap
        (node->code (node-type node))
        (node->code (node-application-rator node))
        (mapcar #'node->code (node-application-rands node))))

(defun code->node-application (type rator rands)
  (make-node-application :type (code->node type)
                         :rator (code->node rator)
                         :rands (mapcar #'code->node rands)))

(setf (gethash :nap *cc*) #'code->node-application)

;; direct application

(defmethod node->code ((node node-direct-application))
  (list :nda
        (node->code (node-type node))
        (node->code (node-direct-application-rator-type node))
        (node-direct-application-rator node)
        (mapcar #'node->code (node-direct-application-rands node))))

(defun code->node-direct-application (type rator-type rator rands)
  (make-node-direct-application :type (code->node type)
                                :rator-type (code->node rator-type)
                                :rator rator
                                :rands (mapcar #'code->node rands)))

(setf (gethash :nda *cc*) #'code->node-direct-application)

;; abstraction

(defmethod node->code ((node node-abstraction))
  (list :nab
        (node->code (node-type node))
        (node-abstraction-vars node)
        (node->code (node-abstraction-subexpr node))))

(defun code->node-abstraction (type vars subexpr)
  (make-node-abstraction :type (code->node type)
                         :vars vars
                         :subexpr (code->node subexpr)))

(setf (gethash :nab *cc*) #'code->node-abstraction)

;; let

(defmethod node->code ((node node-let))
  (list :let
        (node->code (node-type node))
        (mapcar (lambda (binding)
                  (destructuring-bind (name . value) binding
                    (cons name (node->code value))))
                (node-let-bindings node))
        (node->code (node-let-subexpr node))))

(defun code->node-let (type bindings subexpr)
  (make-node-let :type (code->node type)
                 :bindings (mapcar (lambda (binding)
                                     (destructuring-bind (name . value) binding
                                       (cons name (code->node value))))
                                   bindings)
                 :subexpr (code->node subexpr)))

(setf (gethash :let *cc*) #'code->node-let)

;; lisp

(defmethod node->code ((node node-lisp))
  (list :lisp
        (node->code (node-type node))
        (node-lisp-vars node)
        (node-lisp-form node)))

(defun code->node-lisp (type vars form)
  (make-node-lisp :type (code->node type)
                  :vars vars
                  :form form))

(setf (gethash :lisp *cc*) #'code->node-lisp)

;; branch

(defmethod node->code ((node match-branch))
  (list :branch
        (node->code (match-branch-pattern node))
        (node->code (match-branch-body node))))

(defun code->match-branch (pattern body)
  (make-match-branch :pattern (code->node pattern)
                     :body (code->node body)))

(setf (gethash :branch *cc*) #'code->match-branch)

;; match

(defmethod node->code ((node node-match))
  (list :match
        (node->code (node-type node))
        (node->code (node-match-expr node))
        (mapcar #'node->code (node-match-branches node))))

(defun code->node-match (type expr branches)
  (make-node-match :type (code->node type)
                   :expr (code->node expr)
                   :branches (mapcar #'code->node branches)))

(setf (gethash :match *cc*) #'code->node-match)

;; while

(defmethod node->code ((node node-while))
  (list 'node-while
        (node->code (node-type node))
        (node-while-label node)
        (node->code (node-while-expr node))
        (node->code (node-while-body node))))

(defun code->node-while (type label expr body)
  (make-node-while :type (code->node type)
                   :label label
                   :expr (code->node expr)
                   :body (code->node body)))

(setf (gethash :while *cc*) #'code->node-while)

;; while-let

(defmethod node->code ((node node-while-let))
  (list :while-let
        (node->code (node-type node))
        (node-while-let-label node)
        (node->code (node-while-let-pattern node))
        (node->code (node-while-let-expr node))
        (node->code (node-while-let-body node))))

(defun code->node-while-let (type label pattern expr body)
  (make-node-while-let :type (code->node type)
                       :label label
                       :pattern (code->node pattern)
                       :expr (code->node expr)
                       :body (code->node body)))

(setf (gethash :while-let *cc*) #'code->node-while-let)

;; loop

(defmethod node->code ((node node-loop))
  (list :loop
        (node->code (node-type node))
        (node-loop-label node)
        (node->code (node-loop-body node))))

(defun code->node-loop (type label body)
  (make-node-loop :type (code->node type)
                  :label label
                  :body (code->node body)))

(setf (gethash :loop *cc*) #'code->node-loop)

;; break

(defmethod node->code ((node node-break))
  (list :break
        (node->code (node-type node))
        (node-break-label node)))

(defun code->node-break (type label)
  (make-node-break :type (code->node type)
                   :label label))

(setf (gethash :break *cc*) #'code->node-break)

;; continue

(defmethod node->code ((node node-continue))
  (list :continue
        (node->code (node-type node))
        (node-continue-label node)))

(defun code->node-continue (type label)
  (make-node-continue :type (code->node type)
                      :label label))

(setf (gethash :continue *cc*) #'code->node-continue)

;; sequence

(defmethod node->code ((node node-seq))
  (list :seq
        (node->code (node-type node))
        (mapcar #'node->code (node-seq-nodes node))))

(defun code->node-seq (type nodes)
  (make-node-seq :type (code->node type)
                 :nodes (mapcar #'code->node nodes)))

(setf (gethash :seq *cc*) #'code->node-seq)

;; return

(defmethod node->code ((node node-return))
  (list :return
        (node->code (node-type node))
        (node->code (node-return-expr node))))

(defun code->node-return (type expr)
  (make-node-return :type (code->node type)
                    :expr (code->node expr)))

(setf (gethash :return *cc*) #'code->node-return)

;; field

(defmethod node->code ((node node-field))
  (list :field
        (node->code (node-type node))
        (node-field-name node)
        (node->code (node-field-dict node))))

(defun code->node-field (type name dict)
  (make-node-field :type (code->node type)
                   :name name
                   :dict (code->node dict)))

(setf (gethash :field *cc*) #'code->node-field)

;; dynamic-extent

(defmethod node->code ((node node-dynamic-extent))
  (list :dynamic-extent
        (node->code (node-type node))
        (node-dynamic-extent-name node)
        (node->code (node-dynamic-extent-node node))
        (node->code (node-dynamic-extent-body node))))

(defun code->node-dynamic-extent (type name node body)
  (make-node-dynamic-extent :type (code->node type)
                            :name name
                            :node (code->node node)
                            :body (code->node body)))

(setf (gethash :dynamic-extent *cc*) #'code->node-dynamic-extent)

;; bind

(defmethod node->code ((node node-bind))
  (list :bind
        (node->code (node-type node))
        (node-bind-name node)
        (node->code (node-bind-expr node))
        (node->code (node-bind-body node))))

(defun code->node-bind (type name expr body)
  (make-node-bind :type (code->node type)
                  :name name
                  :expr (code->node expr)
                  :body (code->node body)))

(setf (gethash :bind *cc*) #'code->node-bind)

;;; Environment

(defmethod make-source-form ((node env:function-env-entry))
  `(code->node ',(node->code node)))

(defmethod node->code ((node env:function-env-entry))
  (list :e-fee
        (env:function-env-entry-name node)
        (env:function-env-entry-arity node)))

(defun code->function-env-entry (name arity)
  (env:make-function-env-entry :name name
                               :arity arity))

(setf (gethash :e-fee *cc*) #'code->function-env-entry)

;; constructor entry

;; fixme collect these

(defmethod make-source-form ((node env:constructor-entry))
  `(code->node ',(node->code node)))

(defmethod node->code ((node env:constructor-entry))
  (list :e-ce
        (env:constructor-entry-name node)
        (env:constructor-entry-arity node)
        (env:constructor-entry-constructs node)
        (env:constructor-entry-classname node)
        (env:constructor-entry-compressed-repr node)))

(defun code->constructor-entry (name arity constructs classname compressed-repr)
  (env:make-constructor-entry :name name
                              :arity arity
                              :constructs constructs
                              :classname classname
                              :compressed-repr compressed-repr))

(setf (gethash :e-ce *cc*) #'code->constructor-entry)



(defmethod make-source-form ((node env:type-entry))
  `(code->node ',(node->code node)))

(defmethod node->code ((node env:type-entry))
  (list :e-te
        (env:type-entry-name node)
        (env:type-entry-runtime-type node)
        (node->code (env:type-entry-type node))
        (mapcar #'node->code (env:type-entry-tyvars node))
        (env:type-entry-constructors node)
        (env:type-entry-explicit-repr node)
        (env:type-entry-enum-repr node)
        (env:type-entry-newtype node)
        (env:type-entry-docstring node)
        (env:type-entry-location node)))

(defun code->type-entry (name runtime-type type tyvars constructors
                         explicit-repr enum-repr newtype docstring location)
  (env:make-type-entry :name name
                       :runtime-type runtime-type
                       :type (code->node type)
                       :tyvars (mapcar #'code->node tyvars)
                       :constructors constructors
                       :explicit-repr explicit-repr
                       :enum-repr enum-repr
                       :newtype newtype
                       :docstring docstring
                       :location location))

(setf (gethash :e-te *cc*) #'code->type-entry)

;; specialization-entry

(defmethod make-source-form ((node env:specialization-entry))
  `(code->node ',(node->code node)))

(defmethod node->code ((node env:specialization-entry))
  (list :e-spe
        (env:specialization-entry-from node)
        (env:specialization-entry-to node)
        (node->code (env:specialization-entry-to-ty node))))

(defun code->specialization-entry (from to to-ty)
  (env:make-specialization-entry :from from
                                 :to to
                                 :to-ty (code->node to-ty)))

(setf (gethash :e-spe *cc*) #'code->specialization-entry)

;; struct-entry

(defmethod make-source-form ((node env:struct-entry))
  `(code->node ',(node->code node)))

(defmethod node->code ((node env:struct-entry))
  (list :e-ste
        (env:struct-entry-name node)
        (env:struct-entry-fields node)
        (hash-table->plist (env:struct-entry-field-tys node) #'node->code)
        (hash-table->plist (env:struct-entry-field-idx node))))

(defun code->struct-entry (name fields field-tys field-idx)
  (env:make-struct-entry :name name
                         :fields fields
                         :field-tys (plist->hash-table field-tys #'code->node)
                         :field-idx (plist->hash-table field-idx)))

(setf (gethash :e-ste *cc*) #'code->struct-entry)



;;; util

;; for hash tables

(defun hash-table->plist (m &optional (kf #'identity))
  (loop :for k :being :the :hash-keys :of m
        :for v :being :the :hash-values :of m
        :append (list k (funcall kf v))))

(defun plist->hash-table (plist &optional (kf #'identity))
  (let ((m (make-hash-table :test #'equal)))
    (loop :for (k v) :on plist :by #'cddr
          :do (setf (gethash k m) (funcall kf v)))
    m))

;; for class unqualified methods

(defun alist->plist (m &optional (kf #'identity) (vf #'identity))
  (loop :for (k . v) :in m
        :append (list (funcall kf k) (funcall vf v))))

(defun plist->alist (plist &optional (kf #'identity) (vf #'identity))
  (loop :for (k v) :on plist :by #'cddr
        :collect (cons (funcall kf k) (funcall vf v))))

;; ty-class

(defmethod make-source-form ((node env:ty-class))
  `(code->node ',(node->code node)))

(defmethod node->code ((node env:ty-class))
  (list :e-c
        (env:ty-class-name node)
        (node->code (env:ty-class-predicate node))
        (mapcar #'node->code (env:ty-class-superclasses node))
        (env:ty-class-class-variables node)
        (hash-table->plist (env:ty-class-class-variable-map node))
        (mapcar #'node->code (env:ty-class-fundeps node))
        (alist->plist (env:ty-class-unqualified-methods node) #'identity #'node->code)
        (env:ty-class-codegen-sym node)
        (alist->plist (env:ty-class-superclass-dict node) #'node->code #'identity)
        (hash-table->plist (env:ty-class-superclass-map node))
        (env:ty-class-docstring node)
        (env:ty-class-location node)))

(defun code->ty-class (name predicate superclasses class-variables class-variable-map fundeps unqualified-methods codegen-sym superclass-dict superclass-map docstring location)
  (env:make-ty-class :name name
                     :predicate (code->node predicate)
                     :superclasses (mapcar #'code->node superclasses)
                     :class-variables class-variables
                     :class-variable-map (plist->hash-table class-variable-map)
                     :fundeps (mapcar #'code->node fundeps)
                     :unqualified-methods (plist->alist unqualified-methods #'identity #'code->node)
                     :codegen-sym codegen-sym
                     :superclass-dict (plist->alist superclass-dict #'code->node #'identity)
                     :superclass-map (plist->hash-table superclass-map)
                     :docstring docstring
                     :location location))

(setf (gethash :e-c *cc*) #'code->ty-class)

;; ty-class-instance

(defmethod make-source-form ((node env:ty-class-instance))
  `(code->node ',(node->code node)))

(defmethod node->code ((node env:ty-class-instance))
  (list :e-c-i
        (mapcar #'node->code (env:ty-class-instance-constraints node))
        (node->code (env:ty-class-instance-predicate node))
        (env:ty-class-instance-codegen-sym node)
        (hash-table->plist (env:ty-class-instance-method-codegen-syms node))
        (env:ty-class-instance-docstring node)))

(defun code->ty-class-instance (constraints predicate codegen-sym method-codegen-syms docstring)
  (env:make-ty-class-instance :constraints (mapcar #'code->node constraints)
                              :predicate (code->node predicate)
                              :codegen-sym codegen-sym
                              :method-codegen-syms (plist->hash-table method-codegen-syms)
                              :docstring docstring))

(setf (gethash :e-c-i *cc*) #'code->ty-class-instance)
