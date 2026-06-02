(defpackage #:coalton-impl/codegen/ast
  (:use
   #:cl
   #:coalton-impl/codegen/pattern)
  (:import-from #:coalton-impl/parser/base #:define-node)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:node                               ; STRUCT
   #:node-type                          ; READER
   #:copy-node                          ; FUNCTION
   #:node-list                          ; TYPE
   #:binding-list                       ; TYPE
   #:node-literal                       ; STRUCT
   #:make-node-literal                  ; CONSTRUCTOR
   #:node-literal-p                     ; FUNCTION
   #:node-literal-value                 ; READER
   #:node-variable                      ; STRUCT
   #:make-node-variable                 ; CONSTRUCTOR
   #:node-variable-p                    ; FUNCTION
   #:node-variable-value                ; READER
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-p                 ; FUNCTION
   #:node-application-properties        ; READER
   #:node-application-rator             ; READER
   #:node-application-rands             ; READER
   #:node-application-keyword-rands     ; READER
   #:node-application-keyword-arg       ; STRUCT
   #:make-node-application-keyword-arg  ; CONSTRUCTOR
   #:node-application-keyword-arg-keyword ; READER
   #:node-application-keyword-arg-value ; READER
   #:node-application-keyword-arg-supplied-p ; READER
   #:keyword-arg-list                   ; TYPE
   #:node-direct-application            ; STRUCT
   #:make-node-direct-application       ; CONSTRUCTOR
   #:node-direct-application-properties ; READER
   #:node-direct-application-rator-type ; READER
   #:node-direct-application-rator      ; READER
   #:node-direct-application-rands      ; READER
   #:node-direct-application-keyword-rands ; READER
   #:node-direct-application-p          ; FUNCTION
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-vars              ; READER
   #:node-abstraction-keyword-params    ; READER
   #:node-abstraction-subexpr           ; READER
   #:node-abstraction-p                 ; FUNCTION
   #:keyword-param                      ; STRUCT
   #:make-keyword-param                 ; CONSTRUCTOR
   #:keyword-param-keyword              ; READER
   #:keyword-param-var                  ; READER
   #:keyword-param-supplied-p-var       ; READER
   #:keyword-param-list                 ; TYPE
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-p                         ; FUNCTION
   #:node-let-bindings                  ; READER
   #:node-let-subexpr                   ; READER
   #:node-dynamic-binding               ; STRUCT
   #:make-node-dynamic-binding          ; CONSTRUCTOR
   #:node-dynamic-binding-name          ; READER
   #:node-dynamic-binding-value         ; READER
   #:node-dynamic-binding-list          ; TYPE
   #:node-dynamic-let                   ; STRUCT
   #:make-node-dynamic-let              ; CONSTRUCTOR
   #:node-dynamic-let-p                 ; FUNCTION
   #:node-dynamic-let-bindings          ; READER
   #:node-dynamic-let-subexpr           ; READER
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-p                        ; FUNCTION
   #:node-lisp-vars                     ; READER
   #:node-lisp-form                     ; READER
   #:node-locally                       ; STRUCT
   #:make-node-locally                  ; CONSTRUCTOR
   #:node-locally-p                     ; FUNCTION
   #:node-locally-noinline-functions    ; READER
   #:node-locally-type-check            ; READER
   #:node-locally-subexpr               ; READER
   #:match-branch                       ; STRUCT
   #:make-match-branch                  ; CONSTRUCTOR
   #:match-branch-pattern               ; READER
   #:match-branch-body                  ; READER
   #:branch-list                        ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; READER
   #:node-match-branches                ; READER
   #:catch-branch                       ; STRUCT
   #:make-catch-branch                  ; CONSTRUCTOR
   #:catch-branch-pattern               ; READER
   #:catch-branch-body                  ; READER
   #:catch-branch-list                  ; TYPE
   #:node-catch                         ; STRUCT
   #:make-node-catch                    ; CONSTRUCTOR
   #:node-catch-expr                    ; READER
   #:node-catch-branches                ; READER
   #:resumable-branch                   ; STRUCT
   #:make-resumable-branch              ; CONSTRUCTOR
   #:resumable-branch-pattern           ; READER
   #:resumable-branch-body              ; READER
   #:resumable-branch-list              ; TYPE
   #:node-resumable                     ; STRUCT
   #:make-node-resumable                ; CONSTRUCTOR
   #:node-resumable-expr                ; READER
   #:node-resumable-branches            ; READER
   #:node-for-binding                  ; STRUCT
   #:make-node-for-binding             ; CONSTRUCTOR
   #:node-for-binding-name             ; READER
   #:node-for-binding-type             ; READER
   #:node-for-binding-init             ; READER
   #:node-for-binding-step             ; READER
   #:node-for-binding-list             ; TYPE
   #:node-for                          ; STRUCT
   #:make-node-for                     ; CONSTRUCTOR
   #:node-for-bindings                 ; READER
   #:node-for-sequential-p             ; READER
   #:node-for-returns                  ; READER
   #:node-for-termination-kind         ; READER
   #:node-for-termination-expr         ; READER
   #:node-for-body                     ; READER
   #:node-for-label                    ; READER
   #:node-break                         ; STRUCT
   #:make-node-break                    ; CONSTRUCTOR
   #:node-break-label                   ; READER
   #:node-continue                      ; STRUCT
   #:make-node-continue                 ; CONSTRUCTOR
   #:node-continue-label                ; READER
   #:node-seq                           ; STRUCT
   #:make-node-seq                      ; CONSTRUCTOR
   #:node-seq-nodes                     ; READER
   #:node-return-from                   ; STRUCT
   #:make-node-return-from              ; CONSTRUCTOR
   #:node-return-from-name              ; READER
   #:node-return-from-expr              ; READER
   #:node-throw                         ; STRUCT
   #:make-node-throw                    ; CONSTRUCTOR
   #:node-throw-expr                    ; READER
   #:node-resume-to                     ; STRUCT
   #:make-node-resume-to                ; CONSTRUCTOR
   #:node-resume-to-expr                ; READER
   #:node-block                         ; STRUCT
   #:make-node-block                    ; CONSTRUCTOR
   #:node-block-name                    ; READER
   #:node-block-body                    ; READER
   #:node-field                         ; STRUCT
   #:make-node-field                    ; CONSTRUCTOR
   #:node-field-name                    ; READER
   #:node-field-dict                    ; READER
   #:node-field-p                       ; FUNCTION
   #:node-dynamic-extent                ; STRUCT
   #:make-node-dynamic-extent           ; CONSTRUCTOR
   #:node-dynamic-extent-name           ; READER
   #:node-dynamic-extent-node           ; READER
   #:node-dynamic-extent-body           ; READER
   #:node-bind                          ; STRUCT
   #:make-node-bind                     ; CONSTRUCTOR
   #:node-bind-name                     ; READER
   #:node-bind-expr                     ; READER
   #:node-bind-body                     ; READER
   #:node-values                        ; STRUCT
   #:make-node-values                   ; CONSTRUCTOR
   #:node-values-p                      ; FUNCTION
   #:node-values-nodes                  ; READER
   #:node-values-bind                   ; STRUCT
   #:make-node-values-bind              ; CONSTRUCTOR
   #:node-values-bind-p                 ; FUNCTION
   #:node-values-bind-vars              ; READER
   #:node-values-bind-expr              ; READER
   #:node-values-bind-body              ; READER
   #:node-variables                     ; FUNCTION
   #:node-binding-sccs                  ; FUNCTION
   #:node-free-p                        ; FUNCTION
   #:node-application-symbol-rator      ; FUNCTION
   #:node-rands                         ; FUNCTION
   #:node-rator-name                    ; FUNCTION
   #:node-rator-type                    ; FUNCTION
   #:node-properties                    ; FUNCTION
   ))

;;;;
;;;; Codegen AST - Typed Expression Nodes  
;;;;
;;;; This module defines the Abstract Syntax Tree structures used during code
;;;; generation, after type checking. These nodes include complete type information
;;;; and are optimized for translation to Common Lisp code.
;;;;
;;;; This is the SECOND of two AST systems in the Coalton compiler:
;;;; 1. Parser AST (parser/expression.lisp): Untyped nodes from parsing
;;;; 2. Codegen AST (this module): Typed nodes for code generation
;;;;

(in-package #:coalton-impl/codegen/ast)

;;;
;;; Compiler Backend IR
;;;


(defclass node ()
  ;; The `type` slot can be accessed by the exported function `node-type`.
  ((type :initarg :type :accessor %node-type :type tc:ty))
  (:documentation "Abstract base of the codegen node representation."))

(defun node-p (x) (and (typep x 'node) t))

(defun %copy-node (node)
  "A shallow copy of NODE. The representation is CLOS, so there is no struct
copier; copy slots through the metaobject protocol."
  (let* ((class (class-of node))
         (copy (allocate-instance class)))
    (dolist (slot (sb-mop:class-slots class) copy)
      (let ((name (sb-mop:slot-definition-name slot)))
        (when (slot-boundp node name)
          (setf (slot-value copy name) (slot-value node name)))))))

(defun copy-node (node &optional (new-type nil supplied-p))
  "Make a copy of `node`, optionally with a `new-type`."
  (declare (type node node)
           (type (or null tc:ty) new-type)
           (values node &optional))
  (let ((result (%copy-node node)))
    (when supplied-p
      (setf (%node-type result) new-type))
    result))

(defun node-type (node)
  "Get the stored type of `node`."
  (declare (type node node)
           (values tc:ty &optional))
  (%node-type node))

(defmethod make-load-form ((self node) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons parser:identifier node))) x)))

(deftype binding-list ()
  '(satisfies binding-list-p))

(defun lisp-coalton-var-alist-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol parser:identifier))) x)))

(deftype lisp-coalton-var-alist ()
  "An association list of cons cells pairing lisp symbols (`symbol`) with
coalton symbols (`parser:identifier`)"
  '(satisfies lisp-coalton-var-alist-p))

(define-node node-literal (node)
  "Literal values like 1 or \"hello\""
  (value :type util:literal-value))

(define-node node-variable (node)
  "Variables like x or y"
  (value :type parser:identifier))

(define-node keyword-param ()
  "A keyword parameter in a compiled lambda list."
  (keyword :type keyword)
  (var :type parser:identifier)
  (supplied-p-var :type parser:identifier))

(defmethod make-load-form ((self keyword-param) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun keyword-param-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'keyword-param-p x)))

(deftype keyword-param-list ()
  '(satisfies keyword-param-list-p))

(define-node node-application-keyword-arg ()
  "A keyword argument in a compiled call."
  (keyword :type keyword)
  (value :type node)
  (supplied-p :type (or null node) :default nil))

(defmethod make-load-form ((self node-application-keyword-arg) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun keyword-arg-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-application-keyword-arg-p x)))

(deftype keyword-arg-list ()
  '(satisfies keyword-arg-list-p))

(define-node node-application (node)
  "Function application (f x)"
  ;; properties stores extra information for the optimizer; its only valid
  ;; keys are `:inline' and `:noinline'.
  (properties :type list)
  (rator :type node)
  (rands :type node-list)
  (keyword-rands :type keyword-arg-list :default nil))

(define-node node-direct-application (node)
  "Fully saturated function application of a known function"
  ;; properties stores extra information for the optimizer; its only valid
  ;; keys are `:inline' and `:noinline'.
  (properties :type list)
  (rator-type :type tc:ty)
  (rator :type parser:identifier)
  (rands :type node-list)
  (keyword-rands :type keyword-arg-list :default nil))

(define-node node-abstraction (node)
  "Lambda literals (fn (x) x)"
  (vars :type parser:identifier-list)
  (keyword-params :type keyword-param-list :default nil)
  (subexpr :type node))

(define-node node-let (node)
  "Introduction of local mutually-recursive bindings (let ((x 2)) (+ x x))"
  (bindings :type binding-list)
  (subexpr :type node))

(define-node node-dynamic-binding ()
  "A special-variable binding used by dynamic-bind."
  (name :type parser:identifier)
  (value :type node))

(defmethod make-load-form ((self node-dynamic-binding) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun node-dynamic-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-dynamic-binding-p x)))

(deftype node-dynamic-binding-list ()
  '(satisfies node-dynamic-binding-list-p))

(define-node node-dynamic-let (node)
  "A dynamic scope wrapper implemented with Common Lisp special bindings."
  (bindings :type node-dynamic-binding-list)
  (subexpr :type node))

(define-node node-lisp (node)
  "An embedded lisp form"
  (vars :type lisp-coalton-var-alist)
  (form :type t))

(define-node node-locally (node)
  "Node for the optimizer to use, similar to `cl:locally'."
  (noinline-functions :type parser:identifier-list)
  (type-check :type (or null (integer 0 3)) :default nil)
  (subexpr :type node))

(define-node match-branch ()
  "A branch of a match expression"
  (pattern :type pattern)
  (body :type node))

(defmethod make-load-form ((self match-branch) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'match-branch-p x)))

(deftype branch-list ()
  '(satisfies branch-list-p))

(define-node node-match (node)
  "A pattern matching construct. Uses MATCH-BRANCH to represent branches"
  (expr :type node)
  (branches :type branch-list))

(define-node catch-branch ()
  "A branch of a catch expression."
  (pattern :type pattern)
  (body :type node))

(defmethod make-load-form ((self catch-branch) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun catch-branch-list-p (xs)
  (and (alexandria:proper-list-p xs)
       (every #'catch-branch-p xs)))

(deftype catch-branch-list ()
  '(satisfies catch-branch-list-p))

(define-node node-catch (node)
  "An exception-catching construct. Uses CATCH-BRANCH to represent branches"
  (expr :type node)
  (branches :type catch-branch-list))

(define-node resumable-branch ()
  "A branch of a resumable expression."
  (pattern :type pattern)
  (body :type node))

(defmethod make-load-form ((self resumable-branch) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun resumable-branch-list-p (xs)
  (and (alexandria:proper-list-p xs)
       (every #'resumable-branch-p xs)))

(deftype resumable-branch-list ()
  '(satisfies resumable-branch-list-p))

(define-node node-resumable (node)
  "A construct for continuing from a non-stack-unwinding transfer of control.
   Uses RESUMABLE-BRANCH to represent branches"
  (expr :type node)
  (branches :type resumable-branch-list))


(define-node node-for-binding ()
  "A single `for` variable with an initializer and optional step expression."
  (name :type parser:identifier)
  (type :type tc:ty)
  (init :type node)
  (step :type (or null node) :default nil))

(defmethod make-load-form ((self node-for-binding) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun node-for-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-for-binding-p x)))

(deftype node-for-binding-list ()
  '(satisfies node-for-binding-list-p))

(define-node node-for (node)
  "A labelled imperative `for` with explicit bindings and step expressions."
  (label :type keyword)
  (bindings :type node-for-binding-list)
  (sequential-p :type boolean :default nil)
  (returns :type (or null node) :default nil)
  (termination-kind :type (member nil :while :until :repeat) :default nil)
  (termination-expr :type (or null node) :default nil)
  (body :type node))

(define-node node-break (node)
  "A break statement used to exit a `for`."
  (label :type keyword))

(define-node node-continue (node)
  "A continue statement used to skip to the next iteration of a `for`."
  (label :type keyword))

(define-node node-seq (node)
  "A series of statements to be executed sequentially"
  (nodes :type node-list))

(define-node node-return-from (node)
  "A return statement, used for explicit returns in functions"
  (name :type symbol)
  (expr :type node))

(define-node node-throw (node)
  "A node that throws an exception, its argument."
  (expr :type node))

(define-node node-resume-to (node)
  "A node that invokes a resumption, if any exists."
  (expr :type node))

(define-node node-block (node)
  "A return target, used for explicit returns in functions"
  (name :type symbol)
  (body :type node))

(define-node node-field (node)
  "Accessing a superclass on a typeclass dictionary"
  (name :type parser:identifier)
  (dict :type node))

(define-node node-dynamic-extent (node)
  "A single stack allocated binding"
  (name :type parser:identifier)
  (node :type node)
  (body :type node))

(define-node node-bind (node)
  "A single non-recursive binding"
  (name :type parser:identifier)
  (expr :type node)
  (body :type node))

(define-node node-values (node)
  "Produce multiple values."
  (nodes :type node-list))

(define-node node-values-bind (node)
  "Bind multiple values and evaluate body."
  (vars :type parser:identifier-list)
  (expr :type node)
  (body :type node))

;;;
;;; Functions
;;;

(defun node-binding-sccs (bindings)
  "Returns a list of SCCs ordered from least to most depended on."
  (declare (type binding-list bindings))

  (let ((binding-names (mapcar #'car bindings)))
    (algo:tarjan-scc
     (loop :for (name . node) :in bindings
           :collect (cons name (intersection binding-names (node-variables node)))))))

(defun node-rands (node)
  (declare (type (or node-application node-direct-application))
           (values node-list &optional))

  (etypecase node
    (node-direct-application
     (node-direct-application-rands node))

    (node-application
     (node-application-rands node))))

(defun node-rator-name (node)
  "Returns the name of the function being called if it is known"
  (declare (type (or node-application node-direct-application))
           (values (or null parser:identifier) &optional))

  (etypecase node
    (node-direct-application
     (node-direct-application-rator node))

    (node-application
     (when (node-variable-p (node-application-rator node))
       (node-variable-value (node-application-rator node))))))

(defun node-rator-type (node)
  (declare (type (or node-application node-direct-application))
           (values tc:ty &optional))

  (etypecase node
    (node-direct-application
     (node-direct-application-rator-type node))

    (node-application
     (node-type (node-application-rator node)))))

(defun node-properties (node)
  (declare (type (or node-application node-direct-application))
           (values list &optional))

  (etypecase node
    (node-direct-application
     (node-direct-application-properties node))

    (node-application
     (node-application-properties node))))
