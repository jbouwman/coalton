;;;;
;;;; Mirror of expression nodes in src/parser/expression.lisp with
;;;; types attached.
;;;;

(defpackage #:coalton-impl/typechecker/expression
  (:use
   #:cl
   #:coalton-impl/typechecker/pattern)
  (:import-from
   #:coalton-impl/parser/base
   #:define-node)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:util #:coalton-impl/util))
  (:export
   #:node                               ; STRUCT
   #:node-type                          ; ACCESSOR
   #:node-list                          ; TYPE
   #:node-variable                      ; STRUCT
   #:make-node-variable                 ; CONSTRUCTOR
   #:node-variable-name                 ; ACCESSOR
   #:node-variable-list                 ; TYPE
   #:node-accessor                      ; STRUCT
   #:make-node-accessor                 ; CONSTRUCTOR
   #:node-accessor-name                 ; ACCESSOR
   #:node-literal                       ; STRUCT
   #:make-node-literal                  ; CONSTRUCTOR
   #:node-literal-value                 ; ACCESSOR
   #:node-integer-literal               ; STRUCT
   #:make-node-integer-literal          ; CONSTRUCTOR
   #:node-integer-literal-value         ; ACCESSOR
   #:node-bind                          ; STRUCT
   #:make-node-bind                     ; CONSTRUCTOR
   #:node-bind-pattern                  ; ACCESSOR
   #:node-bind-expr                     ; ACCESSOR
   #:node-values-bind                   ; STRUCT
   #:make-node-values-bind              ; CONSTRUCTOR
   #:node-values-bind-patterns          ; ACCESSOR
   #:node-values-bind-expr              ; ACCESSOR
   #:node-body-element                  ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-body                          ; STRUCT
   #:make-node-body                     ; CONSTRUCTOR
   #:node-body-nodes                    ; ACCESSOR
   #:node-body-last-node                ; ACCESSOR
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-params            ; ACCESSOR
   #:node-abstraction-keyword-params    ; ACCESSOR
   #:node-abstraction-body              ; ACCESSOR
   #:node-abstraction-p                 ; FUNCTION
   #:keyword-param                      ; STRUCT
   #:make-keyword-param                 ; CONSTRUCTOR
   #:keyword-param-keyword              ; ACCESSOR
   #:keyword-param-value-var            ; ACCESSOR
   #:keyword-param-supplied-p-var       ; ACCESSOR
   #:keyword-param-list                 ; TYPE
   #:node-let-binding                   ; STRUCT
   #:make-node-let-binding              ; CONSTRUCTOR
   #:node-let-binding-name              ; ACCESSOR
   #:node-let-binding-value             ; ACCESSOR
   #:node-let-binding-list              ; TYPE
   #:node-dynamic-binding               ; STRUCT
   #:make-node-dynamic-binding          ; CONSTRUCTOR
   #:node-dynamic-binding-name          ; ACCESSOR
   #:node-dynamic-binding-value         ; ACCESSOR
   #:node-dynamic-binding-list          ; TYPE
   #:node-for-binding                  ; STRUCT
   #:make-node-for-binding             ; CONSTRUCTOR
   #:node-for-binding-name             ; ACCESSOR
   #:node-for-binding-init             ; ACCESSOR
   #:node-for-binding-step             ; ACCESSOR
   #:node-for-binding-list             ; TYPE
   #:node-let-declare                   ; STRUCT
   #:make-node-let-declare              ; CONSTRUCTOR
   #:node-let-declare-name              ; ACCESSOR
   #:node-let-declare-type              ; ACCESSOR
   #:node-let-declare-list              ; TYPE
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-declares                  ; ACCESSOR
   #:node-let-body                      ; ACCESSOR
   #:node-dynamic-let                   ; STRUCT
   #:make-node-dynamic-let              ; CONSTRUCTOR
   #:node-dynamic-let-bindings          ; ACCESSOR
   #:node-dynamic-let-subexpr           ; ACCESSOR
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-vars                     ; ACCESSOR
   #:node-lisp-var-names                ; ACCESSOR
   #:node-lisp-body                     ; ACCESSOR
   #:node-match-branch                  ; STRUCT
   #:make-node-match-branch             ; CONSTRUCTOR
   #:node-match-branch-pattern          ; ACCESSOR
   #:node-match-branch-body             ; ACCESSOR
   #:node-match-branch-list             ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-catch-branch                  ; STRUCT
   #:make-node-catch-branch             ; CONSTRUCTOR
   #:node-catch-branch-pattern          ; ACCESSOR
   #:node-catch-branch-body             ; ACCESSOR
   #:node-catch-branch-list             ; TYPE
   #:node-catch                         ; STRUCT
   #:make-node-catch                    ; CONSTRUCTOR
   #:node-catch-expr                    ; ACCESSOR
   #:node-catch-branches                ; ACCESSOR
   #:node-resumable-branch              ; STRUCT
   #:make-node-resumable-branch         ; CONSTRUCTOR
   #:node-resumable-branch-pattern      ; ACCESSOR
   #:node-resumable-branch-body         ; ACCESSOR
   #:node-resumable-branch-list         ; TYPE
   #:node-resumable                     ; STRUCT
   #:make-node-resumable                ; CONSTRUCTOR
   #:node-resumable-expr                ; ACCESSOR
   #:node-resumable-branches            ; ACCESSOR
   #:node-progn                         ; STRUCT
   #:make-node-progn                    ; CONSTRUCTOR
   #:node-progn-body                    ; ACCESSOR
   #:node-unsafe                        ; STRUCT
   #:make-node-unsafe                   ; CONSTRUCTOR
   #:node-unsafe-body                   ; ACCESSOR
   #:node-block                         ; STRUCT
   #:make-node-block                    ; CONSTRUCTOR
   #:node-block-name                    ; ACCESSOR
   #:node-block-body                    ; ACCESSOR
   #:node-the                           ; STRUCT
   #:make-node-the                      ; CONSTRUCTOR
   #:node-the-type                      ; ACCESSOR
   #:node-the-expr                      ; ACCESSOR
   #:node-return-from                   ; STRUCT
   #:make-node-return-from              ; CONSTRUCTOR
   #:node-return-from-name              ; ACCESSOR
   #:node-return-from-expr              ; ACCESSOR
   #:node-values                        ; STRUCT
   #:make-node-values                   ; CONSTRUCTOR
   #:node-values-nodes                  ; ACCESSOR
   #:node-throw                         ; STRUCT
   #:make-node-throw                    ; CONSTRUCTOR
   #:node-throw-expr                    ; ACCESSOR
   #:node-resume-to                     ; STRUCT
   #:make-node-resume-to                ; CONSTRUCTOR
   #:node-resume-to-expr                ; ACCESSOR
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
   #:node-application-keyword-rands     ; ACCESSOR
   #:node-application-keyword-arg       ; STRUCT
   #:make-node-application-keyword-arg  ; CONSTRUCTOR
   #:node-application-keyword-arg-keyword ; ACCESSOR
   #:node-application-keyword-arg-value ; ACCESSOR
   #:node-application-keyword-arg-list  ; TYPE
   #:node-or                            ; STRUCT
   #:make-node-or                       ; CONSTRUCTOR
   #:node-or-nodes                      ; ACCESSOR
   #:node-and                           ; STRUCT
   #:make-node-and                      ; CONSTRUCTOR
   #:node-and-nodes                     ; ACCESSOR
   #:node-if                            ; STRUCT
   #:make-node-if                       ; CONSTRUCTOR
   #:node-if-expr                       ; ACCESSOR
   #:node-if-then                       ; ACCESSOR
   #:node-if-else                       ; ACCESSOR
   #:node-when                          ; STRUCT
   #:make-node-when                     ; CONSTRUCTOR
   #:node-when-expr                     ; ACCESSOR
   #:node-when-body                     ; ACCESSOR
   #:node-unless                        ; STRUCT
   #:make-node-unless                   ; CONSTRUCTOR
   #:node-unless-expr                   ; ACCESSOR
   #:node-unless-body                   ; ACCESSOR
   #:node-for                          ; STRUCT
   #:make-node-for                     ; CONSTRUCTOR
   #:node-for-label                    ; ACCESSOR
   #:node-for-bindings                 ; ACCESSOR
   #:node-for-sequential-p             ; ACCESSOR
   #:node-for-returns                  ; ACCESSOR
   #:node-for-termination-kind         ; ACCESSOR
   #:node-for-termination-expr         ; ACCESSOR
   #:node-for-body                     ; ACCESSOR
   #:node-break                         ; STRUCT
   #:make-node-break                    ; CONSTRUCTOR
   #:node-break-label                   ; ACCESSOR
   #:node-continue                      ; STRUCT
   #:make-node-continue                 ; CONSTRUCTOR
   #:node-continue-label                ; ACCESSOR
   #:node-cond-clause                   ; STRUCT
   #:make-node-cond-clause              ; CONSTRUCTOR
   #:node-cond-clause-expr              ; ACCESSOR
   #:node-cond-clause-body              ; ACCESSOR
   #:node-cond-clause-list              ; TYPE
   #:node-cond                          ; STRUCT
   #:make-node-cond                     ; CONSTRUCTOR
   #:node-cond-clauses                  ; ACCESSOR
   #:node-do-bind                       ; STRUCT
   #:make-node-do-bind                  ; CONSTRUCTOR
   #:node-do-bind-pattern               ; ACCESSOR
   #:node-do-bind-expr                  ; ACCESSOR
   #:node-do-body-element               ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-do                            ; STRUCT
   #:make-node-do                       ; CONSTRUCTOR
   #:node-do-nodes                      ; ACCESSOR
   #:node-do-last-node                  ; ACCESSOR
   ))

(in-package #:coalton-impl/typechecker/expression)

;;;
;;; Expression Nodes
;;;

(define-node node () :abstract
  (type :type tc:qualified-ty)
  (location :type source:location))

(defmethod source:location ((self node))
  (node-location self))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(define-node node-variable (node)
  (name :type parser:identifier))

(defun node-variable-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-variable-p x)))

(deftype node-variable-list ()
  '(satisfies node-variable-list-p))

(define-node node-accessor (node)
  (name :type string))

(define-node node-literal (node)
  (value :type (and util:literal-value (not integer))))

(define-node node-integer-literal (node)
  (value :type integer))

(define-node node-bind ()
  (pattern :type pattern)
  (expr :type node)
  (location :type source:location))

(defmethod source:location ((self node-bind))
  (node-bind-location self))

(define-node node-values-bind ()
  (patterns :type pattern-list)
  (expr :type node)
  (location :type source:location))

(defmethod source:location ((self node-values-bind))
  (node-values-bind-location self))

(deftype node-body-element ()
  '(or node node-bind node-values-bind))

(defun node-body-element-p (x)
  (typep x 'node-body-element))

(defun node-body-element-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-body-element-p x)))

(deftype node-body-element-list ()
  '(satisfies node-body-element-list-p))

(define-node node-body ()
  (nodes :type node-body-element-list)
  (last-node :type node))

(define-node keyword-param ()
  (keyword :type keyword)
  (value-var :type parser:identifier)
  (supplied-p-var :type parser:identifier))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun keyword-param-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'keyword-param-p x))))

(deftype keyword-param-list ()
  '(satisfies keyword-param-list-p))

(define-node node-abstraction (node)
  (params :type pattern-list)
  (keyword-params :type keyword-param-list :default nil)
  (body :type node-body))

(define-node node-let-binding ()
  (name :type node-variable)
  (value :type node)
  (location :type source:location))

(defmethod source:location ((self node-let-binding))
  (node-let-binding-location self))

(defun node-let-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-let-binding-p x)))

(deftype node-let-binding-list ()
  '(satisfies node-let-binding-list-p))

(define-node node-dynamic-binding ()
  (name :type node-variable)
  (value :type node)
  (location :type source:location))

(defmethod source:location ((self node-dynamic-binding))
  (node-dynamic-binding-location self))

(defun node-dynamic-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-dynamic-binding-p x)))

(deftype node-dynamic-binding-list ()
  '(satisfies node-dynamic-binding-list-p))

(define-node node-for-binding ()
  (name :type node-variable)
  (init :type node)
  (step :type (or null node) :default nil)
  (location :type source:location))

(defmethod source:location ((self node-for-binding))
  (node-for-binding-location self))

(defun node-for-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-for-binding-p x)))

(deftype node-for-binding-list ()
  '(satisfies node-for-binding-list-p))

(define-node node-let (node)
  (bindings :type node-let-binding-list)
  (body :type node-body))

(define-node node-dynamic-let (node)
  (bindings :type node-dynamic-binding-list)
  (subexpr :type node))

(define-node node-lisp (node)
  (vars :type node-variable-list)
  (var-names :type util:symbol-list)
  (body :type t))

(define-node node-match-branch ()
  (pattern :type pattern)
  (body :type node-body)
  (location :type source:location))

(defmethod source:location ((self node-match-branch))
  (node-match-branch-location self))

(defun node-match-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-match-branch-p x)))

(deftype node-match-branch-list ()
  '(satisfies node-match-branch-list-p))

(define-node node-match (node)
  (expr :type node)
  (branches :type node-match-branch-list))

(define-node node-progn (node)
  (body :type node-body))

(define-node node-unsafe (node)
  (body :type node-body))

;; node-the does not exist in this AST!

(define-node node-block (node)
  (name :type symbol)
  (body :type node-body))

(define-node node-return-from (node)
  (name :type symbol)
  ;; Bare (return) is rewritten to a zero-value NODE-VALUES during
  ;; control-flow resolution, so the returned expression is always explicit.
  (expr :type node))

(define-node node-values (node)
  ;; Multiple values expression, lowered directly by codegen.
  (nodes :type node-list))

(define-node node-throw (node)
  ;; The thrown expression
  (expr :type (or null node)))

(define-node node-resume-to (node)
  ;; The resumption instance
  (expr :type (or null node)))

(define-node node-resumable-branch ()
  (pattern :type pattern)
  (body :type node-body)
  (location :type source:location))

(defmethod source:location ((self node-resumable-branch))
  (node-resumable-branch-location self))

(defun node-resumable-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-resumable-branch-p x)))

(deftype node-resumable-branch-list ()
  '(satisfies node-resumable-branch-list-p))

(define-node node-resumable (node)
  (expr :type node)
  (branches :type node-resumable-branch-list))

(define-node node-catch-branch ()
  (pattern :type pattern)
  (body :type node-body)
  (location :type source:location))

(defmethod source:location ((self node-catch-branch))
  (node-catch-branch-location self))

(defun node-catch-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-catch-branch-p x)))

(deftype node-catch-branch-list ()
  '(satisfies node-catch-branch-list-p))

(define-node node-catch (node)
  (expr :type node)
  (branches :type node-catch-branch-list))

(define-node node-application (node)
  (rator :type node)
  (rands :type node-list)
  (keyword-rands :type node-application-keyword-arg-list :default nil))

(define-node node-application-keyword-arg ()
  (keyword :type keyword)
  (value :type node))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun node-application-keyword-arg-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'node-application-keyword-arg-p x))))

(deftype node-application-keyword-arg-list ()
  '(satisfies node-application-keyword-arg-list-p))

(define-node node-or (node)
  (nodes :type node-list))

(define-node node-and (node)
  (nodes :type node-list))

(define-node node-if (node)
  (expr :type node)
  (then :type node)
  (else :type node))

(define-node node-when (node)
  (expr :type node)
  (body :type node-body))

(define-node node-unless (node)
  (expr :type node)
  (body :type node-body))

(define-node node-for (node)
  (label :type keyword)
  (bindings :type node-for-binding-list)
  (sequential-p :type boolean :default nil)
  (returns :type (or null node) :default nil)
  (termination-kind :type (member nil :while :until :repeat) :default nil)
  (termination-expr :type (or null node) :default nil)
  (body :type node-body))

(define-node node-break (node)
  (label :type keyword))

(define-node node-continue (node)
  (label :type keyword))

(define-node node-cond-clause ()
  (expr :type node)
  (body :type node-body)
  (location :type source:location))

(defmethod source:location ((self node-cond-clause))
  (node-cond-clause-location self))

(defun node-cond-clause-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-cond-clause-p x)))

(deftype node-cond-clause-list ()
  '(satisfies node-cond-clause-list-p))

(define-node node-cond (node)
  (clauses :type node-cond-clause-list))

(define-node node-do-bind ()
  (pattern :type pattern)
  (expr :type node)
  (location :type source:location))

(defmethod source:location ((self node-do-bind))
  (node-do-bind-location self))

(deftype node-do-body-element ()
  '(or node node-bind node-values-bind node-do-bind))

(defun node-do-body-element-p (x)
  (typep x 'node-do-body-element))

(defun node-do-body-element-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-do-body-element-p x)))

(deftype node-do-body-element-list ()
  '(satisfies node-do-body-element-list-p))

(define-node node-do (node)
  (nodes :type node-do-body-element-list)
  (last-node :type node))

;;;
;;; Methods
;;;

(defmethod tc:apply-substitution (subs (node node-variable))
  (declare (type tc:substitution-list subs)
           (values node-variable))
  (make-node-variable
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :name (node-variable-name node)))

(defmethod tc:apply-substitution (subs (node node-accessor))
  (declare (type tc:substitution-list subs)
           (values node-accessor))
  (make-node-accessor
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :name (node-accessor-name node)))

(defmethod tc:apply-substitution (subs (node node-literal))
  (declare (type tc:substitution-list subs)
           (values node-literal))
  (make-node-literal
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :value (node-literal-value node)))

(defmethod tc:apply-substitution (subs (node node-integer-literal))
  (declare (type tc:substitution-list subs)
           (values node-integer-literal))
  (make-node-integer-literal
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :value (node-integer-literal-value node)))

(defmethod tc:apply-substitution (subs (node node-bind))
  (declare (type tc:substitution-list subs)
           (values node-bind))
  (make-node-bind
   :pattern (tc:apply-substitution subs (node-bind-pattern node))
   :expr (tc:apply-substitution subs (node-bind-expr node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-values-bind))
  (declare (type tc:substitution-list subs)
           (values node-values-bind))
  (make-node-values-bind
   :patterns (tc:apply-substitution subs (node-values-bind-patterns node))
   :expr (tc:apply-substitution subs (node-values-bind-expr node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-body))
  (declare (type tc:substitution-list subs)
           (values node-body))
  (make-node-body
   :nodes (tc:apply-substitution subs (node-body-nodes node))
   :last-node (tc:apply-substitution subs (node-body-last-node node))))

(defmethod tc:apply-substitution (subs (node node-abstraction))
  (declare (type tc:substitution-list subs)
           (values node-abstraction))
  (make-node-abstraction
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :params (tc:apply-substitution subs (node-abstraction-params node))
   :keyword-params (node-abstraction-keyword-params node)
   :body (tc:apply-substitution subs (node-abstraction-body node))))

(defmethod tc:apply-substitution (subs (node node-let-binding))
  (declare (type tc:substitution-list subs)
           (values node-let-binding))
  (make-node-let-binding
   :name (tc:apply-substitution subs (node-let-binding-name node))
   :value (tc:apply-substitution subs (node-let-binding-value node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-dynamic-binding))
  (declare (type tc:substitution-list subs)
           (values node-dynamic-binding))
  (make-node-dynamic-binding
   :name (tc:apply-substitution subs (node-dynamic-binding-name node))
   :value (tc:apply-substitution subs (node-dynamic-binding-value node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-for-binding))
  (declare (type tc:substitution-list subs)
           (values node-for-binding))
  (make-node-for-binding
   :name (tc:apply-substitution subs (node-for-binding-name node))
   :init (tc:apply-substitution subs (node-for-binding-init node))
   :step (tc:apply-substitution subs (node-for-binding-step node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-let))
  (declare (type tc:substitution-list subs)
           (values node-let))
  (make-node-let
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :bindings (tc:apply-substitution subs (node-let-bindings node))
   :body (tc:apply-substitution subs (node-let-body node))))

(defmethod tc:apply-substitution (subs (node node-dynamic-let))
  (declare (type tc:substitution-list subs)
           (values node-dynamic-let))
  (make-node-dynamic-let
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :bindings (tc:apply-substitution subs (node-dynamic-let-bindings node))
   :subexpr (tc:apply-substitution subs (node-dynamic-let-subexpr node))))

(defmethod tc:apply-substitution (subs (node node-lisp))
  (declare (type tc:substitution-list subs)
           (values node-lisp))
  (make-node-lisp
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :vars (tc:apply-substitution subs (node-lisp-vars node))
   :var-names (node-lisp-var-names node)
   :body (node-lisp-body node)))

(defmethod tc:apply-substitution (subs (node node-match-branch))
  (declare (type tc:substitution-list subs)
           (values node-match-branch))
  (make-node-match-branch
   :pattern (tc:apply-substitution subs (node-match-branch-pattern node))
   :body (tc:apply-substitution subs (node-match-branch-body node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-match))
  (declare (type tc:substitution-list subs)
           (values node-match))
  (make-node-match
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :expr (tc:apply-substitution subs (node-match-expr node))
   :branches (tc:apply-substitution subs (node-match-branches node))))

(defmethod tc:apply-substitution (subs (node node-catch-branch))
  (declare (type tc:substitution-list subs)
           (values node-catch-branch))
  (make-node-catch-branch
   :pattern (tc:apply-substitution subs (node-catch-branch-pattern node))
   :body (tc:apply-substitution subs (node-catch-branch-body node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-catch))
  (declare (type tc:substitution-list subs)
           (values node-catch))
  (make-node-catch
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :expr (tc:apply-substitution subs (node-catch-expr node))
   :branches (tc:apply-substitution subs (node-catch-branches node))))

(defmethod tc:apply-substitution (subs (node node-resumable-branch))
  (declare (type tc:substitution-list subs)
           (values node-resumable-branch))
  (make-node-resumable-branch
   :pattern (tc:apply-substitution subs (node-resumable-branch-pattern node))
   :body (tc:apply-substitution subs (node-resumable-branch-body node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-resumable))
  (declare (type tc:substitution-list subs)
           (values node-resumable))
  (make-node-resumable
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :expr (tc:apply-substitution subs (node-resumable-expr node))
   :branches (tc:apply-substitution subs (node-resumable-branches node))))

(defmethod tc:apply-substitution (subs (node node-progn))
  (declare (type tc:substitution-list subs)
           (values node-progn))
  (make-node-progn
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :body (tc:apply-substitution subs (node-progn-body node))))

(defmethod tc:apply-substitution (subs (node node-unsafe))
  (declare (type tc:substitution-list subs)
           (values node-unsafe))
  (make-node-unsafe
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :body (tc:apply-substitution subs (node-unsafe-body node))))

(defmethod tc:apply-substitution (subs (node node-block))
  (declare (type tc:substitution-list subs)
           (values node-block))
  (make-node-block
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :name (node-block-name node)
   :body (tc:apply-substitution subs (node-block-body node))))

(defmethod tc:apply-substitution (subs (node node-return-from))
  (declare (type tc:substitution-list subs)
           (values node-return-from))
  (make-node-return-from
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :name (node-return-from-name node)
   :expr (tc:apply-substitution subs (node-return-from-expr node))))

(defmethod tc:apply-substitution (subs (node node-values))
  (declare (type tc:substitution-list subs)
           (values node-values))
  (make-node-values
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :nodes (tc:apply-substitution subs (node-values-nodes node))))

(defmethod tc:apply-substitution (subs (node node-application))
  (declare (type tc:substitution-list subs)
           (values node-application))
  (make-node-application
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :rator (tc:apply-substitution subs (node-application-rator node))
   :rands (tc:apply-substitution subs (node-application-rands node))
   :keyword-rands (tc:apply-substitution subs (node-application-keyword-rands node))))

(defmethod tc:apply-substitution (subs (node node-application-keyword-arg))
  (declare (type tc:substitution-list subs)
           (values node-application-keyword-arg))
  (make-node-application-keyword-arg
   :keyword (node-application-keyword-arg-keyword node)
   :value (tc:apply-substitution subs (node-application-keyword-arg-value node))))

(defmethod tc:apply-substitution (subs (node node-throw))
  (declare (type tc:substitution-list subs)
           (values node-throw))
  (make-node-throw
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :expr (tc:apply-substitution subs (node-throw-expr node))))

(defmethod tc:apply-substitution (subs (node node-resume-to))
  (declare (type tc:substitution-list subs)
           (values node-resume-to))
  (make-node-resume-to
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :expr (tc:apply-substitution subs (node-resume-to-expr node))))

(defmethod tc:apply-substitution (subs (node node-or))
  (declare (type tc:substitution-list subs)
           (values node-or))
  (make-node-or
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :nodes (tc:apply-substitution subs (node-or-nodes node))))

(defmethod tc:apply-substitution (subs (node node-and))
  (declare (type tc:substitution-list subs)
           (values node-and))
  (make-node-and
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :nodes (tc:apply-substitution subs (node-and-nodes node))))

(defmethod tc:apply-substitution (subs (node node-if))
  (declare (type tc:substitution-list subs)
           (values node-if))
  (make-node-if
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :expr (tc:apply-substitution subs (node-if-expr node))
   :then (tc:apply-substitution subs (node-if-then node))
   :else (tc:apply-substitution subs (node-if-else node))))

(defmethod tc:apply-substitution (subs (node node-when))
  (declare (type tc:substitution-list subs)
           (values node-when))
  (make-node-when
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :expr (tc:apply-substitution subs (node-when-expr node))
   :body (tc:apply-substitution subs (node-when-body node))))

(defmethod tc:apply-substitution (subs (node node-unless))
  (declare (type tc:substitution-list subs)
           (values node-unless))
  (make-node-unless
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :expr (tc:apply-substitution subs (node-unless-expr node))
   :body (tc:apply-substitution subs (node-unless-body node))))

(defmethod tc:apply-substitution (subs (node node-for))
  (declare (type tc:substitution-list subs)
           (values node-for))
  (make-node-for
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :label (node-for-label node)
   :bindings (tc:apply-substitution subs (node-for-bindings node))
   :sequential-p (node-for-sequential-p node)
   :returns (tc:apply-substitution subs (node-for-returns node))
   :termination-kind (node-for-termination-kind node)
   :termination-expr (tc:apply-substitution subs (node-for-termination-expr node))
   :body (tc:apply-substitution subs (node-for-body node))))

(defmethod tc:apply-substitution (subs (node node-break))
  (declare (type tc:substitution-list subs)
           (values node-break))
  node)

(defmethod tc:apply-substitution (subs (node node-continue))
  (declare (type tc:substitution-list subs)
           (values node-continue))
  node)

(defmethod tc:apply-substitution (subs (node node-cond-clause))
  (declare (type tc:substitution-list subs)
           (values node-cond-clause))
  (make-node-cond-clause
   :expr (tc:apply-substitution subs (node-cond-clause-expr node))
   :body (tc:apply-substitution subs (node-cond-clause-body node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-cond))
  (declare (type tc:substitution-list subs)
           (values node-cond))
  (make-node-cond
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :clauses (tc:apply-substitution subs (node-cond-clauses node))))

(defmethod tc:apply-substitution (subs (node node-do-bind))
  (declare (type tc:substitution-list subs)
           (values node-do-bind))
  (make-node-do-bind
   :pattern (tc:apply-substitution subs (node-do-bind-pattern node))
   :expr (tc:apply-substitution subs (node-do-bind-expr node))
   :location (source:location node)))

(defmethod tc:apply-substitution (subs (node node-do))
  (declare (type tc:substitution-list subs)
           (values node-do))
  (make-node-do
   :type (tc:apply-substitution subs (node-type node))
   :location (source:location node)
   :nodes (tc:apply-substitution subs (node-do-nodes node))
   :last-node (tc:apply-substitution subs (node-do-last-node node))))
