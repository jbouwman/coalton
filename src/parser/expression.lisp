(defpackage #:coalton-impl/parser/expression
  (:use
   #:cl
   #:coalton-impl/source
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern
   #:coalton-impl/parser/macro)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util)
   (#:const #:coalton-impl/constants))
  (:export
   #:node                               ; STRUCT
   #:node-location                        ; ACCESSOR
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
   #:make-node-integerl-literal         ; CONSTRUCTOR
   #:node-integer-literal-value         ; ACCESSOR
   #:node-bind                          ; STRUCT
   #:make-node-bind                     ; CONSTRUCTOR
   #:node-bind-pattern                  ; ACCESSOR
   #:node-bind-expr                     ; ACCESSOR
   #:node-bind-location                   ; ACCESSOR
   #:node-body-element                  ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-body                          ; STRUCT
   #:make-node-body                     ; CONSTRUCTOR
   #:node-body-nodes                    ; ACCESSOR
   #:node-body-last-node                ; ACCESSOR
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-params            ; ACCESSOR
   #:node-abstraction-body              ; ACCESSOR
   #:node-abstraction-p                 ; FUNCTION
   #:node-let-binding                   ; STRUCT
   #:make-node-let-binding              ; CONSTRUCTOR
   #:node-let-binding-name              ; ACCESSOR
   #:node-let-binding-value             ; ACCESSOR
   #:node-let-binding-location            ; ACCESSOR
   #:node-let-binding-list              ; TYPE
   #:node-let-declare                   ; STRUCT
   #:make-node-let-declare              ; CONSTRUCTOR
   #:node-let-declare-name              ; ACCESSOR
   #:node-let-declare-type              ; ACCESSOR
   #:node-let-declare-location            ; ACCESSOR
   #:node-let-declare-list              ; TYPE
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-declares                  ; ACCESSOR
   #:node-let-body                      ; ACCESSOR
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-type                     ; ACCESSOR
   #:node-lisp-vars                     ; ACCESSOR
   #:node-lisp-var-names                ; ACCESSOR
   #:node-lisp-body                     ; ACCESSOR
   #:node-match-branch                  ; STRUCT
   #:make-node-match-branch             ; CONSTRUCTOR
   #:node-match-branch-pattern          ; ACCESSOR
   #:node-match-branch-body             ; ACCESSOR
   #:node-match-branch-location           ; ACCESSOR
   #:node-match-branch-list             ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-progn                         ; STRUCT
   #:make-node-progn                    ; CONSTRUCTOR
   #:node-progn-body                    ; ACCESSOR
   #:node-the                           ; STRUCT
   #:make-node-the                      ; CONSTRUCTOR
   #:node-the-type                      ; ACCESSOR
   #:node-the-expr                      ; ACCESSOR
   #:node-return                        ; STRUCT
   #:make-node-return                   ; CONSTRUCTOR
   #:node-return-expr                   ; ACCESSOR
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
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
   #:node-cond-clause                   ; STRUCT
   #:make-node-cond-clause              ; CONSTRUCTOR
   #:node-cond-clause-expr              ; ACCESSOR
   #:node-cond-clause-body              ; ACCESSOR
   #:node-cond-clause-location            ; ACCESSOR
   #:node-cond-clause-list              ; TYPE
   #:node-cond                          ; STRUCT
   #:make-node-cond                     ; CONSTRUCTOR
   #:node-cond-clauses                  ; ACCESSOR
   #:node-do-bind                       ; STRUCT
   #:make-node-do-bind                  ; CONSTRUCTOR
   #:node-do-bind-pattern               ; ACCESSOR
   #:node-do-bind-expr                  ; ACCESSOR
   #:node-do-bind-location                ; ACCESSOR
   #:node-do-body-element               ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-do                            ; STRUCT
   #:node-while                         ; STRUCT
   #:make-node-while                    ; CONSTRUCTOR
   #:node-while-label                   ; ACCESSOR
   #:node-while-expr                    ; ACCESSOR
   #:node-while-body                    ; ACCESSOR
   #:node-while-let                     ; STRUCT
   #:make-node-while-let                ; CONSTRUCTOR
   #:node-while-let-label               ; ACCESSOR
   #:node-while-let-pattern             ; ACCESSOR
   #:node-while-let-expr                ; ACCESSOR
   #:node-while-let-body                ; ACCESSOR
   #:node-loop                          ; STRUCT
   #:make-node-loop                     ; CONSTRUCTOR
   #:node-loop-body                     ; ACCESSOR
   #:node-loop-label                    ; ACCESSOR
   #:node-break                         ; STRUCT
   #:make-node-break                    ; CONSTRUCTOR
   #:node-break-label                   ; ACCESSOR
   #:node-continue                      ; STRUCT
   #:make-node-continue                 ; CONSTRUCTOR
   #:node-continue-label                ; ACCESSOR
   #:node-for                           ; STRUCT
   #:make-node-for                      ; CONSTRUCTOR
   #:node-for-label                     ; ACCESSOR
   #:node-for-pattern                   ; ACCESSOR
   #:node-for-expr                      ; ACCESSOR
   #:node-for-body                      ; ACCESSOR
   #:make-node-do                       ; CONSTRUCTOR
   #:node-do-nodes                      ; ACCESSOR
   #:node-do-last-node                  ; ACCESSOR
   #:parse-expression                   ; FUNCTION
   #:parse-body                         ; FUNCTION
   #:parse-variable                     ; FUNCTION
   ))

(in-package #:coalton-impl/parser/expression)

(defvar *macro-expansion-count* 0)

(declaim (type util:symbol-list *loop-label-context*))
(defvar *loop-label-context* nil
  "A list of known labels encountered during parse. 

Parsing (BREAK label) and (CONTINUE label) forms fails unless the label is found in
this list.

Rebound to NIL parsing an anonymous FN.")

(defconstant +macro-expansion-max+ 500)

;;;; # Expression Parsing
;;;;
;;;; Note that "expression" in the EBNF corresponds to the struct "node" in the lisp code.
;;;;
;;;; node-literal := <a lisp literal value>
;;;;
;;;; node-variable := <a lisp symbol not including "_" or starting with ".">
;;;;
;;;; node-accessor := <a lisp symbol starting with ".">
;;;;
;;;; ty := <defined in src/parser/types.lisp>
;;;;
;;;; qualified-ty := <defined in src/parser/types.lisp>
;;;;
;;;; pattern := <defined in src/parser/pattern.lisp>
;;;;
;;;; lisp-form := <an arbitrary lisp form>
;;;;
;;;; expression := node-variable
;;;;             | node-accessor
;;;;             | node-literal
;;;;             | node-abstraction
;;;;             | node-let
;;;;             | node-lisp 
;;;;             | node-match
;;;;             | node-progn
;;;;             | node-the
;;;;             | node-return
;;;;             | node-application
;;;;             | node-or
;;;;             | node-and
;;;;             | node-if
;;;;             | node-when
;;;;             | node-unless
;;;;             | node-cond
;;;;             | node-do
;;;;
;;;; node-bind := "(" "let" pattern "=" expression ")"
;;;;
;;;; node-body-element := expression | shorthand-let
;;;;
;;;; node-body := node-body-element* expression
;;;;
;;;; node-abstraction := "(" "fn" "(" pattern* ")" node-body ")"
;;;;
;;;; node-let-binding := "(" identifier expression ")"
;;;;
;;;; node-let-declare := "(" "declare" identifier qualified-ty ")"
;;;;
;;;; node-let := "(" "let" "(" (node-let-binding | node-let-declare)+ ")" body ")"
;;;;
;;;; node-lisp := "(" "lisp" type "(" variable* ")" lisp-form+ ")"
;;;;
;;;; node-match-branch := "(" pattern body ")"
;;;;
;;;; node-match := "(" "match" pattern match-branch* ")"
;;;;
;;;; node-progn := "(" "progn" body ")"
;;;;
;;;; node-the := "(" "the" type expression ")"
;;;;
;;;; node-return := "(" "return" expression? ")"
;;;;
;;;; node-application := "(" expression expression* ")"
;;;;
;;;; node-or := "(" "or" expression+ ")"
;;;;
;;;; node-and := "(" "and" expression+ ")"
;;;;
;;;; node-if := "(" "if" expression expression expression ")"
;;;;
;;;; node-when := "(" "when" expression body ")"
;;;;
;;;; node-unless := "(" "unless" expression body ")"
;;;;
;;;; node-cond-clause := "(" expression body ")"
;;;;
;;;; node-cond := "(" "cond" cond-clause+ ")"
;;;;
;;;; node-do-bind "(" pattern "<-" expression ")"
;;;;
;;;; node-do-body-element := expression
;;;;                       | node-bind
;;;;                       | node-do-bind
;;;;
;;;; node-do := node-do-body-element* expression

(defstruct (node
            (:constructor nil)
            (:copier nil))
  (location (util:required 'location) :type location :read-only t))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defstruct (node-variable
            (:include node)
            (:copier nil))
  (name (util:required 'name) :type identifier :read-only t))

(defun node-variable-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-variable-p x)))

(deftype node-variable-list ()
  '(satisfies node-variable-list-p))

(defstruct (node-accessor
            (:include node)
            (:copier nil))
  (name (util:required 'name) :type string :read-only t))

(defstruct (node-literal
            (:include node)
            (:copier nil))
  (value (util:required 'value) :type (and util:literal-value (not integer)) :read-only t))

(defstruct (node-integer-literal
            (:include node)
            (:copier nil))
  (value (util:required 'value) :type integer :read-only t))

;;
;; Does not subclass node, can only appear in a node body
;;
(defstruct (node-bind
            (:copier nil))
  (pattern (util:required 'pattern) :type pattern :read-only t)
  (expr    (util:required 'expr)    :type node    :read-only t)
  (location  (util:required 'location)  :type location    :read-only t))

(deftype node-body-element ()
  '(or node node-bind))

(defun node-body-element-p (x)
  (typep x 'node-body-element))

(defun node-body-element-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-body-element-p x)))

(deftype node-body-element-list ()
  '(satisfies node-body-element-list-p))

;;
;; Does not subclass node, can only appear directly within some nodes
;;
;; - must contain at least one node
;; - cannot be terminated by a `node-bind'
;; - does not have source information (but its children do)
;;
(defstruct (node-body
            (:copier nil))
  (nodes     (util:required 'nodes)     :type node-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                   :read-only t))

(defstruct (node-abstraction
            (:include node)
            (:copier nil))
  (params (util:required 'params) :type pattern-list :read-only t)
  (body   (util:required 'body)   :type node-body    :read-only t))

(defstruct (node-let-binding
            (:copier nil))
  (name   (util:required 'name)   :type node-variable   :read-only t)
  (value  (util:required 'value)  :type node            :read-only t)
  (location (util:required 'location) :type location :read-only t))

(defun node-let-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-let-binding-p x)))

(deftype node-let-binding-list ()
  '(satisfies node-let-binding-list-p))

(defstruct (node-let-declare
            (:copier nil))
  (name   (util:required 'name)   :type node-variable   :read-only t)
  (type   (util:required 'type)   :type qualified-ty    :read-only t)
  (location (util:required 'location) :type location :read-only t))

(defun node-let-declare-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-let-declare-p x)))

(deftype node-let-declare-list ()
  '(satisfies node-let-declare-list-p))

(defstruct (node-let
            (:include node)
            (:copier nil))
  (bindings (util:required 'bindings) :type node-let-binding-list :read-only t)
  (declares (util:required 'declares) :type node-let-declare-list :read-only t)
  (body     (util:required 'body)     :type node-body             :read-only t))

(defstruct (node-lisp
            (:include node)
            (:copier nil))
  (type      (util:required 'type)      :type ty                 :read-only t)
  (vars      (util:required 'vars)      :type node-variable-list :read-only t)
  (var-names (util:required 'var-names) :type util:symbol-list   :read-only t)
  (body      (util:required 'body)      :type t                  :read-only t))

(defstruct (node-match-branch
            (:copier nil))
  (pattern (util:required 'pattern) :type pattern         :read-only t)
  (body    (util:required 'body)    :type node-body       :read-only t)
  (location  (util:required 'location)  :type location :read-only t))

(defun node-match-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-match-branch-p x)))

(deftype node-match-branch-list ()
  '(satisfies node-match-branch-list-p))

(defstruct (node-match
            (:include node)
            (:copier nil))
  (expr     (util:required 'expr)     :type node                   :read-only t)
  (branches (util:required 'branches) :type node-match-branch-list :read-only t))

(defstruct (node-progn
            (:include node)
            (:copier nil))
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-the
            (:include node)
            (:copier nil))
  (type (util:required 'type) :type ty   :read-only t)
  (expr (util:required 'expr) :type node :read-only t))

(defstruct (node-return
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type (or null node) :read-only t))

(defstruct (node-application
            (:include node)
            (:copier nil))
  (rator (util:required 'rator) :type node      :read-only t)
  (rands (util:required 'rands) :type node-list :read-only t))

(defstruct (node-or
            (:include node)
            (:copier nil))
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-and
            (:include node)
            (:copier nil))
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-if
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node :read-only t)
  (then (util:required 'expr) :type node :read-only t)
  (else (util:required 'else) :type node :read-only t))

(defstruct (node-when
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node      :read-only t)
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-unless
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node      :read-only t)
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-cond-clause
            (:copier nil))
  (expr   (util:required 'expr)   :type node            :read-only t)
  (body   (util:required 'body)   :type node-body       :read-only t)
  (location (util:required 'location) :type location :read-only t))

(defun node-cond-clause-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-cond-clause-p x)))

(deftype node-cond-clause-list ()
  '(satisfies node-cond-clause-list-p))

(defstruct (node-cond
            (:include node)
            (:copier nil))
  (clauses (util:required 'clauses) :type node-cond-clause-list :read-only t))

(defstruct (node-do-bind
            (:copier nil))
  (pattern (util:required 'name)   :type pattern         :read-only t)
  (expr    (util:required 'expr)   :type node            :read-only t)
  (location  (util:required 'location) :type location :read-only t))

(deftype node-do-body-element ()
  '(or node node-bind node-do-bind))

(defun node-do-body-element-p (x)
  (typep x 'node-do-body-element))

(defun node-do-body-element-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-do-body-element-p x)))

(deftype node-do-body-element-list ()
  '(satisfies node-do-body-element-list-p))

(defstruct (node-do
            (:include node)
            (:copier nil))
  (nodes     (util:required 'nodes)     :type node-do-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                      :read-only t))

(defstruct (node-while
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword   :read-only t)
  (expr  (util:required 'expr)  :type node      :read-only t)
  (body  (util:required 'body)  :type node-body :read-only t))

(defstruct (node-while-let
            (:include node)
            (:copier nil))
  (label   (util:required 'label)   :type keyword   :read-only t)
  (pattern (util:required 'pattern) :type pattern   :read-only t)
  (expr    (util:required 'expr)    :type node      :read-only t)
  (body    (util:required 'body)    :type node-body :read-only t))

(defstruct (node-break
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-continue
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-loop
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword   :read-only t)
  (body  (util:required 'body)  :type node-body :read-only t))

(defstruct (node-for
            (:include node)
            (:copier nil))
  (label   (util:required 'label)   :type keyword   :read-only t)
  (pattern (util:required 'pattern) :type pattern   :read-only t)
  (expr    (util:required 'expr)    :type node      :read-only t)
  (body    (util:required 'body)    :type node-body :read-only t))


(defun parse-expression (form source)
  (declare (type cst:cst form)
           (values node &optional))

  (cond
    ;;
    ;; Atoms
    ;;

    ((cst:atom form)
     (typecase (cst:raw form)
       (null
        (source-error "Malformed expression"
                      (make-note (make-location source form)
                                 "unexpected `nil` or `()`")))

       (symbol
        (if (char= #\. (aref (symbol-name (cst:raw form)) 0))
            (parse-accessor form source)
            (parse-variable form source)))

       (t
        (parse-literal form source))))

    ;;
    ;; Dotted Lists
    ;;

    ((not (cst:proper-list-p form))
     (source-error "Malformed expression"
                   (make-note (make-location source form)
                              "unexpected dotted list")))

    ;;
    ;; Keywords
    ;;


    ((and (cst:atom (cst:first form))
          (eq 'coalton:fn (cst:raw (cst:first form))))
     (let ((params)
           (body))

       ;; (fn)
       (unless (cst:consp (cst:rest form))
         (source-error "Malformed function"
                       (make-note (make-location source form)
                                  "expected function arguments"
                                  :end t)))

       ;; (fn (...))
       (unless (cst:consp (cst:rest (cst:rest form)))
         (source-error "Malformed function"
                       (make-note (make-location source form)
                                  "expected function body"
                                  :end t)))

       ;; (fn x ...)
       ;;
       ;; NOTE: (fn () ...) is allowed
       (when (and (cst:atom (cst:second form))
                  (not (null (cst:raw (cst:second form)))))
         (source-error "Malformed function"
                       (make-note (make-location source (cst:second form))
                                  "malformed argument list")
                       (make-help (make-location source (cst:second form))
                                  "add parentheses"
                                  (lambda (existing)
                                    (concatenate 'string "(" existing ")")))))
       ;; Bind *LOOP-LABEL-CONTEXT* to NIL to disallow BREAKing from
       ;; or CONTINUING with loops that enclose the FN form.
       (let ((*loop-label-context* nil))
         (setf params
               (loop :for vars := (cst:second form) :then (cst:rest vars)
                     :while (cst:consp vars)
                     :collect (parse-pattern (cst:first vars) source)))
         (setf body (parse-body (cst:nthrest 2 form) form source))
         (make-node-abstraction
          :params params
          :body body
          :location (make-location source form)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:let (cst:raw (cst:first form))))

     ;; (let)
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed let"
                     (make-note (make-location source form)
                                "expected let binding list"
                                :end t)))

     ;; (let (...))
     (unless (cst:consp (cst:rest (cst:rest form)))
       (source-error "Malformed let"
                     (make-note (make-location source form)
                                "expected let body"
                                :end t)))

     (unless (cst:proper-list-p (cst:second form)) 
       (source-error "Malformed let"
                     (make-note (make-location source (cst:second form))
                                "expected binding list")))

     (let* (declares

            (bindings (loop :for bindings := (cst:second form) :then (cst:rest bindings)
                            :while (cst:consp bindings)
                            :for binding := (cst:first bindings)
                            ;; if binding is in the form (declare x y+)
                            :if (and (cst:consp binding)
                                     (cst:consp (cst:rest form))
                                     (cst:consp (cst:rest (cst:rest form)))
                                     (cst:atom (cst:first binding))
                                     (eq (cst:raw (cst:first binding)) 'coalton:declare))
                              :do (push (parse-let-declare binding source) declares)
                            :else
                              :collect (parse-let-binding binding source))))

       (make-node-let
        :bindings bindings
        :declares (nreverse declares)
        :body (parse-body (cst:nthrest 2 form) form source)
        :location (make-location source form))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:lisp (cst:raw (cst:first form))))
     ;; (lisp)
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed lisp expression"
                     (make-note (make-location source form)
                                "expected expression type" :end t)))

     ;; (lisp T)
     (unless (cst:consp (cst:rest (cst:rest form)))
       (source-error "Malformed lisp expression"
                     (make-note (make-location source form)
                                "expected binding list" :end t)))

     ;; (lisp T (...))
     (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
       (source-error "Malformed lisp expression"
                     (make-note (make-location source form)
                                "expected body")))

     (let ((vars (loop :for vars := (cst:third form) :then (cst:rest vars)
                       :while (cst:consp vars)
                       :collect (parse-variable (cst:first vars) source))))
       (make-node-lisp
        :type (parse-type (cst:second form) source)
        :vars vars
        :var-names (mapcar #'node-variable-name vars)
        :body (cst:raw (cst:nthrest 3 form))
        :location (make-location source form))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:match (cst:raw (cst:first form))))

     ;; (match)
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed match expression"
                     (make-note (make-location source form)
                                "expected expression"
                                :end t)))

     (make-node-match
      :expr (parse-expression (cst:second form) source)
      :branches (loop :for branches := (cst:nthrest 2 form) :then (cst:rest branches)
                      :while (cst:consp branches)
                      :collect (parse-match-branch (cst:first branches) source))
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:progn (cst:raw (cst:first form))))
     (make-node-progn
      :body (parse-body (cst:rest form) form source)
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:the (cst:raw (cst:first form))))
     ;; (the)
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed the expression"
                     (make-note (make-location source form)
                                "expected type"
                                :end t)))

     ;; (the T)
     (unless (cst:consp (cst:rest (cst:rest form)))
       (source-error "Malformed the expression"
                     (make-note (make-location source form)
                                "expected value"
                                :end t)))

     ;; (the a b c)
     (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
       (source-error "Malformed the expression"
                     (make-note (make-location source (cst:source (cst:first (cst:rest (cst:rest (cst:rest form)))))) ; TODO grep for make-location cst:source
                                "unexpected trailing form")))

     (make-node-the
      :type (parse-type (cst:second form) source)
      :expr (parse-expression (cst:third form) source)
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:return (cst:raw (cst:first form))))
     (let (expr)

       ;; (return ...)
       (when (cst:consp (cst:rest form))
         ;; (return a b ...)
         (when (cst:consp (cst:rest (cst:rest form)))
           (source-error "Malformed return expression"
                         (make-note (make-location source (cst:source (cst:first (cst:rest (cst:rest form)))))
                                    "unexpected trailing form")))

         (setf expr (parse-expression (cst:second form) source)))

       (make-node-return
        :expr expr
        :location (make-location source form))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:or (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed or expression"
                     (make-note (make-location source form)
                                "expected one or more arguments"
                                :end t)))

     (make-node-or
      :nodes (loop :for args := (cst:rest form) :then (cst:rest args)
                   :while (cst:consp args)
                   :for arg := (cst:first args)
                   :collect (parse-expression arg source))
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:and (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed and expression"
                     (make-note (make-location source form)
                                "expected one or more arguments"
                                :end t)))

     (make-node-and
      :nodes (loop :for args := (cst:rest form) :then (cst:rest args)
                   :while (cst:consp args)
                   :for arg := (cst:first args)
                   :collect (parse-expression arg source))
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:if (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed if expression"
                     (make-note (make-location source form)
                                "expected a predicate"
                                :end t)))

     (unless (cst:consp (cst:rest (cst:rest form)))
       (source-error "Malformed if expression"
                     (make-note (make-location source form)
                                "expected a form"
                                :end t)))

     (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
       (source-error "Malformed if expression"
                     (make-note (make-location source form)
                                "expected a form"
                                :end t)))

     (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
       (source-error "Malformed if expression"
                     (make-note (make-location source (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form))))))
                                "unexpected trailing form"
                                :end t)))

     (make-node-if
      :expr (parse-expression (cst:second form) source)
      :then (parse-expression (cst:third form) source)
      :else (parse-expression (cst:fourth form) source)
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:when (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed when expression"
                     (make-note (make-location source form)
                                "expected a predicate"
                                :end t)))

     (make-node-when
      :expr (parse-expression (cst:second form) source)
      :body (parse-body (cst:rest (cst:rest form)) form source)
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:unless (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed unless expression"
                     (make-note (make-location source form)
                                "expected a predicate"
                                :end t)))

     (make-node-unless
      :expr (parse-expression (cst:second form) source)
      :body (parse-body (cst:rest (cst:rest form)) form source)
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:cond (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (source-error "Malformed cond expression"
                     (make-note (make-location source form)
                                "expected one or more clauses"
                                :end t)))

     (make-node-cond
      :clauses (loop :for clauses := (cst:rest form) :then (cst:rest clauses)
                     :while (cst:consp clauses)
                     :for clause := (cst:first clauses)
                     :collect (parse-cond-clause clause source))
      :location (make-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:do (cst:raw (cst:first form))))
     (parse-do form source))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:while (cst:raw (cst:first form))))

     (multiple-value-bind (label labelled-body) (take-label form)
       ;; (while [label])
       (unless (cst:consp labelled-body)
         (source-error "Malformed while expression"
                       (make-note (make-location source form)
                                  "expected condition"
                                  :end t)))
       ;; (while [label] condition)
       (unless (cst:consp (cst:rest labelled-body))
         (source-error "Malformed while expression"
                       (make-note (make-location source form)
                                  "expected body"
                                  :end t)))
       (let ((*loop-label-context*
               (if label 
                   (list* label const:+default-loop-label+ *loop-label-context*)
                   (cons const:+default-loop-label+ *loop-label-context*))))

         (make-node-while
          :location (make-location source form)
          :label (or label const:+default-loop-label+)
          :expr (parse-expression (cst:first labelled-body) source)
          :body (parse-body (cst:rest labelled-body) form source)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:while-let (cst:raw (cst:first form))))

     (multiple-value-bind (label labelled-body) (take-label form)
       ;; (while-let [label])
       (unless (cst:consp labelled-body)
         (source-error "Malformed while-let expression"
                       (make-note (make-location source form)
                                  "expected pattern"
                                  :end t))) 

       ;; (while-let [label] pattern)
       (unless (and (cst:consp (cst:rest labelled-body))
                    (eq 'coalton:= (cst:raw (cst:second labelled-body))))
         (source-error "Malformed while-let expression"
                       (make-note (make-location source form)
                                  "expected ="
                                  :end t)))
       
       ;; (when-let [label] pattern =)
       (unless (cst:consp (cst:nthrest 2 labelled-body))
         (source-error "Malformed while-let expression"
                       (make-note (make-location source form)
                                  "expected expression"
                                  :end t)))
       
       ;; (when-let pattern = expr)
       (unless (cst:consp (cst:nthrest 3 labelled-body))
         (source-error "Malformed while-let expression"
                       (make-note (make-location source form)
                                  "expected body"
                                  :end t)))
       (let* ((*loop-label-context*
                (if label
                    (list* label const:+default-loop-label+ *loop-label-context*)
                    (cons const:+default-loop-label+ *loop-label-context*))))
         (make-node-while-let
          :location (make-location source form)
          :label (or label const:+default-loop-label+)
          :pattern (parse-pattern (cst:first labelled-body) source) 
          :expr (parse-expression (cst:third labelled-body) source)
          :body (parse-body (cst:nthrest 3 labelled-body) form source)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:loop (cst:raw (cst:first form))))
     (multiple-value-bind (label labelled-body) (take-label form)
       (unless (cst:consp labelled-body)
         (source-error "Malformed loop expression"
                       (make-note (make-location source form)
                                  "expected a loop body"
                                  :end t)))

       (let* ((*loop-label-context*
                (if label
                    (list* label const:+default-loop-label+ *loop-label-context*)
                    (cons const:+default-loop-label+ *loop-label-context*))))
         (make-node-loop
          :location (make-location source form)
          :label (or label const:+default-loop-label+)
          :body (parse-body labelled-body form source)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:break (cst:raw (cst:first form))))

     (multiple-value-bind (label postlabel) (take-label form)
       (unless (cst:null postlabel)
         (source-error "Invalid argument in break"
                       (make-note (make-location source form)
                                  (if label
                                      "unexpected argument after label"
                                      "expected a keyword"))))

       (if label
           (unless (member label *loop-label-context*)
             (source-error "Invalid label in break"
                           (make-note (make-location source (cst:second form))
                                      "label not found in any enclosing loop")))
           (unless *loop-label-context*
             (source-error "Invalid break"
                           (make-note (make-location source form)
                                      "break does not appear in an enclosing loop"))))
       
       (make-node-break :location (make-location source form)
                        :label (or label (car *loop-label-context*)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:continue (cst:raw (cst:first form))))

     (multiple-value-bind (label postlabel) (take-label form)
       (unless (cst:null postlabel)
         (source-error "Invalid argument in continue"
                       (make-note (make-location source form)
                                  (if label
                                      "unexpected argument after label"
                                      "expected a keyword"))))

       (if label
           (unless (member label *loop-label-context*)
             (source-error "Invalid label in continue"
                           (make-note (make-location source (cst:second form))
                                      "label not found in any enclosing loop")))
           (unless *loop-label-context*
             (source-error "Invalid continue"
                           (make-note (make-location source form)
                                      "continue does not appear in an enclosing loop"))))
       
       (make-node-continue :location (make-location source form)
                           :label (or label (car *loop-label-context*)))))
    

    ((and (cst:atom (cst:first form))
          (eq 'coalton:for (cst:raw (cst:first form))))

     (multiple-value-bind (label labelled-body) (take-label form)
       ;; (for [label])
       (unless (cst:consp labelled-body)
         (source-error "Malformed for expression"
                       (make-note (make-location source form)
                                  "expected pattern"
                                  :end t))) 
       
       ;; (for [label] pattern)
       (unless (and (cst:consp (cst:rest labelled-body))
                    (cst:atom (cst:second labelled-body))
                    (eq 'coalton:in (cst:raw (cst:second labelled-body))))
         (source-error "Malformed for expression"
                       (make-note (make-location source form)
                                  "expected in"
                                  :end t)))

       ;; (for [label] pattern in)
       (unless (cst:consp (cst:nthrest 2 labelled-body))
         (source-error "Malformed for expression"
                       (make-note (make-location source form)
                                  "expected expression"
                                  :end t)))
       
       ;; (for [label] pattern in expr)
       (unless (cst:consp (cst:nthrest 3 labelled-body))
         (source-error "Malformed for expression"
                       (make-note (make-location source form)
                                  "expected body"
                                  :end t)))
       
       (let ((*loop-label-context*
               (if label
                   (list* label const:+default-loop-label+ *loop-label-context*)
                   (cons const:+default-loop-label+ *loop-label-context*))))
         (make-node-for
          :location (make-location source form)
          :label (or label const:+default-loop-label+)
          :pattern (parse-pattern (cst:first labelled-body) source) 
          :expr (parse-expression (cst:third labelled-body) source)
          :body (parse-body (cst:nthrest 3 labelled-body) form  source)))))

    ;;
    ;; Macros
    ;;

    ((and (cst:atom (cst:first form))
          (symbolp (cst:raw (cst:first form)))
          (macro-function (cst:raw (cst:first form))))

     (let ((*macro-expansion-count* (+ 1 *macro-expansion-count*)))

       (when (= *macro-expansion-count* +macro-expansion-max+)
         (source-error "Invalid macro expansion"
                       (make-note (make-location source form)
                                  "macro expansion limit hit")))

       (with-context (:macro
                      "Error occurs within macro context. Source locations may be imprecise")
         (parse-expression (expand-macro form) source))))

    ;;
    ;; Function Application
    ;;

    (t
     (make-node-application
      :rator (parse-expression (cst:first form) source)
      :rands (loop :for rands := (cst:rest form) :then (cst:rest rands)
                   :while (cst:consp rands)
                   :for rand := (cst:first rands)
                   :collect (parse-expression rand source))
      :location (make-location source form)))))

(defun parse-variable (form source)
  (declare (type cst:cst form)
           (values node-variable &optional))

  (unless (and (cst:atom form)
               (identifierp (cst:raw form)))
    (source-error "Invalid variable"
                  (make-note (make-location source form)
                             "expected identifier")))

  (when (string= "_" (symbol-name (cst:raw form)))
    (source-error "Invalid variable"
                  (make-note (make-location source form)
                             "invalid variable name '_'")))

  (when (char= #\. (aref (symbol-name (cst:raw form)) 0))
    (source-error "Invalid variable"
                  (make-note (make-location source form)
                             "variables cannot start with '.'")))

  (make-node-variable
   :name (cst:raw form)
   :location (make-location source form)))

(defun parse-accessor (form source)
  (declare (type cst:cst form)
           (values node-accessor))

  (assert (cst:atom form))
  (assert (symbolp (cst:raw form)))
  (assert (char= #\. (aref (symbol-name (cst:raw form)) 0)))

  (make-node-accessor
   :name (subseq (symbol-name (cst:raw form)) 1)
   :location (make-location source form)))

(defun parse-literal (form source)
  (declare (type cst:cst form)
           (values node &optional))

  (assert (cst:atom form))

  (typecase (cst:raw form)
    (integer
     (make-node-integer-literal
      :value (cst:raw form)
      :location (make-location source form)))

    (util:literal-value
     (make-node-literal
      :value (cst:raw form)
      :location (make-location source form)))

    (t
     (source-error "Invalid literal"
                   (make-note (make-location source form)
                              "unknown literal type")))))

(defun parse-body (form enclosing-form source)
  (declare (type cst:cst form)
           (values node-body &optional))

  (when (cst:atom form)
    (source-error "Malformed function"
                  (make-note (make-location source enclosing-form)
                             "expected body"
                             :end t)))

  (assert (cst:proper-list-p form))

  (let* (last-node

         (nodes (loop :for nodes := form :then (cst:rest nodes)
                      :while (cst:consp nodes)

                      ;; Not the last node
                      :if (cst:consp (cst:rest nodes))
                        :collect (parse-body-element (cst:first nodes) source)

                      ;; The last node
                      :else
                        :do (setf last-node (parse-body-last-node (cst:first nodes) source)))))

    
    (make-node-body
     :nodes nodes
     :last-node last-node)))

(defun shorthand-let-p (form)
  "Returns t if FORM is in the form of (let x = y+)"
  (declare (type cst:cst form)
           (values boolean))

  (cond
    ((cst:atom form)
     nil)

    ;; (let)
    ((not (cst:consp (cst:rest form)))
     nil)

    ;; (let x)
    ((not (cst:consp (cst:rest (cst:rest form))))
     nil)

    ;; (let x =)
    ((not (cst:consp (cst:rest (cst:rest (cst:rest form)))))
     nil)

    (t
     (and (cst:atom (cst:first form))
          (eq (cst:raw (cst:first form)) 'coalton:let)
          (cst:atom (cst:third form))
          (eq (cst:raw (cst:third form)) 'coalton:=)))))

;; Forms passed to parse-node-bind must be previously verified by `shorthand-let-p'
(defun parse-node-bind (form source)
  (declare (type cst:cst form)
           (values node-bind))

  (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
    (source-error "Malformed shorthand let"
                  (make-note (make-location source (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form))))))
                             "unexpected trailing form")))

  (make-node-bind
   :pattern (parse-pattern (cst:second form) source)
   :expr (parse-expression (cst:fourth form) source)
   :location (make-location source form)))

(defun parse-body-element (form source)
  (declare (type cst:cst form)
           (values node-body-element &optional))

  (when (cst:atom form)
    (return-from parse-body-element
      (parse-expression form source)))

  (unless (cst:proper-list-p form)
    (source-error "Malformed body expression"
                  (make-note (make-location source form)
                             "unexpected dotted list")))


  (if (shorthand-let-p form)
      (parse-node-bind form source)
      (parse-expression form source)))

(defun parse-body-last-node (form source)
  (declare (type cst:cst form)
           (values node &optional))

  (when (shorthand-let-p form)
    (source-error "Malformed body expression"
                  (make-note (make-location source form)
                             "body forms cannot be terminated by a shorthand let")))

  (parse-expression form source))

(defun parse-let-binding (form source)
  (declare (type cst:cst form)
           (values node-let-binding &optional))

  (when (cst:atom form)
    (source-error "Malformed let binding"
                  (make-note (make-location source form)
                             "expected list")))

  (unless (cst:proper-list-p form)
    (source-error "Malformed let binding"
                  (make-note (make-location source form)
                             "unexpected dotted list")))

  ;; (x)
  (unless (cst:consp (cst:rest form))
    (source-error "Malformed let binding"
                  (make-note (make-location source form)
                             "let bindings must have a value"
                             :end t)))

  ;; (a b c ...)
  (when (cst:consp (cst:rest (cst:rest form)))
    (source-error "Malformed let binding"
                  (make-note (make-location source (cst:first (cst:rest (cst:rest form))))
                             "unexpected trailing form")))

  (make-node-let-binding
   :name (parse-variable (cst:first form) source)
   :value (parse-expression (cst:second form) source)
   :location (make-location source form)))

(defun parse-match-branch (form source)
  (declare (type cst:cst form)
           (values node-match-branch &optional))

  (when (cst:atom form)
    (source-error "Malformed match branch"
                  (make-note (make-location source form)
                             "expected list")))

  (unless (cst:proper-list-p form)
    (source-error "Malformed match branch"
                  (make-note (make-location source form)
                             "unexpected dotted list")) )

  ;; (P)
  (unless (cst:consp (cst:rest form))
    (source-error "Malformed match branch"
                  (make-note (make-location source form)
                             "expected body"
                             :end t)))

  (make-node-match-branch
   :pattern (parse-pattern (cst:first form) source)
   :body (parse-body (cst:rest form) form source)
   :location (make-location source form)))

(defun parse-cond-clause (form source)
  (declare (type cst:cst form)
           (values node-cond-clause))

  (when (cst:atom form)
    (source-error "Malformed cond clause"
                  (make-note (make-location source form)
                             "expected list")))

  (unless (cst:proper-list-p form)
    (source-error "Malformed cond clause"
                  (make-note (make-location source form)
                             "unexpected dotted list")))

  (make-node-cond-clause
   :expr (parse-expression (cst:first form) source)
   :body (parse-body (cst:rest form) form source)
   :location (make-location source form)))

(defun parse-do (form source)
  (declare (type cst:cst form))

  (assert (cst:consp form))

  (unless (cst:consp (cst:rest form))
    (source-error "Malformed do expression"
                  (make-note (make-location source form)
                             "expected one or more forms"
                             :end t)))

  (let* (last-node

         (nodes (loop :for nodes := (cst:rest form) :then (cst:rest nodes)
                      :while (cst:consp nodes)
                      :for node := (cst:first nodes)

                      ;; Not the last node
                      :if (cst:consp (cst:rest nodes))
                        :collect (parse-do-body-element node source)

                      :else
                        :do (setf last-node (parse-do-body-last-node node (cst:first form) source)))))

    (make-node-do
     :nodes nodes
     :last-node last-node
     :location (make-location source form))))

(defun do-bind-p (form)
  "Returns t if FORM is in the form of (x <- y+)"
  (declare (type cst:cst form)
           (values boolean))

  (cond
    ((not (cst:consp form))
     nil)

    ;; (x)
    ((not (cst:consp (cst:rest form)))
     nil)

    ;; (x y)
    ((not (cst:consp (cst:rest (cst:rest form))))
     nil)

    ;; (x (y) ...)
    ((not (cst:atom (cst:second form)))
     nil)

    (t
     (eq 'coalton:<- (cst:raw (cst:second form))))))

;; Forms passed to this function must first be validated with `do-bind-p'
(defun parse-node-do-bind (form source)
  (declare (type cst:cst form)
           (values node-do-bind))

  (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (source-error "Malformed bind form"
                  (make-note (make-location source (cst:first (cst:rest (cst:rest (cst:rest form)))))
                             "unexpected trailing form")))

  (make-node-do-bind
   :pattern (parse-pattern (cst:first form) source)
   :expr (parse-expression (cst:third form) source)
   :location (make-location source form)))

(defun parse-do-body-element (form source)
  (declare (type cst:cst form)
           (values node-do-body-element &optional))

  (cond
    ((shorthand-let-p form)
     (parse-node-bind form source))

    ((do-bind-p form)
     (parse-node-do-bind form source))

    (t
     (parse-expression form source))))

(defun parse-do-body-last-node (form parent-form source)
  (declare (type cst:cst form)
           (type cst:cst parent-form)
           (values node &optional))

  (when (shorthand-let-p form)
    (source-error "Malformed do expression"
                  (make-note (make-location source form)
                             "do expressions cannot be terminated by a shorthand let")
                  (make-note (make-location source parent-form)
                             "when parsing do expression")))

  (when (do-bind-p form)
    (source-error "Malformed do expression"
                  (make-note (make-location source form)
                             "do expression cannot be terminated by a bind")
                  (make-note (make-location source parent-form)
                             "when parsing do expression")))

  (parse-expression form source))

(defun parse-let-declare (form source)
  (declare (type cst:cst form)
           (values node-let-declare))

  (assert (cst:consp form))
  (assert (cst:consp (cst:rest form)))
  (assert (cst:consp (cst:rest (cst:rest form))))

  (assert (cst:atom (cst:first form)))
  (assert (eq (cst:raw (cst:first form)) 'coalton:declare))

  (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (source-error "Malformed declare"
                  (make-note (make-location source (cst:fourth form))
                             "unexpected form")))


  (make-node-let-declare
   :name (parse-variable (cst:second form) source)
   :type (parse-qualified-type (cst:third form) source)
   :location (make-location source form)))

(defun take-label (form)
  "Takes form (HEAD . (MAYBEKEYWORD . REST)) and returns two values,
either

MAYBEKEYWORD REST 

if MAYBEKEYWORD is a keyword, or else

NIL (MAYBEKEYWORD . REST)  

if (CST:SECOND FORM) is not a keyword."
  (declare (type cst:cst form)
           (values (or keyword null) cst:cst))
  (if (and (cst:consp (cst:rest form))
           (cst:atom (cst:second form))
           (keywordp (cst:raw (cst:second form))))
      (values (cst:raw (cst:second form))
              (cst:nthrest 2 form))
      (values nil (cst:rest form))))
