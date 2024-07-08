(defpackage #:coalton-impl/codegen/program
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/translate-expression
   #:translate-toplevel)
  (:import-from
   #:coalton-impl/codegen/translate-instance
   #:translate-instance)
  (:import-from
   #:coalton-impl/codegen/codegen-class
   #:codegen-class-definitions)
  (:import-from
   #:coalton-impl/codegen/codegen-expression
   #:codegen-expression)
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:codegen-type-definition)
  (:import-from
   #:coalton-impl/codegen/optimizer
   #:optimize-bindings)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:settings #:coalton-impl/settings)
   (#:global-lexical #:coalton-impl/global-lexical)
   (#:rt #:coalton-impl/runtime)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:compile-translation-unit))

(in-package #:coalton-impl/codegen/program)

;; The following functions control the output order of compiled
;; definitions and interleaved lisp expressions.
;;
;; Toplevel define and instance forms are compiled to 1 or more named,
;; lisp-source-valued output definitions: when these definitions are
;; generated, they are associated with the starting source offset of their
;; toplevel form:
;;
;;   toplevel definition:
;;     #<def .... (300 . 345)>
;;
;;   bindings (lisp definitions):
;;     (300 b1 .. bn)
;;
;; Then when compile-definitions emits the full set of output
;; definitions, any lisp source forms that occurred earlier in the
;; file are emitted first.

(defun bindings-offset (bindings offsets)
  "Given a list of binding names, and a name -> offset map, return the earliest binding start offset."
  (apply #'min
         (mapcar (lambda (binding)
                   (or (gethash (car binding) offsets) 0))
                 bindings)))

(defun merge-forms (lisp defs)
  "Merge a list of toplevel-lisp forms and compiled definitions, ensuring that lisp forms are always placed before the original source locations of the compiled definitions."
  (let (result)
    (loop :for def :in defs
          :for next-def-offset := (car def)
          :do (loop :while (< (caar lisp) next-def-offset)
                    :do (push (pop lisp) result))
              (push def result)
          :finally (loop :while lisp
                         :do (push (pop lisp) result)))
    (nreverse result)))

(defun compile-definitions (sccs definitions lisp-forms offsets env)
  "Compile SCCs and generate a final output definition list, merging any present lisp sources."
  (let ((bindings (loop :for scc :in sccs
                        :for bindings := (remove-if-not (lambda (binding)
                                                          (find (car binding) scc))
                                                        definitions)
                        :collect (cons (bindings-offset bindings offsets)
                                       (compile-scc bindings env))))
        (lisp-forms (mapcar (lambda (lisp-form)
                              (cons (car (parser:toplevel-lisp-form-source lisp-form))
                                    (parser:toplevel-lisp-form-body lisp-form)))
                            lisp-forms)))
    (mapcan #'cdr (merge-forms bindings lisp-forms))))

(defun definition-bindings (definitions env offsets)
  (loop :for define :in definitions
        :for offset := (car (tc:toplevel-define-source define))
        :for name := (tc:node-variable-name (tc:toplevel-define-name define))
        :for compiled-node := (translate-toplevel define env)

        :do (when settings:*coalton-dump-ast*
              (format t "~A :: ~A~%~A~%~%~%"
                      name
                      (tc:lookup-value-type env name)
                      (tc:binding-value define)))
            (setf (gethash name offsets) offset)
        :collect (cons name compiled-node)))

(defun instance-bindings (instances env offsets)
  (loop :for instance :in instances
        :for offset := (car (tc:toplevel-define-instance-source instance))
        :for instance-bindings := (translate-instance instance env)

        :do (dolist (binding instance-bindings)
              (setf (gethash (car binding) offsets) offset))
        :append instance-bindings))

(defun compile-translation-unit (translation-unit monomorphize-table env)
  (declare (type tc:translation-unit translation-unit)
           (type hash-table monomorphize-table)
           (type tc:environment env))

  (let* ((offsets (make-hash-table))
         (definitions
           (append
            (definition-bindings (tc:translation-unit-definitions translation-unit) env offsets)
            (instance-bindings (tc:translation-unit-instances translation-unit) env offsets)))
         (definition-names (mapcar #'car definitions)))

    (multiple-value-bind (definitions env)
        (optimize-bindings definitions monomorphize-table *package* env)

      (let ((sccs (node-binding-sccs definitions))
            (lisp-forms (tc:translation-unit-lisp-forms translation-unit)))

        (values
         `(progn
            ;; Muffle redefinition warnings in SBCL. A corresponding
            ;; SB-EXT:UNMUFFLE-CONDITIONS appears at the bottom.
            #+sbcl ,@(when settings:*emit-type-annotations*
                       (list '(declaim (sb-ext:muffle-conditions sb-kernel:redefinition-warning))))

            ,@(when (tc:translation-unit-types translation-unit)
                (list
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                    ,@(loop :for type :in (tc:translation-unit-types translation-unit)
                            :append (codegen-type-definition type env)))))

            ,@(when (tc:translation-unit-classes translation-unit)
                (list
                 `(eval-when (:compile-toplevel :load-toplevel :execute)
                    ,@(codegen-class-definitions
                       (tc:translation-unit-classes translation-unit)
                       env))))

            #+sbcl
            ,@(when (eq sb-ext:*block-compile-default* :specified)
                (list
                 `(declaim (sb-ext:start-block ,@definition-names))))

            ,@(compile-definitions sccs definitions lisp-forms offsets env)

            #+sbcl
            ,@(when (eq sb-ext:*block-compile-default* :specified)
                (list
                 `(declaim (sb-ext:end-block))))

            #+sbcl ,@(when settings:*emit-type-annotations*
                       (list '(declaim (sb-ext:unmuffle-conditions sb-kernel:redefinition-warning))))

            (values))
         env)))))

(defun compile-function (name node env)
  (declare (type symbol name)
           (type node-abstraction node)
           (type tc:environment env))
  (let ((type-decs
           (when settings:*emit-type-annotations*
             (append
              (loop :for name :in (node-abstraction-vars node)
                    :for i :from 0
                    :for arg-ty := (nth i (tc:function-type-arguments (node-type node)))
                    :collect `(type ,(tc:lisp-type arg-ty env) ,name))
              (list `(values ,(tc:lisp-type (node-type (node-abstraction-subexpr node)) env)
                             &optional))))))

    `(defun ,name ,(node-abstraction-vars node)
       (declare (ignorable ,@(node-abstraction-vars node))
                ,@type-decs)
       ,(codegen-expression (node-abstraction-subexpr node) name env))))

(defun compile-scc (bindings env)
  (declare (type binding-list bindings)
           (type tc:environment env))
  (append
   ;; Predeclare symbol macros
   (loop :for (name . node) :in bindings
         :collect `(global-lexical:define-global-lexical ,name ,(tc:lisp-type (node-type node) env)))

   ;; Compile functions
   (loop :for (name . node) :in bindings
         :if (node-abstraction-p node)
           :append (list
                    (compile-function name node env)
                    `(setf
                      ,name
                      ,(rt:construct-function-entry
                        `#',name (length (node-abstraction-vars node))))))

  ;; Compile variables
  (loop :for (name . node) :in bindings
        :if (not (node-abstraction-p node))
          :collect `(setf
                      ,name
                      ,(codegen-expression node nil env)))

  ;; Docstrings
  (loop :for (name . node) :in bindings
        :for entry := (tc:lookup-name env name :no-error t)
        :for type := (tc:lookup-value-type env name :no-error t)
        :for docstring
          := (cond
               ((and entry (tc:name-entry-docstring entry) type)
                (format nil "~A :: ~A~%~A" name type (tc:name-entry-docstring entry)))

               ((and entry (tc:name-entry-docstring entry))
                (tc:name-entry-docstring entry))

                (type
                 (format nil "~A :: ~A" name type)))
        :append (when docstring
                  (list `(setf (documentation ',name 'variable)
                               ,docstring)))
        :append (when (and entry (node-abstraction-p node))
                  (list `(setf (documentation ',name 'function)
                               ,docstring))))))
